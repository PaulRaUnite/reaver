

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="ApronLp";
              Log.level=Log.Debug3}


exception NotSupported of string

module LpX = struct
  type x = string
  let string_of_x x = x
end

module TheLp = Ocamlqsopt_ex.Lp.Make(LpX)
module MyDualizer = Ocamlqsopt_ex.Lp.Dualizer(TheLp)

type lp_expr_t = TheLp.expr
type lp_conss_t = TheLp.constrs
type lp_cons_t = TheLp.constr
type lp_obj_fct_t = TheLp.obj

type obj_val_t = Apron.Coeff.t
type solution_t = (ApronUtil.var_t -> Apron.Coeff.t)

let rec eliminate_const_lst lst =
    List.fold_left (fun (flag, flst) l -> (eliminate_const_expr_lst l (flag, flst)) ) (false, []) lst 

and eliminate_const_expr_lst exp (flag, acc) =
  match exp with
  | TheLp.Const(x) -> (flag, acc) (* ok im doing this cause it forces constnts to be removed *)
  | _ -> let (f, e') = (eliminate_const_expr exp flag) in (flag || f, e'::acc)

and eliminate_const_expr exp flag =
  match exp with
  TheLp.Sum(slst) -> 
    let (inner_flag, inner_lst) = (eliminate_const_lst slst ) in
      (flag || inner_flag , TheLp.Sum(inner_lst))
  | TheLp.Mul(a, exp) -> 
                  let (inner_flag, inner_exp) = eliminate_const_expr exp flag in
                    ((flag || inner_flag) , TheLp.Mul(a, inner_exp ))
  | TheLp.Var(v) -> (true, TheLp.Var(v))
  | TheLp.Const(x) -> (flag, TheLp.Const(x))

let eliminate_sub_exp e1 e2 = 
  let (f1, e1') = eliminate_const_expr e1 false in 
  let (f2, e2') = eliminate_const_expr e2 false in 
  if f1 = true && f2 = true then (e1', e2') else
  if f1 = false && f2 = true then (e1, e2') else
  if f1 = true && f2 = false then (e1', e2) else
  (e1, e2)

let rec eliminate_const_expr exp =
  match exp with
    TheLp.Sum(es) -> TheLp.Sum(List.map eliminate_const_expr es)
  | TheLp.Mul(a, exp) -> TheLp.Mul(a, eliminate_const_expr exp)
  | TheLp.Var(_) as v -> v 
  | TheLp.Const(x) -> TheLp.Const(Maxstratutil.Num.GmpRatio.zero) 

let eliminate_constants_constr constr =
   match constr with
       TheLp.Le (e1, e2) -> let (e1', e2') = (eliminate_const_expr e1, eliminate_const_expr e2) in TheLp.Le (e1', e2')
     | TheLp.Eq (e1, e2) -> let (e1', e2') = (eliminate_const_expr e1, eliminate_const_expr e2) in TheLp.Eq(e1', e2')

let eliminate_constants_constrs constrs = List.map (fun a -> (eliminate_constants_constr a) ) constrs

(*****************************************************************************)
(********************** expression conversion ********************************)
(*****************************************************************************)

(* converts APRON coefficients into LP coefficients *)
let scalar_to_numinf c =
  if (Apron.Scalar.is_infty c>0) then Maxstratutil.Num.InfGmpRatio.Infty
  else if (Apron.Scalar.is_infty c<0) then Maxstratutil.Num.InfGmpRatio.Neginfty
  else if Apron.Scalar.equal_int c 1 then Maxstratutil.Num.InfGmpRatio.one
  else if Apron.Scalar.equal_int c (-1) then 
    Maxstratutil.Num.InfGmpRatio.minus_one
  else Maxstratutil.Num.InfGmpRatio.Num(
         Maxstratutil.Num.GmpRatio.from_string (Apron.Scalar.to_string c))

let scalar_to_num c =
  if Apron.Scalar.equal_int c 1 then Maxstratutil.Num.GmpRatio.one
  else if Apron.Scalar.equal_int c (-1) then 
    Maxstratutil.Num.GmpRatio.minus_one
  else Maxstratutil.Num.GmpRatio.from_string 
    (Apron.Scalar.to_string c)
 
let coeff_to_num c = 
  if Apron.Coeff.is_zero c then Maxstratutil.Num.GmpRatio.zero
  else
  match c with
    |Apron.Coeff.Scalar(c) -> scalar_to_num c 
    |_-> raise (NotSupported "interval coefficients")

let num_to_coeff c = 
  Apron.Coeff.s_of_mpqf 
    (Mpqf.of_string (Maxstratutil.Num.GmpRatio.to_string c))

(* converts an APRON linexpr1 into LP expression *)
let linexpr_to_lp_expr rename linexpr =
(*  Log.debug3_o logger Apron.Linexpr1.print "linexpr = " linexpr; *)
  let const = Apron.Linexpr1.get_cst linexpr in 
  let terms = ref [TheLp.Const (coeff_to_num const)] in
  Apron.Linexpr1.iter (fun c v -> 
      if Apron.Coeff.equal_int c 1 then 
        terms := (TheLp.Var (rename (Apron.Var.to_string v)))::!terms
      else if not(Apron.Coeff.is_zero c) then 
        terms := (TheLp.Mul (coeff_to_num c,
                           TheLp.Var (rename (Apron.Var.to_string v))))::!terms)
    linexpr;
  TheLp.Sum !terms

(* converts an APRON lincons1 into LP constraint *)
let lincons_to_lp_cons rename lincons =
  let e = linexpr_to_lp_expr rename (Apron.Lincons1.get_linexpr1 lincons) in
  match Apron.Lincons1.get_typ lincons with
    |Apron.Lincons0.EQ -> 
      TheLp.Eq (TheLp.Const(Maxstratutil.Num.GmpRatio.from_int 0), e) 
    |Apron.Lincons0.SUPEQ -> 
      TheLp.Le (TheLp.Const(Maxstratutil.Num.GmpRatio.from_int 0), e)
    |_ -> raise (NotSupported "strict inequalities")

(*****************************************************************************)
(********************** create constraints ***********************************)
(*****************************************************************************)
 
let make_conss_conss ?(rename=(fun x -> x)) linconss =
  Array.to_list (Array.mapi (fun i _ ->
      lincons_to_lp_cons rename (Apron.Lincons1.array_get linconss i))
    linconss.Apron.Lincons1.lincons0_array)

let make_conss_equ ?(rename_lhs=(fun x -> x)) ?(rename_rhs=(fun x -> x)) (v,e)=
  TheLp.Eq (TheLp.Var (rename_lhs (Apron.Var.to_string v)), 
      linexpr_to_lp_expr rename_rhs e)

let make_conss_lequ ?(rename_lhs=(fun x -> x)) ?(rename_rhs=(fun x -> x)) (v,e)=   TheLp.Le (TheLp.Var (rename_lhs (Apron.Var.to_string v)), 
      linexpr_to_lp_expr rename_rhs e)

let make_conss_gequ ?(rename_lhs=(fun x -> x)) ?(rename_rhs=(fun x -> x)) (v,e)=
   TheLp.Le (linexpr_to_lp_expr rename_rhs e,
             TheLp.Var (rename_lhs (Apron.Var.to_string v)))
    
let make_conss_equs ?(rename_lhs=(fun x -> x)) ?(rename_rhs=(fun x -> x)) equs =
  List.map (make_conss_equ ~rename_lhs ~rename_rhs) (Array.to_list equs)

let make_conss_lequs ?(rename_lhs=(fun x -> x)) ?(rename_rhs=(fun x -> x)) equs = 
  List.map (make_conss_lequ ~rename_lhs ~rename_rhs) (Array.to_list equs)

let make_conss_gequs ?(rename_lhs=(fun x -> x)) ?(rename_rhs=(fun x -> x)) equs = 
  List.map (make_conss_gequ ~rename_lhs ~rename_rhs) (Array.to_list equs)

let make_conss_vars_bounded_one vars = 
  List.map (fun v -> 
      TheLp.Le (TheLp.Var (Apron.Var.to_string v), 
                TheLp.Const(Maxstratutil.Num.GmpRatio.from_int 1)))
    (Array.to_list vars)

let make_conss_vars_bounded_minusone vars = 
  List.map (fun v -> 
      TheLp.Le (TheLp.Mul (Maxstratutil.Num.GmpRatio.minus_one,
                           TheLp.Var (Apron.Var.to_string v)), 
                TheLp.Const(Maxstratutil.Num.GmpRatio.from_int 1)))
    (Array.to_list vars)

let concat_conss = List.append

let print_lp_conss fmt lp_conss = 
  Format.pp_print_string fmt (TheLp.string_of_constrs lp_conss)

let print_lp_sol vars fmt sol =
  Array.iter (fun v ->  
      Format.pp_print_newline fmt ();
      Apron.Var.print fmt v;
      Format.pp_print_string fmt " = ";
      Apron.Coeff.print fmt (sol v);
      )
    vars

(*****************************************************************************)
(********************** create objective function ****************************)
(*****************************************************************************)

let make_obj_fct_max ?(rename=(fun x -> x)) linexpr = 
  TheLp.Max (linexpr_to_lp_expr rename linexpr)

let make_obj_fct_max_sum_vars vars = 
  TheLp.Max(TheLp.Sum 
    (List.map (fun v -> TheLp.Var (Apron.Var.to_string v))(Array.to_list vars)))

let make_obj_fct_max_negsum_vars vars = 
  TheLp.Max(TheLp.Sum 
    (List.map (fun v -> TheLp.Mul (Maxstratutil.Num.GmpRatio.minus_one,
                           TheLp.Var (Apron.Var.to_string v)))
      (Array.to_list vars)))

(*****************************************************************************)
(********************** solve LP problem *************************************)
(*****************************************************************************)

let solve conss obj_fct = 
  let (obj_val,sol) = TheLp.solve' (TheLp.new_prob_name ()) conss obj_fct in  
  let obj_val = match obj_val with
    |Maxstratutil.Num.InfGmpRatio.Infty -> 
       Apron.Coeff.Scalar (Apron.Scalar.of_infty 1)
    |Maxstratutil.Num.InfGmpRatio.Neginfty -> 
       Apron.Coeff.Scalar (Apron.Scalar.of_infty (-1))
    |Maxstratutil.Num.InfGmpRatio.Num x -> num_to_coeff x
  in
  (obj_val,(fun v -> 
(*num_to_coeff (sol (Apron.Var.to_string v))))*)
              try num_to_coeff (sol (Apron.Var.to_string v))
              with Ocamlqsopt_ex.Lp.Unsup -> 
                Log.warn_o logger Format.pp_print_string 
                  "solution -inf for " (Apron.Var.to_string v);
                Apron.Coeff.Scalar (Apron.Scalar.of_infty (-1))))

let get_bounded_vars conss vars =
  let conss2 = make_conss_vars_bounded_minusone vars in
  let conss = eliminate_constants_constrs conss in
  let conss = concat_conss conss2 conss in
  Log.debug2_o logger (print_lp_conss) "conss: " conss;
  let obj_fct = make_obj_fct_max_negsum_vars vars in
  let (obj_val,sol) = TheLp.solve' (TheLp.new_prob_name ()) conss obj_fct in
  Log.debug2 logger
           ("obj_val = "^(Maxstratutil.Num.InfGmpRatio.to_string obj_val));
  Array.of_list (List.filter (fun v ->
         Log.debug2 logger 
           ((Apron.Var.to_string v)^" = "^
           (Maxstratutil.Num.GmpRatio.to_string (sol (Apron.Var.to_string v))));
        Maxstratutil.Num.GmpRatio.(=.) (sol (Apron.Var.to_string v)) 
                                       (Maxstratutil.Num.GmpRatio.from_int 0))
      (Array.to_list vars))
 
