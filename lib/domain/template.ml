(******************************************************************************)
(* template *)
(* template domain (emulation) *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Domain.Template";
              Log.level=Log.Debug3}

type apronman_t = Polka.loose Polka.t
type bddapronman_t = (Env.var_t, apronman_t) Bddapron.Domain0.bdd 
type bddaprondoman_t = (Env.var_t, apronman_t,bddapronman_t, Env.var_t Bddapron.Domain0.t) Bddapron.Domain0.man

type elt_t = Apron.Linexpr1.t
type template_t = elt_t list
type cons_t = Apron.Lincons1.t
type bound_t = Apron.Coeff.t

module type TEMPLATE_T =
  (Domain.T with type doman_param_t = template_t
            with type t = Env.var_t Bddapron.Domain0.t
            with type numdomain_t = apronman_t Apron.Abstract0.t
            with type doman_t = bddaprondoman_t * template_t
  ) 

(******************************************************************************)
(* Parsing, Printing, Constants *)
(******************************************************************************)

let template_empty = [] 

let print_bound = Apron.Coeff.print
let print_elt = Apron.Linexpr1.print
let print_cons = Apron.Lincons1.print
let template_print = Util.list_print print_elt

let create_interval_conss env =
  let vars = Util.list_inter env.Env.n_vars env.Env.s_vars in
  let nvars = List.map (fun v -> "-"^v) vars in
  List.map (Apron.Parser.linexpr1_of_string env.Env.apronenv) 
    (List.append vars nvars)

let create_zone_conss env =
  let vars = Util.list_inter env.Env.n_vars env.Env.s_vars in
  let rec build vars tt =
    match vars with
      |[] -> tt
      |v1::tlvars -> 
         let tt = List.fold_right
           (fun v2 t -> 
             List.append 
               [Apron.Parser.linexpr1_of_string env.Env.apronenv (v1^"-"^v2);
               Apron.Parser.linexpr1_of_string env.Env.apronenv (v2^"-"^v1)]
              t)
           tlvars 
           (List.append 
             [Apron.Parser.linexpr1_of_string env.Env.apronenv v1;
              Apron.Parser.linexpr1_of_string env.Env.apronenv ("-"^v1)] tt)
         in
         build tlvars tt
  in
  build vars []

let create_octagon_conss env =
  let vars = Util.list_inter env.Env.n_vars env.Env.s_vars in
  let rec build vars tt =
    match vars with
      |[] -> tt
      |v1::tlvars -> 
         let tt = List.fold_right
           (fun v2 t -> 
             List.append 
              [Apron.Parser.linexpr1_of_string env.Env.apronenv (v1^"+"^v2);
               Apron.Parser.linexpr1_of_string env.Env.apronenv (v1^"-"^v2);
               Apron.Parser.linexpr1_of_string env.Env.apronenv (v2^"-"^v1);
               Apron.Parser.linexpr1_of_string env.Env.apronenv ("-"^v1^"-"^v2)]
              t)
           tlvars 
           (List.append 
             [Apron.Parser.linexpr1_of_string env.Env.apronenv v1;
              Apron.Parser.linexpr1_of_string env.Env.apronenv ("-"^v1)] tt)
         in
         build tlvars tt
  in
  build vars []

(* parses a template from a string list *)
let template_of_strlist env strlist =
  List.fold_right 
    (fun str t -> 
      match str with
	|"INT" -> List.append (create_interval_conss env) t
	|"ZONE" -> List.append (create_zone_conss env) t 
	|"OCT" -> List.append (create_octagon_conss env) t 
	|_ -> (Apron.Parser.linexpr1_of_string env.Env.apronenv str)::t) 
    strlist []

(******************************************************************************)
(* Helpers *)
(******************************************************************************)

(* constraint that is always true *)
let cons_true env = Apron.Lincons1.make 
  (Apron.Linexpr1.make env.Env.apronenv) Apron.Lincons0.EQ

(* constraint that is always false *)
let cons_false env = Apron.Lincons1.make_unsat env.Env.apronenv

(* instantiates the constraint with the bound v (constraint>=v) *)
let cons_instantiate env elt v =
  match v with
  |Apron.Coeff.Scalar vv -> 
  begin
    if (Apron.Scalar.is_infty vv)=0 then
      let cons = Apron.Lincons1.make (Apron.Linexpr1.copy elt)
        (Apron.Lincons0.SUPEQ) in
      Apron.Lincons1.set_cst cons (Apron.Coeff.neg v);
      cons
    else 
      if (Apron.Scalar.is_infty vv)>0 then cons_false env
      else cons_true env
  end
  |_ -> assert(false)

(* instantiates the template with the bounds *)
let template_instantiate env template (bounds:bound_t list) = 
  let linconss = Apron.Lincons1.array_make env.Env.apronenv (List.length template) in
  Util.list_iteri2
    (fun i cons (v:bound_t) ->
      let cons = 
        begin match v with
        |Apron.Coeff.Scalar vv -> 
        begin
          if (Apron.Scalar.is_infty vv)=0 then
          begin 
            let cons = Apron.Lincons1.make (Apron.Linexpr1.copy cons)
               (Apron.Lincons0.SUPEQ) in
            Apron.Lincons1.set_cst cons (Apron.Coeff.neg v);
            cons
          end
          else
            if (Apron.Scalar.is_infty vv)>0 then
              cons_false env
            else cons_true env
        end
        |_ -> assert(false) 
        end
      in
      Apron.Lincons1.array_set linconss i cons)
    template bounds;
  linconss

let neginfty = Apron.Coeff.Scalar(Apron.Scalar.of_infty (-1))
let infty = Apron.Coeff.Scalar(Apron.Scalar.of_infty 1)

(*let print fmt t s = 
  Util.list_print ~csep:" and "
    (Apron.Lincons1.print fmt)
    (List.iter2 
      (fun tt ss -> 
        let newtt = Apron.Lincons1.copy tt in
        Apron.Lincons1.set_cst newtt ss;
        newtt) 
      t s)

let print_template fmt t = print fmt t 
  Util.list_print 
    (fun ss ->
       Apron.Lincons1.print fmt (ss)
       ) 
    s*)

let bottom t = List.map (fun _ -> neginfty) t
let top t = List.map (fun _ -> infty) t

(******************************************************************************)
(* overapproximates the given polyhedron s using the template tt *)
let apron_approx_template apronenv apronman tt s =
  let get_max s t =
    let minmax = Apron.Abstract1.bound_linexpr apronman s t in
    Apron.Scalar.neg (minmax.Apron.Interval.inf)
  in
  let calc_conss tt s = List.fold_right
    (fun t res ->
(*      Log.debug3_o logger (Apron.Linexpr1.print) "template = " t; *)
      let max = get_max s t in
(*      Log.debug3_o logger (Apron.Scalar.print) "+max >=0 :: " max; *)
      if (Apron.Scalar.is_infty max)>0 then res
      else if (Apron.Scalar.is_infty max)=0 then 
      begin
        let e = Apron.Linexpr0.copy (Apron.Linexpr1.get_linexpr0 t) in
        Apron.Linexpr0.set_cst e (Apron.Coeff.Scalar max);
        (Apron.Lincons0.make e Apron.Lincons0.SUPEQ)::res
      end
      else assert(false))
    tt []
  in
(*  Log.debug3_o logger (Apron.Abstract1.print) "s = " s; *)
  let res = if (Apron.Abstract1.is_top apronman s) ||
     (Apron.Abstract1.is_bottom apronman s) then s
  else 
    Apron.Abstract1.of_lincons_array apronman apronenv
     {Apron.Lincons1.lincons0_array = Array.of_list (calc_conss tt s);
      Apron.Lincons1.array_env =  apronenv}
  in
(*  Log.debug3_o logger (Apron.Abstract1.print) "res= " res; *)
  res

let bddapron_approx_template env doman tt s =
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let slist = Bddapron.Domain0.to_bddapron doman s in
  List.fold_left (fun res (sb,sn) ->
      let sn2 = {Apron.Abstract1.abstract0 = sn; 
                 Apron.Abstract1.env = env.Env.apronenv } in
      Bddapron.Domain0.join doman res 
       (Bddapron.Domain0.of_bddapron doman env.Env.env 
        [(sb, 
         Apron.Abstract1.abstract0 
            (apron_approx_template env.Env.apronenv apronman tt sn2))]
      ))
    (Bddapron.Domain0.bottom doman env.Env.env) slist

(******************************************************************************)
(** {2 module EmuProd: template product emulation} *)
(******************************************************************************)

module EmuProd =
struct
type t = Env.var_t Bddapron.Domain0.t

type numdomain_t = apronman_t Apron.Abstract0.t

type doman_param_t = template_t
type doman_t = bddaprondoman_t * template_t

let make_man () : bddaprondoman_t = 
  let bman: bddapronman_t = 
    Bddapron.Domain0.make_bdd (Polka.manager_alloc_loose ()) in
  Bddapron.Domain0.man_of_bdd bman

let domanref = ref(None)

let make_doman (template:doman_param_t) = (make_man (), template)

let makeinit_doman (template:doman_param_t) = 
  let d = (make_man (), template) in
  domanref := Some d;
  d

let doman () = 
  match !domanref with
    |Some d -> d
    |None -> assert(false)

let get_doman_param () = 
  match !domanref with
    |Some (_,template) -> template
    |None -> assert(false)

let convexify doman env s = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let l = Bddapron.Domain0.to_bddapron doman s in
  let (b,n) = List.fold_left 
    (fun (bacc,nacc) (be,ne) -> 
      (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc be,
       Apron.Abstract0.join apronman nacc ne))
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond,
     ApronUtil.bottom0 apronman env.Env.apronenv)
    l
  in
  Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond 
    (Bddapron.Domain0.of_apron doman env.Env.env n) b

let meet_condition ?(doman=doman()) env  s e = 
  let (doman,_) = doman in
  convexify doman env 
    (Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond s e)

let is_bottom ?(doman=doman()) env s = 
  let (doman,_) = doman in
  Bddapron.Domain0.is_bottom doman s

let print ?(doman=doman()) env fmt s = 
  let (doman,_) = doman in
  Bddapron.Domain0.print doman env.Env.env fmt s

let is_eq ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.is_eq doman s1 s2

let bottom ?(doman=doman()) env = 
  let (doman,_) = doman in
  Bddapron.Domain0.bottom doman env.Env.env 

let top ?(doman=doman()) env = 
  let (doman,_) = doman in
  Bddapron.Domain0.top doman env.Env.env 

let canonicalize ?(doman=doman()) env s = 
  let (doman,_) = doman in
  Bddapron.Domain0.canonicalize doman s

let is_leq ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.is_leq doman s1 s2

let join ?(doman=doman()) env s1 s2 = 
  let (doman,template) = doman in
  convexify doman env (Bddapron.Domain0.join doman  
       (bddapron_approx_template env doman template s1)
       (bddapron_approx_template env doman template s2))

let meet ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  convexify doman env (Bddapron.Domain0.meet doman s1 s2)

let widening ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  convexify doman env (Bddapron.Domain0.widening doman s1 s2)

let assign_lexpr ?(doman=doman()) env s equs = 
  let (doman,_) = doman in
  let (vars,exprs) = Env.split_equs equs in 
  convexify doman env
    (Bddapron.Domain0.assign_lexpr doman env.Env.env env.Env.cond s 
       vars exprs None)

let forget_list ?(doman=doman()) env s vars = 
  let (doman,_) = doman in
  convexify doman env 
    (Bddapron.Domain0.forget_list doman env.Env.env s  vars)

let substitute_lexpr ?(doman=doman()) env s equs = 
  let (doman,_) = doman in
  let (vars,exprs) = Env.split_equs equs in 
  convexify doman env 
    (Bddapron.Domain0.substitute_lexpr doman env.Env.env env.Env.cond s 
       vars exprs None)

let flow ?(doman=doman()) env s equs staycond = 
  raise (Domain.NotSupported "Template.EmuProd.flow")
let accel ?(doman=doman()) ?(dir=`Forward) env s equs g = 
  raise (Domain.NotSupported "Template.EmuProd.accel")

let of_boolnumlist ?(doman=doman()) env bnlist = 
  let (doman,_) = doman in
  match bnlist with
    |(bexpr,apron)::[] -> 
       Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond 
         (Bddapron.Domain0.of_apron doman env.Env.env apron) bexpr
    |_ -> convexify doman env 
            (Bddapron.Domain0.of_bddapron doman env.Env.env bnlist)

let of_num ?(doman=doman()) env apron = 
  let (doman,_) = doman in
  Bddapron.Domain0.of_apron doman env.Env.env apron

let of_boolexpr ?(doman=doman()) env bexpr =  
  let (doman,_) = doman in
  Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond
    (top env) bexpr

let to_boolexprbool ?(doman=doman()) env s =
  let (doman,_) = doman in
  let l = Bddapron.Domain0.to_bddapron doman s in
  List.fold_left 
    (fun bacc (be,_)-> Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc be)
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond)
    l

let to_boolexpr ?(doman=doman()) env s =
  let (doman,_) = doman in
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let l = Bddapron.Domain0.to_bddapron doman s in
  List.fold_left 
    (fun bacc (be,ne)-> 
       Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc 
         (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond be
           (BddapronUtil.apron_to_boolexpr env.Env.env env.Env.cond
             apronman
             (ApronUtil.abstract1_of_abstract0 env.Env.apronenv ne))))
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond)
    l

let to_boolnumlist ?(doman=doman()) env s = 
  let (doman,_) = doman in
  Bddapron.Domain0.to_bddapron doman s

let to_boollinconsslist ?(doman=doman()) env s =
  let (doman,_) = doman in
  let apronman = Bddapron.Domain0.man_get_apron doman in
  List.map (fun (b,n) -> (b,Apron.Abstract1.to_lincons_array apronman
                        (ApronUtil.abstract1_of_abstract0 env.Env.apronenv n)))
    (Bddapron.Domain0.to_bddapron doman s)

let meet_to_boolexpr ?(doman=doman()) env s bexpr = 
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond bexpr 
    (to_boolexpr ~doman env s)

let meetbool_to_boolexpr ?(doman=doman()) env s bexpr = 
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond bexpr 
    (to_boolexprbool ~doman env s)
end

(******************************************************************************)
(** {2 module EmuPow: template power emulation} *)
(******************************************************************************)

module EmuPow =
struct
type t = Env.var_t Bddapron.Domain0.t

type numdomain_t = apronman_t Apron.Abstract0.t

type doman_param_t = template_t
type doman_t = bddaprondoman_t * template_t

let make_man () : bddaprondoman_t = 
  let bman: bddapronman_t = 
    Bddapron.Domain0.make_bdd (Polka.manager_alloc_loose ()) in
  Bddapron.Domain0.man_of_bdd bman

let domanref = ref(None)

let make_doman (template:doman_param_t) = (make_man (), template)

let makeinit_doman (template:doman_param_t) = 
  let d = (make_man (), template) in
  domanref := Some d;
  d

let doman () = 
  match !domanref with
    |Some d -> d
    |None -> assert(false)

let get_doman_param () = 
  match !domanref with
    |Some (_,template) -> template
    |None -> assert(false)

let meet_condition ?(doman=doman()) env  s e = 
  let (doman,_) = doman in
  Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond s e

let is_bottom ?(doman=doman()) env s = 
  let (doman,_) = doman in
  Bddapron.Domain0.is_bottom doman s

let print ?(doman=doman()) env fmt s = 
  let (doman,_) = doman in
  Bddapron.Domain0.print doman env.Env.env fmt s

let is_eq ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.is_eq doman s1 s2

let bottom ?(doman=doman()) env = 
  let (doman,_) = doman in
  Bddapron.Domain0.bottom doman env.Env.env 

let top ?(doman=doman()) env = 
  let (doman,_) = doman in
  Bddapron.Domain0.top doman env.Env.env 

let canonicalize ?(doman=doman()) env s = 
  let (doman,_) = doman in
  Bddapron.Domain0.canonicalize doman s

let is_leq ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.is_leq doman s1 s2

let join ?(doman=doman()) env s1 s2 = 
  let (doman,template) = doman in
  Bddapron.Domain0.join doman  
       (bddapron_approx_template env doman template s1)
       (bddapron_approx_template env doman template s2)

let meet ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.meet doman s1 s2

let widening ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.widening doman s1 s2

let assign_lexpr ?(doman=doman()) env s equs = 
  let (doman,_) = doman in
  let (vars,exprs) = Env.split_equs equs in 
  Bddapron.Domain0.assign_lexpr doman env.Env.env env.Env.cond s 
       vars exprs None

let forget_list ?(doman=doman()) env s vars = 
  let (doman,_) = doman in
  Bddapron.Domain0.forget_list doman env.Env.env s  vars

let substitute_lexpr ?(doman=doman()) env s equs = 
  let (doman,_) = doman in
  let (vars,exprs) = Env.split_equs equs in 
  Bddapron.Domain0.substitute_lexpr doman env.Env.env env.Env.cond s 
       vars exprs None

let flow ?(doman=doman()) env s equs staycond = 
  raise (Domain.NotSupported "Template.EmuPow.flow")
let accel ?(doman=doman()) ?(dir=`Forward) env s equs g = 
  raise (Domain.NotSupported "Template.EmuPow.accel")

let of_boolnumlist ?(doman=doman()) env bnlist = 
  let (doman,_) = doman in
  match bnlist with
    |(bexpr,apron)::[] -> 
       Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond 
         (Bddapron.Domain0.of_apron doman env.Env.env apron) bexpr
    |_ -> Bddapron.Domain0.of_bddapron doman env.Env.env bnlist

let of_num ?(doman=doman()) env apron = 
  let (doman,_) = doman in
  Bddapron.Domain0.of_apron doman env.Env.env apron

let of_boolexpr ?(doman=doman()) env bexpr =  
  let (doman,_) = doman in
  Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond
    (top env) bexpr

let to_boolexprbool ?(doman=doman()) env s =
  let (doman,_) = doman in
  let l = Bddapron.Domain0.to_bddapron doman s in
  List.fold_left 
    (fun bacc (be,_)-> Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc be)
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond)
    l

let to_boolexpr ?(doman=doman()) env s =
  let (doman,_) = doman in
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let l = Bddapron.Domain0.to_bddapron doman s in
  List.fold_left 
    (fun bacc (be,ne)-> 
       Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc 
         (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond be
           (BddapronUtil.apron_to_boolexpr env.Env.env env.Env.cond
             apronman
             (ApronUtil.abstract1_of_abstract0 env.Env.apronenv ne))))
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond)
    l

let to_boolnumlist ?(doman=doman()) env s = 
  let (doman,_) = doman in
  Bddapron.Domain0.to_bddapron doman s

let to_boollinconsslist ?(doman=doman()) env s =
  let (doman,_) = doman in
  let apronman = Bddapron.Domain0.man_get_apron doman in
  List.map (fun (b,n) -> (b,Apron.Abstract1.to_lincons_array apronman
                        (ApronUtil.abstract1_of_abstract0 env.Env.apronenv n)))
    (Bddapron.Domain0.to_bddapron doman s)

let meet_to_boolexpr ?(doman=doman()) env s bexpr = 
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond bexpr 
    (to_boolexpr ~doman env s)

let meetbool_to_boolexpr ?(doman=doman()) env s bexpr = 
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond bexpr 
    (to_boolexprbool ~doman env s)
end

(******************************************************************************)
(** {2 module Pow: template power domain} *)
(******************************************************************************)

module Pow =
struct
type t = Env.var_t Bddapron.Domain0.t

type numdomain_t = apronman_t Apron.Abstract0.t

type doman_param_t = template_t
type doman_t = bddaprondoman_t * template_t

let make_man () : bddaprondoman_t = 
  let bman: bddapronman_t = 
    Bddapron.Domain0.make_bdd (Polka.manager_alloc_loose ()) in
  Bddapron.Domain0.man_of_bdd bman

let domanref = ref(None)

let make_doman (template:doman_param_t) = (make_man (), template)

let makeinit_doman (template:doman_param_t) = 
  let d = (make_man (), template) in
  domanref := Some d;
  d

let doman () = 
  match !domanref with
    |Some d -> d
    |None -> assert(false)

let get_doman_param () = 
  match !domanref with
    |Some (_,template) -> template
    |None -> assert(false)

let meet_condition ?(doman=doman()) env  s e = 
  let (doman,_) = doman in
  Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond s e

let is_bottom ?(doman=doman()) env s = 
  let (doman,_) = doman in
  Bddapron.Domain0.is_bottom doman s

let print ?(doman=doman()) env fmt s = 
  let (doman,_) = doman in
  Bddapron.Domain0.print doman env.Env.env fmt s

let is_eq ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.is_eq doman s1 s2

let bottom ?(doman=doman()) env = 
  let (doman,_) = doman in
  Bddapron.Domain0.bottom doman env.Env.env 

let top ?(doman=doman()) env = 
  let (doman,_) = doman in
  Bddapron.Domain0.top doman env.Env.env 

let canonicalize ?(doman=doman()) env s = 
  let (doman,_) = doman in
  Bddapron.Domain0.canonicalize doman s

let is_leq ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.is_leq doman s1 s2

let join ?(doman=doman()) env s1 s2 = 
  let (doman,template) = doman in
  Bddapron.Domain0.join doman  
       (bddapron_approx_template env doman template s1)
       (bddapron_approx_template env doman template s2)

let meet ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.meet doman s1 s2

let widening ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.widening doman s1 s2

let assign_lexpr ?(doman=doman()) env s equs = 
  let (doman,_) = doman in
  let (vars,exprs) = Env.split_equs equs in 
  Bddapron.Domain0.assign_lexpr doman env.Env.env env.Env.cond s 
       vars exprs None

let forget_list ?(doman=doman()) env s vars = 
  let (doman,_) = doman in
  Bddapron.Domain0.forget_list doman env.Env.env s  vars

let substitute_lexpr ?(doman=doman()) env s equs = 
  let (doman,_) = doman in
  let (vars,exprs) = Env.split_equs equs in 
  Bddapron.Domain0.substitute_lexpr doman env.Env.env env.Env.cond s 
       vars exprs None

let flow ?(doman=doman()) env s equs staycond = 
  raise (Domain.NotSupported "Template.EmuPow.flow")
let accel ?(doman=doman()) ?(dir=`Forward) env s equs g = 
  raise (Domain.NotSupported "Template.EmuPow.accel")

let of_boolnumlist ?(doman=doman()) env bnlist = 
  let (doman,_) = doman in
  match bnlist with
    |(bexpr,apron)::[] -> 
       Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond 
         (Bddapron.Domain0.of_apron doman env.Env.env apron) bexpr
    |_ -> Bddapron.Domain0.of_bddapron doman env.Env.env bnlist

let of_num ?(doman=doman()) env apron = 
  let (doman,_) = doman in
  Bddapron.Domain0.of_apron doman env.Env.env apron

let of_boolexpr ?(doman=doman()) env bexpr =  
  let (doman,_) = doman in
  Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond
    (top env) bexpr

let to_boolexprbool ?(doman=doman()) env s =
  let (doman,_) = doman in
  let l = Bddapron.Domain0.to_bddapron doman s in
  List.fold_left 
    (fun bacc (be,_)-> Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc be)
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond)
    l

let to_boolexpr ?(doman=doman()) env s =
  let (doman,_) = doman in
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let l = Bddapron.Domain0.to_bddapron doman s in
  List.fold_left 
    (fun bacc (be,ne)-> 
       Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc 
         (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond be
           (BddapronUtil.apron_to_boolexpr env.Env.env env.Env.cond
             apronman
             (ApronUtil.abstract1_of_abstract0 env.Env.apronenv ne))))
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond)
    l

let to_boolnumlist ?(doman=doman()) env s = 
  let (doman,_) = doman in
  Bddapron.Domain0.to_bddapron doman s

let to_boollinconsslist ?(doman=doman()) env s =
  let (doman,_) = doman in
  let apronman = Bddapron.Domain0.man_get_apron doman in
  List.map (fun (b,n) -> (b,Apron.Abstract1.to_lincons_array apronman
                        (ApronUtil.abstract1_of_abstract0 env.Env.apronenv n)))
    (Bddapron.Domain0.to_bddapron doman s)

let meet_to_boolexpr ?(doman=doman()) env s bexpr = 
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond bexpr 
    (to_boolexpr ~doman env s)

let meetbool_to_boolexpr ?(doman=doman()) env s bexpr = 
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond bexpr 
    (to_boolexprbool ~doman env s)
end
