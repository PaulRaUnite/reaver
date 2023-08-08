(******************************************************************************)
(* FinDisPart *)
(* finitely disjunctive partitioned domain *)
(* author: Peter Schrammel *)
(* version: 0.9.3 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Domain.FinDisPart";
              Log.level=Log.Debug3}

type 'apronman aprontuple_t = 
  'apronman Apron.Abstract1.t PSette.t * 'apronman Apron.Abstract1.t

type 'apronman bddaprontuple_t = Env.boolexpr_t * 'apronman aprontuple_t 

(*
abstract value: (pp,i)

pp: env = "P" state variables + all input variables
i:  env = "I" state variables + all input variables
*)

(******************************************************************************)
(* Helpers *)
(******************************************************************************)
let print_apron_with_env fmt s = 
       Apron.Abstract1.print fmt s;
       Format.pp_print_string fmt " with ";
       Apron.Environment.print fmt (Apron.Abstract1.env s)

(******************************************************************************)
(* best-effort total order-comparison of APRON abstract values *)
let compare_apron apronman s1 s2 = 
  Log.debug2_o logger (print_apron_with_env) "comp s1 = " s1;
  Log.debug2_o logger (print_apron_with_env) "with s2 = " s2;
  let hash = (Apron.Abstract1.hash apronman s1)-
       (Apron.Abstract1.hash apronman s2) in
  let res = if hash<>0 then hash 
    else
      if Apron.Abstract1.is_eq apronman s1 s2 then 0 
      else -1
  in
  Log.debug2_o logger (Format.pp_print_int) "    res = " res;
  res

(******************************************************************************)
(* checks whether vars1 and vars2 are related in g *)
(* TODO: currently we check only whether they do not occur in g, 
     but we should check whether they are related in 
       individual constraints in g *)
let is_indep_in_guard env g vars1 vars2 =
(*  let linconss = BddapronUtil.boolexpr_to_linconss env.Env.env env.Env.cond 
    doman env.Env.b_vars g in*)
  Log.debug3_o logger (Env.print_boolexpr env) "is_indep_in_guard: " g;
  let gvars = Bddapron.Expr0.support env.Env.env env.Env.cond 
    (Bddapron.Expr0.Bool.to_expr g) in
  let res = (PSette.is_empty (PSette.inter (Util.list2psette (compare) 
                  vars1) gvars)) ||
    (PSette.is_empty (PSette.inter (Util.list2psette (compare) vars2) gvars))
  in
  Log.debug3_o logger (Format.pp_print_bool) "res = " res;
  res 

(******************************************************************************)
(* returns true if the given equation is finitely generating w.r.t. 
     the given abstract value partition *)
let is_finitely_generating env (pvars,ivars) (v,expr) =
  let inputvars = BddapronUtil.vars_to_apronvars env.Env.env env.Env.ni_vars in
  let ivars = BddapronUtil.apronvars_to_vars env.Env.env ivars in
  let check_equ (v,ee) =
    let dims = ApronUtil.dims_of_env env.Env.apronenv in
    let e = Apron.Linexpr1.get_linexpr0 ee in
    let is_cst0 = Apron.Coeff.is_zero (Apron.Linexpr0.get_cst e) in
    let rec check_coeffs j (sv,invars) =
      if j>=dims then (sv,invars)
      else
      let zero_coeff = Apron.Coeff.is_zero (Apron.Linexpr0.get_coeff e j) in
      let pmone_coeff = (Apron.Coeff.equal_int 
        (Apron.Linexpr0.get_coeff e j) 1) || (Apron.Coeff.equal_int 
        (Apron.Linexpr0.get_coeff e j) (-1)) in
      let vv = Apron.Environment.var_of_dim (Apron.Linexpr1.get_env ee) j in
      let is_input = Util.array_mem vv inputvars in
      match (sv,is_cst0,zero_coeff,pmone_coeff,is_input) with
       |(None  ,true ,_    ,true ,false) -> check_coeffs (j+1) (Some vv,invars)
       |(None  ,_    ,false,_    ,true ) -> check_coeffs (j+1) (sv,vv::invars)
       |(_     ,_    ,true ,_    ,_    ) -> check_coeffs (j+1) (sv,invars)
       |_ -> (Some v,[v])
    in
    check_coeffs 0 (None,[])
  in
  Log.debug3_o logger (BddapronUtil.print_equation env.Env.env 
          env.Env.cond) "equ: " (v,expr);
  match expr with
    |`Apron(expr) ->
      begin
        let gl = Cudd.Mtbdd.guardleafs expr in
        let len = Array.length gl in
        let rec check j =
          if j>=len then true
          else
            let apronequ = (BddapronUtil.var_to_apronvar env.Env.env v,
               BddapronUtil.apronaction_to_linexpr env.Env.env env.Env.cond 
               (snd gl.(j)))
            in
            Log.debug3_o logger (ApronUtil.print_equation) 
              "apronequ: " apronequ;
            match check_equ apronequ with
              (* coeffs=0 -> true *)
	      |(None,[]) -> check (j+1) 
              (* svar coeffs=0 -> check indep inputs ivars *)
	      |(None,invars) -> 
                let invars = List.map (BddapronUtil.apronvar_to_var env.Env.env)
                  invars in
                if is_indep_in_guard env (fst gl.(j)) 
                     ivars invars then check (j+1)
                else false
              (* 1svar coeff=+-1, cst=0, sv in pvars -> check indep v sv *)
	      |(Some sv,[]) ->
                Log.debug3 logger "+-x";
                let sv = BddapronUtil.apronvar_to_var env.Env.env sv in
                if (not (List.mem sv ivars)) &&
                   (is_indep_in_guard env (fst gl.(j)) 
                      ivars [sv]) then check (j+1)
                else false
	      |(_,invars) -> false
        in 
        let res = check 0 in
        Log.debug3_o logger (Format.pp_print_bool) "is_fin_gen = " res;
        res
      end
    |_ -> assert(false)

(******************************************************************************)
(* returns the variables corresponding to the partition of the abstract
      value *)
let get_tuple_vars (env:Env.t) apronman (pp,i) = 
  let vars = BddapronUtil.vars_to_apronvars env.Env.env env.Env.ns_vars in
  if (Apron.Abstract1.is_bottom apronman i) && (PSette.is_empty pp) then
    (vars,[||])
  else
    let varsi = ApronUtil.vars_of_env (Apron.Abstract1.env i) in
    (Util.array_diff vars varsi, varsi)

(******************************************************************************)
(* changes the environment of the given APRON value *)
let change_env_vars (env:Env.t) apronman s vars = 
  let newvars = Util.array_diff vars (ApronUtil.vars_of_env (Apron.Abstract1.env s)) in
(*  Log.debug3_o logger (print_apron_with_env) "change_env: s = " s;
  Log.debug3_o logger (ApronUtil.print_vars) "vars = " vars;
  Log.debug3_o logger (ApronUtil.print_vars) "newvars = " newvars;*)
  let res = if newvars=vars then 
      Apron.Abstract1.top apronman (ApronUtil.env_of_vars env.Env.apronenv vars)
    else Apron.Abstract1.change_environment apronman s
           (ApronUtil.env_of_vars env.Env.apronenv vars) false 
  in
(*  let res = Apron.Abstract1.forget_array apronman res newvars false in*)
(*  Log.debug3_o logger (print_apron_with_env) "change_env: res = " res;*)
  res

(******************************************************************************)
(* changes the environment of the given set of APRON values *)
let change_env_tuple_vars (env:Env.t) apronman 
    (pp:'a Apron.Abstract1.t PSette.t) vars :'a Apron.Abstract1.t PSette.t =
  Util.psette_map (fun p -> change_env_vars env apronman p vars) (compare_apron apronman) pp

(******************************************************************************)
(* converts the set of APRON values into a single APRON value *)
let convexify_tuple env apronman ppapronenv pp = 
  PSette.fold (fun p pi -> Apron.Abstract1.join apronman p pi) 
   pp (Apron.Abstract1.bottom apronman ppapronenv)

(******************************************************************************)
(* concats two APRON values *)
let concat_abstract (env:Env.t) apronman s1 s2 = 
  let vars = Util.array_union 
    (ApronUtil.vars_of_env (Apron.Abstract1.env s1))
    (ApronUtil.vars_of_env (Apron.Abstract1.env s2))
  in
  let s1 = change_env_vars env apronman s1 vars in
  let s2 = change_env_vars env apronman s2 vars in
(*  Log.debug3_o logger (print_apron_with_env) "concat1 = " s1;
  Log.debug3_o logger (print_apron_with_env) "concat2 = " s2;*)
  Apron.Abstract1.meet apronman s1 s2

(******************************************************************************)
(* converts a BddApron value to an APRON value *)
let bddapron_to_apron (env:Env.t) doman apronenv s  = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let bnlist = Bddapron.Domain0.to_bddapron doman
        (BddapronUtil.abstract_convexify env.Env.env env.Env.cond doman s) 
  in 
  if (List.length bnlist)<1 then 
   Apron.Abstract1.bottom apronman apronenv
  else ApronUtil.abstract1_of_abstract0 apronenv
    (snd (List.hd bnlist))

(******************************************************************************)
(** {2 module finitely disjunctive partitioned logico-numerical product domain} *)
(******************************************************************************)
module FdpProd(Man: Domain.BDDAPRON_MAN_T) =
struct
type t = Man.apronman_t  bddaprontuple_t
type numdomain_t = Man.apronman_t Apron.Abstract0.t
type doman_t = (Env.var_t, Env.var_t Bddapron.Domain0.t) Man.man_t
type doman_param_t = unit

let domanref = ref(None)

let make_doman () = Man.make_man ()

let makeinit_doman () : doman_t = 
  let d = Man.make_man () in
  domanref := Some d;
  d

let doman () = 
  match !domanref with
    |Some d -> d
    |None -> assert(false)

let get_doman_param () = ()

let print_with_env env fmt (b,(pp,i)) = 
  BddapronUtil.print_boolexpr env.Env.env env.Env.cond fmt b;
  Format.pp_print_string fmt " and ";
  PSette.print ~first:"{" ~last:"}" 
    (fun fmt p -> Apron.Abstract1.print fmt p;
       Format.pp_print_string fmt " with ";
       Apron.Environment.print fmt (Apron.Abstract1.env p)
    ) fmt pp;
  Format.pp_print_string fmt " x (";
  Apron.Abstract1.print fmt i;
  Format.pp_print_string fmt " with ";
  Apron.Environment.print fmt (Apron.Abstract1.env i);
  Format.pp_print_string fmt ")"

let bottom ?(doman=doman()) env = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond,
    (PSette.empty (compare_apron apronman),
     Apron.Abstract1.bottom apronman env.Env.apronenv)) (*s_apronenv*)

let top ?(doman=doman()) env = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  (Bddapron.Expr0.Bool.dtrue env.Env.env env.Env.cond,
    (PSette.empty (compare_apron apronman),
     Apron.Abstract1.top apronman env.Env.apronenv)) (*s_apronenv*)

let meet_condition ?(doman=doman()) env (b,(pp,i)) e =  
  let apronman = Bddapron.Domain0.man_get_apron doman in
  Log.debug3_o logger (print_with_env env) "meet s = " (b,(pp,i));
  Log.debug3_o logger (Env.print_boolexpr env) "with g = " e;
  let bb =  Cudd.Bdd.exist env.Env.cond.Bdd.Cond.supp
    (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond b e) in
  let res = 
    if Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond bb then bottom env
    else
    begin
      let (varsp,varsi) = get_tuple_vars env apronman (pp,i) in
      (*  Log.debug3_o logger (ApronUtil.print_vars) "varsp = " varsp;
          Log.debug3_o logger (ApronUtil.print_vars) "varsi = " varsi; *)
      (* no information about equations at this point, thus
        decouple p variables from others *)
      let g  = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
        (BddapronUtil.boolexpr_forget_vars env.Env.env env.Env.cond doman 
          (BddapronUtil.apronvars_to_vars env.Env.env varsp) e)
        (BddapronUtil.boolexpr_forget_vars env.Env.env env.Env.cond doman 
          (List.append (BddapronUtil.apronvars_to_vars env.Env.env varsi) 
             env.Env.ni_vars) e)
      in
      Log.debug3_o logger (Env.print_boolexpr env) "relaxed g = " g;
      let ppi = if PSette.is_empty pp then
        PSette.singleton (compare_apron apronman) (bddapron_to_apron env doman env.Env.apronenv
          (Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond
            (Bddapron.Domain0.of_apron doman env.Env.env 
              (Apron.Abstract1.abstract0 
                 (Apron.Abstract1.change_environment apronman i env.Env.apronenv
                   false))) g))
        else 
         Util.psette_map (fun p -> 
           let pie = Bddapron.Domain0.meet_condition doman 
             env.Env.env env.Env.cond
             (Bddapron.Domain0.of_apron doman env.Env.env 
             (Apron.Abstract1.abstract0 
               (Apron.Abstract1.change_environment apronman 
                 (concat_abstract env apronman p i) env.Env.apronenv false)))
             g
           in 
           Log.debug3_o logger (BddapronUtil.print_abstract env.Env.env doman) "pie = " pie; 
           bddapron_to_apron env doman env.Env.apronenv pie)
           (compare_apron apronman) pp
      in
      let bot = Apron.Abstract1.bottom apronman 
            (ApronUtil.env_of_vars env.Env.apronenv varsi) in
      (*  Log.debug3_o logger (print_with_env) "ppi = " (ppi,bot); *)
      let ppp = if PSette.is_empty pp then PSette.empty (compare_apron apronman)
        else change_env_tuple_vars env apronman ppi varsp 
      in
      (bb,(ppp,PSette.fold 
         (fun pi i -> 
            let pi = change_env_vars env apronman pi varsi in
(*       Log.debug3_o logger (print_apron_with_env) "pi-env = " pi;
       Log.debug3_o logger (print_apron_with_env) "i-env = " i; *)
            Apron.Abstract1.join apronman i pi)
         ppi bot))
    end
  in
  Log.debug3_o logger (print_with_env env) "res = " res;
  res

let is_bottom ?(doman=doman()) env (b,(pp,i)) = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  (Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond b) &&
     (Apron.Abstract1.is_bottom apronman i) && (PSette.is_empty pp)

let print ?(doman=doman()) env fmt (b,(pp,i)) = 
  BddapronUtil.print_boolexpr env.Env.env env.Env.cond fmt b;
  Format.pp_print_string fmt " and ";
  PSette.print ~first:"{" ~last:"}" (Apron.Abstract1.print) fmt pp;
  Format.pp_print_string fmt " x ";
  Apron.Abstract1.print fmt i

let is_eq ?(doman=doman()) env (b1,(pp1,i1)) (b2,(pp2,i2)) = 
  Log.debug3_o logger (print_with_env env) "is s1 = " (b1,(pp1,i1));
  Log.debug3_o logger (print_with_env env) "eq s2 = " (b2,(pp2,i2));
  let apronman = Bddapron.Domain0.man_get_apron doman in
  (Bddapron.Expr0.Bool.is_eq env.Env.env env.Env.cond b1 b2) &&
  (Apron.Abstract1.is_eq apronman i1 i2) &&
  (PSette.equal pp1 pp2)

let canonicalize ?(doman=doman()) env s = ()

let is_leq ?(doman=doman()) env (b1,(pp1,i1)) (b2,(pp2,i2)) = 
  Log.debug3_o logger (print_with_env env) "is s1 = " (b1,(pp1,i1));
  Log.debug3_o logger (print_with_env env) "eq s2 = " (b2,(pp2,i2));
  let res = 
  if not (Bddapron.Expr0.Bool.is_eq env.Env.env env.Env.cond b1 b2) then false
  else
  begin
    let apronman = Bddapron.Domain0.man_get_apron doman in
    let (varsp1,varsi1) = get_tuple_vars env apronman (pp1,i1) in  
    let (varsp2,varsi2) = get_tuple_vars env apronman (pp2,i2) in 
    let varsi12 = Util.array_inter varsi1 varsi2 in
    let varsp1i2 = Util.array_inter varsp1 varsi2 in
    (Sette.subset (Util.array2sette varsi1) (Util.array2sette varsi2)) &&
    (Apron.Abstract1.is_leq apronman 
      (change_env_vars env apronman i1 varsi12) 
      (change_env_vars env apronman i2 varsi12)) &&
    (PSette.subset 
      (change_env_tuple_vars env apronman pp1 varsp1i2) 
      (change_env_tuple_vars env apronman pp2 varsp1i2)) &&
    (Apron.Abstract1.is_leq apronman 
      (convexify_tuple env apronman 
        (ApronUtil.env_of_vars env.Env.apronenv varsp1i2)
        (change_env_tuple_vars env apronman pp1 varsp1i2)) 
      (change_env_vars env apronman i2 varsp1i2))
  end
  in
  Log.debug3_o logger (Format.pp_print_bool) "res = " res;
  res


let join ?(doman=doman()) env (b1,(pp1,i1)) (b2,(pp2,i2)) = 
  Log.debug3_o logger (print_with_env env) "join s1 = " (b1,(pp1,i1));
  Log.debug3_o logger (print_with_env env) "with s2 = " (b2,(pp2,i2));
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let (varsp1,varsi1) = get_tuple_vars env apronman (pp1,i1) in
  let (varsp2,varsi2) = get_tuple_vars env apronman (pp2,i2) in
  let varsi12 = Util.array_inter varsi1 varsi2 in
  let varsp12 = Util.array_inter varsp1 varsp2 in
  let varsp1i2 = Util.array_inter varsp1 varsi2 in
  let varsp2i1 = Util.array_inter varsp2 varsi1 in
  let i12 = Apron.Abstract1.join apronman 
       (change_env_vars env apronman i1 varsi12) 
       (change_env_vars env apronman i2 varsi12) in
  let p1i2 = Apron.Abstract1.join apronman
         (convexify_tuple env apronman 
           (ApronUtil.env_of_vars env.Env.apronenv varsp1i2)
           (change_env_tuple_vars env apronman pp1 varsp1i2))
         (change_env_vars env apronman i2 varsp1i2) in
  let p2i1 = Apron.Abstract1.join apronman
         (convexify_tuple env apronman 
            (ApronUtil.env_of_vars env.Env.apronenv varsp2i1)
            (change_env_tuple_vars env apronman pp2 varsp2i1))
         (change_env_vars env apronman i1 varsp2i1) in
  let p12 = if (Array.length varsp12)=0 then PSette.empty (compare_apron apronman)
    else PSette.union 
      (change_env_tuple_vars env apronman pp1 varsp12) 
      (change_env_tuple_vars env apronman pp2 varsp12)
  in
  let res = (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond b1 b2,
    (p12,
     concat_abstract env apronman i12 (concat_abstract env apronman p1i2 p2i1)))
  in
  Log.debug3_o logger (print_with_env env) "res = " res;
  res


(* (P1,I1) /\ (P2,I2) = (\/p1 in P1, p2 in P2: p1/\p2, I1/\ I2 *)
let meet ?(doman=doman()) env (b1,(pp1,i1)) (b2,(pp2,i2)) = 
  Log.debug3_o logger (print_with_env env) "meet s1 = " (b1,(pp1,i1));
  Log.debug3_o logger (print_with_env env) "with s2 = " (b2,(pp2,i2));
  let b = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond b1 b2 in
  let res = 
  if Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond b then
    bottom env
  else
  begin  
    let apronman = Bddapron.Domain0.man_get_apron doman in
    (b,(PSette.fold (fun p1 pp ->
      (PSette.fold (fun p2 pp -> 
        let p = Apron.Abstract1.meet apronman p1 p2 in
        if Apron.Abstract1.is_bottom apronman p then pp
        else PSette.add p pp)
      pp2 pp))
      pp1 (PSette.empty (compare_apron apronman)),
      Apron.Abstract1.meet apronman i1 i2))
  end
  in
  Log.debug3_o logger (print_with_env env) "res = " res;
  res
  
let widening ?(doman=doman()) env (b1,(pp1,i1)) (b2,(pp2,i2)) = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let (varsp1,varsi1) = get_tuple_vars env apronman (pp1,i1) in
  let (varsp2,varsi2) = get_tuple_vars env apronman (pp2,i2) in
  let varsp1i2 = Util.array_inter varsp1 varsi2 in
  let p1p2 = if (Array.length varsp2)=0 then PSette.empty (compare_apron apronman)
    else PSette.union 
      (change_env_tuple_vars env apronman pp1 varsp2)
      (change_env_tuple_vars env apronman pp2 varsp2)
  in
  let p1i2 = change_env_tuple_vars env apronman pp1 varsp1i2 in
  let p1i2 = convexify_tuple env apronman 
        (ApronUtil.env_of_vars env.Env.apronenv varsp1i2) p1i2 in
  let pp1i1 = concat_abstract env apronman p1i2 i1 in 
  (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond b1 b2,
    (p1p2,Apron.Abstract1.widening apronman pp1i1 i2))

let to_boolnumlist ?(doman=doman()) env (b,(pp,i)) = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  PSette.elements 
    (Util.psette_map (fun p -> 
      (b,Apron.Abstract1.abstract0 
          (Apron.Abstract1.change_environment apronman 
             (concat_abstract env apronman p i) env.Env.apronenv false)))
      (compare) pp)

let assign_lexpr ?(doman=doman()) env ((b,(pp,i)):t) equs :t = 
  Log.debug3_o logger (print_with_env env) "assign_lexpr to s = " (b,(pp,i));
  Log.debug3_o logger (Env.print_equations env) "with equs = " equs;
  let equs = BddapronUtil.simplify_equs env.Env.env env.Env.cond equs b in
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let (varsp,varsi) = get_tuple_vars env apronman (pp,i) in  
  let (pequs,iequs) = List.partition (is_finitely_generating env (varsp,varsi)) 
    (Env.get_num_equs env equs) in
  Log.debug3_o logger (Env.print_equations env) "fin. gen. equs = " pequs;
  let pvars = BddapronUtil.vars_to_apronvars env.Env.env
    (fst (Env.split_equs pequs)) in
  let ivars = BddapronUtil.vars_to_apronvars env.Env.env 
    (fst (Env.split_equs iequs)) in
  let (vars,exprs) = Env.split_equs equs in
  let res = 
    if (Array.length varsp)<>0 then 
      PSette.fold 
        (fun p s -> 
          let sp = Bddapron.Domain0.of_bddapron doman env.Env.env
            (to_boolnumlist ~doman env (b,(PSette.singleton (compare_apron apronman) p,i))) in
          Log.debug3_o logger (BddapronUtil.print_abstract env.Env.env doman) 
            "sp = " sp;
          let spost = BddapronUtil.abstract_convexify env.Env.env env.Env.cond
              doman (Bddapron.Domain0.assign_lexpr doman
                env.Env.env env.Env.cond sp vars exprs None) in
          Log.debug3_o logger (BddapronUtil.print_abstract env.Env.env doman) 
            "spost = " spost;
          if Bddapron.Domain0.is_bottom doman spost then s 
          else
            let (b,n) = List.hd (Bddapron.Domain0.to_bddapron doman spost) in
            let p = if (Array.length pvars)>0 then 
                PSette.singleton (compare_apron apronman) (change_env_vars env apronman 
                  (ApronUtil.abstract1_of_abstract0 env.Env.apronenv n) pvars)
	      else PSette.empty (compare_apron apronman)
            in
            join env s (b,(p,
              change_env_vars env apronman 
                (ApronUtil.abstract1_of_abstract0 env.Env.apronenv n) ivars)))
        pp (bottom env)
    else
    begin
      let sp = Bddapron.Domain0.of_bddapron doman env.Env.env
            [(b,Apron.Abstract1.abstract0 
               (Apron.Abstract1.change_environment apronman i env.Env.apronenv
                   false))] in
      Log.debug3_o logger (BddapronUtil.print_abstract env.Env.env doman) 
            "sp = " sp;
      let spost = BddapronUtil.abstract_convexify env.Env.env env.Env.cond
              doman (Bddapron.Domain0.assign_lexpr doman
                env.Env.env env.Env.cond sp vars exprs None) in
      Log.debug3_o logger (BddapronUtil.print_abstract env.Env.env doman) 
            "spost = " spost;
      if Bddapron.Domain0.is_bottom doman spost then bottom env
      else
        let (b,n) = List.hd (Bddapron.Domain0.to_bddapron doman spost) in
        let p = if (Array.length pvars)>0 then 
            PSette.singleton (compare_apron apronman) (change_env_vars env apronman 
              (ApronUtil.abstract1_of_abstract0 env.Env.apronenv n) pvars)
	  else PSette.empty (compare_apron apronman)
        in
        (b, (p,
            change_env_vars env apronman 
              (ApronUtil.abstract1_of_abstract0 env.Env.apronenv n) ivars))
    end
  in
  Log.debug3_o logger (print_with_env env) "res = " res;
  res

let forget_list ?(doman=doman()) env (b,(pp,i)) vars = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let (varsp,varsi) = get_tuple_vars env apronman (pp,i) in
  let apronvars = BddapronUtil.vars_to_apronvars env.Env.env vars in
  let papronvars = Util.array_inter apronvars varsp in
  let iapronvars = Util.array_inter apronvars varsi in
  let bsupp = BddapronUtil.supp_of_vars env.Env.env 
    (Util.list_inter vars env.Env.b_vars) in
  (Cudd.Bdd.exist bsupp b,
    (Util.psette_map (fun p -> 
       Apron.Abstract1.forget_array apronman p papronvars false) (compare_apron apronman) pp,
     Apron.Abstract1.forget_array apronman i iapronvars false))
   
let substitute_lexpr ?(doman=doman()) env s equs = 
  raise (Domain.NotSupported "Domain.FinDisPart.substitute_lexpr")

let of_boolexpr ?(doman=doman()) env bexpr =  
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let bnlist = Bddapron.Domain0.to_bddapron doman
    (Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond
      (Bddapron.Domain0.top doman env.Env.env) bexpr)
  in
  if (List.length bnlist)<1 then bottom env
  else 
    let (bb,pp) = List.fold_left (fun (bb,pp) (b,n) -> 
      (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bb b,
       PSette.add (ApronUtil.abstract1_of_abstract0 env.Env.s_apronenv n) pp))
      (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond,
       (PSette.empty (compare_apron apronman))) bnlist
    in
    (bb,(pp,Apron.Abstract1.bottom apronman env.Env.s_apronenv))

let flow ?(doman=doman()) env (b,(pp,i)) equs staycond = 
  let equs = BddapronUtil.simplify_equs env.Env.env env.Env.cond equs 
    staycond in
  Log.debug3_o logger (print_with_env env) "flow to s = " (b,(pp,i));
  Log.debug3_o logger (Env.print_boolexpr env) "with staycond = " staycond;
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let (varsp,varsi) = get_tuple_vars env apronman (pp,i) in
  let contvars = BddapronUtil.vars_to_apronvars env.Env.env 
    (fst (List.split equs)) in
  let (varsp,varsi) = (Util.array_diff varsp contvars,
                       Util.array_union varsi contvars) in
  let (_,(_,ii)) = top env in
  let ppp = if (Array.length varsp)=0 then PSette.empty (compare_apron apronman) 
    else change_env_tuple_vars env apronman pp varsp in
  let s = (b,(ppp,change_env_vars env apronman ii varsi)) in
  let res = meet_condition ~doman env s staycond in
  Log.debug3_o logger (print_with_env env) "res = " res;
  res

let accel ?(doman=doman()) ?(dir=`Forward) env s equs g = 
  raise (Domain.NotSupported "Template.FinDisPart.accel")

let of_boolnumlist ?(doman=doman()) env bnlist = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let (bb,pp) = List.fold_left (fun (bb,pp) (b,n) -> 
      (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bb b,
       PSette.add (ApronUtil.abstract1_of_abstract0 env.Env.s_apronenv n) pp))
      (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond,
       (PSette.empty (compare_apron apronman))) bnlist
  in
  (bb,(pp,Apron.Abstract1.bottom apronman env.Env.s_apronenv))

let to_boollinconsslist ?(doman=doman()) env (b,(pp,i)) =
  let apronman = Bddapron.Domain0.man_get_apron doman in
  (b,Apron.Abstract1.to_lincons_array apronman i)::
  (List.map (fun p -> (b,Apron.Abstract1.to_lincons_array apronman p)) 
    (PSette.elements pp))

let of_num ?(doman=doman()) env apron = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  (Bddapron.Expr0.Bool.dtrue env.Env.env env.Env.cond,
   (PSette.empty (compare_apron apronman),
   ApronUtil.abstract1_of_abstract0 env.Env.s_apronenv apron))

let to_boolexprbool ?(doman=doman()) env (b,_) = b 

let to_boolexpr ?(doman=doman()) env (b,(pp,i)) =
  let apronman = Bddapron.Domain0.man_get_apron doman in
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond b
    (PSette.fold (fun p bacc -> 
      Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc
        (BddapronUtil.apron_to_boolexpr env.Env.env env.Env.cond apronman
          (concat_abstract env apronman p i)))
      pp (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond))

let meet_to_boolexpr ?(doman=doman()) env s bexpr = 
  raise (Domain.NotSupported "Domain.FinDisPart.meet_to_boolexpr")

let meetbool_to_boolexpr ?(doman=doman()) env s bexpr = 
  raise (Domain.NotSupported "Domain.FinDisPart.meetbool_to_boolexpr")
end

module FdpProdInt = FdpProd(DomainStd.Box)

(*
(******************************************************************************)
let is_acyclic_arc cfg arcid = 
  let plocs = PSHGraph.predvertex cfg arcid in
  assert((Array.length plocs)=1);
  let ploc = plocs.(0) in
  let (locs,_) = PSHGraph.reachable ~filter:(fun h -> h<>arcid) cfg ploc in
  not (PSette.mem ploc locs)
*)
