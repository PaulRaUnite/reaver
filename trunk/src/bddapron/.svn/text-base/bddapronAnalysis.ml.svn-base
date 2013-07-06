(******************************************************************************)
(* BddapronAnalysis *)
(* utilities for analysis *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="BddapronAnalysis";
              Log.level=Log.Debug}

type direction_t = ApronAccel.direction_t


(*****************************************************************************)
(* general utilities *)
(*****************************************************************************)
(** iterates the given image operator (on abstract domain values) 
     until convergence (without widening) *)
let fixedpoint env doman image s_start =
  let s = ref(Bddapron.Domain0.bottom doman env) in
  let s_next = ref(s_start) in
  while not(Bddapron.Domain0.is_eq doman !s !s_next) do
    s := !s_next;
    s_next := image !s_next;
    s_next := Bddapron.Domain0.join doman !s !s_next;
    Log.debug3_o logger (BddapronUtil.print_abstract env doman) 
      "s_next = " !s_next;
  done;
  !s_next

(*****************************************************************************)
(** iterates the given image operator (on predicates) 
     until convergence (without widening) *)
let fixedpoint2 env cond init image =
  let s = ref(Bddapron.Expr0.Bool.dfalse env cond) in
  let s_next = ref(init) in
  while not(Bddapron.Expr0.Bool.is_eq env cond !s !s_next) do
    s := !s_next;
    s_next := image !s_next;
    s_next := Bddapron.Expr0.Bool.dor env cond !s !s_next;
    Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) 
     "s_next = " !s_next;
  done;
  !s_next

(*****************************************************************************)
(* logico-numerical reachability *)
(*****************************************************************************)
(** compute image *)
let image ?(dir=`Forward) env cond doman g equs ivars s =
  Log.debug3_o logger (BddapronUtil.print_abstract env doman) "s = " s;
  Log.debug3_o logger (ApronAccel.print_direction) "dir = " dir;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "g = " g;
  Log.debug3_o logger (BddapronUtil.print_equations env cond) "equs = " equs;
  Log.debug3_o logger (BddapronUtil.print_vars env) "ivars = " ivars;
  let (vars,exprs) = List.split equs in
  let g = Bddapron.Expr0.Bool.dand env cond cond.Bdd.Cond.careset g in
  let sres = 
    match dir with
      |`Forward -> Bddapron.Domain0.assign_lexpr doman env cond
        (Bddapron.Domain0.meet_condition doman env cond s g)  
          vars exprs None
      |`Backward -> Bddapron.Domain0.meet_condition doman env cond
        (Bddapron.Domain0.substitute_lexpr doman env cond s vars exprs None) g 
  in
  Bddapron.Domain0.forget_list doman env sres ivars

(*****************************************************************************)
(** checks reachability of phi2 from phi1 by equs *)
let check_reach ?(dir=`Forward) env cond doman g equs ivars phi1 phi2 =
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "phi1 = " phi1;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "phi2 = " phi2;
  Log.debug3_o logger (BddapronUtil.print_equations env cond) "equs = " equs;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "g = " g;
  Log.debug3_o logger (ApronAccel.print_direction) "dir = " dir;
  let s_res = image ~dir env cond doman g equs ivars
      (BddapronUtil.boolexpr_to_abstract env cond doman phi1) in
  Log.debug3_o logger (BddapronUtil.print_abstract env doman) "s_res = " s_res;
  let res = not (Bddapron.Domain0.is_bottom doman 
    (Bddapron.Domain0.meet_condition doman env cond s_res phi2)) in
  Log.debug3_o logger (Format.pp_print_bool) "reachable = " res;
  res

(*****************************************************************************)
(** computes the precondition for arriving by equs in phi2 *)
let pre_assertion env cond doman g equs phi1 phi2  =
  Log.debug3_o logger (BddapronUtil.print_equations env cond) "equs = " equs;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "g = " g;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "phi1 = " phi1;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "phi2 = " phi2;
  let (vars,exprs) = List.split equs in
  let s = BddapronUtil.boolexpr_to_abstract env cond doman phi2 in
  let s_pre = Bddapron.Domain0.substitute_lexpr doman env cond s vars exprs 
    None in
  Log.debug3_o logger (BddapronUtil.print_abstract env doman) "s_pre = " s_pre;
  let ass = Bddapron.Expr0.Bool.tdrestrict 
    (BddapronUtil.abstract_to_boolexpr env cond doman s_pre)
      (Bddapron.Expr0.Bool.dand env cond phi1 g) in
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "pre-ass = " ass;
  ass

(*****************************************************************************)
(* compute boolean reachability using abstract domain values *)
(*****************************************************************************)
(** computes the image by boolean equations *)
let bool_image ?(dir=`Forward) env cond doman g bequs in_vars phi =
  let s = BddapronUtil.boolexpr_to_abstract env cond doman phi in
  let s_res = image ~dir env cond doman g bequs in_vars s in
  BddapronUtil.abstract_to_boolexpr env cond doman s_res

(*****************************************************************************)
(** returns the boolean-reachable states as boolean expression *)
let bool_reach ?(dir=`Forward) env cond doman g bequs in_vars phi =
  let s = BddapronUtil.boolexpr_to_abstract env cond doman phi in
  let s_reach = fixedpoint env doman 
    (image ~dir env cond doman g bequs in_vars) s in
  BddapronUtil.abstract_to_boolexpr env cond doman s_reach

(*****************************************************************************)
(** returns the boolean-reachable states as boolean expression *)
let check_bool_reach ?(dir=`Forward) env cond doman g bequs in_vars phi1 phi2 =
  let s = BddapronUtil.boolexpr_to_abstract env cond doman phi1 in
  let s_res = Bddapron.Domain0.meet_condition doman env cond
    (image ~dir env cond doman g bequs in_vars s) phi2 in
  not (Bddapron.Domain0.is_bottom doman s_res)

(*****************************************************************************)
(* check boolean reachability *)
(*****************************************************************************)

(*****************************************************************************)
(* computes the reachable expression from phi1 by fexprlist *)
let get_freach env cond phi1 ass2 fexprlist = 
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "phi1: " phi1;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "ass2: " ass2;
  List.fold_left
    (fun faccu fe -> 
       Bddapron.Expr0.Bool.dand env cond faccu fe)
    (Bddapron.Expr0.Bool.dand env cond phi1 ass2)
    fexprlist

(*****************************************************************************)
(* returns true if phi2 is reachable from phi1 by f *)
(* builds the formula: phi1(x) /\ x'=f_b(x,i) /\ phi2(x') 
   and checks whether it is satisfiable *)
(* does NOT use SMT *)
(* TODO: try save phi2 and f in a table *)
let check_bool_reach2 get_primed_var env cond assertion equs phi1 phi2 =
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "phi1: " phi1;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "phi2: " phi2;
  Log.debug3_o logger (BddapronUtil.print_equations env cond) "equs: " equs;
(*  let substvars = List.map (fun v -> (v,Env.get_primedvar v)) 
    (Util.list_diff env.Env.s_vars env.Env.n_vars) in
  let phi2p = Bddapron.Expr0.Bool.of_expr 
    (get_primed_expr env (Bddapron.Expr0.Bool.to_expr phi2) substvars) in
*)
  let phi2p = Cudd.Bdd.varmap phi2 in
  let fexprlist = BddapronUtil.get_fexprlist get_primed_var env cond equs in 
  let fexpre = List.fold_left
    (fun faccu fe -> 
       Bddapron.Expr0.Bool.dand env cond faccu fe)
    (Bddapron.Expr0.Bool.dand env cond assertion phi1)
    fexprlist
  in
  (* the formula is false <=> it is unsatisfiable <=> phi2 is not reachable *)
  let res = not (Cudd.Bdd.is_inter_empty fexpre phi2p) in
  Log.debug3_o logger (Format.pp_print_bool) "check_reach: " res;
  res

(*****************************************************************************)
let check_bool_reach3 env cond phi1f phi2 =
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "phi1f: " phi1f;
  let phi2p = Cudd.Bdd.varmap phi2 in
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "phi2: " phi2p;
  (* the formula is false <=> it is unsatisfiable <=> phi2 is not reachable *)
  let res = not (Cudd.Bdd.is_inter_empty phi1f phi2p) in
  Log.debug3_o logger (Format.pp_print_bool) "check_reach: " res;
  res

(*****************************************************************************)
(* returns the boolean postcondition, computation via predicates *)
let bool_image2 get_unprimed_var env cond fexprlist assertion forget_supp s =
  let sass = Bddapron.Expr0.Bool.dand env cond s assertion in
  let fexpr = List.fold_left
    (fun faccu feq -> 
      Bddapron.Expr0.Bool.dand env cond faccu feq)
    sass
    fexprlist in
  let post = Cudd.Bdd.exist forget_supp fexpr in
  let unprimed = BddapronUtil.get_unprimed_boolexpr get_unprimed_var env cond post in
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "post: " post;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "unprimed: " unprimed;
  unprimed  

(*****************************************************************************)
(* returns the boolean precondition, computation via predicates *)
(* state variables in forget_supp are primed, substvars = (v,v')*) 
let bool_preimage2 get_primed_var invars env cond fexprlist assertion forget_supp s =
  let spass = Bddapron.Expr0.Bool.dand env cond assertion
    (BddapronUtil.get_primed_boolexpr 
      ~ignorevars:(Util.list2psette (compare) invars)  
      get_primed_var env cond s) in
  let fexpr = List.fold_left
    (fun faccu feq -> 
      Bddapron.Expr0.Bool.dand env cond faccu feq)
    spass
    fexprlist in
  Cudd.Bdd.exist forget_supp fexpr

(*****************************************************************************)
(* computes the precondition for arriving by (vars,exprs) in dinv *)
let bool_pre_assertion2 get_primed_var env cond sinv dinv equs assertion invars =
  Log.debug3_o logger (BddapronUtil.print_equations env cond) "beqs: " equs;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "sinv: " sinv;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "dinv: " dinv;
  let (vars,_ ) = List.split equs in
  let primedvars = List.map get_primed_var vars in
  let forget_supp = BddapronUtil.supp_of_vars env primedvars in
  let fexprlist = BddapronUtil.get_fexprlist get_primed_var 
    env cond equs  in
  let spass = Bddapron.Expr0.Bool.dand env cond sinv
    (Bddapron.Expr0.Bool.dand env cond assertion
      (BddapronUtil.get_primed_boolexpr 
        ~ignorevars:(Util.list2psette (compare) invars) 
        get_primed_var env cond dinv)) in
  let s_pre = List.fold_left
    (fun faccu feq -> 
      Bddapron.Expr0.Bool.dand env cond faccu feq)
    spass
    fexprlist 
  in
  let s_pre = Cudd.Bdd.exist forget_supp s_pre in
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "s_pre = " s_pre;
  let ass = 
    Bddapron.Expr0.Bool.tdrestrict s_pre
    (Bddapron.Expr0.Bool.dand env cond sinv assertion) in
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "pre-ass = " ass;
  ass

(*****************************************************************************)
(* returns the boolean-reachable states as boolean expression *)
let bool_reach2 ?(dir=`Forward) ~get_primed_var ~get_unprimed_var env cond bequs invars s_start assertion =
  Log.debug3_o logger (Util.list_print (env.Bdd.Env.symbol.Bdd.Env.print)) "invars = " invars;
  let (svars,_ ) = List.split bequs in
  let (fnext,forget_vars) = match dir with 
    |`Forward -> (bool_image2 get_unprimed_var,List.append svars invars)
    |`Backward -> (bool_preimage2 get_primed_var invars,List.append (List.map get_primed_var svars) invars)
  in
  Log.debug3_o logger (Util.list_print (env.Bdd.Env.symbol.Bdd.Env.print)) "forget_vars = " forget_vars;

  let forget_supp = Cudd.Bdd.dand cond.Bdd.Cond.supp 
    (BddapronUtil.supp_of_vars env forget_vars) in
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "forget_supp = " forget_supp;
  let fexprlist = BddapronUtil.get_fexprlist get_primed_var env cond bequs in
  (* compute fixed point *)
(*  let s_start = Cudd.Bdd.exist s_start cond.Bdd.Cond.supp in*)
  Log.debug2_o logger (BddapronUtil.print_boolexpr env cond) "s_init = " s_start;
  fixedpoint2 env cond s_start 
    (fnext env cond fexprlist assertion forget_supp)

(*****************************************************************************)
(* checking equations *)
(*****************************************************************************)
(** returns true if equs represents the identity function w.r.t to phi (boolean) *)
 let is_id_bequs2 get_primed_var get_unprimed_var env cond g bequs phi =
  let phivars = Bddapron.Expr0.support env cond
    (Bddapron.Expr0.Bool.to_expr phi) in
  let (inveq,ninveq) = List.partition 
    (fun (v,_) -> PSette.mem v phivars) bequs in
  let fexprlist = BddapronUtil.get_fexprlist get_primed_var env cond inveq in
  let vars = PSette.elements (Bdd.Env.vars env) in
  let (svars,_ ) = List.split bequs in
  let forget_vars = Util.list_diff vars (List.map get_primed_var svars) in
  let forget_supp = Cudd.Bdd.dand cond.Bdd.Cond.supp 
    (BddapronUtil.supp_of_vars env forget_vars) in
  let post = bool_image2 get_unprimed_var env cond fexprlist g 
    forget_supp phi in
  let res = Bddapron.Expr0.Bool.is_eq env cond post 
    (Cudd.Bdd.exist cond.Bdd.Cond.supp phi) in
  Log.debug2_o logger (BddapronUtil.print_equations env cond) 
    "is_id_bequs2: inveq = " inveq;
  Log.debug2_o logger (BddapronUtil.print_equations env cond) 
    "is_id_bequs2: ninveq = " ninveq;
  Log.debug2_o logger (BddapronUtil.print_boolexpr env cond) 
    "is_id_bequs2: phi  = " phi;
  Log.debug2_o logger (BddapronUtil.print_boolexpr env cond) 
    "is_id_bequs2: post = " post;
  res && (BddapronUtil.is_id_equs env cond ninveq)
  
(*****************************************************************************)
(** returns true if equs represents the identity function w.r.t to phi (boolean);       numerical identity is checked syntactically *)
let is_id_equs2 get_primed_var get_unprimed_var env cond g bequs phi =
  let (nequs,bequs) = List.partition (BddapronUtil.is_num_equ env) bequs in
  (is_id_bequs2 get_primed_var get_unprimed_var env cond g bequs phi) &&
  (BddapronUtil.is_id_equs env cond nequs)
