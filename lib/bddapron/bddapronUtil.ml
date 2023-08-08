(******************************************************************************)
(* BddapronUtil *)
(* utilities for manipulating BDD APRON entities *)
(* author: Peter Schrammel *)
(* version: 0.9.3 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="BddapronUtil";
              Log.level=Log.Debug}

exception NotLinearAction of string (** a BddApron expression is not convertible
                                        into an APRON linear expression *)

exception NotSupported of string (** some feature is not supported *)


type 'a env_t = 'a Bddapron.Env.t (** BddApron environment *)
type 'a cond_t = 'a Bddapron.Cond.t (** BddApron conditions *)
type 'a boolexpr_t = 'a Bddapron.Expr0.Bool.t (** boolean expression (BDD) *)
type ('a, 'b, 'c, 'd) doman_t = ('a, 'b, 'c, 'd) Bddapron.Domain0.man 
                                     (** BddApron domain manager *)
type 'a var_t = 'a  (** variable *)
type 'a vars_t = 'a list  (** list of variables *)
type 'a vararr_t = 'a array  (** array of variables *)
type 'd abstract_t = 'd Bddapron.Domain0.t (** BddApron abstract value *)
type 'a expr_t = 'a Bddapron.Expr0.t (** BddApron expression *)
type 'a equ_t = 'a * 'a expr_t (** BddApron equation *)
type 'a equs_t = 'a equ_t list (** BddApron equations *)
type 'a action_t = 'a Bddapron.Apronexpr.t (** leaf of a BddApron numerical 
                                               expression *)
type 'a actions_t = 'a Bddapron.Apronexpr.t array (** array of actions *)

(******************************************************************************)
(* printing *)
(******************************************************************************)

(** prints a boolean expression (a BDD) *)
let print_boolexpr env cond fmt boolexpr =
  Bddapron.Expr0.Bool.print env cond fmt boolexpr

(** prints a BddApron equation *)
let print_equation ?(cont=false) env cond fmt (v,e) =
  if cont then Format.pp_print_string fmt ".";
  env.Bdd.Env.symbol.Bdd.Env.print fmt v;
  if not cont then Format.pp_print_string fmt "'";
  Format.pp_print_string fmt " = ";
  Bddapron.Expr0.print env cond fmt e  

(** prints BddApron equations *)
let print_equations ?(cont=false) env cond fmt equs =
  List.iter 
    (fun x -> Format.pp_print_newline fmt (); print_equation env cond fmt x)
    equs

(** prints a BddApron abstract value *)
let print_abstract env doman fmt s =
  Bddapron.Domain0.print doman env fmt s

(** prints a BddApron expression value *)
let print_expr env cond fmt expr =
  Bddapron.Expr0.print env cond fmt expr

(** prints a list of variables *)
let print_vars env fmt vars =
  Util.list_print (env.Bdd.Env.symbol.Bdd.Env.print) fmt vars

(******************************************************************************)
(* operations on boolean expressions *)
(******************************************************************************)
(** simplifies a boolean expression by phi (and the careset) *)
let simplify_boolexpr env cond expr phi = 
  let phi2 = Bddapron.Expr0.Bool.dand env cond phi 
       cond.Bdd.Cond.careset in
  if not (Bddapron.Expr0.Bool.is_cst env cond phi2) then
    Bddapron.Expr0.Bool.tdrestrict expr phi2
  else expr

(******************************************************************************)
(** splits a boolean expression by phi  
       e /\ phi, e /\ -phi *)
let split_boolexpr env cond e phi =
  let e1 = Bddapron.Expr0.Bool.tdrestrict 
    (Bddapron.Expr0.Bool.dand env cond e phi) 
    cond.Bdd.Cond.careset
  in
  let e2 = Bddapron.Expr0.Bool.tdrestrict 
    (Bddapron.Expr0.Bool.dand env cond e 
      (Bddapron.Expr0.Bool.dnot env cond phi)) 
    cond.Bdd.Cond.careset
  in
  (e1,e2)

(******************************************************************************)
(** computes the careset of meaningful values for an enumerated type variable *)
let get_enum_careset_for_var env cond var typname = 
  match Bddapron.Env.typdef_of_typ env  typname with
  |`Benum(labels) ->
    let range = Array.fold_right
      (fun l ee ->
        Bddapron.Expr0.Bool.dor env cond ee
          (Bddapron.Expr0.Benum.eq_label env cond
            (Bddapron.Expr0.Benum.var env cond var)
	    l))
      labels 
      (Bddapron.Expr0.Bool.dfalse env cond)
    in
    range

(******************************************************************************)
(** removes meaningless values of enumerated types from boolexpr *)
let normalize_benumneg env cond boolexpr =
  let supp = Bddapron.Expr0.support env cond (`Bool boolexpr) in
  PSette.fold
    (fun v e ->
      match Bddapron.Env.typ_of_var env v with
	|`Benum(typname) -> 
           let careset = get_enum_careset_for_var env cond v typname in
           Bddapron.Expr0.Bool.tdrestrict e careset 
	|_ -> e)
    supp boolexpr

(******************************************************************************)
(** splits a boolexpr in a list of 
     (boolean bdd, numerical constraint conjunction) *) 
let boolexpr_to_numconvex_list3 env cond boolexpr =
 let cuddman = env.Bdd.Env.cudd in
 let rec descend b n supp =
    Log.debug_o logger (print_boolexpr env cond) "supp = " supp;
    if Cudd.Bdd.is_cst supp then 
      if (Cudd.Bdd.is_false b) || (Cudd.Bdd.is_false n) then []
      else [(b,n)]
    else 
    begin
      Log.debug_o logger (print_boolexpr env cond) "supp = " supp;
      let topvar = Cudd.Bdd.topvar supp in
      let topvarbdd = Cudd.Bdd.ithvar cuddman topvar in
      let newsupp = Cudd.Bdd.exist topvarbdd supp in
      if not (PMappe.mem topvar env.Bdd.Env.idcondvar) then
      begin
        let topvarnbdd = Cudd.Bdd.dnot topvarbdd in
        let nbdd = (Cudd.Bdd.dand n topvarbdd) in
        let nnbdd = (Cudd.Bdd.dand n topvarnbdd) in
        let bbdd = (Cudd.Bdd.tdrestrict b topvarbdd) in
        let bnbdd = (Cudd.Bdd.tdrestrict b topvarnbdd) in
        List.append
          (descend bbdd nbdd newsupp)
          (descend bnbdd nnbdd newsupp)
      end
      else
        (descend b n newsupp)
   end
  in
  descend boolexpr (Cudd.Bdd.dtrue cuddman) (Cudd.Bdd.support boolexpr)

(******************************************************************************)
(** splits a boolexpr in a list of 
     (numerical constraint conjunction /\ boolean bdd *) 
let boolexpr_to_numconvex_list env cond boolexpr =
  List.map (fun (b,n) -> Cudd.Bdd.dand b n)
    (boolexpr_to_numconvex_list3 env cond boolexpr)

(******************************************************************************)
(** returns the list of conjunctions (paths) of the given bdd *)
let boolexpr_to_dnf env boolexpr  =
  let cubelist = ref [] in
  let add_to_list cubarr = cubelist := cubarr::!cubelist in
  Cudd.Bdd.iter_cube add_to_list boolexpr;
  List.map (fun c -> Cudd.Bdd.cube_of_minterm env.Bdd.Env.cudd c) !cubelist

(******************************************************************************)
(** removes the expressions in boolexprlist from boolexpr 
   by tdrestrict *)
let boolexpr_remove_exprs env cond boolexpr boolexprlist = 
  List.fold_right
    (fun e ee -> 
      let ee2 = Cudd.Bdd.tdrestrict ee e in 
      if not (Bddapron.Expr0.Bool.is_false env cond ee2) then
        ee2
      else ee)
    boolexprlist boolexpr
  
(******************************************************************************)
(** returns the list of guards below a certain level of the given BDD *)
let bdd_split_guards_level level bdd = Bdd.Decompose.split_level bdd level

(******************************************************************************)
(** removes the variables in the given support from the boolean expression *)
let boolexpr_forget_supp supp boolexpr = 
  Cudd.Bdd.exist supp boolexpr

(******************************************************************************)
(** removes the numerical constraints from a boolean expression *)
let boolexpr_remove_cond boolsupp boolexpr = 
  Cudd.Bdd.exist 
   (Cudd.Bdd.support_diff (Cudd.Bdd.support boolexpr) boolsupp) 
   boolexpr
 
(******************************************************************************)
(** computes the number of boolean states represented by boolexpr 
    (boolexpr is supposed to contain only boolean state variables) *)
let bool_space_size env cond boolstatevars boolexpr =
  Log.debug_o logger (print_boolexpr env cond) "bexpr=" boolexpr;
  let n_bvars = float_of_int 
    (List.fold_left
      (fun n v -> 
        let tid = PMappe.find v env.Bdd.Env.vartid in
        n + (Array.length tid))
      0 boolstatevars) in
  Log.debug3_o logger (Format.pp_print_float) "n_bvars=" n_bvars;
  let cnt = ref(0.0) in
  Cudd.Bdd.iter_cube 
    (fun cube -> 
       Log.debug3_o logger (Util.list_print 
           (fun fmt a -> Format.pp_print_string fmt (match a with 
             |Cudd.Man.False -> "f" |Cudd.Man.True -> "t" |_ -> "0"))) 
         "arr=" (Array.to_list cube);
      let n_cvars = float_of_int (Array.fold_left  
        (fun n a -> match a with |Cudd.Man.False |Cudd.Man.True ->  n+1 |_ -> n)
        0 cube) in
      cnt := !cnt +. (2.0 ** (n_bvars -. n_cvars));
      Log.debug3_o logger (Format.pp_print_float) "n_bvars=" n_bvars;
      Log.debug3_o logger (Format.pp_print_float) "n_cvars=" n_cvars;
      Log.debug3_o logger (Format.pp_print_float) "(b-c) =" 
        (n_bvars -. n_cvars);
      Log.debug3_o logger (Format.pp_print_float) "2^(b-c) =" 
        (2.0 ** (n_bvars -. n_cvars));
      Log.debug3_o logger (Format.pp_print_float) "cnt=" !cnt;
    ) 
    boolexpr;
  (int_of_float !cnt)

(******************************************************************************)
(* operations on expressions and equations *)
(******************************************************************************)
(** simplifies an equation by the given expression (and the careset) *) 
let simplify_equ env cond phi (v,expr) = 
  let phi2 = Bddapron.Expr0.Bool.dand env cond phi 
       cond.Bdd.Cond.careset in
  if not (Bddapron.Expr0.Bool.is_cst env cond phi2) then
    (v,Bddapron.Expr0.tdrestrict expr phi2)
  else (v,expr)

(******************************************************************************)
(** simplifies an equation system by phi *)
let simplify_equs env cond equs phi =
   List.map (simplify_equ env cond phi) equs

(******************************************************************************)
(** returns the set of guards in the given equations *)
let get_guards_equs env equs =
  List.fold_right
    (fun (_,e) set -> 
      match e with
      |`Bool(e) -> PSette.add (Cudd.Bdd.dnot e) (PSette.add e set)
      |`Apron(e) -> 
        let guardleaves = Cudd.Mtbdd.guardleafs e in
        Array.fold_right (fun (g,_) set -> PSette.add g set) 
          guardleaves set
      |`Benum(e) -> 
        let guardleaves = Bdd.Enum.guardlabels env e in
        List.fold_right (fun (g,_) set -> PSette.add g set) 
          guardleaves set
      |`Bint(e) -> 
        let guardleaves = Bdd.Int.guardints env.Bdd.Env.cudd e in
        List.fold_right (fun (g,_) set -> PSette.add g set) 
          guardleaves set)
    equs (PSette.empty (compare))

(******************************************************************************)
(** returns the identity transition function for a set of variables *)
let get_id_equs_for env cond vars =
  List.map (fun v -> (v,Bddapron.Expr0.var env cond v)) vars

(******************************************************************************)
(** returns the =cst 0 transition function for a set of numerical variables *)
let get_zero_equs_for env cond vars =
  List.map (fun v -> 
    (v,`Apron (Bddapron.Expr0.Apron.cst env cond 
                   (Apron.Coeff.s_of_int 0))))
    vars

(******************************************************************************)
(** computes the boolean expression x'=f(x) *)
let get_fexpr get_primed_var env cond equs =
  List.fold_left
    (fun faccu (v,expr) -> 
      let vexpr = Bddapron.Expr0.var env cond
        (get_primed_var v) in
      Bddapron.Expr0.Bool.dand env cond 
        faccu
        (Bddapron.Expr0.eq env cond vexpr expr))
    (Bddapron.Expr0.Bool.dtrue env cond)
    equs

(*****************************************************************************)
(** computes the list of boolean expressions x'=f(x) *)
let get_fexprlist get_primed_var env cond equs =
  List.map
    (fun (v,expr) -> 
      Log.debug3_o logger (print_equation env cond) "equ: " (v,expr);
      let vexpr = Bddapron.Expr0.var env cond (get_primed_var v) in
      let fe = (Bddapron.Expr0.eq env cond vexpr expr) in
      Log.debug3_o logger (print_boolexpr env cond) "fe: " fe;
      fe)
    equs

(******************************************************************************)
(* operations on abstract values *)
(******************************************************************************)
(** convexifies the given BddApron value *)
let abstract_convexify env cond doman s =
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let l = Bddapron.Domain0.to_bddapron doman s in
  let (b,n) = List.fold_left 
    (fun (bacc,nacc) (be,ne) -> 
      (Bddapron.Expr0.Bool.dor env cond bacc be,
       Apron.Abstract0.join apronman nacc ne))
    (Bddapron.Expr0.Bool.dfalse env cond,
     ApronUtil.bottom0 apronman (Bddapron.Env.apron env))
    l
  in
  Bddapron.Domain0.meet_condition doman env cond 
    (Bddapron.Domain0.of_apron doman env n) b

(******************************************************************************)
(** changes the domain of the given abstract value *)
let change_domain env old_doman s new_doman =
  let old_bddapron = Bddapron.Domain0.to_bddapron old_doman s in
  let new_apronman = Bddapron.Domain0.man_get_apron new_doman  in
  let apronenv = Bddapron.Env.apron env in 
  let new_bddapron = List.map
    (fun (b,n) -> 
      (b,
       Apron.Abstract1.abstract0 
         (ApronUtil.change_domain (ApronUtil.abstract1_of_abstract0 apronenv n) 
           new_apronman)))
    old_bddapron
  in
  Bddapron.Domain0.of_bddapron new_doman env new_bddapron

(******************************************************************************)
(* checking equations and expressions *)
(******************************************************************************)
(** checks whether a non-numerical equation is either identity or constant *)
let is_id_or_const_equ env cond (v,expr) =
    (Bddapron.Expr0.Bool.is_true env cond
      (Bddapron.Expr0.eq env cond expr (Bddapron.Expr0.var env cond v)))
    ||
    (let is_cst = 
       match expr with
        |`Bool(e) -> Bddapron.Expr0.Bool.is_cst env cond e
        |`Benum(e) -> Bdd.Enum.is_cst e
        |`Bint(e) -> Bdd.Int.is_cst e
        |_ -> assert(false)
     in
     if is_cst then true
     else
     begin 
       Log.debug3_o logger (print_expr env cond) 
         "is_id_or_const: false; failed for " expr; 
       false
     end)
 
(******************************************************************************)
(** checks whether all non-numerical equations 
    are either identity or constant *)
let is_id_or_const_equs env cond equs =
  let rec checkequ equs =
    match equs with
      |[] -> true
      |equ::tl -> if is_id_or_const_equ env cond equ then checkequ tl else false
  in checkequ equs

(******************************************************************************)
(** checks whether all equations are identity *)
let is_id_equs env cond equs =
  let rec checkequ equs =
    match equs with
      |[] -> true
      |(v,expr)::tl ->
         if Bddapron.Expr0.Bool.is_true env cond
          (Bddapron.Expr0.eq env cond expr (Bddapron.Expr0.var env cond v)) then
           checkequ tl
         else
         begin 
           Log.debug3_o logger (print_equation env cond) 
             "is_id: false; failed for " (v,expr); 
           false
         end
  in checkequ equs

(******************************************************************************)
(** returns true if the BDD contains only numerical constraints *)
let is_num_boolexpr cond boolexpr =
  let supp = Cudd.Bdd.support boolexpr in
  let condsupp = cond.Bdd.Cond.supp in
  Cudd.Bdd.is_true (Cudd.Bdd.support_diff supp condsupp)

(******************************************************************************)
(** checks whether the numerical equation is x'<>0 *)
let is_nonzero_equ env (_,expr) = 
  match expr with
    |`Apron(e) -> 
       let guardleaves = Cudd.Mtbdd.guardleafs e in
       if (Array.length guardleaves)<>1 then true
       else
         let (_,l) = guardleaves.(0) in
         not (Bddapron.ApronexprDD.is_zero env l)
    |_ -> assert(false)

(******************************************************************************)
(** checks whether the equation is numeric *)
let is_num_equ env equ =
      let (v,_) = equ in
      match (Bddapron.Env.typ_of_var env v) with
       |`Int -> true
       |`Real -> true
       |_ -> false

(******************************************************************************)
(** checks whether expr depends on the variable v *)
let depends_on_var_expr env cond expr v =
  PSette.mem v (Bddapron.Expr0.support env cond expr)

(******************************************************************************)
(** checks whether expr depends on at least one variable in vars *)
let depends_on_some_var_expr env cond expr vars =
  List.exists (fun v -> depends_on_var_expr env cond expr v) vars

(******************************************************************************)
(** checks whether the equations depend on at least one variable in vars *)
let depends_on_some_var_equs env cond equs vars =
  let dvars = Util.list2psette (compare) vars in
  Log.debug3_o logger 
    (PSette.print (env.Bdd.Env.symbol.Bdd.Env.print)) "vars=" dvars;
  if PSette.is_empty dvars then true
  else
  begin
    let rec check flist =
    match flist with
      |[] -> false
      |(_,expr)::tl -> 
      begin          
        let suppvars = Bddapron.Expr0.support env cond expr in
        Log.debug3_o logger (PSette.print (env.Bdd.Env.symbol.Bdd.Env.print)) 
          "suppvars=" suppvars;
        let empty = PSette.is_empty (PSette.inter dvars suppvars) in
        if empty then check tl
        else true
      end
    in
    check equs
  end



(******************************************************************************)
(* variables and support *)
(******************************************************************************)
(** returns the boolean (and enumerated) variables *)
let boolvars_of_env env =
  List.filter 
    (fun v -> match (Bddapron.Env.typ_of_var env v) with 
      |`Bool -> true
      |`Benum(_) -> true
      |`Bint(_,_) -> true
      |_ -> false
    )
    (Util.psette2list (Bddapron.Env.vars env)) 

(******************************************************************************)
(** returns the support of the given variables *)
let supp_of_vars env vars =
  let b_ids = List.flatten 
    (List.map
      (fun arr -> Array.to_list arr)
      (Util.pmappe2valuelist 
        (PMappe.filter 
          (fun v _ -> List.exists (fun var -> var = v) vars)
           env.Bdd.Env.vartid))) 
  in
  List.fold_left 
    (fun supp id ->
      Cudd.Bdd.dand supp (Cudd.Bdd.ithvar env.Bdd.Env.cudd id))
    (Cudd.Bdd.dtrue env.Bdd.Env.cudd)
    b_ids

(******************************************************************************)
(** computes the support set of the leaves of an Bddapron.Apron expression *)
let get_numleaf_suppset env expr = 
  match expr with 
    |`Apron(e) -> 
       let guardleaves = Cudd.Mtbdd.guardleafs e in
       Array.fold_right (fun (_,l) supp ->
         PSette.union supp (Bddapron.Apronexpr.support env.Bdd.Env.symbol l))
         guardleaves (PSette.empty (compare))
    |_ -> assert(false)  (* numerical expression expected *)
 
(******************************************************************************)
(** returns the support for all variables above (>=) 
    the given level in the BDD *)
let get_supp_above_level env cond level = 
  let build_supp ilow ihigh supp0 = 
    let suppref = ref(supp0) in
    for i=ilow to ihigh do
      suppref := Cudd.Bdd.dand !suppref (Cudd.Bdd.ithvar env.Bdd.Env.cudd i)
    done;
    !suppref
  in
  let supp = ref(Cudd.Bdd.dtrue env.Bdd.Env.cudd) in
  let env_indexhigh = env.Bdd.Env.bddindex-1 in
  supp := build_supp (max level env.Bdd.Env.bddindex0) 
                         env_indexhigh !supp;
  let cond_indexhigh = cond.Bdd.Cond.bddindex-1 in
  supp := build_supp (max level cond.Bdd.Cond.bddindex0) 
                         cond_indexhigh !supp;
  !supp

(******************************************************************************)
(** primes all variables in the expression *)
let get_primed_expr ?(ignorevars=(PSette.empty (compare))) 
    get_primed_var env cond expr =
  let substvars = List.map (fun v -> (v,get_primed_var v)) 
    (PSette.elements (PSette.diff (Bddapron.Expr0.support env cond expr) 
                                  ignorevars)) in
  Bddapron.Expr0.substitute_by_var env cond expr substvars

(** primes all variables in the boolean expression *)
let get_primed_boolexpr ?(ignorevars=(PSette.empty (compare)))
    get_primed_var env cond boolexpr = 
  Bddapron.Expr0.Bool.of_expr 
    (get_primed_expr ~ignorevars get_primed_var env cond (Bddapron.Expr0.Bool.to_expr boolexpr))

(** un-primes all variables in the expression *)
let get_unprimed_expr get_unprimed_var env cond expr =
  let substvars = List.map (fun v -> (v, get_unprimed_var v)) 
    (PSette.elements (Bddapron.Expr0.support env cond expr)) in
  Bddapron.Expr0.substitute_by_var env cond expr substvars

(** un-primes all variables in the boolean expression *)
let get_unprimed_boolexpr get_unprimed_var env cond boolexpr = 
  Bddapron.Expr0.Bool.of_expr 
    (get_unprimed_expr get_unprimed_var env cond (Bddapron.Expr0.Bool.to_expr boolexpr))

(******************************************************************************)
(* product of equations *)
(******************************************************************************)
(** builds the product factorizes a numerical transition function 
    by common actions *)
let product_numequs env array_table numequs = 
  match numequs with
    |(_,`Apron(e))::restequs -> List.fold_left
      (fun arr equ -> 
        match equ with
          |(_,`Apron(e)) -> MtbddUtil.arraymtbdd_product array_table arr
            (MtbddUtil.basemtbdd_to_arraymtbdd array_table e)
          |_ -> assert(false) (* no numerical equation *)
       )
      (MtbddUtil.basemtbdd_to_arraymtbdd array_table e)
      restequs
    |_ -> assert(false) (* no numerical equation *)

(******************************************************************************)
(** builds the product factorizes a transition function by common actions *)
let product_equs env arrset_table equs = 
  match equs with
    |(_,e)::restequs -> List.fold_left
      (fun set (_,e) -> 
        MtbddUtil.arrsetmtbdd_product arrset_table set
          (MtbddUtil.bddapronexpr_to_arrsetmtbdd env arrset_table e))
      (MtbddUtil.bddapronexpr_to_arrsetmtbdd env arrset_table e)
      restequs
    |_ -> assert(false) (* no equation *)

(******************************************************************************)
(** returns the list of guards below (<) a certain level of
    the given MTBDD *)
let get_guardset_below_level_mtbdd env cond level mtbdd =
  let supp = get_supp_above_level env cond level in
  Log.debug3_o logger (Format.pp_print_int) "level = " level;
  Log.debug3_o logger (print_boolexpr env cond) "supp = " supp;
  let nodes = Cudd.Mtbdd.nodes_below_level mtbdd (Some level) in
  let guardset = 
    Array.fold_right 
      (fun node res -> 
(*        let g = Cudd.Bdd.exist supp (Cudd.Mtbdd.guard_of_node mtbdd node) in
        Log.debug3_o logger (print_boolexpr env cond) "g = " g;
        PSette.add g res)*)
        let gg = boolexpr_to_dnf env (Cudd.Mtbdd.guard_of_node mtbdd node) in
        List.fold_right
          (fun g set -> PSette.add g set)
          (List.map (Cudd.Bdd.exist supp) gg) res)
      nodes (PSette.empty (compare))
  in
  guardset

(******************************************************************************)
(* conversions *)
(******************************************************************************)
(** primes a variable *)
let get_primed_var env v = 
  env.Bdd.Env.symbol.Bdd.Env.unmarshal 
   ((env.Bdd.Env.symbol.Bdd.Env.marshal v)^"'")

(******************************************************************************)
(** unprimes a variable *)
let get_unprimed_var env v = 
  env.Bdd.Env.symbol.Bdd.Env.unmarshal 
   (String.sub (env.Bdd.Env.symbol.Bdd.Env.marshal v) 
      0 ((String.length (env.Bdd.Env.symbol.Bdd.Env.marshal v))-1))

(******************************************************************************)
let var_to_apronvar env v = Apron.Var.of_string 
  (env.Bdd.Env.symbol.Bdd.Env.marshal v)
let vars_to_apronvars env vars =
  Array.of_list 
    (List.fold_right 
      (fun v l ->
         match Bddapron.Env.typ_of_var env v with
	   |`Int |`Real -> (var_to_apronvar env v)::l
           |_ -> l) 
       vars [])

(******************************************************************************)
let apronvar_to_var env v =  
  env.Bdd.Env.symbol.Bdd.Env.unmarshal (Apron.Var.to_string v)
let apronvars_to_vars env apronvars =
  Array.to_list (Array.map (apronvar_to_var env) apronvars)

(******************************************************************************)
(** converts a Bddapron numerical leaf expression into a 
    Bddapron.Apron expression *)
let apronexpr_to_apronexprDD env apronexpr = Cudd.Mtbdd.cst_u env.Bdd.Env.cudd
  (Cudd.Mtbdd.unique env.Bdd.Env.ext.Bddapron.Env.table apronexpr)

(******************************************************************************)
(** extracts the Bddapron numerical leaf expression from 
    a Bddapron.Apron expression (provided that there is a single leaf) *)
let apronexprDD_to_apronexpr env apronexprDD = 
  let gl = Cudd.Mtbdd.guardleafs apronexprDD in
  assert((Array.length gl)=1);
  snd (gl.(0))

(******************************************************************************)
(** converts an APRON expression into a BddApron numerical expression *)
let linexpr_to_apronexprDD env linexpr1 =
  let symbol = env.Bdd.Env.symbol in
  apronexpr_to_apronexprDD env
    (Bddapron.Apronexpr.Lin 
      (Bddapron.Apronexpr.Lin.of_linexpr1 symbol linexpr1))

(******************************************************************************)
(** converts an APRON constraint into a boolean expression *)
let lincons_to_boolexpr env cond lincons1 =
  let linexpr = linexpr_to_apronexprDD env 
    (Apron.Lincons1.get_linexpr1 lincons1) in
  match Apron.Lincons1.get_typ lincons1 with
    |Apron.Lincons0.EQ -> 
          Bddapron.Expr0.Apron.eq env cond linexpr
    |Apron.Lincons0.SUPEQ -> 
          Bddapron.Expr0.Apron.supeq env cond linexpr
    |Apron.Lincons0.SUP -> 
          Bddapron.Expr0.Apron.sup env cond linexpr
    |Apron.Lincons0.DISEQ -> 
          Bddapron.Expr0.Bool.dnot env cond
            (Bddapron.Expr0.Apron.eq env cond linexpr)
    |_ -> raise (NotSupported "congruences")

(******************************************************************************)
(** converts a conjunction of APRON constraints into a 
   BddApron boolean expression *)
let linconss_to_boolexpr env cond linconss =
  let apronenv = Bddapron.Env.apron env in
  Array.fold_left
    (fun b lincons0 ->
      let c = ApronUtil.lincons1_of_lincons0 apronenv lincons0 in
      let bexpr = lincons_to_boolexpr env cond c in
      Bddapron.Expr0.Bool.dand env cond b bexpr)
    (Bddapron.Expr0.Bool.dtrue env cond)
    (linconss.Apron.Lincons1.lincons0_array)

(******************************************************************************)
(** converts an APRON abstract value into a 
   BddApron boolean expression *)
let apron_to_boolexpr env cond apronman n =
  linconss_to_boolexpr env cond (Apron.Abstract1.to_lincons_array apronman n)

(******************************************************************************)
(** builds a BddApron abstract value from a (purely) boolean expression and
    an APRON abstract value *)
let bddapron_to_abstract env doman boolexpr numabstract =
  Bddapron.Domain0.of_bddapron doman env 
    [(boolexpr,numabstract.Apron.Abstract1.abstract0)]

(******************************************************************************)
(** converts a BddApron.Apronexpr.t into an Apron.Linexpr1.t *)
let apronaction_to_linexpr env cond action =
  let symbol = env.Bdd.Env.symbol in
  let apronenv = Bddapron.Env.apron env in
  match action with 
    |Bddapron.Apronexpr.Lin(e) -> 
	Bddapron.Apronexpr.Lin.to_linexpr1 symbol apronenv e
    |Bddapron.Apronexpr.Tree(e) -> 
    begin
      try Bddapron.Apronexpr.Lin.to_linexpr1 symbol apronenv 
        (Bddapron.Apronexpr.lin_of_tree symbol e)
      with Exit -> raise (NotLinearAction (Print.string_of_print 
            (Bddapron.Apronexpr.print symbol) action))
    end
    |_ -> raise (NotLinearAction (Print.string_of_print 
            (Bddapron.Apronexpr.print symbol) action))

(******************************************************************************)
(** extracts the numerical part of a boolean expression as a polyhedron;
    only exact for convex expressions *)
let boolexpr_to_linconss env cond doman boolvars expr =
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let apronenv = Bddapron.Env.apron env in

  let bn = Bddapron.Domain0.to_bddapron doman
    (Bddapron.Domain0.meet_condition doman env cond 
      (Bddapron.Domain0.top doman env)
      (Bddapron.Expr0.Bool.exist env cond boolvars expr)) in
  (* if expr is contradictory return bottom *) 
  if Util.list_is_empty bn then 
    Apron.Abstract1.to_lincons_array apronman
      (Apron.Abstract1.bottom apronman apronenv)
  else
    let (_,nn) = (List.hd bn) in
    Apron.Abstract1.to_lincons_array apronman 
      (ApronUtil.abstract1_of_abstract0 apronenv nn)

(******************************************************************************)
(** converts a boolean expression into a BddApron abstract value *)
let boolexpr_to_abstract env cond doman boolexpr =  
  Bddapron.Domain0.meet_condition doman env cond 
    (Bddapron.Domain0.top doman env) boolexpr

(******************************************************************************)
(** converts a BddApron abstract value into a boolean expression *)
let abstract_to_boolexpr env cond doman abstract =  
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let apronenv = Bddapron.Env.apron env in
  let l = Bddapron.Domain0.to_bddapron doman abstract in
  List.fold_left 
    (fun bacc (be,ne)-> 
       Bddapron.Expr0.Bool.dor env cond bacc 
         (Bddapron.Expr0.Bool.dand env cond be
           (linconss_to_boolexpr env cond 
             (Apron.Abstract1.to_lincons_array apronman 
                (ApronUtil.abstract1_of_abstract0 apronenv ne)))))
    (Bddapron.Expr0.Bool.dfalse env cond)
    l

(******************************************************************************)
(** converts a BddApron.Expr0.Apron.t into an Apron.Linexpr1.t,
    provided that the guard equals true *)
let apronexpr_to_linexpr env cond expr = 
  let guardleaves = Cudd.Mtbdd.guardleafs expr in
  assert((Array.length guardleaves)=1);
  let (g,a) = guardleaves.(0) in
  Log.debug3_o logger (Bddapron.Apronexpr.print env.Bdd.Env.symbol) 
    "action: " a;
  assert(Bddapron.Expr0.Bool.is_true env cond g);
  apronaction_to_linexpr env cond a

(******************************************************************************)
(** extracts a list of equations with APRON linear expressions from 
   BDD APRON equations *)
let numequs_of_equs env cond equs =
  let numequs = List.filter (is_num_equ env) equs in
  Array.of_list (List.map 
    (fun (v,expr) ->
      Log.debug3_o logger (print_equation env cond) "equation: " (v,expr); 
      match expr with 
	|`Apron(e) -> 
           (var_to_apronvar env v, 
            apronexpr_to_linexpr env cond e)
        |_ -> assert(false))
    numequs)

(******************************************************************************)
(** extracts a list of equations and their guard conjunctions 
     with APRON linear expressions from BDD APRON equations *)
let numequs_of_equs_with_guards env cond boolvars (equs:'a equs_t) =
  let numequs = List.filter (is_num_equ env) equs in
  let (vars,_) = List.split numequs in
  let nvars = Array.of_list vars in
  let bsupp = supp_of_vars env boolvars in
  let tabarr = MtbddUtil.make_table_action_array env in
  let numarr = product_numequs env tabarr numequs in
  let tabarrset = MtbddUtil.make_table_action_arrset env in
  let numarrset = MtbddUtil.arraymtbdd_to_arrsetmtbdd (compare) tabarrset 
    numarr in
  let numarrset = MtbddUtil.arrsetmtbdd_exists tabarrset numarrset bsupp in
  let dnfgl = MtbddUtil.mtbdd_to_dnf env.Bdd.Env.cudd numarrset in
  List.fold_left 
    (fun res (g,arrset) ->
      PSette.fold
        (fun arr res ->
          let eqs = Util.array_map2 (fun v a ->
             (var_to_apronvar env v, apronaction_to_linexpr env cond a))
            nvars arr in
          (g,eqs)::res)
        arrset res)
    [] dnfgl

(******************************************************************************)
(** computes the boolean expression with topologically closed constraints 
    (the expression will be approximated by the conjunction of an
     expression over boolean variables and 
     a conjunction of numerical constraints *)
(* g^bool /\ closure(/\i g^num_i) *)
(* TODO: better solution directly manipulating BDD??? 
   ite(c,f+,f-) ===> ite(c,ite(_c,f-,f+),f-) *)
let boolexpr_topoclose env cond doman boolexpr =
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let apronenv = Bddapron.Env.apron env in
  let abstract = Bddapron.Domain0.meet_condition doman env cond
        (Bddapron.Domain0.top doman env) boolexpr in
  let l = Bddapron.Domain0.to_bddapron doman abstract in 
  Log.debug3_o logger (print_abstract env doman) "abstract = " abstract;
  List.fold_right
    (fun (be,ne) res -> 
      let closed_conss = Apron.Abstract1.to_lincons_array apronman
              (Apron.Abstract1.closure apronman 
                (ApronUtil.abstract1_of_abstract0 apronenv ne)) in
       Bddapron.Expr0.Bool.dor env cond res 
         (Bddapron.Expr0.Bool.dand env cond be
         (linconss_to_boolexpr env cond closed_conss)))
    l (Bddapron.Expr0.Bool.dfalse env cond)

(******************************************************************************)
(** splits a boolexpr into a list of boolexprs 
   with convex numerical constraints *)
let boolexpr_to_numconvex_list2 ?(forget_vars=[]) env cond doman boolexpr =
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let apronenv = Bddapron.Env.apron env in
  let l = Bddapron.Domain0.to_bddapron doman 
    (Bddapron.Domain0.forget_list doman env 
      (Bddapron.Domain0.meet_condition doman env cond
        (Bddapron.Domain0.top doman env) boolexpr)
      forget_vars)
  in
  List.map
    (fun (be,ne)-> Bddapron.Expr0.Bool.dand env cond be
         (linconss_to_boolexpr env cond 
           (Apron.Abstract1.to_lincons_array apronman
             (ApronUtil.abstract1_of_abstract0 apronenv ne))))
    l

(******************************************************************************)
(** removes variables from a boolexpr, 
    assuming that the numerical constraints are convex *)
let boolexpr_forget_vars env cond doman forget_vars boolexpr =
(*  Bddapron.Formula.Expr0.Bool.forget doman env cond boolexpr forget_vars *)
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let apronenv = Bddapron.Env.apron env in
  let l = Bddapron.Domain0.to_bddapron doman 
    (Bddapron.Domain0.forget_list doman env 
      (Bddapron.Domain0.meet_condition doman env cond
        (Bddapron.Domain0.top doman env) boolexpr)
      forget_vars)
  in
  List.fold_right
    (fun (be,ne) res -> Bddapron.Expr0.Bool.dor env cond res
       (Bddapron.Expr0.Bool.dand env cond be
         (linconss_to_boolexpr env cond 
           (Apron.Abstract1.to_lincons_array apronman
             (ApronUtil.abstract1_of_abstract0 apronenv ne)))))
    l ( Bddapron.Expr0.Bool.dfalse env cond)

(******************************************************************************)
(** removes the non-numerical inputs from the non-numerical expression *)
let expr_forget_supp supp expr =
  match expr with 
    |`Bool(e) -> `Bool(Cudd.Bdd.exist supp e)
    |`Benum(e) -> 
       `Benum({Bdd.Enum.typ=e.Bdd.Enum.typ;
               Bdd.Enum.reg=(Array.map 
                (fun bdd -> Cudd.Bdd.exist supp bdd) e.Bdd.Enum.reg)})
    |`Bint(e) ->
       `Bint({Bdd.Int.signed=e.Bdd.Int.signed;
              Bdd.Int.reg=(Array.map 
                (fun bdd -> Cudd.Bdd.exist supp bdd) e.Bdd.Int.reg)})
    |`Apron(e) -> assert(false)  (* boolean expression expected *)

(******************************************************************************)
(** converts numerical equations into the list (boolguard,APRON guard, APRON equs)) *)
let numequs_to_guardedactions env cond 
    ?(assertion=Bddapron.Expr0.Bool.dtrue env cond) doman numequs =
  let numarr_table = MtbddUtil.make_table_action_array env in
  let numarr = product_numequs env numarr_table numequs in
  let numarr = Cudd.Mtbdd.ite assertion numarr (Cudd.Mtbdd.cst env.Bdd.Env.cudd numarr_table [||]) in
  let res = Array.fold_left 
    (fun res (g,l) ->
      Log.debug3_o logger (MtbddUtil.print_array (MtbddUtil.print_action env)) 
        "actions: " l;
      let gbnn = boolexpr_to_numconvex_list3 env cond g in
      match gbnn with
      | [] -> res
      | gbnn -> 
      begin  
      List.fold_left 
        (fun res (gb,gn) ->
          if (Array.length l)=0 then (gb,
            ApronUtil.linconss_false (Bddapron.Env.apron env),l)::res
          else 
          begin
            Log.debug3_o logger (print_boolexpr env cond) "bool-guard: " gb;
            Log.debug3_o logger (print_boolexpr env cond) "boolnum-guard: " gn;
            let gn = boolexpr_to_linconss env cond doman [] gn in
            Log.debug3_o logger ApronUtil.print_linconss "num-guard: " gn;
            (gb,gn,l)::res 
          end) 
        res gbnn
      end)
    [] (Cudd.Mtbdd.guardleafs numarr)
  in
  res

(** generates a list of a given number of disjoint boolean expressions over the given set of variables such that their disjunction is true *)
let generate_boolencoding env cond vars n = 
  let rec generate vars b n =  
    if n<=1 then [b]
    else
    match vars with 
    |[] -> assert(false);
    |v::tvars -> 
      let v = Bddapron.Expr0.Bool.var env cond v in
      let b1 = Bddapron.Expr0.Bool.dand env cond b v in
      let b2 = Bddapron.Expr0.Bool.dand env cond b 
        (Bddapron.Expr0.Bool.dnot env cond v) in
      List.append
        (generate tvars b1 (n/2))
        (generate tvars b2 (n-(n/2)))
  in
  generate vars (Bddapron.Expr0.Bool.dtrue env cond) n
  
