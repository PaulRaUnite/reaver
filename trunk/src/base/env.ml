(******************************************************************************)
(* env *)
(* environment data structure and utilities *)
(* author: Peter Schrammel *)
(* version: 0.9.3 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Base.Env";
              Log.level=Log.Debug3}

type var_t = string
type symbol_t = var_t Bddapron.Env.symbol

type bddapronenv_t = var_t Bddapron.Env.t
type bddaproncond_t = var_t Bddapron.Cond.t
type cuddman_t = Cudd.Man.v Cudd.Man.t
type apronenv_t = Apron.Environment.t

type typ = var_t Bddapron.Env.typ
type typdef = var_t Bddapron.Env.typdef

type expr_t = var_t BddapronUtil.expr_t
type boolexpr_t = var_t BddapronUtil.boolexpr_t
type numexpr_t = var_t Bddapron.Expr0.Apron.t
type vars_t = var_t BddapronUtil.vars_t
type equ_t = var_t BddapronUtil.equ_t
type equs_t = var_t BddapronUtil.equs_t

type zeroexpr_t = numexpr_t
type zerodef_t = var_t * numexpr_t
type zerodefs_t = zerodef_t list


type t =
{
  env : bddapronenv_t;
  cond : bddaproncond_t;
  cuddman : cuddman_t;
  apronenv : apronenv_t;
  i_apronenv : apronenv_t;
  s_apronenv : apronenv_t;
  s_vars : vars_t;
  i_vars : vars_t;
  b_vars : vars_t;
  n_vars : vars_t;
  bi_vars : vars_t;
  ni_vars : vars_t;
  bs_vars : vars_t;
  ns_vars : vars_t;

  primed_vars : vars_t;
  new_i_vars : vars_t;
  mutable zero_vars : vars_t;
  disc_q_vars : vars_t;
  cont_q_vars : vars_t;
  q_i_vars : vars_t;
  p_vars : vars_t;
}

let symbol = Bddapron.Env.string_symbol
let bool_size = ref(-1)
let cond_size = ref(-1)
let zeros_size = ref(-1)
let bool_i_size = ref(0)
let z_i_size = ref(0)
let q_size = ref(0)
let p_size = ref(30)
let cudd_print_limit = ref 100 (* up to which BDD size formulas are printed*)

let zero_prefix = "__z"
let p_prefix = "__p"
let epsilon = 0.015625
let init_var = "init"
(*let ctrl_var = "_ctrl"*)

(******************************************************************************)
(* CUDD variable index layout *)
(******************************************************************************)
(* 

[0...bool_size]: Boolean and enumerated variables
- 1 for each Bool
- m_t bits for each Benum or Bint of type t
- 3 for each bit of a state variable (var,primed_var,new_input)
- 1 for each bit of an input variable

[bool_size+1...bool_size+1+cond_size]: numerical constraints
- 2 per term (c, -c)
*)

let print_vars env fmt vars =
  Util.list_print (Format.pp_print_string) fmt vars

let print_boolexpr env fmt bexpr =
  BddapronUtil.print_boolexpr env.env env.cond fmt bexpr

let print_equations env fmt equs =
  BddapronUtil.print_equations env.env env.cond fmt equs


(******************************************************************************)
(* CUDD utilities etc. *)
(******************************************************************************)

(* computes the careset (admissible combinations of numerical constraints) *)  
let compute_careset env = 
  Bdd.Cond.compute_careset env.cond ~normalized:true

(* start variable reordering of CUDD *) 
let cudd_reorder env = Cudd.Man.reduce_heap env.cuddman 
  Cudd.Man.REORDER_SIFT 0

let get_s_varslist env =
  List.fold_left
    (fun l v -> (v,(Bddapron.Env.typ_of_var env.env v))::l)
    [] env.s_vars 

(* grouping variables *)
let cudd_group env s_vars  =
  (* fix the order of all bits of a variable including primed and new_inputs *)
  List.iter
    (fun (var,typ) ->
      match typ with
        | #Bdd.Env.typ ->
	  let tid = PMappe.find var env.Bdd.Env.vartid in
	  Cudd.Man.group env.Bdd.Env.cudd tid.(0) (3*(Array.length tid)) 
            Cudd.Man.MTR_FIXED
	| _ -> ())
     s_vars;
  (* make zero-variables reorderable *)
  (*Cudd.Man.group env.Bdd.Env.cudd 0 !zeros_size Cudd.Man.MTR_DEFAULT;*)
  (* make finite type variables (all their bits etc) reorderable *)
  (*Cudd.Man.group env.Bdd.Env.cudd !zeros_size (!bool_size - !zeros_size) *)
  Cudd.Man.group env.Bdd.Env.cudd 0 !bool_size
    Cudd.Man.MTR_DEFAULT;
  (* make numerical constraints reorderable *)
  Cudd.Man.group env.Bdd.Env.cudd !bool_size !cond_size 
    Cudd.Man.MTR_DEFAULT;
  (* keep numerical constraints below the finite type varibles *)
  Cudd.Man.group env.Bdd.Env.cudd 0 (!bool_size + !cond_size + 1) 
    Cudd.Man.MTR_FIXED

(* setting up varmap for exchanging unprimed and primed variable *)
let cudd_make_varmap env s_vars =
  let tab = Array.init (!bool_size + !cond_size) (fun i -> i) in
  List.iter
    (fun (var,typ) ->
      match typ with
	| #Bdd.Env.typ ->
	  let tid = PMappe.find var env.Bdd.Env.vartid in
	  let length = Array.length tid in (* size of type *)
	  Array.iter
	    (fun id -> 
	      tab.(id) <- id+length;
	      tab.(id+length) <- id)
	    tid
	| _ -> ())
      s_vars;
    Cudd.Man.set_varmap env.Bdd.Env.cudd tab

(******************************************************************************)
(* utilities *)
(******************************************************************************)

(* extracts boolean resp. numerical variables from the environment *)
let env_to_boolnumvars env =
   List.partition 
      (fun v -> 
         match (Bddapron.Env.typ_of_var env v) with 
           | `Int -> false | `Real -> false |_ -> true)
      (PSette.elements (Bddapron.Env.vars env))

(* returns a q variable for the given zero-crossing id *)
let get_q_var zvar is_continuous = 
  "_q"^zvar^(if is_continuous then "c" else "d")

(* returns a the ith q bool input variable for the given zero-crossing id *)
let get_q_bool_i_var zvar i = 
  (get_q_var zvar true)^"_i"^(string_of_int i)

(* returns a new input variable with the same type as v *)
let get_newinput env v = 
  let new_v = v^"''" in
  Log.debug_o logger Format.pp_print_string "new input: " new_v;
  new_v

(* returns the list of new inputs *)
let create_newinputs s_vars =
  List.map (fun (v,t) -> (v^"''",t)) s_vars

let size_of_booltype env t =
  match t with
  |`Bool -> 1
  |`Benum(td) -> Bdd.Enum.size_of_typ env td
  |`Bint(_,s) -> s
  |_ -> 0

let number_of_boolvars_varstyp env vars =
    Mappe.fold
      (fun _ t size -> size + (size_of_booltype env t))
      vars 0

let number_of_boolvars (env:t) (vars:vars_t) =
  List.fold_left
    (fun size v -> 
       size + (size_of_booltype env.env (Bddapron.Env.typ_of_var env.env v)))
    0 vars

(* computes the size of BDD layout sections *)
let compute_sizes cuddman typedefs s_vars i_vars =
  let tempenv = Bddapron.Env.make cuddman ~symbol:symbol 
              ~relational:false ~bddindex0:0 ~bddsize:1
  in
  PMappe.iter (Bddapron.Env.add_typ_with tempenv) typedefs; 
  Log.debug2_o logger 
    (fun fmt td -> PMappe.iter (fun a b -> Format.pp_print_string fmt ("\n"^a^"="); Bdd.Env.print_typdef (Format.pp_print_string) fmt b) td) "typedefs:" typedefs;
  let number_of_num_vars vars =
    Mappe.fold
      (fun _ t num ->
        num + match t with
	  |`Int |`Real -> 1
	  |_ -> 0)
      vars 0
  in
  let bool_state = number_of_boolvars_varstyp tempenv s_vars in
  let bool_input = number_of_boolvars_varstyp tempenv i_vars in
  let num_vars = (number_of_num_vars s_vars) + (number_of_num_vars i_vars) in
  (bool_state,bool_input,num_vars)

(* returns the variable corresponding to the BDD index i *) 
let var_of_cuddid env i =
  let s = PMappe.filter
    (fun v iarr -> 
      Array.fold_right (fun ii accu -> accu || (ii=i)) iarr false)
    env.env.Bdd.Env.vartid
  in
  let (v,_) = PMappe.choose s in
  v

let rec make_bool_vars prefix n = 
  if n=0 then [] 
  else (prefix^(string_of_int n),`Bool)::(make_bool_vars prefix (n-1))

(******************************************************************************)
(* creates the environment *)
(******************************************************************************)
let make ?(num_factor=1) typedefs s_varstyp i_varstyp =
  
(*  let s_varstyp = Mappe.add ctrl_var `Bool s_varstyp in*)

  let cuddman = Cudd.Man.make_v() in 
  Cudd.Man.print_limit := !cudd_print_limit;

  (* compute index ranges *)
  let (bool_state_size,bool_input_size,num_size) = 
    compute_sizes cuddman typedefs s_varstyp i_varstyp in

  zeros_size := num_size*2*num_factor;
  bool_size := 3*bool_state_size + bool_input_size + 10* !zeros_size +1 + !p_size; 

  (* a factor 2 is minimum, our benchmarks need 3 without retry *)
  cond_size := 20*num_size*num_factor; 
  Log.debug_o logger (Format.pp_print_int) "bool_size=" !bool_size;
  Log.debug_o logger (Format.pp_print_int) "cond_size=" !cond_size;
  Log.debug_o logger (Format.pp_print_int) "zeros_size=" !zeros_size;

  (* create environment *)
  let env = Bddapron.Env.make cuddman ~symbol:symbol 
              ~relational:false ~bddindex0:0 
              ~bddsize:(!bool_size)
  in
  let cond = Bddapron.Cond.make cuddman ~symbol:symbol 
    ~bddindex0:(!bool_size) ~bddsize:!cond_size
  in
  (* add types *)
  PMappe.iter (Bddapron.Env.add_typ_with env) typedefs; 
  (* add variables *)
  let p_varstyp = make_bool_vars p_prefix !p_size in
  let (p_vars,_) = List.split p_varstyp in
  let i_varstyp = Util.mappe2list i_varstyp in
  let _ = Bddapron.Env.add_vars_with env i_varstyp in
  let s_varstyp = Util.mappe2list s_varstyp in
  let new_i_varstyp = create_newinputs s_varstyp in
  let (new_i_vars,_) = List.split new_i_varstyp in
  let _ = Bddapron.Env.add_vars_with env new_i_varstyp in
  let (bi_vars,ni_vars) = env_to_boolnumvars env in
  let _ = Bddapron.Env.add_vars_with env p_varstyp in
  let _ = Bddapron.Env.add_vars_with env s_varstyp in
  (* add primed variables *)
  let primed_varstyp = List.map 
    (fun (v,t) -> (BddapronUtil.get_primed_var env v,t)) s_varstyp in
  let _ = Bddapron.Env.add_vars_with env primed_varstyp in

  (* lists for different kinds of variables *)
  let (b_vars,n_vars) = env_to_boolnumvars env in
  let b_vars = List.concat [b_vars;p_vars] in
  let (s_vars,_) = List.split s_varstyp in
  let i_vars = List.append bi_vars ni_vars in
  let bs_vars = Util.list_inter b_vars s_vars in
  let ns_vars = Util.list_inter n_vars s_vars in
  let (primed_vars,_) = List.split primed_varstyp in

  (* add zerodef variables *)
  let rec make_zero_varstyp l = 
    if l<0 then []
    else (zero_prefix^(string_of_int l),`Bool)::(make_zero_varstyp (l-1))
  in
  let _ = Bddapron.Env.add_vars_with env (make_zero_varstyp (!zeros_size-1)) in

  (* group variables*)
(*  cudd_group env s_vars; *)

  (* set variable mapping for exchanging primed and unprimed variables *)
  cudd_make_varmap env s_varstyp;

  (* create environment for APRON *)
  let is_int v = match Bddapron.Env.typ_of_var env v with
    |`Int -> true |_ -> false in
  let (iintvars,irealvars) = List.partition (is_int) ni_vars in
  let (iintvars,irealvars) = (BddapronUtil.vars_to_apronvars env iintvars,
                              BddapronUtil.vars_to_apronvars env irealvars) in
  let (sintvars,srealvars) = List.partition (is_int) ns_vars in
  let (pintvars,prealvars) = 
    (BddapronUtil.vars_to_apronvars env 
      (List.map (BddapronUtil.get_primed_var env) sintvars),
     BddapronUtil.vars_to_apronvars env 
      (List.map (BddapronUtil.get_primed_var env) srealvars)) in
  let (sintvars,srealvars) = (BddapronUtil.vars_to_apronvars env sintvars,
                              BddapronUtil.vars_to_apronvars env srealvars) in

  let i_apronenv = Apron.Environment.make iintvars irealvars in 
  let s_apronenv = Apron.Environment.make sintvars srealvars in 
  let apronenv = Apron.Environment.make 
    (Array.concat [sintvars;pintvars;iintvars]) 
    (Array.concat [srealvars;prealvars;irealvars]) in 
(*  Log.debug2_o logger (Apron.Environment.print) "apronenv: " apronenv;
  Log.debug2_o logger (Apron.Environment.print) "s_apronenv: " s_apronenv;
  Log.debug2_o logger (Apron.Environment.print) "i_apronenv: " i_apronenv; *)

  {env;
   cond;
   cuddman;
   apronenv;
   i_apronenv;
   s_apronenv;
   s_vars;
   i_vars;
   b_vars;
   n_vars;
   bi_vars;
   ni_vars;
   bs_vars;
   ns_vars;
   new_i_vars;
   primed_vars;
   zero_vars = [];
   disc_q_vars = [];
   cont_q_vars = [];
   q_i_vars = [];
   p_vars;
  }

(** splits the equations into a list of variables and a list of expressions *)
let split_equs equs = List.split equs 

(** returns the boolean equations *)
let get_bool_equs env es = List.filter (fun (v,_) -> List.mem v env.b_vars) es

(** returns the numerical equations *)
let get_num_equs env es = List.filter (fun (v,_) -> List.mem v env.n_vars) es


(******************************************************************************)
(* zero crossing definition table *)
(******************************************************************************)

let find_zero_var zeros zexpr = Util.assoclist_find_key zeros zexpr
let get_zero_def zeros zvar = List.assoc zvar zeros

let get_newzero_var env = 
  let l = List.length env.zero_vars in
  if l >= !zeros_size then 
    raise Bdd.Env.Bddindex;
  let v = zero_prefix^(string_of_int l) in
  env.zero_vars <- List.append env.zero_vars [v];
  v

let print_zero_defs env fmt zeros =
  Util.list_print
    (fun fmt (z,e) ->
      Format.pp_print_string fmt z;
      Format.pp_print_string fmt " = ";
      Bddapron.Expr0.Apron.print env.env env.cond fmt e)
    fmt zeros
