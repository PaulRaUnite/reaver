(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** framework base: environment *)

type var_t = string (** variable type *)
type symbol_t = var_t Bddapron.Env.symbol (**BddApron variable type *)

type bddapronenv_t = var_t Bddapron.Env.t (** BddApron environment *)
type bddaproncond_t = var_t Bddapron.Cond.t (** BddApron numerical constraints *)
type cuddman_t = Cudd.Man.v Cudd.Man.t (** CUDD manager *)
type apronenv_t = Apron.Environment.t (** APRON environment *)

type typ = var_t Bddapron.Env.typ (** type *)
type typdef = var_t Bddapron.Env.typdef (** type definition *)

type expr_t = var_t BddapronUtil.expr_t (** expression *)
type boolexpr_t = var_t BddapronUtil.boolexpr_t (** Boolean expression *)
type numexpr_t = var_t Bddapron.Expr0.Apron.t (** numerical expression *) 
type vars_t = var_t BddapronUtil.vars_t (** variables *)
type equ_t = var_t BddapronUtil.equ_t (** equation (transition function) *)
type equs_t = var_t BddapronUtil.equs_t (** equations (transition functions) *)

type zeroexpr_t = numexpr_t (** zero-crossing expression *)
type zerodef_t = var_t * numexpr_t (** zero-crossing definition *)
type zerodefs_t = zerodef_t list (** zero-crossing definition table *)

(** {2 environment } *)

(** environment structure with APRON and BddApron environments and
    (s)tate, (i)nput, (b)oolean and (n)umerical variables *)
type t =
    {
      env : bddapronenv_t; (* logico-numerical environment (BDD APRON) *)
      cond : bddaproncond_t;
      cuddman : cuddman_t;
      apronenv : apronenv_t; (* numerical environment (APRON) 
                                  (corresponding to n_vars)*)
      i_apronenv : apronenv_t; (* numerical input environment 
                                  (corresponding to ni_vars) *)
      s_apronenv : apronenv_t; (* numerical input environment 
                                  (corresponding to ns_vars) *)
      s_vars : vars_t; (* all state variables (only non-primed) *)
      i_vars : vars_t; (* all input variables 
                          (including new inputs (double primed)) *)
      b_vars : vars_t; (* all Boolean variables 
                          (including primed and new inputs) *)
      n_vars : vars_t; (* all numerical variables 
                          (including primed and new inputs) *)
      bi_vars : vars_t; (* Boolean input variables (including new inputs) *)
      ni_vars : vars_t; (* numerical input variables (including new inputs)*)
      bs_vars : vars_t; (* Boolean state variables (only non-primed) *)
      ns_vars : vars_t; (* numerical state variables (only non-primed) *)

      primed_vars : vars_t; (* primed state variables *)
      new_i_vars : vars_t;  (* new input variables (double primed) *)
      mutable zero_vars : vars_t; (* placeholders for zero-crossings *)
      disc_q_vars : vars_t;  (* state variables introduced during 
                                translation of discrete zero-crossings *)
      cont_q_vars : vars_t; (* state variables introduced during 
                               translation of continuous zero-crossings *)
      q_i_vars : vars_t; (* input variables introduced during 
                            translation of zero-crossings *)
    }

(** {2 constants} *)

val symbol : symbol_t (** BddApron variable type structure *)
val bool_size : int ref (** number of Boolean variables *)
val cond_size : int ref (** number of numerical constraints *)
val zeros_size : int ref (** number of zero-crossing variables *)
val cudd_print_limit : int ref (** up to which BDD size formulas are printed *)
val init_var : string (** "init" variable *)
val epsilon : float (** epsilon (small real constant) *)

(** {2 constructor} *)

(** creates the environment *)
val make : ?num_factor:int -> (var_t, var_t Bddapron.Env.typdef) PMappe.t-> 
  (var_t, var_t Bddapron.Env.typ) Mappe.t -> 
  (var_t, var_t Bddapron.Env.typ) Mappe.t -> t

(** returns a new input variable with the same type as the given variable *)
val get_newinput :  t -> var_t -> var_t

(** returns a (discrete=false or continuous=true) q variable for the
    given zero-crossing variable *)
val get_q_var : var_t -> bool -> var_t

(** returns a the ith q bool input variable for the given zero-crossing 
    variable *)
val get_q_bool_i_var :  var_t -> int -> var_t

(** returns the (var,type) map of the state variables *)
val get_s_varsmap :  t -> (var_t, var_t Bddapron.Env.typ) Mappe.t

(** computes the careset (some admissible combinations of 
    numerical constraints *)  
val compute_careset :  t -> unit

(** {2 zero-crossing definitions} *)

(** returns the zero-crossing variable corresponding to 
    the given zero-crossing expression *)
val find_zero_var : zerodefs_t -> zeroexpr_t -> var_t

(** returns the zero-crossing corresponding to 
    the given zero-crossing variable *)
val get_zero_def : zerodefs_t -> var_t -> zeroexpr_t

(** returns a new zero-crossing variable *)
val get_newzero_var :  t -> var_t

(** prints the zero-crossing definitions *)
val print_zero_defs :  t -> Format.formatter -> zerodefs_t -> unit

(** {2 utilities} *)

val print_vars : t -> Format.formatter -> vars_t -> unit
val print_boolexpr : t -> Format.formatter -> boolexpr_t -> unit
val print_equations : t -> Format.formatter -> equs_t -> unit

(** splits the equations into a list of variables and a list of expressions *)
val split_equs : equs_t -> vars_t * expr_t list

(** returns the boolean equations *)
val get_bool_equs : t -> equs_t -> equs_t

(** returns the numerical equations *)
val get_num_equs : t -> equs_t -> equs_t

(** returns the number of Boolean-equivalent variables in the given 
    variable set *)
val number_of_boolvars : t -> vars_t -> int

(** {2 CUDD utilities } *)

(** groups BDD variables (for fixing order during re-ordering) *)
val cudd_group :  ('a, 'b, 'c, 'd, 'e) Bdd.Env.t0 ->
           ('a, [> 'f Bdd.Env.typ ]) Mappe.t -> unit

(** reorders the BDD variables *)
val cudd_reorder :  t -> unit

(** returns the variable corresponding to the given BDD index *) 
val var_of_cuddid :  t -> int -> var_t

(** defines a BDD variable mapping (for faster variable substitution) *)
val cudd_make_varmap :  ('a, 'b, 'c, 'd, 'e) Bdd.Env.t0 ->
           ('a * [> 'f Bdd.Env.typ ]) list -> unit
