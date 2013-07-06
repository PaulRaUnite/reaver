(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** utilities for BddApron *)

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



(** {2 Printing } *)

(** prints a boolean expression (a BDD) *)
val print_boolexpr : 'a env_t -> 'a cond_t -> Format.formatter -> 
  'a boolexpr_t -> unit

(** prints a BddApron equation *)
val print_equation : ?cont:bool -> 'a env_t -> 'a cond_t -> Format.formatter -> 
  'a equ_t -> unit

(** prints BddApron equations *)
val print_equations : ?cont:bool -> 'a env_t -> 'a cond_t -> 
  Format.formatter -> 'a equs_t -> unit

(** prints a BddApron abstract value *)
val print_abstract : 'a env_t -> ('a, 'b, 'c, 'd) doman_t -> 
  Format.formatter -> 'd abstract_t -> unit

(** prints a BddApron expression value *)
val print_expr : 'a env_t -> 'a cond_t -> Format.formatter -> 'a expr_t -> unit

(** prints a list of variables *)
val print_vars : 'a env_t -> Format.formatter -> 'a vars_t -> unit


(** {2 Operations on Boolean expressions } *)

(** simplifies a boolean expression by phi (and the careset) *)
val simplify_boolexpr : 'a env_t -> 'a cond_t -> 
  'a boolexpr_t -> 'a boolexpr_t -> 'a boolexpr_t

(** splits a boolean expression by phi *)  
val split_boolexpr : 'a env_t -> 'a cond_t -> 
  'a boolexpr_t -> 'a boolexpr_t -> 'a boolexpr_t *  'a boolexpr_t

(** computes the careset of meaningful values for an enumerated type variable *)
val get_enum_careset_for_var : 'a env_t -> 'a cond_t -> 'a var_t -> 'a var_t -> 
  'a boolexpr_t

(** removes meaningless values of enumerated types from boolexpr *)
val normalize_benumneg : 'a env_t -> 'a cond_t -> 'a boolexpr_t -> 'a boolexpr_t

(** splits a boolexpr in a list of 
     (numerical constraint conjunction /\ boolean bdd *) 
val boolexpr_to_numconvex_list : 'a env_t -> 'a cond_t -> 
  'a boolexpr_t -> 'a boolexpr_t list

(** returns the list of conjunctions (paths) of the given bdd *)
val boolexpr_to_dnf : 'a env_t -> 'a boolexpr_t -> 'a boolexpr_t list

(** removes the expressions in boolexprlist from boolexpr 
   by tdrestrict *)
val boolexpr_remove_exprs : 'a env_t -> 'a cond_t -> 
  'a boolexpr_t -> 'a boolexpr_t list -> 'a boolexpr_t

(** computes the boolean expression with topologically closed constraints 
    (the expression will be approximated by the conjunction of an
     expression over boolean variables and 
     a conjunction of numerical constraints *)
val boolexpr_topoclose : 'a env_t -> 'a cond_t -> ('a, 'b, 'c, 'd) doman_t ->
  'a boolexpr_t -> 'a boolexpr_t

(** splits a boolexpr into a list of boolexprs 
   with convex numerical constraints *)
val boolexpr_to_numconvex_list2 : ?forget_vars:'a vars_t -> 'a env_t -> 
  'a cond_t -> ('a, 'b, 'c, 'd) doman_t -> 'a boolexpr_t -> 'a boolexpr_t list

(** returns the list of guards below a certain level of the given BDD *)
val bdd_split_guards_level : int -> 'a boolexpr_t -> 
  ('a boolexpr_t * 'a boolexpr_t) list

(** removes variables from a boolexpr, 
    assuming that the numerical constraints are convex *)
val boolexpr_forget_vars : 'a env_t -> 'a cond_t -> ('a, 'b, 'c, 'd) doman_t ->
  'a vars_t -> 'a boolexpr_t -> 'a boolexpr_t

(** removes the variables in the given support from the boolean expression *)
val boolexpr_forget_supp : 'a boolexpr_t -> 'a boolexpr_t -> 'a boolexpr_t

(** removes the variables in the given support from the non-numerical expression *)
val expr_forget_supp : 'a boolexpr_t -> 'a expr_t -> 'a expr_t

(** removes the numerical constraints from a boolean expression *)
val boolexpr_remove_cond : 'a boolexpr_t -> 'a boolexpr_t -> 'a boolexpr_t

(** computes the number of boolean states represented by boolexpr 
    (boolexpr is supposed to contain only boolean state variables) *)
val bool_space_size : 'a env_t -> 'a cond_t -> 'a vars_t -> 'a boolexpr_t -> int



(** {2 Operations on expressions and equations} *)

(** simplifies an equation by the given expression (and the careset) *) 
val simplify_equ : 'a env_t -> 'a cond_t -> 'a boolexpr_t -> 'a equ_t -> 
  'a equ_t

(** simplifies an equation system by the given expression (and the careset) *)
val simplify_equs : 'a env_t -> 'a cond_t -> 'a equs_t -> 'a boolexpr_t ->
  'a equs_t

(** returns the set of guards in the given equations *)
val get_guards_equs : 'a env_t -> 'a equs_t ->  'a boolexpr_t PSette.t

(** returns the identity transition function for a set of variables *)
val get_id_equs_for : 'a env_t -> 'a cond_t -> 'a vars_t -> 'a equs_t

(** returns the =cst 0 transition function for a set of numerical variables *)
val get_zero_equs_for : 'a env_t -> 'a cond_t -> 'a vars_t -> 'a equs_t

(** computes the boolean expression x'=f(x) *)
val get_fexpr : ('a var_t -> 'a var_t) -> 'a env_t -> 'a cond_t ->  
  'a equs_t -> 'a boolexpr_t

(** computes the list of boolean expressions x'=f(x) *)
val get_fexprlist : ('a var_t -> 'a var_t) -> 'a env_t -> 'a cond_t ->  
  'a equs_t -> 'a boolexpr_t list


(** {2 Operations on abstract values} *)

(** convexifies the given BddApron value *)
val abstract_convexify : 'a env_t -> 'a cond_t -> ('a, 'b, 'c, 'd) doman_t -> 
  'd abstract_t -> 'd abstract_t

(** changes the domain of the given abstract value *)
val change_domain : 'a env_t -> ('a, 'b, 'c, 'd) doman_t -> 'd abstract_t 
  -> ('a, 'e, 'f, 'g) doman_t -> 'g abstract_t

(** {2 Checking equations and expressions} *)

(** checks whether a non-numerical equation is either identity or constant *)
val is_id_or_const_equ : 'a env_t -> 'a cond_t -> 'a equ_t -> bool

(** checks whether all non-numerical equations 
    are either identity or constant *)
val is_id_or_const_equs : 'a env_t -> 'a cond_t -> 'a equs_t -> bool

(** checks whether all equations are identity *)
val is_id_equs : 'a env_t -> 'a cond_t -> 'a equs_t -> bool

(** returns true if the boolean expression contains only numerical constraints*)
val is_num_boolexpr : 'a cond_t -> 'a boolexpr_t -> bool

(** checks whether the numerical equation is x'<>0 *)
val is_nonzero_equ : 'a env_t -> 'a equ_t -> bool

(** checks whether the equation is numeric *)
val is_num_equ : 'a env_t -> 'a equ_t -> bool

(** checks whether expr depends on the variable v *)
val depends_on_var_expr : 'a env_t -> 'a cond_t -> 'a expr_t -> 'a var_t -> bool

(** checks whether expr depends on at least one variable in vars *)
val depends_on_some_var_expr : 'a env_t -> 'a cond_t -> 'a expr_t -> 
  'a vars_t -> bool

(** checks whether the equations depend on at least one variable in vars *)
val depends_on_some_var_equs : 'a env_t -> 'a cond_t -> 'a equs_t -> 
  'a vars_t -> bool



(** {2 Variables and support} *)

(** primes a variable *)
val get_primed_var : 'a env_t -> 'a var_t -> 'a var_t

(** unprimes a variable *)
val get_unprimed_var : 'a env_t -> 'a var_t -> 'a var_t

(** returns the boolean (and enumerated) variables *)
val boolvars_of_env : 'a env_t -> 'a vars_t

(** returns the support of the given variables *)
val supp_of_vars : 'a env_t -> 'a vars_t -> 'a boolexpr_t

(** computes the support set of the leaves of an Bddapron.Apron expression *)
val get_numleaf_suppset : 'a env_t -> 'a expr_t -> 'a var_t PSette.t

(** returns the support for all variables above the given level in the BDD *)
val get_supp_above_level : 'a env_t -> 'a cond_t -> int -> 'a boolexpr_t

(** primes all variables in the expression *)
val get_primed_expr : ?ignorevars:'a var_t PSette.t -> ('a var_t -> 'a var_t) -> 'a env_t -> 'a cond_t -> 'a expr_t -> 'a expr_t

(** primes all variables in the boolean expression *)
val get_primed_boolexpr : ?ignorevars:'a var_t PSette.t -> ('a var_t -> 'a var_t) -> 'a env_t -> 'a cond_t -> 'a boolexpr_t -> 'a boolexpr_t

(** un-primes all variables in the expression *)
val get_unprimed_expr : ('a var_t -> 'a var_t) -> 'a env_t -> 'a cond_t -> 'a expr_t -> 'a expr_t

(** un-primes all variables in the boolean expression *)
val get_unprimed_boolexpr : ('a var_t -> 'a var_t) -> 'a env_t -> 'a cond_t -> 'a boolexpr_t -> 'a boolexpr_t

(** {2 Product of equations} *)

(** builds the product factorizes a numerical transition function 
    by common actions *)
val product_numequs : 'a env_t -> 'a action_t MtbddUtil.array_table_t -> 
  'a equs_t -> 
  'a action_t MtbddUtil.array_mtbdd_t

(** builds the product factorizes a transition function by common actions *)
val product_equs : 'a env_t ->
  'a MtbddUtil.poly_t MtbddUtil.arrset_table_t ->  'a equs_t -> 
  'a MtbddUtil.poly_t MtbddUtil.arrset_mtbdd_t

(** returns the set of guards below a certain level of
    the given MTBDD *)
val get_guardset_below_level_mtbdd :  'a env_t -> 'a cond_t -> int ->
  'b Cudd.Mtbdd.t -> 'a boolexpr_t PSette.t


(** {2 Conversions} *)

val var_to_apronvar : 'a env_t -> 'a var_t -> ApronUtil.var_t
val vars_to_apronvars : 'a env_t -> 'a vars_t -> ApronUtil.vars_t

val apronvar_to_var : 'a env_t -> ApronUtil.var_t -> 'a var_t
val apronvars_to_vars : 'a env_t -> ApronUtil.vars_t -> 'a vars_t

(** converts a Bddapron numerical leaf expression into a 
    Bddapron.Apron expression *)
val apronexpr_to_apronexprDD : 'a env_t -> 'a action_t -> 
  'a Bddapron.Expr0.Apron.t

(** extracts the Bddapron numerical leaf expression from 
    a Bddapron.Apron expression (provided that there is a single leaf) *)
val apronexprDD_to_apronexpr : 'a env_t -> 'a Bddapron.Expr0.Apron.t -> 
  'a action_t 

(** converts an APRON expression into a BddApron numerical expression *)
val linexpr_to_apronexprDD : 'a env_t -> ApronUtil.linexpr_t -> 
  'a Bddapron.Expr0.Apron.t

(** converts an APRON constraint into a boolean expression *)
val lincons_to_boolexpr : 'a env_t -> 'a cond_t -> ApronUtil.lincons_t -> 
  'a boolexpr_t

(** converts a conjunction of APRON constraints into a 
   BddApron boolean expression *)
val linconss_to_boolexpr : 'a env_t -> 'a cond_t -> ApronUtil.linconss_t -> 
  'a boolexpr_t

(** converts an APRON abstract value into a 
   BddApron boolean expression *)
val apron_to_boolexpr : 'a env_t -> 'a cond_t -> 'b ApronUtil.man_t -> 
   'b ApronUtil.abstract_t -> 'a boolexpr_t

(** builds a BddApron abstract value from a (purely) boolean expression and
    an APRON abstract value *)
val bddapron_to_abstract : 'a env_t -> ('a, 'b, 'c, 'd) doman_t -> 
  'a boolexpr_t -> 'b ApronUtil.abstract_t -> 'd abstract_t

(** converts a boolean expression into a BddApron abstract value *)
val boolexpr_to_abstract : 'a env_t -> 'a cond_t -> ('a, 'b, 'c, 'd) doman_t -> 
  'a boolexpr_t -> 'd abstract_t

(** converts a BddApron abstract value into a boolean expression *)
val abstract_to_boolexpr : 'a env_t -> 'a cond_t -> ('a, 'b, 'c, 'd) doman_t -> 
  'd abstract_t -> 'a boolexpr_t 


(** converts a BddApron.Apronexpr.t into an Apron.Linexpr1.t *)
val apronaction_to_linexpr : 'a env_t -> 'a cond_t -> 'a action_t -> 
  ApronUtil.linexpr_t

(** extracts the numerical part of a boolean expression as a polyhedron;
    only exact for convex expressions *)
val boolexpr_to_linconss : 'a env_t -> 'a cond_t-> 
  ('a, 'b, 'c, 'd) doman_t -> 'a vars_t -> 'a boolexpr_t -> 
  ApronUtil.linconss_t

(** converts a BddApron.Expr0.Apron.t into an Apron.Linexpr1.t,
    provided that the guard equals true *)
val apronexpr_to_linexpr : 'a env_t -> 'a cond_t -> 
  'a Bddapron.Expr0.Apron.t ->  ApronUtil.linexpr_t

(** extracts a list of equations with APRON linear expressions from 
   BDD APRON equations *)
val numequs_of_equs : 'a env_t -> 'a cond_t -> 'a equs_t -> ApronUtil.equs_t

(** extracts a list of equations with APRON linear expressions from 
   BDD APRON equations *)
val numequs_of_equs_with_guards : 'a env_t -> 'a cond_t -> 
  'a vars_t -> 'a equs_t -> 
  ('a boolexpr_t * ApronUtil.equs_t) list
