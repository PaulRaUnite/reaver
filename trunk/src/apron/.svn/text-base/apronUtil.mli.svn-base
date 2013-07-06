(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** utilities for APRON *)

type 'a man_t = 'a Apron.Manager.t (** APRON manager *)
type env_t = Apron.Environment.t (** APRON environment *)
type linexpr_t = Apron.Linexpr1.t (** APRON linear expression *)
type lincons_t = Apron.Lincons1.t (** APRON linear constraint *)

(** conjunction of APRON linear constraints *)
type linconss_t = Apron.Lincons1.earray 

type var_t = Apron.Var.t (** APRON variable *)
type vars_t = Apron.Var.t array (** APRON variables *)
type equ_t = var_t * linexpr_t (** APRON equation *)
type equs_t = equ_t array (** APRON equations *)
type 'a abstract_t = 'a Apron.Abstract1.t (** APRON abstract value *)

(** {2 Printing} *)

(** prints an APRON abstract value *)
val print_abstract : Format.formatter -> 'a abstract_t -> unit

(** prints an APRON equation *)
val print_equation : Format.formatter -> equ_t -> unit

(** prints APRON equations *)
val print_equations : Format.formatter -> equs_t -> unit

(** prints APRON variables *)
val print_vars : Format.formatter -> vars_t -> unit

(** prints a conjunction of APRON constraints *)
val print_linconss : Format.formatter -> linconss_t -> unit

(** {2 APRON Helpers} *)

(** primes a variable *)
val get_primed_var : var_t -> var_t

(** builds the conjunction of constraints  v'=e from the given equations *)
val get_fexpr : (var_t -> var_t) -> env_t -> equs_t -> linconss_t

(** gets the array of variables in the APRON environment *)
val vars_of_env : env_t -> vars_t

(** returns the sub-environment containing the given variables *)
val env_of_vars : env_t -> vars_t -> env_t

(** returns the number of dimensions of env *)
val dims_of_env : env_t -> int

(** checks whether the expression depends on at least one of the given 
    variables *)
val linexpr_depends_on_some_var : linexpr_t -> vars_t -> bool

(** creates the linexpr 0 *)
val make_zero_linexpr : env_t -> linexpr_t

(** creates equations (v,0) for the given variables *)
val get_zero_equations_for : env_t -> vars_t -> equs_t

(** creates the constraints (v=0) for the given variables *)
val get_zero_linconss_for : env_t -> vars_t -> linconss_t

(** APRON abstract bottom *)
val bottom0 : 'a man_t -> env_t -> 'a Apron.Abstract0.t

val abstract1_of_abstract0 : env_t -> 'a Apron.Abstract0.t -> 'a abstract_t

val lincons1_of_lincons0 : env_t -> Apron.Lincons0.t -> lincons_t

(** checks two conjunctions of constraints for syntactical equality 
    (without permutations) *)
val linconss_is_eq : linconss_t -> linconss_t -> bool

(** returns an empty conjunction of constraints *)
val linconss_empty : env_t -> linconss_t

(** rename primed variables to unprimed variables in the abstract value *)
val rename_primed_to_unprimed : (var_t -> var_t) ->'a abstract_t -> vars_t -> 'a abstract_t

(** {2 Abstract domain operations} *)

(** generalized time elapse operator *)
val elapse : 'a abstract_t -> 'a abstract_t -> 'a abstract_t 

(** generalized time elapse in the domain of d, 
    the domain of s may be different *)
val elapse_with_domain : 'b abstract_t -> 'a abstract_t -> 'b abstract_t

(** changes the domain of an abstract value *)
val change_domain : 'a abstract_t -> 'b man_t -> 'b abstract_t

(** projects out the given variables from s, but keeps their dimensions *)
val project_forget : 'a abstract_t -> vars_t -> 'a abstract_t

(** projects the zero_vars of s onto the hyperplane where zero_vars=0 *)
val project_to_zero : 'a abstract_t -> vars_t -> 'a abstract_t

(** negation of an abstract value *)
val neg : 'a abstract_t -> 'a abstract_t

(** returns the translator d in the domain man *)
val get_translator : 'a man_t -> equs_t -> linconss_t -> vars_t -> 'a abstract_t



