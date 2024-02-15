(** Normalized condition environments (base module) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 Datatypes } *)
(*  ********************************************************************** *)

type ('a,'b,'c,'d) t = {
  symbol : 'a Env.symbol;
  compare_cond : 'c -> 'c -> int;
  negate_cond : 'b -> 'c -> 'c;
  support_cond : 'b -> 'c -> 'a PSette.t;
  mutable print_cond : 'b -> Format.formatter -> 'c -> unit;

  mutable cudd : 'd Cudd.Man.t;
    (** CUDD manager *)
  mutable bddindex0 : int;
    (** First index for conditions *)
  mutable bddsize : int;
    (** Number of indices dedicated to conditions *)
  mutable bddindex : int;
    (** Next free index in BDDs used by {!idb_of_cond}. *)
  mutable condidb : ('c,int*bool) PDMappe.t;
    (** Two-way association between a condition and a pair of a
	BDD index and a polarity *)
  mutable supp : 'd Cudd.Bdd.t;
    (** Support of conditions *)
  mutable careset : 'd Cudd.Bdd.t;
    (** Boolean formula indicating which logical combination known
	as true could be exploited for simplification.  For
	instance, [x>=1 => x>=0]. *)
  (** Maximum BDD variables used by conditions; forbids automated enlargement in
      none. *)
  bddmax : int option;
}

(*  ********************************************************************** *)
(** {3 Printing} *)
(*  ********************************************************************** *)

val print : 'b -> Format.formatter -> ('a,'b,'c,'d) t -> unit

(*  ********************************************************************** *)
(** {3 Constructors} *)
(*  ********************************************************************** *)

val make :
  symbol:'a Env.symbol ->
  compare_cond:('c -> 'c -> int) ->
  negate_cond:('b -> 'c -> 'c) ->
  support_cond:('b -> 'c -> 'a PSette.t) ->
  print_cond:('b -> Format.formatter -> 'c -> unit) ->
  ?bddindex0:int ->
  ?bddsize:int ->
  ?bddmax:int ->
  'd Cudd.Man.t ->
  ('a,'b,'c,'d) t

val copy : ('a,'b,'c,'d) t -> ('a,'b,'c,'d) t

(*  ********************************************************************** *)
(** {3 Internal functions} *)
(*  ********************************************************************** *)

val permutation : ('a,'b,'c,'d) t -> int array
    (** Compute the permutation for normalizing the environment *)
val permute_with : ('a,'b,'c,'d) t -> int array -> unit
    (** Apply the given permutation to the environment *)
val normalize_with : ('a,'b,'c,'d) t -> int array
    (** Combine the two previous functions, and return the permutation *)
val reduce_with : ('a,'b,'c,'d) t -> 'd Cudd.Bdd.t -> unit
    (** Remove from the environment all conditions that do not
	belong to the given support. Does not perform
	normalization (so there may be "holes" in the allocation
	of indices *)
val clear : ('a,'b,'c,'d) t -> unit
    (** Clear all the conditions (results in a normalized environments) *)
val check_normalized : 'b -> ('a,'b,'c,'d) t -> bool

(*  ********************************************************************** *)
(** {3 Operations} *)
(*  ********************************************************************** *)

val cond_of_idb : ('a,'b,'c,'d) t -> int * bool -> 'c
val idb_of_cond : (('a, _, _, _, _) Env.O.t as 'b) -> ('a, 'b, 'c, 'd) t -> 'c -> int * bool
val compute_careset : ('a,'b,'c,'d) t -> normalized:bool -> unit
val is_leq : ('a,'b,'c,'d) t -> ('a,'b,'c,'d) t -> bool
val is_eq : ('a,'b,'c,'d) t -> ('a,'b,'c,'d) t -> bool
val shift : ('a,'b,'c,'d) t -> int -> ('a,'b,'c,'d) t
val shift_with : ('a,'b,'c,'d) t -> int -> int array
val extend_with : (('a, _, _, _, _) Env.O.t as 'b) -> ('a,'b,'c,'d) t -> int -> unit
val lce : ('a,'b,'c,'d) t -> ('a,'b,'c,'d) t -> ('a,'b,'c,'d) t
val permutation12 : ('a,'b,'c,'d) t -> ('a,'b,'c,'d) t -> int array
val permutation21 : ('a,'b,'c,'d) t -> ('a,'b,'c,'d) t -> int array

(*  ********************************************************************** *)
(** {3 Facility for transient computations} *)
(*  ********************************************************************** *)

type ('a,'b,'c,'d) repo
val save: ('a,'b,'c,'d) t -> ('a,'b,'c,'d) repo
val restore_with: ('a,'b,'c,'d) repo -> ('a,'b,'c,'d) t -> unit

(*  ********************************************************************** *)
(** {3 Level 2} *)
(*  ********************************************************************** *)

type ('a,'b) value = {
  cond : 'a;
  val1 : 'b
}
val make_value : 'a -> 'b -> ('a,'b) value
val get_cond : ('a,'b) value -> 'a
val get_val1 : ('a,'b) value -> 'b
val get_env : ('a, ('b, 'c) Env.value) value -> 'b
val get_val0 : ('a, ('b, 'c) Env.value) value -> 'c
