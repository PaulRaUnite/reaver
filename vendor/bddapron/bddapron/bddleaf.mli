(** Manipulation of lists of guards and leafs (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type ('a,'b) elt = {
  guard : 'a Cudd.Bdd.t;
  leaf : 'b;
}

type ('a,'b) t = ('a,'b) elt list

(*  ********************************************************************** *)
(** {3 Utilities} *)
(*  ********************************************************************** *)

val fold2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
    (** Applies f to all pairs [(elt1,elt2)] with [elt1] in
	[list1] and [elt2] in [list2]. Iterates first of the first
	list, then on the second. *)

(*  ********************************************************************** *)
(** {3 Normalisation} *)
(*  ********************************************************************** *)

val check_unicity : is_equal:('b -> 'b -> bool) -> ('a,'b) elt list -> bool
    (** Checking function: raises [Failure] if problem, returns
	[true] otherwise.

	Checks that
	- no guard is false
	- no abstract value is bottom
	- no duplicatas of abstract values
    *)
val check_disjointness : ('a, 'b) elt list -> bool
    (** Checking function: raises [Failure] if problem, returns
	[true] otherwise. Checks that the guards are exclusive. *)

val cons_unique :
  is_equal:('b -> 'b -> bool) ->
  ('a,'b) elt -> ('a,'b) elt list -> ('a,'b) elt list
    (** Performs the join of a list with an element.

	Assuming that the list argument satisfies the unicity
	property, ensures it in the result *)

val append_unique :
  is_equal:('b -> 'b -> bool) ->
  ('a,'b) elt list -> ('a,'b) elt list -> ('a,'b) elt list
    (** Append the two lists.

	Assuming that the first list argument satisfies the
	unicity property, ensures it in the result *)

val cons_disjoint :
  is_equal:('b -> 'b -> bool) -> merge:('b -> 'b -> 'b) ->
  ('a,'b) elt -> ('a,'b) elt list -> ('a,'b) elt list
    (** Performs the join of a list with an element.

	Assuming that the list argument satisfies the disjointness
	(and unicity) property, ensures it in the result *)

val append_disjoint :
  is_equal:('b -> 'b -> bool) -> merge:('b -> 'b -> 'b) ->
  ('a,'b) elt list -> ('a,'b) elt list -> ('a,'b) elt list
    (** Appends the two lists.

	Assuming that the first list argument satisfies the
	disjointness (and unicity) property, ensures it in the
	result *)

val cons :
  is_equal:('b -> 'b -> bool) -> merge:('b -> 'b -> 'b) ->
  unique:bool ->
  disjoint:bool ->
  ('a,'b) elt -> ('a,'b) elt list -> ('a,'b) elt list
    (** Calls the right cons function depending on the options. *)

val append :
  is_equal:('b -> 'b -> bool) -> merge:('b -> 'b -> 'b) ->
  unique:bool ->
  disjoint:bool ->
  ('a,'b) elt list -> ('a,'b) elt list -> ('a,'b) elt list
    (** Calls the right append function depending on the
    options. *)

val make_unique :
  is_equal:('b -> 'b -> bool) -> merge:('b -> 'b -> 'b) ->
  disjoint:bool ->
  ('a,'b) elt list -> ('a,'b) elt list
    (** Remove duplicatas (by reconstructing the list) *)

(*  ********************************************************************** *)
(** {3 Others} *)
(*  ********************************************************************** *)
val guard : cudd:'a Cudd.Man.t -> ('a,'b) t -> 'a Cudd.Bdd.t
    (** Return the union of guards in the list *)
