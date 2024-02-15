(** Bounded unsigned integer expressions with BDDs *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(**

  This module encode the classical arithmetic and logical operations
  on arrays of bits, each bit being defined by a BDD.  It doesn't
  need any initialization, and there is no in-place modification.

  The type handled by the module is an array of BDDs, which
  represent a processor-like register with the Least Significant Bit
  in first position.

  This module requires the mlcuddidl library.

*)

type 'a t = 'a Cudd.Bdd.t array
  (** type of arrays of bits *)

type dt = Cudd.Man.d t
type vt = Cudd.Man.v t

(*  *********************************************************************** *)
(** {3 Logical operations} *)
(*  *********************************************************************** *)

val lnot: 'a t -> 'a t
  (** Logical negation (for all bits). *)
val shift_left: 'a Cudd.Man.t -> int -> 'a t -> 'a t * 'a Cudd.Bdd.t
  (** [shift_left man t n] shifts the register to the left by [n]
    bits. Returns the resulting register and the carry, which
    contains the last bit shifted out of the register. Assume, as
    for the following functions, that [n] is between [1] and
    the size of the register. *)

val shift_right: 'a Cudd.Man.t -> int -> 'a t -> 'a t * 'a Cudd.Bdd.t
  (** [shift_right t n] shifts the register to the right by [n]
    bits. This an {e arithmetical} shift: the sign is inserted
    as the new most significant bits. Returns the resulting
    register and the carry, which contains the last bit shifted
    out of the register. *)

val shift_right_logical: 'a Cudd.Man.t -> int -> 'a t -> 'a t * 'a Cudd.Bdd.t
  (** Same as [shift_right], but here {e logical} shift: a zero
    is always inserted. *)

val extend: 'a Cudd.Man.t -> signed:bool -> int -> 'a t -> 'a t
  (** register extension or truncation.
    [extend ~signed:b n x] extends the register x by adding [n] most
    significant bits, if [n>0], or truncate the [-n] most significant bits if
    [n<0]. [b] indicates whether the register is considered as a signed one or
    not. *)

(*  *********************************************************************** *)
(** {3 Arithmetic operations} *)
(*  *********************************************************************** *)

val succ: 'a Cudd.Man.t -> 'a t -> 'a t * 'a Cudd.Bdd.t
  (** Successor operation; returns the new register and the carry. *)
val pred: 'a Cudd.Man.t -> 'a t -> 'a t * 'a Cudd.Bdd.t
  (** Predecessor operation; returns the new register and the carry. *)
val add: 'a Cudd.Man.t -> 'a t -> 'a t -> 'a t * 'a Cudd.Bdd.t * 'a Cudd.Bdd.t
  (** Addition; returns the new registerm, the carry, and the
    overflow (for signed integers). *)
val sub: 'a Cudd.Man.t -> 'a t -> 'a t -> 'a t * 'a Cudd.Bdd.t * 'a Cudd.Bdd.t
  (** Substraction; returns the new register, the carry, and the
    overflow (for signed integers). *)
val neg: 'a t -> 'a t
  (** Unary negation; be cautious, if the size of integer is [n],
    the negation of [-2^(n-1)] is itself. *)
val scale: int -> 'a t -> 'a t
  (** Multiplication by a positive constant. *)
val mul: 'a t -> 'a t -> 'a t
  (** (Unsigned) multiplication *)
val ite: 'a Cudd.Bdd.t -> 'a t -> 'a t -> 'a t
  (** if-then-else operation. Zero-size possible. *)

(*  *********************************************************************** *)
(** {3 Predicates} *)
(*  *********************************************************************** *)

val is_cst: 'a t -> bool
  (** Tests whether it contains a constant value. Zero-size possible.  *)
val zero: 'a Cudd.Man.t -> 'a t -> 'a Cudd.Bdd.t
  (** Tests the register to zero. *)
val equal: 'a Cudd.Man.t -> 'a t -> 'a t -> 'a Cudd.Bdd.t
  (** Equality test. *)
val greatereq: 'a Cudd.Man.t -> 'a t -> 'a t -> 'a Cudd.Bdd.t
  (** Signed greater-or-equal test. *)
val greater: 'a Cudd.Man.t -> 'a t -> 'a t -> 'a Cudd.Bdd.t
  (** Signed strictly-greater-than test. *)
val highereq: 'a Cudd.Man.t -> 'a t -> 'a t -> 'a Cudd.Bdd.t
  (** Unsigned greater-or-equal test. *)
val higher: 'a Cudd.Man.t -> 'a t -> 'a t -> 'a Cudd.Bdd.t
  (** Unsigned strictly-greater-than test. *)

(*  *********************************************************************** *)
(** {3 Constants: conversion and predicates} *)
(*  *********************************************************************** *)

val min_size: int -> int
  (** [min_size cst] computes the minimum number of bits required
    to represent the given constant. We have for example [min_size
    0=0], [min_size 1 = 1], [min_size 3 = 2], [min_size (-8) = 4]. *)
val of_int: 'a Cudd.Man.t -> int -> int -> 'a t
   (** [of_int size cst] puts the constant integer [cst] in a constant register
     of size [size]. The fact that [size] is big enough is checked using the
     previous function, and a [Failure "..."] exception is raised in case of
     problem. *)
val to_int: signed:bool -> 'a t -> int
   (** [to_int sign x] converts a constant register to an integer. [sign]
     indicates whether the register is to be interpreted as a signed or
     unsigned. *)
val equal_int: 'a Cudd.Man.t -> 'a t -> int  -> 'a Cudd.Bdd.t
val greatereq_int: 'a Cudd.Man.t -> 'a t -> int -> 'a Cudd.Bdd.t
val greater_int: 'a Cudd.Man.t -> 'a t -> int -> 'a Cudd.Bdd.t
val highereq_int: 'a Cudd.Man.t -> 'a t -> int -> 'a Cudd.Bdd.t
val higher_int: 'a Cudd.Man.t -> 'a t -> int -> 'a Cudd.Bdd.t
  (** Tests w.r.t. a constant register, the size of which is
     defined by the first given register. *)

(*  *********************************************************************** *)
(** {3 Decomposition in guarded form} *)
(*  *********************************************************************** *)

module Minterm : sig
  type t = Cudd.Man.tbool array
    (** Type of a minterm: an array of Booleans extend with undefined value,
	indexed by variable indices. *)
  val is_indet: t -> bool
    (** Is the minterm completely non-determinated ? (ie, contain only
	undefined values) *)
  val of_int: int -> int -> t
    (** Convert a possibly negative integer into a minterm of size [size] *)
  val to_int: signed:bool -> t -> int
    (** Convert a minterm to a (possibly signed) integer.  Raise
	[Invalid_argument] if the minterm is not deterministic. *)
  val iter: (t -> unit) -> t -> unit
    (** Iterate the function on all determinated minterms represented by the
	argument minterm. *)
  val map: (t -> 'a) -> t -> 'a list
    (** Apply the function to all determinated minterms represented by the
	argument minterm and return the list of the results. *)
end

val guard_of_minterm: 'a Cudd.Man.t -> 'a t -> Minterm.t -> 'a Cudd.Bdd.t
  (** Return the guard of the deterministic minterm in the BDD register. *)
val guard_of_int: 'a Cudd.Man.t -> 'a t -> int -> 'a Cudd.Bdd.t
  (** Return the guard of the integer value in the BDD register. *)
val guardints: 'a Cudd.Man.t -> signed:bool -> 'a t -> ('a Cudd.Bdd.t * int) list
  (** Return the list [g -> n] represented by the BDD register. *)

(*  *********************************************************************** *)
(** {3 Evaluation} *)
(*  *********************************************************************** *)

val cofactor : 'a t -> 'a Cudd.Bdd.t -> 'a t
val restrict : 'a t -> 'a Cudd.Bdd.t -> 'a t
val tdrestrict : 'a t -> 'a Cudd.Bdd.t -> 'a t

(*  *********************************************************************** *)
(** {3 Printing} *)
(*  *********************************************************************** *)

val print: (Format.formatter -> int -> unit) -> Format.formatter -> 'a t -> unit
  (** [print f fmt t] prints the register [t] using the formatter
    [fmt] and the function [f] to print BDDs indices. *)

val print_minterm:
  signed:bool ->
  (Format.formatter -> 'a Cudd.Bdd.t -> unit) ->
  Format.formatter -> 'a t -> unit
  (** [print_minterm f fmt t] prints the register [t] using the formatter
    [fmt] and the function [f] to convert BDDs indices to
    names. *)

val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
  (** Permutation (scale [Cudd.Bdd.permute] and [Cudd.Bdd.permute_memo]) *)
val varmap : 'a t -> 'a t
  (** Permutation (scale [Cudd.Bdd.varmap]) *)
val vectorcompose : ?memo:Cudd.Memo.t -> 'a Cudd.Bdd.t array -> 'a t -> 'a t
  (** Composition (scale [Cudd.Bdd.vectorcompose] and
      [Cudd.Bdd.vectorcompose_memo]) *)
