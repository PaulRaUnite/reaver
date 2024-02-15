(** Bounded integer expressions with BDDs *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(**

This module is intended to encode operations based on bounded integers (or
more generally, enumerated types), using BDDs. It represents a layer on top of
[Bddarith], which provides with arrays resizing and signs.

Let us give an example: suppose we have a variable [i: int[0..7]]; this
variable will be encoded by 3 Boolean variables [i0,i1,i2], starting from the
least significant bit. Now, how to translate the expression [i<=5] ? Here the
result will be [i2 and not i1 or not i2]

This module requires the mlcuddidl library and the [Bddreg] module.
*)

(**

The basic types are signed or unsigned
n-bits integers. Integers are defined by a an array of BDDs,
each BDD corresponding to a bit. The order in this array is
going from the LSB (Least Significant Bit) to the
MSB (Most Significant Bit).
*)


type 'a t = {
    signed: bool;
    reg: 'a Cudd.Bdd.t array;
  }
  (** type of an enumerated variable *)

type dt = Cudd.Man.d t
type vt = Cudd.Man.v t

exception Typing of string
  (** Raised when operands are of incompatible type (sign and size) *)

(**
All the functions are basically wrapper around functions of
[Bddarith]. Resizing and conversion from unsigned to signed are automatic.
For instance, When adding or substracting integers, the smallest one is
resized to the size of the larger one, possibly with one more bit if they are
not of the same type (signed or unsigned).
*)

(** {3 Conversion of integers} *)

val extend: 'a Cudd.Man.t -> int -> 'a t -> 'a t

(** {3 Operations on integers} *)

val neg: 'a t -> 'a t
val succ: 'a t -> 'a t
val pred: 'a t -> 'a t
val add: 'a t -> 'a t -> 'a t
val sub: 'a t -> 'a t -> 'a t
val mul: 'a t -> 'a t -> 'a t
val shift_left: int -> 'a t -> 'a t
val shift_right: int -> 'a t -> 'a t
val scale: int -> 'a t -> 'a t
val ite: 'a Cudd.Bdd.t -> 'a t -> 'a t -> 'a t

(** {3 Predicates on integers} *)

val is_cst: 'a t -> bool
val zero: 'a Cudd.Man.t -> 'a t -> 'a Cudd.Bdd.t
val equal: 'a Cudd.Man.t -> 'a t -> 'a t -> 'a Cudd.Bdd.t
val greatereq: 'a Cudd.Man.t -> 'a t -> 'a t -> 'a Cudd.Bdd.t
val greater: 'a Cudd.Man.t -> 'a t -> 'a t -> 'a Cudd.Bdd.t

(** {3 Predicates involving constant integers} *)

val of_int: 'a Cudd.Man.t -> bool -> int -> int -> 'a t
val to_int: 'a t -> int
val equal_int: 'a Cudd.Man.t -> 'a t -> int -> 'a Cudd.Bdd.t
val greatereq_int: 'a Cudd.Man.t -> 'a t -> int -> 'a Cudd.Bdd.t
val greater_int: 'a Cudd.Man.t -> 'a t -> int -> 'a Cudd.Bdd.t

(** {3 Decomposition in guarded form} *)

module Minterm : sig
  val iter: signed:bool -> (int -> unit) -> Reg.Minterm.t -> unit
    (** Iterate the function on all the integer values represented by the
	argument minterm. *)
  val map: signed:bool -> (int -> 'a) -> Reg.Minterm.t -> 'a list
    (** Apply the function to all integer values represented by the
	argument minterm and return the list of the results. *)
end

val guard_of_int: 'a Cudd.Man.t -> 'a t -> int -> 'a Cudd.Bdd.t
  (** Return the guard of the integer value in the BDD register. *)
val guardints: 'a Cudd.Man.t -> 'a t -> ('a Cudd.Bdd.t * int) list
  (** Return the list [g -> n] represented by the BDD register. *)

(** {3 Evaluation} *)

val cofactor : 'a t -> 'a Cudd.Bdd.t -> 'a t
val restrict : 'a t -> 'a Cudd.Bdd.t -> 'a t
val tdrestrict : 'a t -> 'a Cudd.Bdd.t -> 'a t

(** {3 Printing} *)

val print:
  (Format.formatter -> int -> unit) ->
  Format.formatter -> 'a t -> unit
  (** [print f fmt t] prints the register [t] using the formatter
    [fmt] and the function [f] to print BDDs indices. *)
val print_minterm:
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
