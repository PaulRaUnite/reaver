(** Purely arithmetic expressions (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(** Types of numerical variables (distinction is exploited when
    negating constraints) *)

type 'a symbol = 'a Bdd.Env.symbol = {
  compare : 'a -> 'a -> int;
  marshal : 'a -> string;
  unmarshal : string -> 'a;
  mutable print : Format.formatter -> 'a -> unit;
}

type typ = [
  | `Int
  | `Real
]

type ('a,'b) typ_of_var = 'a -> 'b constraint 'b = [> typ]

(*  ********************************************************************** *)
(** {3 Expressions} *)
(*  ********************************************************************** *)

(*  ==================================================================== *)
(** {4 Linear expressions} *)
(*  ==================================================================== *)

module Lin :
  sig
    type 'a term = Mpqf.t * 'a
    type 'a t = {
      cst : Mpqf.t;
      lterm : 'a term list;
    }
    val normalize : 'a symbol -> 'a t -> 'a t
    val compare_lterm : 'a symbol -> 'a term list -> 'a term list -> int
    val compare : 'a symbol -> 'a t -> 'a t -> int
    val var : 'a -> 'a t
    val zero : 'a t
    val one : 'a t
    val cst : Mpqf.t -> 'a t
    val add : 'a symbol -> 'a t -> 'a t -> 'a t
    val sub : 'a symbol -> 'a t -> 'a t -> 'a t
    val scale : Mpqf.t -> 'a t -> 'a t
    val negate : 'a t -> 'a t
    val support : 'a symbol -> 'a t -> 'a PSette.t
    val substitute_by_var : 'a symbol -> 'a t -> ('a,'a) PMappe.t -> 'a t
    val normalize_as_constraint : 'a t -> 'a t
    val print : 'a symbol -> Format.formatter -> 'a t -> unit

    val of_linexpr0 : 'a symbol -> Apron.Environment.t -> Apron.Linexpr0.t -> 'a t
    val of_linexpr1 : 'a symbol -> Apron.Linexpr1.t -> 'a t
    val to_linexpr0 : 'a symbol -> Apron.Environment.t -> 'a t -> Apron.Linexpr0.t
    val to_linexpr1 : 'a symbol -> Apron.Environment.t -> 'a t -> Apron.Linexpr1.t
  end

(*  ==================================================================== *)
(** {4 Polynomial expressions} *)
(*  ==================================================================== *)

module Poly :
  sig
    type 'a varexp = 'a * int
    type 'a monomial = 'a varexp list
    type 'a term = Mpqf.t * 'a monomial
    type 'a t = 'a term list
    val compare_varexp : 'a symbol -> 'a varexp -> 'a varexp -> int
    val compare_monomial : 'a symbol -> 'a monomial -> 'a monomial -> int
    val normalize_monomial : 'a symbol -> 'a monomial -> 'a monomial
    val normalize : 'a symbol -> 'a t -> 'a t
    val normalize_full : 'a symbol -> 'a t -> 'a t
    val compare : 'a symbol -> 'a t -> 'a t -> int
    val cst : Mpqf.t -> 'a t
    val var : 'a -> 'a t
    val add : 'a symbol -> 'a t -> 'a t -> 'a t
    val sub : 'a symbol -> 'a t -> 'a t -> 'a t
    val scale : 'a symbol -> Mpqf.t * 'a monomial -> 'a t -> 'a t
    val mul : 'a symbol -> 'a t -> 'a t -> 'a t
    val div : 'a symbol -> 'a t -> 'a t -> 'a t
    val negate : 'a t -> 'a t
    val support : 'a symbol -> 'a t -> 'a PSette.t
    val substitute_by_var : 'a symbol -> 'a t -> ('a,'a) PMappe.t -> 'a t
    val normalize_as_constraint : 'a t -> 'a t
    val print : 'a symbol -> Format.formatter -> 'a t -> unit
  end

(*  ==================================================================== *)
(** {4 Tree expressions} *)
(*  ==================================================================== *)

module Tree :
  sig
    type unop = Apron.Texpr1.unop = Neg | Cast | Sqrt
    type binop = Apron.Texpr1.binop = Add | Sub | Mul | Div | Mod | Pow
    type typ =
      Apron.Texpr1.typ =
      | Real
      | Int
      | Single
      | Double
      | Extended
      | Quad
    type round = Apron.Texpr1.round = Near | Zero | Up | Down | Rnd
    type 'a t =
      | Cst of Apron.Coeff.t
      | Var of 'a
      | Unop of unop * 'a t * typ * round
      | Binop of binop * 'a t * 'a t * typ * round
    val support : 'a symbol -> 'a t -> 'a PSette.t
    val substitute_by_var : 'a t -> ('a,'a) PMappe.t -> 'a t
    val print : 'a symbol -> Format.formatter -> 'a t -> unit
    val compare : 'a symbol -> 'a t -> 'a t -> int
    val of_expr : 'a symbol -> Apron.Texpr1.expr -> 'a t
    val to_expr : 'a symbol -> 'a t -> Apron.Texpr1.expr
  end

(*  ==================================================================== *)
(** {4 Conversions} *)
(*  ==================================================================== *)

val lin_of_poly : 'a symbol -> 'a Poly.t ->'a  Lin.t
val lin_of_tree : 'a symbol -> 'a Tree.t -> 'a Lin.t
val poly_of_tree : 'a symbol -> 'a Tree.t -> 'a Poly.t
val tree_of_lin : 'a Lin.t -> 'a Tree.t
val tree_of_poly : 'a Poly.t -> 'a Tree.t

(*  ********************************************************************** *)
(** {3 General expressions and operations} *)
(*  ********************************************************************** *)

type 'a t =
  | Lin of 'a Lin.t
  | Poly of 'a Poly.t
  | Tree of 'a Tree.t
type 'a expr = 'a t

val var : 'a symbol -> ('a,'b) typ_of_var -> 'a -> 'a t
val zero : 'a t
val one : 'a t
val cst : Apron.Coeff.t -> 'a t
val add : 'a symbol -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 'a t -> 'a t -> 'a t
val sub : 'a symbol -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 'a t -> 'a t -> 'a t
val mul : 'a symbol -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 'a t -> 'a t -> 'a t
val div : 'a symbol -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 'a t -> 'a t -> 'a t
val gmod : 'a symbol -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 'a t -> 'a t -> 'a t
val negate : 'a t -> 'a t
val cast : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 'a t -> 'a t
val sqrt : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 'a t -> 'a t
val support : 'a symbol -> 'a t -> 'a PSette.t
val substitute_by_var : 'a symbol -> 'a t -> ('a,'a) PMappe.t -> 'a t
val normalize : 'a symbol -> 'a t -> 'a t
val equal : 'a symbol -> 'a t -> 'a t -> bool
val hash : 'a symbol -> 'a t -> int
val compare : 'a symbol -> 'a t -> 'a t -> int
val normalize_as_constraint : 'a t -> 'a t
val is_dependent_on_integer_only : ('a,'b) typ_of_var -> 'a t -> bool
val typ_of_expr : ('a,'b) typ_of_var -> 'a t -> [`Int | `Real]
val print : 'a symbol -> Format.formatter -> 'a t -> unit
val print_typ : Format.formatter -> [> typ] -> unit

val of_linexpr0 : 'a symbol -> Apron.Environment.t -> Apron.Linexpr0.t -> 'a t
val of_linexpr1 : 'a symbol -> Apron.Linexpr1.t -> 'a t
val to_linexpr0 : 'a symbol -> Apron.Environment.t -> 'a t -> Apron.Linexpr0.t
val to_linexpr1 : 'a symbol -> Apron.Environment.t -> 'a t -> Apron.Linexpr1.t
val of_texpr0 : 'a symbol -> Apron.Environment.t -> Apron.Texpr0.t -> 'a t
val of_texpr1 : 'a symbol -> Apron.Texpr1.t -> 'a t
val to_texpr0 : 'a symbol -> Apron.Environment.t -> 'a t -> Apron.Texpr0.t
val to_texpr1 : 'a symbol -> Apron.Environment.t -> 'a t -> Apron.Texpr1.t
val to_apron0 :
  'a symbol -> Apron.Environment.t -> 'a t ->
  [
  | `Lin of Apron.Linexpr0.t
  | `Tree of Apron.Texpr0.t
  ]
val to_apron1 :
  'a symbol -> Apron.Environment.t -> 'a t ->
  [
  | `Lin of Apron.Linexpr1.t
  | `Tree of Apron.Texpr1.t
  ]

(*  ********************************************************************** *)
(** {3 Constraints} *)
(*  ********************************************************************** *)

module Condition :
  sig
    type typ = Apron.Tcons1.typ =
      EQ | SUPEQ | SUP | DISEQ | EQMOD of Apron.Scalar.t
    type 'a t = typ * 'a expr
    val make : ('a,'b) typ_of_var -> typ -> 'a expr -> [ `Cond of 'a t | `Bool of bool ]
    val negate : ('a,'b) typ_of_var -> 'a t -> 'a t
    val support : 'a symbol -> 'a t -> 'a PSette.t
    val print : 'a symbol -> Format.formatter -> 'a t -> unit
    val compare : 'a symbol -> 'a t -> 'a t -> int
    val of_lincons0 : 'a symbol -> ('a,'b) typ_of_var -> Apron.Environment.t -> Apron.Lincons0.t -> [ `Cond of 'a t | `Bool of bool ]
    val of_lincons1 : 'a symbol -> ('a,'b) typ_of_var -> Apron.Lincons1.t -> [ `Cond of 'a t | `Bool of bool ]
    val of_tcons0 : 'a symbol -> ('a,'b) typ_of_var -> Apron.Environment.t -> Apron.Tcons0.t -> [ `Cond of 'a t | `Bool of bool ]
    val of_tcons1 : 'a symbol -> ('a,'b) typ_of_var -> Apron.Tcons1.t -> [ `Cond of 'a t | `Bool of bool ]
    val to_tcons0 : 'a symbol -> Apron.Environment.t -> 'a t -> Apron.Tcons0.t
    val to_tcons1 : 'a symbol -> Apron.Environment.t -> 'a t -> Apron.Tcons1.t
    val to_apron0 :
      'a symbol -> Apron.Environment.t -> 'a t ->
	[
	  | `Lin of Apron.Lincons0.t
	  | `Tree of Apron.Tcons0.t
	]
    val to_apron1 :
      'a symbol -> Apron.Environment.t -> 'a t ->
	[
	  | `Lin of Apron.Lincons1.t
	  | `Tree of Apron.Tcons1.t
	]
  end
