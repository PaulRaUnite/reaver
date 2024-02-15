(** Building BDDAPRON expressions from Abstract Syntax Trees *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 Types} *)
(*  ********************************************************************** *)

(** Constant *)
type cst = [
  | `Bool of bool
  | `Bint of (bool * int) * int
  | `Apron of Apron.Coeff.t
]

(** Unary operators *)
type unop = [
| `Not
| `Apron of Apron.Texpr1.unop * Apron.Texpr1.typ * Apron.Texpr1.round
]

(** Boolean/finite-type binary operators *)
type bbinop = [
  | `Or
  | `And
  | `EQ
  | `NEQ
  | `GT
  | `GEQ
  | `LEQ
  | `LT
]
(** Binary operators *)
type binop = [
| `Bool of bbinop
| `Apron of Apron.Texpr1.binop * Apron.Texpr1.typ * Apron.Texpr1.round
]

(** Expressions *)
type 'a expr = [
  | `Cst of cst
  | `Ref of 'a
  | `Unop of unop * 'a expr
  | `Binop of binop * 'a expr * 'a expr
  | `If of 'a expr * 'a expr * 'a expr
  | `In of 'a expr * 'a expr list
]

(*  ********************************************************************** *)
(** {3 Error and printing functions} *)
(*  ********************************************************************** *)

val print_cst : Format.formatter -> cst -> unit
val print_unop : Format.formatter -> unop -> unit
val print_bbinop : Format.formatter -> bbinop -> unit
val print_binop : Format.formatter -> binop -> unit
val print_expr :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a expr -> unit

(*  ********************************************************************** *)
(** {3 Translation functions} *)
(*  ********************************************************************** *)
exception Error of string
  (** Exception raised in case of typing error *)

val to_expr0 : 'a Env.t -> 'a Cond.t -> string expr -> 'a Expr0.t
val to_expr1 : 'a Env.t -> 'a Cond.t -> string expr -> 'a Expr1.t
val to_listexpr1 : 'a Env.t -> 'a Cond.t -> string expr list -> 'a Expr1.List.t
val to_listexpr2 :
  ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
  'a Env.t -> 'a Cond.t -> string expr list -> 'a Expr2.List.t
val to_boolexpr2 :
  ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
  'a Env.t -> 'a Cond.t -> string expr -> 'a Expr2.Bool.t

(*  ********************************************************************** *)
(** {3 Internal functions} *)
(*  ********************************************************************** *)

val error : ('a, Format.formatter, unit, 'b) format4 -> 'a
val is_zero : 'a expr -> bool

val precedence_of_unop : unop -> int
val precedence_of_binop : binop -> int
val precedence_of_expr : 'a expr -> int

val cst_to_expr0 : 'a Env.t -> 'a Cond.t -> [< cst ] -> 'a Expr0.expr
val apply_bbinop :
  'a Env.t -> 'a Cond.t ->
  bbinop -> 'a Expr0.expr -> 'a Expr0.expr -> 'a Expr0.Bool.t
val apply_binop :
  'a Env.t -> 'a Cond.t -> binop -> 'a Expr0.t -> 'a Expr0.t -> 'a Expr0.t
