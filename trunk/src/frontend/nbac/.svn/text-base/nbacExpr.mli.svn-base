(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** frontend implementation: Hybrid NBAC - abstract syntax *)

exception NbacParseError of string

type symtype = string
val symtype_compare : symtype -> symtype -> int
val symtype_print: Format.formatter -> symtype -> unit
val symtype_null: symtype

type typ = symtype Bddapron.Env.typ
type typdef = symtype Bddapron.Env.typdef

(*  ********************************************************************** *)
(** {2 Expressions} *)
(*  ********************************************************************** *)

(** Constant *)
type cst = [
  | `Bool of bool
  | `Bint of (bool * int) * int
  | `Apron of Apron.Coeff.t
]
type unop = [
| `Not
| `Up
| `Apron of Apron.Texpr1.unop * Apron.Texpr1.typ * Apron.Texpr1.round
]
type bbinop = Or | And | EQ | NEQ | GT | GEQ | LEQ | LT
type binop = [
| `Bool of bbinop
| `Apron of Apron.Texpr1.binop * Apron.Texpr1.typ * Apron.Texpr1.round
]
type 'a expr =
  | Cst of cst
  | Ref of 'a
  | Unop of unop * 'a expr
  | Binop of binop * 'a expr * 'a expr
  | If of 'a expr * 'a expr * 'a expr 
  | Excl of 'a expr list
  | In of 'a expr * 'a expr list

type prog = {
  decl: Program.declaration_t;
  defs: (symtype,symtype expr) Mappe.t;
  jump: (symtype,symtype expr) Mappe.t;
  flow: (symtype,symtype expr) Mappe.t;
  assertion: symtype expr;
  initial: symtype expr;
  final: symtype expr;
}

val etrue : 'a expr
val efalse : 'a expr
val is_true : 'a expr -> bool
val is_false : 'a expr -> bool
val support : ?filter:('a -> bool) -> 'a Sette.t -> 'a expr -> 'a Sette.t

(*  ********************************************************************** *)
(** {2 Printing} *)
(*  ********************************************************************** *)

val string_of_typ : typ -> string
val string_of_expr : symtype expr -> string
val string_of_cst : cst -> string
val is_zero : 'a expr -> bool
val precedence_of_unop : unop -> int
val precedence_of_binop : binop -> int
val precedence_of_expr : symtype expr -> int
val print_typdef : Format.formatter -> symtype Bddapron.Env.typdef -> unit
val print_typ : Format.formatter -> typ -> unit
val print_cst : Format.formatter -> cst -> unit
val print_unop : Format.formatter -> unop -> unit
val print_bbinop : Format.formatter -> bbinop -> unit
val print_binop : Format.formatter -> binop -> unit
val print_expr : Format.formatter -> symtype expr -> unit
val print_equation : Format.formatter -> symtype * symtype expr -> unit

val print : Format.formatter -> prog -> unit
