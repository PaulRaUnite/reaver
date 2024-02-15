(** Finite-type and arithmetical expressions with normalized environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(** Important remark:

    The following functions may require the creation of new
    external conditions in the conditional environment.

    - the various [substitute] and [substitute_by_var] functions
    - [Apron.condition], [Apron.sup], [Apron.supeq], [Apron.eq] functions

*)

(*  ********************************************************************** *)
(** {3 Expressions} *)
(*  ********************************************************************** *)

type 'a t = ('a Env.t, 'a Expr0.t) Env.value
type 'a expr = 'a t
  (** Type of general expressions *)

(*  ====================================================================== *)
(** {4 Boolean expressions} *)
(*  ====================================================================== *)

module Bool : sig
  type 'a t = ('a Env.t, Cudd.Man.v Expr0.Bool.t) Env.value

  val of_expr0 : 'a Env.t -> 'a Expr0.Bool.t -> 'a t
    (** Creation from an expression of level 0 (without environment) *)
  val get_env : 'a t -> 'a Env.t
  val to_expr0 : 'a t -> 'a Expr0.Bool.t
    (** Extract resp. the environment and the underlying
	expression of level 0 *)

  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr
    (** Conversion from/to general expression *)

  val extend_environment : 'a t -> 'a Env.t -> 'a t
    (** Extend the underlying environment to a superenvironment,
	and adapt accordingly the underlying representation *)

  val dtrue : 'a Env.t -> 'a Cond.t -> 'a t
  val dfalse : 'a Env.t -> 'a Cond.t -> 'a t
  val of_bool : 'a Env.t -> 'a Cond.t -> bool -> 'a t
  val var : 'a Env.t -> 'a Cond.t -> 'a -> 'a t

  (** {5 Logical connectors} *)

  val dnot : 'a Cond.t -> 'a t -> 'a t
  val dand : 'a Cond.t -> 'a t -> 'a t -> 'a t
  val dor : 'a Cond.t -> 'a t -> 'a t -> 'a t
    (** [not], [and] and [or] (use of 'd' prefix because of conflict with OCaml
	keywords) *)

  val xor : 'a Cond.t -> 'a t -> 'a t -> 'a t
  val nand : 'a Cond.t -> 'a t -> 'a t -> 'a t
  val nor : 'a Cond.t -> 'a t -> 'a t -> 'a t
  val nxor : 'a Cond.t -> 'a t -> 'a t -> 'a t
    (** Exclusive or, not and, nor or and not xor *)

  val eq : 'a Cond.t -> 'a t -> 'a t -> 'a t
    (** Same as [nxor] *)
  val leq : 'a Cond.t -> 'a t -> 'a t -> 'a t
    (** Implication *)

  val ite : 'a Cond.t -> 'a t -> 'a t -> 'a t -> 'a t
    (** If-then-else *)

  val is_true : 'a Cond.t -> 'a t -> bool
  val is_false : 'a Cond.t -> 'a t -> bool
  val is_cst : 'a Cond.t -> 'a t -> bool
  val is_eq : 'a Cond.t -> 'a t -> 'a t -> bool
  val is_leq : 'a Cond.t -> 'a t -> 'a t -> bool
  val is_inter_false : 'a Cond.t -> 'a t -> 'a t -> bool

  val exist : 'a Cond.t -> 'a list -> 'a t -> 'a t
  val forall : 'a Cond.t -> 'a list -> 'a t -> 'a t

  val cofactor : 'a t -> 'a t -> 'a t
  val restrict : 'a t -> 'a t -> 'a t
  val tdrestrict : 'a t -> 'a t -> 'a t

  val substitute_by_var : ?memo:Cudd.Memo.t -> 'a Cond.t -> 'a t -> ('a * 'a) list -> 'a t
  val substitute : ?memo:Cudd.Memo.t -> 'a Cond.t -> 'a t -> ('a * 'a expr) list -> 'a t

  val print : 'a Cond.t -> Format.formatter -> 'a t -> unit
end

(*  ====================================================================== *)
(** {4 Bounded integer expressions} *)
(*  ====================================================================== *)

module Bint : sig
  type 'a t = ('a Env.t, Cudd.Man.v Bdd.Int.t) Env.value

  val of_expr0 : 'a Env.t -> 'a Expr0.Bint.t -> 'a t
    (** Creation from an expression of level 0 (without environment) *)
  val get_env : 'a t -> 'a Env.t
  val to_expr0 : 'a t -> 'a Expr0.Bint.t
    (** Extract resp. the environment and the underlying
	expression of level 0 *)

  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr
    (** Conversion from/to general expression *)

  val extend_environment : 'a t -> 'a Env.t -> 'a t
    (** Extend the underlying environment to a superenvironment,
	and adapt accordingly the underlying representation *)

  val of_int : 'a Env.t -> 'a Cond.t -> [`Bint of bool * int ] -> int -> 'a t
  val var : 'a Env.t -> 'a Cond.t -> 'a -> 'a t

  val neg : 'a Cond.t -> 'a t -> 'a t
  val succ : 'a Cond.t -> 'a t -> 'a t
  val pred : 'a Cond.t -> 'a t -> 'a t
  val add : 'a Cond.t -> 'a t -> 'a t -> 'a t
  val sub : 'a Cond.t -> 'a t -> 'a t -> 'a t
  val mul : 'a Cond.t -> 'a t -> 'a t -> 'a t
  val shift_left : 'a Cond.t -> int -> 'a t -> 'a t
  val shift_right : 'a Cond.t -> int -> 'a t -> 'a t
  val scale : 'a Cond.t -> int -> 'a t -> 'a t
  val ite : 'a Cond.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t
  val zero : 'a Cond.t -> 'a t -> 'a Bool.t
  val eq : 'a Cond.t -> 'a t -> 'a t -> 'a Bool.t
  val supeq : 'a Cond.t -> 'a t -> 'a t -> 'a Bool.t
  val sup : 'a Cond.t -> 'a t -> 'a t -> 'a Bool.t
  val eq_int : 'a Cond.t -> 'a t -> int -> 'a Bool.t
  val supeq_int : 'a Cond.t -> 'a t -> int -> 'a Bool.t
  val sup_int : 'a Cond.t -> 'a t -> int -> 'a Bool.t

  val cofactor : 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t

  val substitute_by_var : ?memo:Cudd.Memo.t -> 'a Cond.t -> 'a t -> ('a * 'a) list -> 'a t
  val substitute : ?memo:Cudd.Memo.t -> 'a Cond.t -> 'a t -> ('a * 'a expr) list -> 'a t

  val guard_of_int : 'a Cond.t -> 'a t -> int -> 'a Bool.t
    (** Return the guard of the integer value. *)
  val guardints : 'a Cond.t -> 'a t -> ('a Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

  val print : 'a Cond.t -> Format.formatter -> 'a t -> unit
end

(*  ====================================================================== *)
(** {4 Enumerated expressions} *)
(*  ====================================================================== *)

module Benum : sig
  type 'a t = ('a Env.t, Cudd.Man.v Bdd.Enum.t) Env.value
  val of_expr0 : 'a Env.t -> 'a Expr0.Benum.t -> 'a t
    (** Creation from an expression of level 0 (without environment) *)
  val get_env : 'a t -> 'a Env.t
  val to_expr0 : 'a t -> 'a Expr0.Benum.t
    (** Extract resp. the environment and the underlying
	expression of level 0 *)

  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr
    (** Conversion from/to general expression *)

  val extend_environment : 'a t -> 'a Env.t -> 'a t
    (** Extend the underlying environment to a superenvironment,
	and adapt accordingly the underlying representation *)

  val var : 'a Env.t -> 'a Cond.t -> 'a -> 'a t
  val ite : 'a Cond.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t
  val eq : 'a Cond.t -> 'a t -> 'a t -> 'a Bool.t
  val eq_label : 'a Cond.t -> 'a t -> 'a -> 'a Bool.t

  val cofactor : 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t

  val substitute_by_var : ?memo:Cudd.Memo.t -> 'a Cond.t -> 'a t -> ('a * 'a) list -> 'a t
  val substitute : ?memo:Cudd.Memo.t -> 'a Cond.t -> 'a t -> ('a * 'a expr) list -> 'a t

  val guard_of_label : 'a Cond.t -> 'a t -> 'a -> 'a Bool.t
    (** Return the guard of the label. *)

  val guardlabels : 'a Cond.t -> 'a t -> ('a Bool.t * 'a) list
    (** Return the list [g -> label] of guarded values. *)

  val print : 'a Cond.t -> Format.formatter -> 'a t -> unit
end

(*  ====================================================================== *)
(** {4 Arithmetic expressions} *)
(*  ====================================================================== *)

type apron_coeff = Apron.Coeff.t
type apron_typ = Apron.Texpr1.typ
type apron_round = Apron.Texpr1.round
type apron_cons_typ = Apron.Tcons1.typ

module Apron : sig
  type 'a t = ('a Env.t, 'a Expr0.Apron.t) Env.value

  val of_expr0 : 'a Env.t -> 'a Expr0.Apron.t -> 'a t
    (** Creation from an expression of level 0 (without environment) *)
  val get_env : 'a t -> 'a Env.t
  val to_expr0 : 'a t -> 'a Expr0.Apron.t
    (** Extract resp. the environment and the underlying
	expression of level 0 *)

  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr
    (** Conversion from/to general expression *)

  val extend_environment : 'a t -> 'a Env.t -> 'a t
    (** Extend the underlying environment to a superenvironment,
	and adapt accordingly the underlying representation *)

  val var : 'a Env.t -> 'a Cond.t -> 'a -> 'a t
  val cst : 'a Env.t -> 'a Cond.t -> Apron.Coeff.t -> 'a t
  val add : 'a Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t
  val mul : 'a Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t
  val sub : 'a Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t
  val div : 'a Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t
  val gmod : 'a Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t

  val negate : 'a Cond.t -> 'a t -> 'a t
  val sqrt : 'a Cond.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 'a t -> 'a t
  val cast : 'a Cond.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 'a t -> 'a t

  val ite : 'a Cond.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t

  val condition : 'a Cond.t -> Apron.Tcons1.typ -> 'a t -> 'a Bool.t
  val supeq : 'a Cond.t -> 'a t -> 'a Bool.t
  val sup : 'a Cond.t -> 'a t -> 'a Bool.t
  val eq : 'a Cond.t -> 'a t -> 'a Bool.t
  val cofactor : 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t

  val substitute_by_var : ?memo:Cudd.Memo.t -> 'a Cond.t -> 'a t -> ('a * 'a) list -> 'a t
  val substitute : ?memo:Cudd.Memo.t -> 'a Cond.t -> 'a t -> ('a * 'a expr) list -> 'a t

  val print : 'a Cond.t -> Format.formatter -> 'a t -> unit
end

(*  ====================================================================== *)
(** {4 General expressions} *)
(*  ====================================================================== *)

val typ_of_expr : 'a t -> 'a Env.typ
  (** Type of an expression *)

val make : 'a Env.t -> 'a Expr0.t -> 'a t
val of_expr0 : 'a Env.t -> 'a Expr0.t -> 'a t
  (** Creation from an expression of level 0 (without
      environment) *)
val get_env : 'a t -> 'a Env.t
val to_expr0 : 'a t -> 'a Expr0.t
  (** Extract resp. the environment and the underlying expression
      of level 0 *)

val extend_environment : 'a t -> 'a Env.t -> 'a t
  (** Extend the underlying environment to a superenvironment, and
      adapt accordingly the underlying representation *)

val var : 'a Env.t -> 'a Cond.t -> 'a -> 'a t
  (** Expression representing the litteral var *)
val ite : 'a Cond.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t
  (** If-then-else operation *)
val eq : 'a Cond.t -> 'a t -> 'a t -> 'a Bool.t
  (** Equality operation *)

val substitute_by_var : ?memo:Cudd.Memo.t -> 'a Cond.t -> 'a t -> ('a * 'a) list -> 'a t
val substitute_by_var_list : ?memo:Cudd.Memo.t -> 'a Cond.t -> 'a t list -> ('a * 'a) list -> 'a t list
    (** Variable renaming.
	The new variables should already have been declared *)
val substitute : ?memo:Cudd.Memo.t -> 'a Cond.t -> 'a t -> ('a * 'a t) list -> 'a t
val substitute_list : ?memo:Cudd.Memo.t -> 'a Cond.t -> 'a t list -> ('a * 'a t) list -> 'a t list
    (** Parallel substitution of variables by expressions *)

val support : 'a Cond.t -> 'a t -> 'a PSette.t
    (** Support of the expression *)
val support_cond : Cudd.Man.vt -> 'a t -> Cudd.Bdd.vt
    (** Return the support of an expression as a conjunction of the BDD
	identifiers involved in the expression *)

val cofactor : 'a t -> 'a Bool.t -> 'a t
    (** Evaluate the expression. The BDD is assumed to be a cube *)
val restrict : 'a t -> 'a Bool.t -> 'a t
val tdrestrict : 'a t -> 'a Bool.t -> 'a t
    (** Simplify the expression knowing that the BDD is true.  Generalizes
	[cofactor]. *)

val print : 'a Cond.t -> Format.formatter -> 'a t -> unit

val normalize :
  ?reduce:bool -> ?careset:bool ->
  'a Cond.t * 'a t list -> 'a Cond.t * 'a t list

(*  ====================================================================== *)
(** {4 List of expressions} *)
(*  ====================================================================== *)

module List : sig
  type 'a t = ('a Env.t, 'a Expr0.t list) Env.value

  val of_lexpr0 : 'a Env.t -> 'a Expr0.t list -> 'a t
    (** Creation from a list of expressions of level 0 (without
	environment) *)
  val get_env : 'a t -> 'a Env.t
  val to_lexpr0 : 'a t -> 'a Expr0.t list
   (** Extract resp. the environment and the underlying list of
       expressions of level 0 *)

  val of_lexpr : 'a Env.t -> 'a expr list -> 'a t
  val to_lexpr : 'a t -> 'a expr list
    (** Conversion from/to lists of general expression *)

  val extend_environment : 'a t -> 'a Env.t -> 'a t
  val normalize :
    ?reduce:bool -> ?careset:bool ->
    'a Cond.t * 'a t -> 'a Cond.t * 'a t
  val print :
    ?first:(unit,Format.formatter,unit) format ->
    ?sep:(unit,Format.formatter,unit) format ->
    ?last:(unit,Format.formatter,unit) format ->
    'a Cond.t -> Format.formatter -> 'a t -> unit
end

(*  ********************************************************************** *)
(** {3 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O : sig

  type ('a,'b) t = ('b, 'a Expr0.t) Env.value
  constraint 'b = ('a,'c,'d,'e) Env.O.t

  type ('a,'b) expr = ('a,'b) t
    (** Type of general expressions *)

  module Bool : sig
    type ('a,'b) t = ('b, 'a Expr0.Bool.t) Env.value
    constraint 'b = ('a,'c,'d,'e) Env.O.t

    val of_expr : ('b, [> `Bool of 'a Expr0.Bool.t ]) Env.value -> ('a,'b) t
    val to_expr : ('a,'b) t -> ('b, [> `Bool of 'a Expr0.Bool.t ]) Env.value

    val extend_environment : ('a,'b) t -> 'b -> ('a,'b) t

    val dtrue : 'b -> ('a,'b) Cond.O.t -> ('a,'b) t
    val dfalse : 'b -> ('a,'b) Cond.O.t -> ('a,'b) t
    val of_bool : 'b -> ('a,'b) Cond.O.t -> bool -> ('a,'b) t
    val var : 'b -> ('a,'b) Cond.O.t -> 'a -> ('a,'b) t

    (** {5 Logical connectors} *)

    val dnot : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t
    val dand : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val dor : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    (** [not], [and] and [or] (use of 'd' prefix because of conflict with OCaml
	keywords) *)

    val xor : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val nand : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val nor : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val nxor : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    (** Exclusive or, not and, nor or and not xor *)

    val eq : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    (** Same as [nxor] *)
    val leq : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    (** Implication *)

    val ite : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    (** If-then-else *)

    val is_true : ('a,'b) Cond.O.t -> ('a,'b) t -> bool
    val is_false : ('a,'b) Cond.O.t -> ('a,'b) t -> bool
    val is_cst : ('a,'b) Cond.O.t -> ('a,'b) t -> bool
    val is_eq : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> bool
    val is_leq : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> bool
    val is_inter_false : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> bool

    val exist : ('a,'b) Cond.O.t -> 'a list -> ('a,'b) t -> ('a,'b) t
    val forall : ('a,'b) Cond.O.t -> 'a list -> ('a,'b) t -> ('a,'b) t

    val cofactor : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val restrict : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val tdrestrict : ('a,'b) t -> ('a,'b) t -> ('a,'b) t

    val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b) Cond.O.t -> ('a,'b) t -> ('a * 'a) list -> ('a,'b) t
    val substitute : ?memo:Cudd.Memo.t -> ('a,'b) Cond.O.t -> ('a,'b) t -> ('a * ('a,'b) expr) list -> ('a,'b) t

    val print : ('a,'b) Cond.O.t -> Format.formatter -> ('a,'b) t -> unit
  end

  module Bint : sig
    type ('a,'b) t = ('b, Cudd.Man.v Bdd.Int.t) Env.value
    constraint 'b = ('a,'c,'d,'e) Env.O.t

    val of_expr : ('b, [> `Bint of 'a Expr0.Bint.t ]) Env.value -> ('a,'b) t
    val to_expr : ('a,'b) t -> ('b, [> `Bint of 'a Expr0.Bint.t ]) Env.value
    val extend_environment : ('a,'b) t -> 'b -> ('a,'b) t

    val of_int :
      'b -> ('a,'b) Cond.O.t -> [`Bint of bool * int ] -> int -> ('a,'b) t
    val var : 'b -> ('a,'b) Cond.O.t -> 'a -> ('a,'b) t

    val neg : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t
    val succ : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t
    val pred : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t
    val add : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val sub : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val mul : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val shift_left : ('a,'b) Cond.O.t -> int -> ('a,'b) t -> ('a,'b) t
    val shift_right : ('a,'b) Cond.O.t -> int -> ('a,'b) t -> ('a,'b) t
    val scale : ('a,'b) Cond.O.t -> int -> ('a,'b) t -> ('a,'b) t
    val ite : ('a,'b) Cond.O.t -> ('a,'b) Bool.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val zero : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) Bool.t
    val eq : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) Bool.t
    val supeq : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) Bool.t
    val sup : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) Bool.t
    val eq_int : ('a,'b) Cond.O.t -> ('a,'b) t -> int -> ('a,'b) Bool.t
    val supeq_int : ('a,'b) Cond.O.t -> ('a,'b) t -> int -> ('a,'b) Bool.t
    val sup_int : ('a,'b) Cond.O.t -> ('a,'b) t -> int -> ('a,'b) Bool.t

    val cofactor : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    val restrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    val tdrestrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t

    val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b) Cond.O.t -> ('a,'b) t -> ('a * 'a) list -> ('a,'b) t
    val substitute : ?memo:Cudd.Memo.t -> ('a,'b) Cond.O.t -> ('a,'b) t -> ('a * ('a,'b) expr) list -> ('a,'b) t

    val guard_of_int : ('a,'b) Cond.O.t -> ('a,'b) t -> int -> ('a,'b) Bool.t
    (** Return the guard of the integer value. *)
    val guardints : ('a,'b) Cond.O.t -> ('a,'b) t -> (('a,'b) Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

    val print : ('a,'b) Cond.O.t -> Format.formatter -> ('a,'b) t -> unit
  end

  module Benum : sig
    type ('a,'b) t = ('b, Cudd.Man.v Bdd.Enum.t) Env.value
    constraint 'b = ('a,'c,'d,'e) Env.O.t
    val of_expr : ('b, [> `Benum of 'a Expr0.Benum.t ]) Env.value -> ('a,'b) t
    val to_expr : ('a,'b) t -> ('b, [> `Benum of 'a Expr0.Benum.t ]) Env.value
    val extend_environment : ('a,'b) t -> 'b -> ('a,'b) t

    val var : 'b -> ('a,'b) Cond.O.t -> 'a -> ('a,'b) t
    val ite : ('a,'b) Cond.O.t -> ('a,'b) Bool.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val eq : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) Bool.t
    val eq_label : ('a,'b) Cond.O.t -> ('a,'b) t -> 'a -> ('a,'b) Bool.t

    val cofactor : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    val restrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    val tdrestrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t

    val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b) Cond.O.t -> ('a,'b) t -> ('a * 'a) list -> ('a,'b) t
    val substitute : ?memo:Cudd.Memo.t -> ('a,'b) Cond.O.t -> ('a,'b) t -> ('a * ('a,'b) expr) list -> ('a,'b) t

    val guard_of_label : ('a,'b) Cond.O.t -> ('a,'b) t -> 'a -> ('a,'b) Bool.t
    (** Return the guard of the label. *)

    val guardlabels : ('a,'b) Cond.O.t -> ('a,'b) t -> (('a,'b) Bool.t * 'a) list
    (** Return the list [g -> label] of guarded values. *)

    val print : ('a,'b) Cond.O.t -> Format.formatter -> ('a,'b) t -> unit
  end

  module Apron : sig
    type ('a,'b) t = ('b, 'a Expr0.Apron.t) Env.value
    constraint 'b = ('a,'c,'d,'e) Env.O.t

    val of_expr :
      ('b, [> `Apron of 'a Expr0.Apron.t ]) Env.value -> ('a,'b) t

    val to_expr :
      ('a,'b) t -> ('b, [> `Apron of 'a Expr0.Apron.t ]) Env.value

    val extend_environment : ('a,'b) t -> 'b -> ('a,'b) t

    val var : 'b -> ('a,'b) Cond.O.t -> 'a -> ('a,'b) t
    val cst : 'b -> ('a,'b) Cond.O.t -> apron_coeff -> ('a,'b) t
    val add : ('a,'b) Cond.O.t ->
      ?typ:apron_typ -> ?round:apron_round ->
      ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val mul : ('a,'b) Cond.O.t ->
      ?typ:apron_typ -> ?round:apron_round ->
      ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val sub : ('a,'b) Cond.O.t ->
      ?typ:apron_typ -> ?round:apron_round ->
      ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val div : ('a,'b) Cond.O.t ->
      ?typ:apron_typ -> ?round:apron_round ->
      ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val gmod : ('a,'b) Cond.O.t ->
      ?typ:apron_typ -> ?round:apron_round ->
      ('a,'b) t -> ('a,'b) t -> ('a,'b) t

    val negate : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t
    val sqrt : ('a,'b) Cond.O.t -> ?typ:apron_typ -> ?round:apron_round -> ('a,'b) t -> ('a,'b) t
    val cast : ('a,'b) Cond.O.t -> ?typ:apron_typ -> ?round:apron_round -> ('a,'b) t -> ('a,'b) t

    val ite : ('a,'b) Cond.O.t -> ('a,'b) Bool.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t

    val condition : ('a,'b) Cond.O.t -> apron_cons_typ -> ('a,'b) t -> ('a,'b) Bool.t
    val supeq : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) Bool.t
    val sup : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) Bool.t
    val eq : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) Bool.t
    val cofactor : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    val restrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    val tdrestrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t

    val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b) Cond.O.t -> ('a,'b) t -> ('a * 'a) list -> ('a,'b) t
    val substitute : ?memo:Cudd.Memo.t -> ('a,'b) Cond.O.t -> ('a,'b) t -> ('a * ('a,'b) expr) list -> ('a,'b) t

    val print : ('a,'b) Cond.O.t -> Format.formatter -> ('a,'b) t -> unit
  end

  val typ_of_expr : ('a,'b) t -> 'a Env.typ
  (** Type of an expression *)

  val make : 'b -> 'a Expr0.t -> ('a,'b) t
  (** Creation from an expression without environment *)

  val extend_environment : ('a,'b) t -> 'b -> ('a,'b) t
  val var : 'b -> ('a,'b) Cond.O.t -> 'a -> ('a,'b) t
  (** Expression representing the litteral var *)
  val ite : ('a,'b) Cond.O.t -> ('a,'b) Bool.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  (** If-then-else operation *)
  val eq : ('a,'b) Cond.O.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) Bool.t
  (** Equality operation *)

  val substitute_by_var :
    ?memo:Cudd.Memo.t ->
    ('a,'b) Cond.O.t -> ('a,'b) t -> ('a * 'a) list -> ('a,'b) t
  val substitute_by_var_list :
    ?memo:Cudd.Memo.t ->
    ('a,'b) Cond.O.t -> ('a,'b) t list -> ('a * 'a) list -> ('a,'b) t list
    (** Variable renaming.
	The new variables should already have been declared *)
  val substitute :
    ?memo:Cudd.Memo.t ->
    ('a,'b) Cond.O.t -> ('a,'b) t -> ('a * ('a,'b) t) list -> ('a,'b) t
  val substitute_list :
    ?memo:Cudd.Memo.t ->
    ('a,'b) Cond.O.t -> ('a,'b) t list -> ('a * ('a,'b) t) list -> ('a,'b) t list
    (** Parallel substitution of variables by expressions *)

  val support : ('a,'b) Cond.O.t -> ('a,'b) t -> 'a PSette.t
    (** Support of the expression *)
  val support_cond : Cudd.Man.vt -> ('a,'b) t -> Cudd.Bdd.vt
    (** Return the support of an expression as a conjunction of the BDD
	identifiers involved in the expression *)

  val cofactor : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    (** Evaluate the expression. The BDD is assumed to be a cube *)
  val restrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
  val tdrestrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    (** Simplify the expression knowing that the BDD is true.  Generalizes
	[cofactor]. *)

  val print : ('a,'b) Cond.O.t -> Format.formatter -> ('a,'b) t -> unit

  val normalize :
    ?reduce:bool -> ?careset:bool ->
    ('a,'b) Cond.O.t * ('a,'b) t list ->
    ('a,'b) Cond.O.t * ('a,'b) t list

  (*  ====================================================================== *)
  (** {4 List of expressions} *)
  (*  ====================================================================== *)

  module List : sig
    type ('a,'b) t = ('b, 'a Expr0.t list) Env.value
    constraint 'b = ('a,'c,'d,'e) Env.O.t

    val of_lexpr0 : 'b -> 'a Expr0.t list -> ('a,'b) t
    val get_env : ('a,'b) t -> 'b
    val to_lexpr0 : ('a,'b) t -> 'a Expr0.t list
    val of_lexpr : 'b -> ('a,'b) expr list -> ('a,'b) t
    val to_lexpr : ('a,'b) t -> ('a,'b) expr list
    val extend_environment : ('a,'b) t -> 'b -> ('a,'b) t
    val normalize :
      ?reduce:bool -> ?careset:bool ->
      ('a,'b) Cond.O.t * ('a,'b) t ->
      ('a,'b) Cond.O.t * ('a,'b) t
    val print :
      ?first:(unit,Format.formatter,unit) format ->
      ?sep:(unit,Format.formatter,unit) format ->
      ?last:(unit,Format.formatter,unit) format ->
      ('a,'b) Cond.O.t -> Format.formatter -> ('a,'b) t -> unit
  end
end
