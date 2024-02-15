(** Finite-type expressions with normalized environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 Expressions} *)
(*  ********************************************************************** *)

type ('a,'b) t = (('a,'b) Env.t, 'b Expr0.t) Env.value
type ('a,'b) expr = ('a,'b) t
  (** Type of general expressions *)

type 'a dt = ('a,Cudd.Man.d) t
type 'a vt = ('a,Cudd.Man.v) t

(** General expressions are described below, after Boolean, bounded
    integer and enumerated types expressions *)

(*  ====================================================================== *)
(** {4 Boolean expressions} *)
(*  ====================================================================== *)

module Bool : sig
  type ('a,'b) t = (('a,'b) Env.t, 'b Cudd.Bdd.t) Env.value

  type 'a dt = ('a,Cudd.Man.d) t
  type 'a vt = ('a,Cudd.Man.v) t

  val of_expr0 : ('a,'b) Env.t -> 'b Expr0.Bool.t -> ('a,'b) t
    (** Creation from an expression of level 0 (without environment) *)
  val get_env : ('a,'b) t -> ('a,'b) Env.t
  val to_expr0 : ('a,'b) t -> 'b Expr0.Bool.t
    (** Extract resp. the environment and the underlying
	expression of level 0 *)

  val of_expr : ('a,'b) expr -> ('a,'b) t
  val to_expr : ('a,'b) t -> ('a,'b) expr
    (** Conversion from/to general expression *)

  val extend_environment : ('a,'b) t -> ('a,'b) Env.t -> ('a,'b) t
    (** Extend the underlying environment to a superenvironment,
	and adapt accordingly the underlying representation *)

  val dtrue : ('a,'b) Env.t -> ('a,'b) t
  val dfalse : ('a,'b) Env.t -> ('a,'b) t
  val of_bool : ('a,'b) Env.t -> bool -> ('a,'b) t
  val var : ('a,'b) Env.t -> 'a -> ('a,'b) t

  (** {5 Logical connectors} *)

  val dnot : ('a,'b) t -> ('a,'b) t
  val dand : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val dor : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    (** [not], [and] and [or] (use of 'd' prefix because of conflict with OCaml
      keywords) *)

  val xor : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val nand : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val nor : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val nxor : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    (** Exclusive or, not and, nor or and not xor *)

  val eq : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    (** Same as [nxor] *)
  val leq : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    (** Implication *)

  val ite : ('a,'b) t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    (** If-then-else *)

  val is_true : ('a,'b) t -> bool
  val is_false : ('a,'b) t -> bool
  val is_cst : ('a,'b) t -> bool
  val is_eq : ('a,'b) t -> ('a,'b) t -> bool
  val is_leq : ('a,'b) t -> ('a,'b) t -> bool
  val is_inter_false : ('a,'b) t -> ('a,'b) t -> bool

  val exist : 'a list -> ('a,'b) t -> ('a,'b) t
  val forall : 'a list -> ('a,'b) t -> ('a,'b) t

  val cofactor : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val restrict : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val tdrestrict : ('a,'b) t -> ('a,'b) t -> ('a,'b) t

  val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b) t -> ('a * 'a) list -> ('a,'b) t
  val substitute : ?memo:Cudd.Memo.t -> ('a,'b) t -> ('a * ('a,'b) expr) list -> ('a,'b) t

  val print : Format.formatter -> ('a,'b) t -> unit
end

(*  ====================================================================== *)
(** {4 Bounded integer expressions} *)
(*  ====================================================================== *)

module Bint : sig
  type ('a,'b) t = (('a,'b) Env.t, 'b Int.t) Env.value

  type 'a dt = ('a,Cudd.Man.d) t
  type 'a vt = ('a,Cudd.Man.v) t

  val of_expr0 : ('a,'b) Env.t -> 'b Expr0.Bint.t -> ('a,'b) t
    (** Creation from an expression of level 0 (without environment) *)
  val get_env : ('a,'b) t -> ('a,'b) Env.t
  val to_expr0 : ('a,'b) t -> 'b Expr0.Bint.t
    (** Extract resp. the environment and the underlying
	expression of level 0 *)

  val of_expr : ('a,'b) expr -> ('a,'b) t
  val to_expr : ('a,'b) t -> ('a,'b) expr
    (** Conversion from/to general expression *)

  val extend_environment : ('a,'b) t -> ('a,'b) Env.t -> ('a,'b) t
    (** Extend the underlying environment to a superenvironment,
	and adapt accordingly the underlying representation *)

  val of_int :
    ('a,'b) Env.t -> [`Bint of bool * int ] -> int -> ('a,'b) t
  val var : ('a,'b) Env.t -> 'a -> ('a,'b) t

  val neg : ('a,'b) t -> ('a,'b) t
  val succ : ('a,'b) t -> ('a,'b) t
  val pred : ('a,'b) t -> ('a,'b) t
  val add : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val sub : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val mul : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val shift_left : int -> ('a,'b) t -> ('a,'b) t
  val shift_right : int -> ('a,'b) t -> ('a,'b) t
  val scale : int -> ('a,'b) t -> ('a,'b) t
  val ite : ('a,'b) Bool.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val zero : ('a,'b) t -> ('a,'b) Bool.t
  val eq : ('a,'b) t -> ('a,'b) t -> ('a,'b) Bool.t
  val supeq : ('a,'b) t -> ('a,'b) t -> ('a,'b) Bool.t
  val sup : ('a,'b) t -> ('a,'b) t -> ('a,'b) Bool.t
  val eq_int : ('a,'b) t -> int -> ('a,'b) Bool.t
  val supeq_int : ('a,'b) t -> int -> ('a,'b) Bool.t
  val sup_int : ('a,'b) t -> int -> ('a,'b) Bool.t

  val cofactor : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
  val restrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
  val tdrestrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t

  val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b) t -> ('a * 'a) list -> ('a,'b) t
  val substitute : ?memo:Cudd.Memo.t -> ('a,'b) t -> ('a * ('a,'b) expr) list -> ('a,'b) t

  val guard_of_int: ('a,'b) t -> int -> ('a,'b) Bool.t
    (** Return the guard of the integer value. *)
  val guardints: ('a,'b) t -> (('a,'b) Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

  val print : Format.formatter -> ('a,'b) t -> unit
end

(*  ====================================================================== *)
(** {4 Enumerated expressions} *)
(*  ====================================================================== *)

module Benum : sig
  type ('a,'b) t = (('a,'b) Env.t, 'b Enum.t) Env.value

  type 'a dt = ('a,Cudd.Man.d) t
  type 'a vt = ('a,Cudd.Man.v) t

  val of_expr0 : ('a,'b) Env.t -> 'b Expr0.Benum.t -> ('a,'b) t
    (** Creation from an expression of level 0 (without environment) *)
  val get_env : ('a,'b) t -> ('a,'b) Env.t
  val to_expr0 : ('a,'b) t -> 'b Expr0.Benum.t
    (** Extract resp. the environment and the underlying
	expression of level 0 *)

  val of_expr : ('a,'b) expr -> ('a,'b) t
  val to_expr : ('a,'b) t -> ('a,'b) expr
    (** Conversion from/to general expression *)

  val extend_environment : ('a,'b) t -> ('a,'b) Env.t -> ('a,'b) t
    (** Extend the underlying environment to a superenvironment,
	and adapt accordingly the underlying representation *)

  val var : ('a,'b) Env.t -> 'a -> ('a,'b) t
  val ite : ('a,'b) Bool.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val eq : ('a,'b) t -> ('a,'b) t -> ('a,'b) Bool.t
  val eq_label : ('a,'b) t -> 'a -> ('a,'b) Bool.t

  val cofactor : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
  val restrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
  val tdrestrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t

  val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b) t -> ('a * 'a) list -> ('a,'b) t
  val substitute : ?memo:Cudd.Memo.t -> ('a,'b) t -> ('a * ('a,'b) expr) list -> ('a,'b) t

  val guard_of_label : ('a,'b) t -> 'a -> ('a,'b) Bool.t
    (** Return the guard of the label. *)

  val guardlabels : ('a,'b) t -> (('a,'b) Bool.t * 'a) list
    (** Return the list [g -> label] of guarded values. *)

  val print : Format.formatter -> ('a,'b) t -> unit
end

(*  ====================================================================== *)
(** {4 General expressions} *)
(*  ====================================================================== *)

val typ_of_expr : ('a,'b) t -> 'a Env.typ
  (** Type of an expression *)

val make : ('a,'b) Env.t -> 'b Expr0.t -> ('a,'b) t
val of_expr0 : ('a,'b) Env.t -> 'b Expr0.t -> ('a,'b) t
  (** Creation from an expression of level 0 (without environment)
      (make should be considered as obsolete) *)

val get_env : ('a,'b) t -> ('a,'b) Env.t
val to_expr0 : ('a,'b) t -> 'b Expr0.t
  (** Extract resp. the environment and the underlying expression
      of level 0 *)

val extend_environment : ('a,'b) t -> ('a,'b) Env.t -> ('a,'b) t
  (** Extend the underlying environment to a superenvironment, and
      adapt accordingly the underlying representation *)

val var : ('a,'b) Env.t -> 'a -> ('a,'b) t
  (** Expression representing the litteral var *)
val ite : ('a,'b) Bool.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  (** If-then-else operation *)
val eq : ('a,'b) t -> ('a,'b) t -> ('a,'b) Bool.t
  (** Equality operation *)

val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b) t -> ('a * 'a) list -> ('a,'b) t
val substitute_by_var_list : ?memo:Cudd.Memo.t -> ('a,'b) t list -> ('a * 'a) list -> ('a,'b) t list
   (** Variable renaming.
      The new variables should already have been declared *)
val substitute : ?memo:Cudd.Memo.t -> ('a,'b) t -> ('a * ('a,'b) t) list -> ('a,'b) t
val substitute_list : ?memo:Cudd.Memo.t -> ('a,'b) t list -> ('a * ('a,'b) t) list -> ('a,'b) t list
    (** Parallel substitution of variables by expressions *)

val support : ('a,'b) t -> 'a PSette.t
    (** Support of the expression *)

val support_cond : ('a,'b) t -> 'b Cudd.Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
      identifiers involved in the expression *)

val cofactor : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    (** Evaluate the expression. The BDD is assumed to be a cube *)

val restrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
val tdrestrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    (** Simplify the expression knowing that the BDD is true.  Generalizes
      [cofactor]. *)

val print : Format.formatter -> ('a,'b) t -> unit

(*  ====================================================================== *)
(** {4 List of expressions} *)
(*  ====================================================================== *)

module List : sig
  type ('a,'b) t = (('a,'b) Env.t, 'b Expr0.t list) Env.value
  type 'a dt = ('a,Cudd.Man.d) t
  type 'a vt = ('a,Cudd.Man.v) t

  val of_lexpr0 : ('a,'b) Env.t -> 'b Expr0.t list -> ('a,'b) t
  val get_env : ('a,'b) t -> ('a,'b) Env.t
  val to_lexpr0 : ('a,'b) t -> 'b Expr0.t list

  val of_lexpr : ('a,'b) Env.t -> ('a,'b) expr list -> ('a,'b) t
  val to_lexpr : ('a,'b) t -> ('a,'b) expr list
  val extend_environment : ('a,'b) t -> ('a,'b) Env.t -> ('a,'b) t

  val print :
    ?first:(unit,Format.formatter,unit) format ->
    ?sep:(unit,Format.formatter,unit) format ->
    ?last:(unit,Format.formatter,unit) format ->
    Format.formatter -> ('a,'b) t -> unit

end

(*  ********************************************************************** *)
(** {3 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows etxensions). The functions above are actually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig

  (*  ==================================================================== *)
  (** {4 Expressions} *)
  (*  ==================================================================== *)

  type ('a,'b,'c) t = ('b, 'c Expr0.t) Env.value
  constraint 'b = ('a,'d,'e,'c,'f) Env.O.t

  type ('a,'b,'c) expr = ('a,'b,'c) t
    (** Type of general expressions *)

  type ('a,'b) dt = ('a,'b,Cudd.Man.d) t
  type ('a,'b) vt = ('a,'b,Cudd.Man.v) t

  (*  -------------------------------------------------------------------- *)
  (** {5 Boolean expressions} *)
  (*  -------------------------------------------------------------------- *)

  module Bool : sig
    type ('a,'b,'c) t = ('b, 'c Cudd.Bdd.t) Env.value
    constraint 'b = ('a,'d,'e,'c,'f) Env.O.t

    type ('a,'b) dt = ('a,'b,Cudd.Man.d) t
    type ('a,'b) vt = ('a,'b,Cudd.Man.v) t

    val of_expr0 : 'b -> 'c Expr0.Bool.t -> ('a,'b,'c) t
    val get_env : ('a,'b,'c) t -> 'b
    val to_expr0 : ('a,'b,'c) t -> 'c Expr0.Bool.t

    val of_expr : ('b, [> `Bool of 'c Cudd.Bdd.t ]) Env.value -> ('a,'b,'c) t
    val to_expr : ('a,'b,'c) t -> ('b, [> `Bool of 'c Cudd.Bdd.t ]) Env.value

    val extend_environment : ('a,'b,'c) t -> 'b -> ('a,'b,'c) t

    val dtrue : 'b -> ('a,'b,'c) t
    val dfalse : 'b -> ('a,'b,'c) t
    val of_bool : 'b -> bool -> ('a,'b,'c) t
    val var : 'b -> 'a -> ('a,'b,'c) t

    (** {5 Logical connectors} *)

    val dnot : ('a,'b,'c) t -> ('a,'b,'c) t
    val dand : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    val dor : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    (** [not], [and] and [or] (use of 'd' prefix because of conflict with OCaml
	keywords) *)

    val xor : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    val nand : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    val nor : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    val nxor : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    (** Exclusive or, not and, nor or and not xor *)

    val eq : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    (** Same as [nxor] *)
    val leq : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    (** Implication *)

    val ite : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    (** If-then-else *)

    val is_true : ('a,'b,'c) t -> bool
    val is_false : ('a,'b,'c) t -> bool
    val is_cst : ('a,'b,'c) t -> bool
    val is_eq : ('a,'b,'c) t -> ('a,'b,'c) t -> bool
    val is_leq : ('a,'b,'c) t -> ('a,'b,'c) t -> bool
    val is_inter_false : ('a,'b,'c) t -> ('a,'b,'c) t -> bool

    val exist : 'a list -> ('a,'b,'c) t -> ('a,'b,'c) t
    val forall : 'a list -> ('a,'b,'c) t -> ('a,'b,'c) t

    val cofactor : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    val restrict : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    val tdrestrict : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t

    val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b,'c) t -> ('a * 'a) list -> ('a,'b,'c) t
    val substitute : ?memo:Cudd.Memo.t -> ('a,'b,'c) t -> ('a * ('a,'b,'c) expr) list -> ('a,'b,'c) t

    val print : Format.formatter -> ('a,'b,'c) t -> unit
  end

  (*  -------------------------------------------------------------------- *)
  (** {5 Bounded integer expressions} *)
  (*  -------------------------------------------------------------------- *)

  module Bint : sig
    type ('a,'b,'c) t = ('b, 'c Int.t) Env.value
    constraint 'b = ('a,'d,'e,'c,'f) Env.O.t

    type ('a,'b,'c) dt = ('a,'b,Cudd.Man.d) t
    type ('a,'b,'c) vt = ('a,'b,Cudd.Man.v) t

    val of_expr0 : 'b -> 'c Expr0.Bint.t -> ('a,'b,'c) t
    val get_env : ('a,'b,'c) t -> 'b
    val to_expr0 : ('a,'b,'c) t -> 'c Expr0.Bint.t

    val of_expr : ('b, [> `Bint of 'c Int.t ]) Env.value -> ('a,'b,'c) t
    val to_expr : ('a,'b,'c) t -> ('b, [> `Bint of 'c Int.t ]) Env.value
    val extend_environment : ('a,'b,'c) t -> 'b -> ('a,'b,'c) t

    val of_int :
      'b -> [`Bint of bool * int ] -> int -> ('a,'b,'c) t
    val var : 'b -> 'a -> ('a,'b,'c) t

    val neg : ('a,'b,'c) t -> ('a,'b,'c) t
    val succ : ('a,'b,'c) t -> ('a,'b,'c) t
    val pred : ('a,'b,'c) t -> ('a,'b,'c) t
    val add : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    val sub : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    val mul : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    val shift_left : int -> ('a,'b,'c) t -> ('a,'b,'c) t
    val shift_right : int -> ('a,'b,'c) t -> ('a,'b,'c) t
    val scale : int -> ('a,'b,'c) t -> ('a,'b,'c) t
    val ite : ('a,'b,'c) Bool.t -> ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    val zero : ('a,'b,'c) t -> ('a,'b,'c) Bool.t
    val eq : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) Bool.t
    val supeq : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) Bool.t
    val sup : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) Bool.t
    val eq_int : ('a,'b,'c) t -> int -> ('a,'b,'c) Bool.t
    val supeq_int : ('a,'b,'c) t -> int -> ('a,'b,'c) Bool.t
    val sup_int : ('a,'b,'c) t -> int -> ('a,'b,'c) Bool.t

    val cofactor : ('a,'b,'c) t -> ('a,'b,'c) Bool.t -> ('a,'b,'c) t
    val restrict : ('a,'b,'c) t -> ('a,'b,'c) Bool.t -> ('a,'b,'c) t
    val tdrestrict : ('a,'b,'c) t -> ('a,'b,'c) Bool.t -> ('a,'b,'c) t

    val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b,'c) t -> ('a * 'a) list -> ('a,'b,'c) t
    val substitute : ?memo:Cudd.Memo.t -> ('a,'b,'c) t -> ('a * ('a,'b,'c) expr) list -> ('a,'b,'c) t

    val guard_of_int: ('a,'b,'c) t -> int -> ('a,'b,'c) Bool.t
    (** Return the guard of the integer value. *)
    val guardints: ('a,'b,'c) t -> (('a,'b,'c) Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

    val print : Format.formatter -> ('a,'b,'c) t -> unit
  end

  (*  -------------------------------------------------------------------- *)
  (** {5 Enumerated expressions} *)
  (*  -------------------------------------------------------------------- *)

  module Benum : sig
    type ('a,'b,'c) t = ('b, 'c Enum.t) Env.value
    constraint 'b = ('a,'d,'e,'c,'f) Env.O.t

    type ('a,'b) dt = ('a,'b,Cudd.Man.d) t
    type ('a,'b) vt = ('a,'b,Cudd.Man.v) t

    val of_expr0 : 'b -> 'c Expr0.Benum.t -> ('a,'b,'c) t
    val get_env : ('a,'b,'c) t -> 'b
    val to_expr0 : ('a,'b,'c) t -> 'c Expr0.Benum.t

    val of_expr : ('b, [> `Benum of 'c Enum.t ]) Env.value -> ('a,'b,'c) t
    val to_expr : ('a,'b,'c) t -> ('b, [> `Benum of 'c Enum.t ]) Env.value
    val extend_environment : ('a,'b,'c) t -> 'b -> ('a,'b,'c) t

    val var : 'b -> 'a -> ('a,'b,'c) t
    val ite : ('a,'b,'c) Bool.t -> ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
    val eq : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) Bool.t
    val eq_label : ('a,'b,'c) t -> 'a -> ('a,'b,'c) Bool.t

    val cofactor : ('a,'b,'c) t -> ('a,'b,'c) Bool.t -> ('a,'b,'c) t
    val restrict : ('a,'b,'c) t -> ('a,'b,'c) Bool.t -> ('a,'b,'c) t
    val tdrestrict : ('a,'b,'c) t -> ('a,'b,'c) Bool.t -> ('a,'b,'c) t

    val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b,'c) t -> ('a * 'a) list -> ('a,'b,'c) t
    val substitute : ?memo:Cudd.Memo.t -> ('a,'b,'c) t -> ('a * ('a,'b,'c) expr) list -> ('a,'b,'c) t

    val guard_of_label : ('a,'b,'c) t -> 'a -> ('a,'b,'c) Bool.t
    (** Return the guard of the label. *)

    val guardlabels : ('a,'b,'c) t -> (('a,'b,'c) Bool.t * 'a) list
    (** Return the list [g -> label] of guarded values. *)

    val print : Format.formatter -> ('a,'b,'c) t -> unit
  end

  (*  -------------------------------------------------------------------- *)
  (** {5 General expressions} *)
  (*  -------------------------------------------------------------------- *)

  val typ_of_expr : ('a,'b,'c) t -> 'a Env.typ
    (** Type of an expression *)

  val make : 'b -> 'c Expr0.t -> ('a,'b,'c) expr
  val of_expr0 : 'b -> 'c Expr0.t -> ('a,'b,'c) expr
    (** Creation from an expression without environment *)

  val get_env : ('a,'b,'c) t -> 'b
  val to_expr0 : ('a,'b,'c) t -> 'c Expr0.t

  val extend_environment : ('a,'b,'c) t -> 'b -> ('a,'b,'c) t

  val var : 'b -> 'a -> ('a,'b,'c) t
  (** Expression representing the litteral var *)
  val ite : ('a,'b,'c) Bool.t -> ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
  (** If-then-else operation *)
  val eq : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) Bool.t
  (** Equality operation *)

  val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b,'c) t -> ('a * 'a) list -> ('a,'b,'c) t
  val substitute_by_var_list : ?memo:Cudd.Memo.t -> ('a,'b,'c) t list -> ('a * 'a) list -> ('a,'b,'c) t list
    (** Variable renaming.
	The new variables should already have been declared *)
  val substitute : ?memo:Cudd.Memo.t -> ('a,'b,'c) t -> ('a * ('a,'b,'c) t) list -> ('a,'b,'c) t
  val substitute_list : ?memo:Cudd.Memo.t -> ('a,'b,'c) t list -> ('a * ('a,'b,'c) t) list -> ('a,'b,'c) t list
    (** Parallel substitution of variables by expressions *)

  val support : ('a,'b,'c) t -> 'a PSette.t
    (** Support of the expression *)

  val support_cond : ('a,'b,'c) t -> 'c Cudd.Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
	identifiers involved in the expression *)

  val cofactor : ('a,'b,'c) t -> ('a,'b,'c) Bool.t -> ('a,'b,'c) t
    (** Evaluate the expression. The BDD is assumed to be a cube *)

  val restrict : ('a,'b,'c) t -> ('a,'b,'c) Bool.t -> ('a,'b,'c) t
  val tdrestrict : ('a,'b,'c) t -> ('a,'b,'c) Bool.t -> ('a,'b,'c) t
    (** Simplify the expression knowing that the BDD is true.  Generalizes
	[cofactor]. *)

  val print : Format.formatter -> ('a,'b,'c) t -> unit

  (*  -------------------------------------------------------------------- *)
  (** {5 List of expressions} *)
  (*  -------------------------------------------------------------------- *)

  module List : sig
    type ('a,'b,'c) t = ('b, 'c Expr0.t list) Env.value
    constraint 'b = ('a,'d,'e,'c,'f) Env.O.t

    type ('a,'b) dt = ('a,'b,Cudd.Man.d) t
    type ('a,'b) vt = ('a,'b,Cudd.Man.v) t

    val of_lexpr0 : 'b -> 'c Expr0.t list -> ('a,'b,'c) t
    val get_env : ('a,'b,'c) t -> 'b
    val to_lexpr0 : ('a,'b,'c) t -> 'c Expr0.t list

    val of_lexpr : 'b -> ('a,'b,'c) expr list -> ('a,'b,'c) t
    val to_lexpr : ('a,'b,'c) t -> ('a,'b,'c) expr list
    val extend_environment : ('a,'b,'c) t -> 'b -> ('a,'b,'c) t
    val print :
      ?first:(unit,Format.formatter,unit) format ->
      ?sep:(unit,Format.formatter,unit) format ->
      ?last:(unit,Format.formatter,unit) format ->
      Format.formatter -> ('a,'b,'c) t -> unit

  end

end
