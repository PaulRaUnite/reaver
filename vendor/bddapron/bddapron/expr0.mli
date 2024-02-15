(** Finite-type and arithmetical expressions *)

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

type 'a t = [
  | Cudd.Man.v Bdd.Expr0.t
  | `Apron of 'a ApronexprDD.t
]
type 'a expr = 'a t
  (** Type of general expressions *)

(** General expressions are described below, after Boolean, bounded
    integer and enumerated types expressions *)

(*  ====================================================================== *)
(** {4 Boolean expressions} *)
(*  ====================================================================== *)

module Bool : sig
  type 'a t = Cudd.Bdd.vt
  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr

  val dtrue : 'a Env.t -> 'a Cond.t -> 'a t
  val dfalse : 'a Env.t -> 'a Cond.t -> 'a t
  val of_bool : 'a Env.t -> 'a Cond.t -> bool -> 'a t
  val var : 'a Env.t -> 'a Cond.t -> 'a -> 'a t
  val ite : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a t -> 'a t

  val dnot : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t
  val dand : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a t
  val dor : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a t
  val xor : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a t
  val nand : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a t
  val nor : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a t
  val nxor : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a t
  val leq : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a t
  val eq : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a t

  val is_true : 'a Env.t -> 'a Cond.t -> 'a t -> bool
  val is_false : 'a Env.t -> 'a Cond.t -> 'a t -> bool
  val is_cst : 'a Env.t -> 'a Cond.t -> 'a t -> bool
  val is_leq : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> bool
  val is_eq : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> bool
  val is_and_false : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> bool
  val exist : 'a Env.t -> 'a Cond.t -> 'a list -> 'a t -> 'a t
  val forall : 'a Env.t -> 'a Cond.t -> 'a list -> 'a t -> 'a t

  val cofactor : 'a t -> 'a t -> 'a t
  val restrict : 'a t -> 'a t -> 'a t
  val tdrestrict : 'a t -> 'a t -> 'a t
  val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
  val varmap : 'a t -> 'a t

  val substitute_by_var : ?memo:Cudd.Memo.t -> 'a Env.t -> 'a Cond.t -> 'a t -> ('a * 'a) list -> 'a t
  val substitute : ?memo:Cudd.Memo.t -> 'a Env.t -> 'a Cond.t -> 'a t -> ('a * 'a expr) list -> 'a t

  val print : 'a Env.t -> 'a Cond.t -> Format.formatter -> 'a t -> unit
end

(*  ====================================================================== *)
(** {4 Bounded integer expressions} *)
(*  ====================================================================== *)

module Bint : sig
  type 'a t = Cudd.Man.v Bdd.Int.t
  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr

  val of_int : 'a Env.t -> 'a Cond.t -> [`Bint of bool * int ] -> int -> 'a t
  val var : 'a Env.t -> 'a Cond.t -> 'a -> 'a t
  val ite : 'a Env.t -> 'a Cond.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t

  val neg : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t
  val succ : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t
  val pred : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t
  val add : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a t
  val sub : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a t
  val mul : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a t
  val shift_left : 'a Env.t -> 'a Cond.t -> int -> 'a t -> 'a t
  val shift_right : 'a Env.t -> 'a Cond.t -> int -> 'a t -> 'a t
  val scale : 'a Env.t -> 'a Cond.t -> int -> 'a t -> 'a t
  val zero : 'a Env.t -> 'a Cond.t -> 'a t -> 'a Bool.t
  val eq : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a Bool.t
  val eq_int : 'a Env.t -> 'a Cond.t -> 'a t -> int -> 'a Bool.t
  val supeq : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a Bool.t
  val supeq_int : 'a Env.t -> 'a Cond.t -> 'a t -> int -> 'a Bool.t
  val sup : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a Bool.t
  val sup_int : 'a Env.t -> 'a Cond.t -> 'a t -> int -> 'a Bool.t

  val cofactor : 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t
  val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
  val varmap : 'a t -> 'a t

  val substitute_by_var : ?memo:Cudd.Memo.t -> 'a Env.t -> 'a Cond.t -> 'a t -> ('a * 'a) list -> 'a t
  val substitute : ?memo:Cudd.Memo.t -> 'a Env.t -> 'a Cond.t -> 'a t -> ('a * 'a expr) list -> 'a t

  val print : 'a Env.t -> 'a Cond.t -> Format.formatter -> 'a t -> unit
end

(*  ====================================================================== *)
(** {4 Enumerated expressions} *)
(*  ====================================================================== *)

module Benum : sig
  type 'a t = Cudd.Man.v Bdd.Enum.t
  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr
  val var : 'a Env.t -> 'a Cond.t -> 'a -> 'a t
  val ite : 'a Env.t -> 'a Cond.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t
  val eq : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a Bool.t
  val eq_label : 'a Env.t -> 'a Cond.t -> 'a t -> 'a -> 'a Bool.t
  val cofactor : 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t
  val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
  val varmap : 'a t -> 'a t
  val substitute_by_var : ?memo:Cudd.Memo.t -> 'a Env.t -> 'a Cond.t -> 'a t -> ('a * 'a) list -> 'a t
  val substitute : ?memo:Cudd.Memo.t -> 'a Env.t -> 'a Cond.t -> 'a t -> ('a * 'a expr) list -> 'a t
  val print : 'a Env.t -> 'a Cond.t -> Format.formatter -> 'a t -> unit
end

(*  ====================================================================== *)
(** {4 Arithmetic expressions} *)
(*  ====================================================================== *)

type apron_coeff = Apron.Coeff.t
type apron_typ = Apron.Texpr1.typ
type apron_round = Apron.Texpr1.round

module Apron : sig
  type 'a t = 'a ApronexprDD.t
  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr
  val cst : 'a Env.t -> 'a Cond.t -> Apron.Coeff.t -> 'a t
  val var : 'a Env.t -> 'a Cond.t -> 'a -> 'a t
  val add : 'a Env.t -> 'a Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t
  val sub : 'a Env.t -> 'a Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t
  val mul : 'a Env.t -> 'a Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t
  val div : 'a Env.t -> 'a Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t
  val gmod : 'a Env.t -> 'a Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t
  val negate : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t
  val cast :
    'a Env.t -> 'a Cond.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t
  val sqrt :
    'a Env.t -> 'a Cond.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t

  val supeq : 'a Env.t -> 'a Cond.t -> 'a t -> 'a Bool.t
  val sup : 'a Env.t -> 'a Cond.t -> 'a t -> 'a Bool.t
  val eq : 'a Env.t -> 'a Cond.t -> 'a t -> 'a Bool.t

  val ite : 'a Env.t -> 'a Cond.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t

  val cofactor :  'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t
  val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
  val varmap : 'a t -> 'a t

  val substitute_by_var : ?memo:Cudd.Memo.t -> 'a Env.t -> 'a Cond.t -> 'a t -> ('a * 'a) list -> 'a t
  val substitute : ?memo:Cudd.Memo.t -> 'a Env.t -> 'a Cond.t -> 'a t -> ('a * 'a expr) list -> 'a t

  val print : 'a Env.t -> 'a Cond.t -> Format.formatter -> 'a t -> unit
end

(*  ====================================================================== *)
(** {4 General expressions} *)
(*  ====================================================================== *)

(** The following operations raise a [Failure] exception in case of a typing
    error. *)

val typ_of_expr: 'a Env.t -> 'a t -> 'a Env.typ
  (** Type of an expression *)

val var : 'a Env.t -> 'a Cond.t -> 'a -> 'a t
  (** Expression representing the litteral var *)

val ite : 'a Env.t -> 'a Cond.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t
  (** If-then-else operation *)

val cofactor : 'a t -> 'a Bool.t -> 'a t
    (** Evaluate the expression. The BDD is assumed to be a cube *)

val substitute_by_var : ?memo:Cudd.Memo.t -> 'a Env.t -> 'a Cond.t -> 'a t -> ('a * 'a) list -> 'a t
val substitute_by_var_list : ?memo:Cudd.Memo.t -> 'a Env.t -> 'a Cond.t -> 'a t list -> ('a * 'a) list -> 'a t list
    (** Parallel substitution of variables by variables *)
val substitute : ?memo:Cudd.Memo.t -> 'a Env.t -> 'a Cond.t -> 'a t -> ('a*'a t) list -> 'a t
val substitute_list : ?memo:Cudd.Memo.t -> 'a Env.t -> 'a Cond.t -> 'a t list -> ('a*'a t) list -> 'a t list
    (** Parallel substitution of variables by expressions *)

val restrict : 'a t -> 'a Bool.t -> 'a t
val tdrestrict : 'a t -> 'a Bool.t -> 'a t
    (** Simplify the expression knowing that the BDD is true.  Generalizes
	[cofactor]. *)

val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
val varmap : 'a t -> 'a t
  (** Permutation (rather internal) *)

val support : 'a Env.t -> 'a Cond.t -> 'a t -> 'a PSette.t
    (** Return the full support of the expression *)

val eq : 'a Env.t -> 'a Cond.t -> 'a t -> 'a t -> 'a Bool.t
    (** Under which condition are the expressions equal ?  In case of
	arithmetic expressions, do not take into account the careset. *)

val support_cond : Cudd.Man.vt -> 'a t -> Cudd.Bdd.vt
    (** Return the support of an expression as a conjunction of the BDD
	identifiers involved in the expression *)

val conditions_support: 'a Env.t -> 'a Cond.t -> 'a t -> Cudd.Bdd.vt
val conditions_support': 'a Env.t -> 'a Cond.t -> 'a t list -> Cudd.Bdd.vt

(** Printing functions *)
val print : 'a Env.t -> 'a Cond.t -> Format.formatter -> [<'a t] -> unit

val normalize :
  ?reduce:bool -> ?careset:bool ->
  'a Cond.t * 'a t list -> 'a Cond.t * 'a t list

(*  ********************************************************************** *)
(** {3 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O : sig

  val check_typ2 :
    ('a,'b,'c,'d) Env.O.t -> [< 'a t] -> [< 'a t] -> 'a Env.typ

  module Bool : sig
    type 'a t = Cudd.Bdd.vt
    val of_expr : 'a expr -> 'a t
    val to_expr : 'a t -> 'a expr

    val dtrue : 'b -> ('a,'b) Cond.O.t -> 'a t
    val dfalse : 'b -> ('a,'b) Cond.O.t -> 'a t
    val of_bool : 'b -> ('a,'b) Cond.O.t -> bool -> 'a t
    val var : 'b -> ('a,'b) Cond.O.t -> 'a -> 'a t
    val ite : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a t -> 'a t

    val dnot : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t
    val dand : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a t
    val dor : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a t
    val xor : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a t
    val nand : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a t
    val nor : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a t
    val nxor : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a t
    val leq : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a t
    val eq : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a t

    val is_true : 'b -> ('a,'b) Cond.O.t -> 'a t -> bool
    val is_false : 'b -> ('a,'b) Cond.O.t -> 'a t -> bool
    val is_cst : 'b -> ('a,'b) Cond.O.t -> 'a t -> bool
    val is_leq : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> bool
    val is_eq : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> bool
    val is_and_false : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> bool
    val exist : 'b -> ('a,'b) Cond.O.t -> 'a list -> 'a t -> 'a t
    val forall : 'b -> ('a,'b) Cond.O.t -> 'a list -> 'a t -> 'a t

    val cofactor :  'a t -> 'a t -> 'a t
    val restrict :  'a t -> 'a t -> 'a t
    val tdrestrict :  'a t -> 'a t -> 'a t
    val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
    val varmap : 'a t -> 'a t

    val substitute_by_var : ?memo:Cudd.Memo.t -> 'b -> ('a,'b) Cond.O.t -> 'a t -> ('a * 'a) list -> 'a t
    val substitute : ?memo:Cudd.Memo.t -> 'b -> ('a,'b) Cond.O.t -> 'a t -> ('a * 'a expr) list -> 'a t

    val print : 'b -> ('a,'b) Cond.O.t -> Format.formatter -> 'a t -> unit
  end

  module Bint : sig
    type 'a t = Cudd.Man.v Bdd.Int.t
    val of_expr : 'a expr -> 'a t
    val to_expr : 'a t -> 'a expr

    val of_int : 'b -> ('a,'b) Cond.O.t -> [> `Bint of bool * int ] -> int -> 'a t
    val var : 'b -> ('a,'b) Cond.O.t -> 'a -> 'a t
    val ite : 'b -> ('a,'b) Cond.O.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t

    val neg : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t
    val succ : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t
    val pred : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t
    val add : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a t
    val sub : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a t
    val mul : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a t
    val shift_left : 'b -> ('a,'b) Cond.O.t -> int -> 'a t -> 'a t
    val shift_right : 'b -> ('a,'b) Cond.O.t -> int -> 'a t -> 'a t
    val scale : 'b -> ('a,'b) Cond.O.t -> int -> 'a t -> 'a t
    val zero : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a Bool.t
    val eq : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a Bool.t
    val eq_int : 'b -> ('a,'b) Cond.O.t -> 'a t -> int -> 'a Bool.t
    val supeq : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a Bool.t
    val supeq_int : 'b -> ('a,'b) Cond.O.t -> 'a t -> int -> 'a Bool.t
    val sup : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a Bool.t
    val sup_int : 'b -> ('a,'b) Cond.O.t -> 'a t -> int -> 'a Bool.t

    val cofactor :  'a t -> 'a Bool.t -> 'a t
    val restrict :  'a t -> 'a Bool.t -> 'a t
    val tdrestrict :  'a t -> 'a Bool.t -> 'a t
    val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
    val varmap : 'a t -> 'a t

    val substitute_by_var : ?memo:Cudd.Memo.t -> 'b -> ('a,'b) Cond.O.t -> 'a t -> ('a * 'a) list -> 'a t
    val substitute : ?memo:Cudd.Memo.t -> 'b -> ('a,'b) Cond.O.t -> 'a t -> ('a * 'a expr) list -> 'a t

    val print : 'b -> ('a,'b) Cond.O.t -> Format.formatter -> 'a t -> unit
  end

  module Benum : sig
    type 'a t = Cudd.Man.v Bdd.Enum.t
    val of_expr : 'a expr -> 'a t
    val to_expr : 'a t -> 'a expr
    val var : 'b -> ('a,'b) Cond.O.t -> 'a -> 'a t
    val ite : 'b -> ('a,'b) Cond.O.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t
    val eq : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a Bool.t
    val eq_label : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a -> 'a Bool.t
    val cofactor :  'a t -> 'a Bool.t -> 'a t
    val restrict :  'a t -> 'a Bool.t -> 'a t
    val tdrestrict :  'a t -> 'a Bool.t -> 'a t
    val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
    val varmap : 'a t -> 'a t
    val substitute_by_var : ?memo:Cudd.Memo.t -> 'b -> ('a,'b) Cond.O.t -> 'a t -> ('a * 'a) list -> 'a t
    val substitute : ?memo:Cudd.Memo.t -> 'b -> ('a,'b) Cond.O.t -> 'a t -> ('a * 'a expr) list -> 'a t
    val print : 'b -> ('a,'b) Cond.O.t -> Format.formatter -> 'a t -> unit
  end

  module Apron : sig
    type 'a t = 'a ApronexprDD.t
    val of_expr : [> `Apron of 'a t ] -> 'a t
    val to_expr : 'a t -> [> `Apron of 'a t ]
    val cst : 'b -> ('a,'b) Cond.O.t -> apron_coeff -> 'a t
    val var : 'b -> ('a,'b) Cond.O.t -> 'a -> 'a t
    val add :'b -> ('a,'b) Cond.O.t ->
      ?typ:apron_typ -> ?round:apron_round ->
      'a t -> 'a t -> 'a t
    val sub : 'b -> ('a,'b) Cond.O.t ->
      ?typ:apron_typ -> ?round:apron_round ->
      'a t -> 'a t -> 'a t
    val mul : 'b -> ('a,'b) Cond.O.t ->
      ?typ:apron_typ -> ?round:apron_round ->
      'a t -> 'a t -> 'a t
    val div : 'b -> ('a,'b) Cond.O.t ->
      ?typ:apron_typ -> ?round:apron_round ->
      'a t -> 'a t -> 'a t
    val gmod : 'b -> ('a,'b) Cond.O.t ->
      ?typ:apron_typ -> ?round:apron_round ->
      'a t -> 'a t -> 'a t
    val negate : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t
    val cast :
      'b -> ('a,'b) Cond.O.t -> ?typ:apron_typ -> ?round:apron_round ->
      'a t -> 'a t
    val sqrt :
      'b -> ('a,'b) Cond.O.t -> ?typ:apron_typ -> ?round:apron_round ->
      'a t -> 'a t

    val supeq : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a Bool.t
    val sup : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a Bool.t
    val eq : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a Bool.t

    val ite : 'b -> ('a,'b) Cond.O.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t

    val cofactor :  'a t -> 'a Bool.t -> 'a t
    val restrict : 'a t -> 'a Bool.t -> 'a t
    val tdrestrict : 'a t -> 'a Bool.t -> 'a t
    val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
    val varmap : 'a t -> 'a t

    val substitute_by_var : ?memo:Cudd.Memo.t -> 'b -> ('a,'b) Cond.O.t -> 'a t -> ('a * 'a) list -> 'a t
    val substitute : ?memo:Cudd.Memo.t -> 'b -> ('a,'b) Cond.O.t -> 'a t -> ('a * 'a expr) list -> 'a t

    val print : 'b -> ('a,'b) Cond.O.t -> Format.formatter -> 'a t -> unit
  end

(** The following operations raise a [Failure] exception in case of a typing
  error. *)

  val typ_of_expr: ('a,'b,'c,'d) Env.O.t -> [< 'a t] -> 'a Env.typ
  (** Type of an expression *)

  val var : 'b -> ('a,'b) Cond.O.t -> 'a -> 'a t
  (** Expression representing the litteral var *)

  val ite : 'b -> ('a,'b) Cond.O.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t
  (** If-then-else operation *)

  val cofactor :  'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t
  val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
  val permute_list : ?memo:Cudd.Memo.t -> 'a t list -> int array -> 'a t list
  val varmap : 'a t -> 'a t

  val substitute_by_var : ?memo:Cudd.Memo.t -> 'b -> ('a,'b) Cond.O.t -> 'a t -> ('a * 'a) list -> 'a t
  val substitute_by_var_list : ?memo:Cudd.Memo.t -> 'b -> ('a,'b) Cond.O.t -> 'a t list -> ('a * 'a) list -> 'a t list
    (** Parallel substitution of variables by variables *)
  val substitute : ?memo:Cudd.Memo.t -> 'b -> ('a,'b) Cond.O.t -> 'a t -> ('a * 'a t) list -> 'a t
  val substitute_list : ?memo:Cudd.Memo.t -> 'b -> ('a,'b) Cond.O.t -> 'a t list -> ('a * 'a t) list -> 'a t list
    (** Parallel substitution of variables by expressions *)

  val support : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a PSette.t
    (** Return the full support of the expression *)

  val eq : 'b -> ('a,'b) Cond.O.t -> 'a t -> 'a t -> 'a Bool.t
    (** Under which condition are the expressions equal ?  In case of
	arithmetic expressions, do not take into account the careset. *)

  val support_cond : Cudd.Man.vt -> 'a t -> Cudd.Bdd.vt
    (** Return the support of an expression as a conjunction of the BDD
	identifiers involved in the expression *)

  val conditions_support: 'b -> ('a, 'b) Cond.O.t -> 'a t -> Cudd.Bdd.vt
  val conditions_support': 'b -> ('a, 'b) Cond.O.t -> 'a t list -> Cudd.Bdd.vt

  (** Printing functions *)

  val print : 'b -> ('a,'b) Cond.O.t -> Format.formatter -> [<'a t] -> unit
  val print_bdd : 'b -> ('a,'b) Cond.O.t -> Format.formatter -> Cudd.Bdd.vt -> unit

  val normalize :
    ?reduce:bool -> ?careset:bool ->
    ('a,'b) Cond.O.t * 'a t list ->
    ('a,'b) Cond.O.t * 'a t list

  val compose_of_lvarexpr :
    'b -> ('a,'b) Cond.O.t -> 'a t list -> ('a * 'a t) list -> Cudd.Bdd.vt array option * ('a, 'a t) PMappe.t

end
