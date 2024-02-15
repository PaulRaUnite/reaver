(** Finite-type and arithmetical expressions with variable and condition environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

(*  ********************************************************************** *)
(** {3 Opened signature} *)
(*  ********************************************************************** *)

module O : sig

  (*  ==================================================================== *)
  (** {4 Boolean expressions} *)
  (*  ==================================================================== *)

  module Bool : sig
    type ('a,'b)  t = (('a,'b)  Cond.O.t, ('a,'b)  Expr1.O.Bool.t) Bdd.Cond.value

   val of_expr0 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     'b -> ('a,'b) Cond.O.t -> 'a Expr0.Bool.t -> ('a,'b) t
   val of_expr1 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     ('a,'b) Cond.O.t -> ('a,'b) Expr1.O.Bool.t -> ('a,'b) t
   val get_env : ('a,'b) t -> 'b
   val get_cond : ('a,'b) t -> ('a,'b) Cond.O.t
   val to_expr0 : ('a,'b) t -> 'a Expr0.Bool.t
   val to_expr1 : ('a,'b) t -> ('a,'b) Expr1.O.Bool.t

    val of_expr :
      (('a,'b) Cond.O.t, ('b, [> `Bool of 'a Expr0.Bool.t ]) Env.value) Bdd.Cond.value ->
      ('a,'b)  t
    val to_expr :
      ('a,'b)  t ->
      (('a,'b) Cond.O.t, ('b, [> `Bool of 'a Expr0.Bool.t ]) Env.value) Bdd.Cond.value
   val extend_environment : ('a,'b) t -> 'b -> ('a,'b) t

   val is_false : ('a,'b) t -> bool
   val is_true : ('a,'b) t -> bool

   val print : Format.formatter -> ('a,'b) t -> unit
  end

  (*  ==================================================================== *)
  (** {4 General expressions} *)
  (*  ==================================================================== *)

  type ('a,'b) t = (('a,'b) Cond.O.t, ('a,'b) Expr1.O.t) Bdd.Cond.value

  type ('a,'b) expr = ('a,'b) t

  val of_expr0 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'b -> ('a,'b) Cond.O.t -> 'a Expr0.t -> ('a,'b) t
  val of_expr1 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    ('a,'b) Cond.O.t -> ('a,'b) Expr1.O.t -> ('a,'b) t
   val get_env : ('a,'b) t -> 'b
   val get_cond : ('a,'b) t -> ('a,'b) Cond.O.t
   val to_expr0 : ('a,'b) t -> 'a Expr0.t
   val to_expr1 : ('a,'b) t -> ('a,'b) Expr1.O.t

  val extend_environment : ('a,'b) t -> 'b -> ('a,'b) t
  val print : Format.formatter -> ('a,'b) t -> unit

  (*  ==================================================================== *)
  (** {4 List of expressions} *)
  (*  ==================================================================== *)

  module List : sig
    type ('a,'b) t = (('a,'b) Cond.O.t, ('a,'b) Expr1.O.List.t) Bdd.Cond.value

   val of_lexpr0 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     'b -> ('a,'b) Cond.O.t -> 'a Expr0.t list -> ('a,'b) t
   val of_lexpr1 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     'b -> ('a,'b) Cond.O.t -> ('a,'b) Expr1.O.t list -> ('a,'b) t
   val of_listexpr1 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     ('a,'b) Cond.O.t -> ('a,'b) Expr1.O.List.t -> ('a,'b) t
   val get_env : ('a,'b) t -> 'b
   val get_cond : ('a,'b) t -> ('a,'b) Cond.O.t
   val to_lexpr0 : ('a,'b) t -> 'a Expr0.t list
   val to_lexpr1 : ('a,'b) t -> ('a,'b) Expr1.O.t list
   val to_listexpr1 : ('a,'b) t -> ('a,'b) Expr1.O.List.t

   val extend_environment : ('a,'b) t -> 'b -> ('a,'b) t

   val print : Format.formatter -> ('a,'b) t -> unit
  end
end

(*  ********************************************************************** *)
(** {3 Closed signature} *)
(*  ********************************************************************** *)

type 'a t = ('a Cond.t, 'a Expr1.t) Bdd.Cond.value
type 'a expr = 'a t

val of_expr0 :
  ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
  'a Env.t -> 'a Cond.t -> 'a Expr0.t -> 'a t
  (** Creation from an expression of level 0 (without
      environment) *)
val of_expr1 :
  ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
  'a Cond.t -> 'a Expr1.t -> 'a t
  (** Creation from an expression of level 1 (without condition
      environment) *)
val get_env : 'a t -> 'a Env.t
val get_cond : 'a t -> 'a Cond.t
  (** Extract resp. the environment and the condition environment *)
val to_expr0 : 'a t -> 'a Expr0.t
val to_expr1 : 'a t -> 'a Expr1.t
  (** Extract the underlying expression of level 0 and 1 *)
val extend_environment : 'a t -> 'a Env.t -> 'a t
  (** Extend the underlying environment to a superenvironment, and
      adapt accordingly the underlying representation *)

val print : Format.formatter -> 'a t -> unit

module Bool : sig
  type 'a t = ('a Cond.t, 'a Expr1.Bool.t) Bdd.Cond.value

  val of_expr0 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'a Env.t -> 'a Cond.t -> 'a Expr0.Bool.t -> 'a t
    (** Creation from an expression of level 0 (without
	environment) *)
  val of_expr1 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'a Cond.t -> 'a Expr1.Bool.t -> 'a t
    (** Creation from an expression of level 1 (without condition
	environment) *)
  val get_env : 'a t -> 'a Env.t
  val get_cond : 'a t -> 'a Cond.t
    (** Extract resp. the environment and the condition environment *)
  val to_expr0 : 'a t -> 'a Expr0.Bool.t
  val to_expr1 : 'a t -> 'a Expr1.Bool.t
    (** Extract the underlying expression of level 0 and 1 *)

  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr
    (** Conversion from/to general expression *)
  val extend_environment : 'a t -> 'a Env.t -> 'a t
    (** Extend the underlying environment to a superenvironment,
	and adapt accordingly the underlying representation *)

  val is_false : 'a t -> bool
  val is_true : 'a t -> bool

  val print : Format.formatter -> 'a t -> unit
end

module List : sig
  type 'a t = ('a Cond.t, 'a Expr1.List.t) Bdd.Cond.value

  val of_lexpr0 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'a Env.t -> 'a Cond.t -> 'a Expr0.t list -> 'a t
    (** Creation from a list of expressions of level 0 (without
	environment) *)
  val of_lexpr1 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'a Env.t -> 'a Cond.t -> 'a Expr1.t list -> 'a t
    (** Creation from a list of expressions of level 1 (without
	condition environment) *)
  val of_listexpr1 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'a Cond.t -> 'a Expr1.List.t -> 'a t
    (** Creation from an expression list of level 1 (without condition
	environment) *)
  val get_env : 'a t -> 'a Env.t
  val get_cond : 'a t -> 'a Cond.t
     (** Extract resp. the environment and the condition environment *)
  val to_lexpr0 : 'a t -> 'a Expr0.t list
  val to_listexpr1 : 'a t -> 'a Expr1.List.t
  val to_lexpr1 : 'a t -> 'a Expr1.t list
     (** Extract the underlying list of expressions of level 0 and 1 *)
  val extend_environment : 'a t -> 'a Env.t -> 'a t
  val print : Format.formatter -> 'a t -> unit
end
