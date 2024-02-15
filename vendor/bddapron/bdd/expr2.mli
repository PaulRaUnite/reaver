(** Finite-type expressions linked to normalized environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 General expressions} *)
(*  ********************************************************************** *)

type 'a t = ('a Env.t, 'a Expr0.t) Env.value
type 'a expr = 'a t
  (** Type of general expressions *)

(*  ********************************************************************** *)
(** {3 Boolean expressions} *)
(*  ********************************************************************** *)

module Bool : sig
  type 'a t = ('a Env.t, 'a Cudd.Bdd.t) Env.value

  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr

  val extend_environment : 'a t -> 'a Env.t -> 'a t

  val dtrue : 'a Env.t -> 'a t
  val dfalse : 'a Env.t -> 'a t
  val of_bool : 'a Env.t -> bool -> 'a t
  val var : 'a Env.t -> string -> 'a t

  (** {5 Logical connectors} *)

  val dnot : 'a t -> 'a t
  val dand : 'a t -> 'a t -> 'a t
  val dor : 'a t -> 'a t -> 'a t
    (** [not], [and] and [or] (use of 'd' prefix because of conflict with OCaml
	keywords) *)

  val xor : 'a t -> 'a t -> 'a t
  val nand : 'a t -> 'a t -> 'a t
  val nor : 'a t -> 'a t -> 'a t
  val nxor : 'a t -> 'a t -> 'a t
    (** Exclusive or, not and, nor or and not xor *)

  val eq : 'a t -> 'a t -> 'a t
    (** Same as [nxor] *)
  val leq : 'a t -> 'a t -> 'a t
    (** Implication *)

  val ite : 'a t -> 'a t -> 'a t -> 'a t
    (** If-then-else *)

  val is_true : 'a t -> bool
  val is_false : 'a t -> bool
  val is_cst : 'a t -> bool
  val is_eq : 'a t -> 'a t -> bool
  val is_leq : 'a t -> 'a t -> bool
  val is_inter_false : 'a t -> 'a t -> bool

  val exist : string list -> 'a t -> 'a t
  val forall : string list -> 'a t -> 'a t

  val cofactor : 'a t -> 'a t -> 'a t
  val restrict : 'a t -> 'a t -> 'a t
  val tdrestrict : 'a t -> 'a t -> 'a t

  val substitute_by_var : 'a t -> (string * string) list -> 'a t
  val substitute : 'a t -> (string * 'a expr) list -> 'a t

  val print : Format.formatter -> 'a t -> unit
end

(*  ********************************************************************** *)
(** {3 Bounded integer expressions} *)
(*  ********************************************************************** *)

module Bint : sig
  type 'a t = ('a Env.t, 'a Int.t) Env.value

  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr
  val extend_environment : 'a t -> 'a Env.t -> 'a t

  val of_int :
    'a Env.t -> [`Tbint of bool * int ] -> int -> 'a t
  val var : 'a Env.t -> string -> 'a t

  val neg : 'a t -> 'a t
  val succ : 'a t -> 'a t
  val pred : 'a t -> 'a t
  val add : 'a t -> 'a t -> 'a t
  val sub : 'a t -> 'a t -> 'a t
  val mul : 'a t -> 'a t -> 'a t
  val shift_left : int -> 'a t -> 'a t
  val shift_right : int -> 'a t -> 'a t
  val scale : int -> 'a t -> 'a t
  val ite : 'a Bool.t -> 'a t -> 'a t -> 'a t
  val zero : 'a t -> 'a Bool.t
  val eq : 'a t -> 'a t -> 'a Bool.t
  val supeq : 'a t -> 'a t -> 'a Bool.t
  val sup : 'a t -> 'a t -> 'a Bool.t
  val eq_int : 'a t -> int -> 'a Bool.t
  val supeq_int : 'a t -> int -> 'a Bool.t

  val cofactor : 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t

  val substitute_by_var : 'a t -> (string * string) list -> 'a t
  val substitute : 'a t -> (string * 'a expr) list -> 'a t

  val guard_of_int: 'a t -> int -> 'a Bool.t
    (** Return the guard of the integer value. *)
  val guardints: 'a t -> ('a Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

  val print : Format.formatter -> 'a t -> unit
end

(*  ********************************************************************** *)
(** {3 Enumerated expressions} *)
(*  ********************************************************************** *)

module Benum : sig
  type 'a t = ('a Env.t, 'a Enum.t) Env.value

  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr
  val extend_environment : 'a t -> 'a Env.t -> 'a t

  val var : 'a Env.t -> string -> 'a t
  val ite : 'a Bool.t -> 'a t -> 'a t -> 'a t
  val eq : 'a t -> 'a t -> 'a Bool.t
  val eq_label : 'a t -> string -> 'a Bool.t

  val cofactor : 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t

  val substitute_by_var : 'a t -> (string * string) list -> 'a t
  val substitute : 'a t -> (string * 'a expr) list -> 'a t

  val guard_of_label : 'a t -> string -> 'a Bool.t
    (** Return the guard of the label. *)

  val guardlabels : 'a t -> ('a Bool.t * string) list
    (** Return the list [g -> label] of guarded values. *)

  val print : Format.formatter -> 'a t -> unit
end


val typ_of_expr : 'a expr -> [
  | `Bool
  | `Bint of bool * int
  | `Benum of string
]
  (** Type of an expression *)

val make : 'a Env.t -> 'a Expr0.t -> 'a expr
  (** Creation from an expression without environment *)

val extend_environment : 'a expr -> 'a Env.t -> 'a expr

val var : 'a Env.t -> string -> 'a expr
  (** Expression representing the litteral var *)
val ite : 'a Bool.t -> 'a expr -> 'a expr -> 'a expr
  (** If-then-else operation *)
val eq : 'a expr -> 'a expr -> 'a Bool.t
  (** Equality operation *)

val substitute_by_var : 'a expr -> (string * string) list -> 'a expr
    (** Variable renaming.
	The new variables should already have been declared *)
val substitute : 'a expr -> (string * 'a expr) list -> 'a expr
    (** Parallel substitution of variables by expressions *)

val support : 'a expr -> string PSette.t
    (** Support of the expression *)

val support_cond : 'a expr -> 'a Cudd.Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
	identifiers involved in the expression *)

val cofactor : 'a expr -> 'a Bool.t -> 'a expr
    (** Evaluate the expression. The BDD is assumed to be a cube *)

val restrict : 'a expr -> 'a Bool.t -> 'a expr
val tdrestrict : 'a expr -> 'a Bool.t -> 'a expr
    (** Simplify the expression knowing that the BDD is true.  Generalizes
	[cofactor]. *)

val print : Format.formatter -> 'a expr -> unit

(*  ********************************************************************** *)
(** {3 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
    types (this allows etxensions). The functions above are axtually derived from
    the functions below by just constraining their types.  We provide here also
    more internal functions *)

module O : sig

  (*  ==================================================================== *)
  (** {4 Expressions} *)
  (*  ==================================================================== *)

  type ('a,'b) t = ('a, 'b Expr0.t) Env.value
  constraint 'a = ('c,'d,'b) #Env.O.t
  type ('a,'b) expr = ('a,'b) t
(** Type of general expressions *)

  (*  -------------------------------------------------------------------- *)
  (** {5 Boolean expressions} *)
  (*  -------------------------------------------------------------------- *)

  module Bool : sig
    type ('a,'b) t = ('a, 'b Cudd.Bdd.t) Env.value
    constraint 'a = ('c,'d,'b) #Env.O.t

    val of_expr : ('a, [> `Bool of 'b Cudd.Bdd.t ]) Env.value -> ('a,'b) t
    val to_expr : ('a,'b) t -> ('a, [> `Bool of 'b Cudd.Bdd.t ]) Env.value

    val extend_environment : ('a,'b) t -> 'a -> ('a,'b) t

    val dtrue : 'a -> ('a,'b) t
    val dfalse : 'a -> ('a,'b) t
    val of_bool : 'a -> bool -> ('a,'b) t
    val var : 'a -> string -> ('a,'b) t

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

    val exist : string list -> ('a,'b) t -> ('a,'b) t
    val forall : string list -> ('a,'b) t -> ('a,'b) t

    val cofactor : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val restrict : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val tdrestrict : ('a,'b) t -> ('a,'b) t -> ('a,'b) t

    val substitute_by_var : ('a,'b) t -> (string * string) list -> ('a,'b) t
    val substitute : ('a,'b) t -> (string * ('a,'b) expr) list -> ('a,'b) t

    val print : Format.formatter -> ('a,'b) t -> unit
  end

  (*  -------------------------------------------------------------------- *)
  (** {5 Bounded integer expressions} *)
  (*  -------------------------------------------------------------------- *)

  module Bint : sig
    type ('a,'b) t = ('a, 'b Int.t) Env.value
    constraint 'a = ('c,'d,'b) #Env.O.t

    val of_expr : ('a, [> `Bint of 'b Int.t ]) Env.value -> ('a,'b) t
    val to_expr : ('a,'b) t -> ('a, [> `Bint of 'b Int.t ]) Env.value
    val extend_environment : ('a,'b) t -> 'a -> ('a,'b) t

    val of_int :
      'a -> [`Tbint of bool * int ] -> int -> ('a,'b) t
    val var : 'a -> string -> ('a,'b) t

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

    val cofactor : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    val restrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    val tdrestrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t

    val substitute_by_var : ('a,'b) t -> (string * string) list -> ('a,'b) t
    val substitute : ('a,'b) t -> (string * ('a,'b) expr) list -> ('a,'b) t

    val guard_of_int: ('a,'b) t -> int -> ('a,'b) Bool.t
    (** Return the guard of the integer value. *)
    val guardints: ('a,'b) t -> (('a,'b) Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

    val print : Format.formatter -> ('a,'b) t -> unit
  end

  (*  -------------------------------------------------------------------- *)
  (** {5 Enumerated expressions} *)
  (*  -------------------------------------------------------------------- *)

  module Benum : sig
    type ('a,'b) t = ('a, 'b Enum.t) Env.value
    constraint 'a = ('c,'d,'b) #Env.O.t

    val of_expr : ('a, [> `Benum of 'b Enum.t ]) Env.value -> ('a,'b) t
    val to_expr : ('a,'b) t -> ('a, [> `Benum of 'b Enum.t ]) Env.value
    val extend_environment : ('a,'b) t -> 'a -> ('a,'b) t

    val var : 'a -> string -> ('a,'b) t
    val ite : ('a,'b) Bool.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    val eq : ('a,'b) t -> ('a,'b) t -> ('a,'b) Bool.t
    val eq_label : ('a,'b) t -> string -> ('a,'b) Bool.t

    val cofactor : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    val restrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t
    val tdrestrict : ('a,'b) t -> ('a,'b) Bool.t -> ('a,'b) t

    val substitute_by_var : ('a,'b) t -> (string * string) list -> ('a,'b) t
    val substitute : ('a,'b) t -> (string * ('a,'b) expr) list -> ('a,'b) t

    val guard_of_label : ('a,'b) t -> string -> ('a,'b) Bool.t
    (** Return the guard of the label. *)

    val guardlabels : ('a,'b) t -> (('a,'b) Bool.t * string) list
    (** Return the list [g -> label] of guarded values. *)

    val print : Format.formatter -> ('a,'b) t -> unit
  end

  val typ_of_expr : ('a,'b) t -> [
    | `Bool
    | `Bint of bool * int
    | `Benum of string
  ]
  (** Type of an expression *)

  val make : (('a,'b,'d) #Env.O.t as 'e) -> 'b Expr0.t -> ('e,'b) expr
  (** Creation from an expression without environment *)

  val extend_environment : ('a,'b) t -> 'a -> ('a,'b) t

  val var : 'a -> string -> ('a,'b) t
  (** Expression representing the litteral var *)
  val ite : ('a,'b) Bool.t -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  (** If-then-else operation *)
  val eq : ('a,'b) t -> ('a,'b) t -> ('a,'b) Bool.t
  (** Equality operation *)

  val substitute_by_var : ('a,'b) t -> (string * string) list -> ('a,'b) t
    (** Variable renaming.
	The new variables should already have been declared *)
  val substitute : ('a,'b) t -> (string * ('a,'b) t) list -> ('a,'b) t
    (** Parallel substitution of variables by expressions *)

  val support : ('a,'b) t -> string PSette.t
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

  (*  ==================================================================== *)
  (** {4 Internal} *)
  (*  ==================================================================== *)

  val mapunop : ('a -> 'b) -> ('c, 'a) Env.value -> ('c, 'b) Env.value
  val normalize :
    ('a -> int array -> 'a) ->
    (('b, 'c,'e) #Env.O.t, 'a) Env.value ->
    ('b, 'c,'e) #Env.O.t -> 'a

  val check_envvar : ('a,'b,'d) #Env.O.t -> string -> unit
  val check_lvar : (('a,'b,'d) #Env.O.t, 'd) Env.value -> string list -> unit

  val check_value2 :
    ('a -> int array -> 'a) ->
    ('b -> int array -> 'b) ->
    (('c,'d,'f) #Env.O.t as 'g, 'a) Env.value -> ('g, 'b) Env.value ->
    'g * 'a * 'b
  val mapbinop :
    ('a -> int array -> 'a) ->
    ('b -> int array -> 'b) ->
    ('a -> 'b -> 'c) ->
    (('d,'e,'g) #Env.O.t as 'h, 'a) Env.value -> ('h, 'b) Env.value ->
    ('h, 'c) Env.value
  val mapbinope :
    ('a -> int array -> 'a) ->
    ('b -> int array -> 'b) ->
    ('d -> 'a -> 'b -> 'c) ->
    (('e,'f,'h) #Env.O.t as 'd, 'a) Env.value -> ('d, 'b) Env.value ->
    ('d, 'c) Env.value
  val check_value3 :
    ('a -> int array -> 'a) ->
    ('b -> int array -> 'b) ->
    ('c -> int array -> 'c) ->
    (('d,'e,'g) #Env.O.t as 'h, 'a) Env.value ->
    ('h, 'b) Env.value -> ('h, 'c) Env.value ->
    'h * 'a * 'b * 'c
  val mapterop :
    ('a -> int array -> 'a) ->
    ('b -> int array -> 'b) ->
    ('c -> int array -> 'c) ->
    ('a -> 'b -> 'c -> 'd) ->
    (('e,'f,'h) #Env.O.t as 'i, 'a) Env.value ->
    ('i, 'b) Env.value -> ('i, 'c) Env.value ->
    ('i, 'd) Env.value

  val check_lvarvalue :
    ('a -> int array -> 'a) ->
    ('b -> int array -> 'b) ->
    (('c,'d,'f) #Env.O.t as 'g, 'a) Env.value ->
    ('h * ('g, 'b) Env.value) list ->
    'g * 'a * ('h * 'b) list
end
