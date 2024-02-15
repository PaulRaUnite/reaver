(** Boolean (abstract) domain *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 Abstract domain} *)
(*  ********************************************************************** *)

type 'a t = 'a Expr0.Bool.t
  (** Abstract value *)

type dt = Cudd.Man.d t
type vt = Cudd.Man.v t

val size : 'a t -> int
  (** Size of an abstract value (number of nodes) *)
val print :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b) Env.t -> Format.formatter -> 'b t -> unit

val bottom : ('a,'b) Env.t -> 'b t
val top : ('a,'b) Env.t -> 'b t
  (** Constructors *)

val is_bottom : ('a,'b) Env.t -> 'b t -> bool
val is_top : ('a,'b) Env.t -> 'b t -> bool
val is_leq : ('a,'b) Env.t -> 'b t -> 'b t -> bool
val is_eq : ('a,'b) Env.t -> 'b t -> 'b t -> bool
val is_variable_unconstrained : ('a,'b) Env.t -> 'b t -> 'a -> bool
  (** Tests *)

val meet : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t
val join : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t
val meet_condition : ('a,'b) Env.t -> 'b t -> 'b Expr0.Bool.t -> 'b t
  (** Lattice operations *)

val assign_lexpr :
  ?relational:bool ->
  ?nodependency:bool ->
  ('a,'b) Env.t ->
  'b t -> 'a list -> 'b Expr0.expr list -> 'b t
  (** Assignement

    If [nodependency=true], which means that no expression depends on the
    assigned variables, it uses an optimized algorithm.

    If [relational=true], it is assumed that [env#bddincr=2] (checked),
    starting from a pair index. It is also advised to have paired variables in
    groups.

    [rel=true] is most probably much better for assignements of a few
    variables. *)
val substitute_lexpr :
  ('a,'b) Env.t ->
  'b t -> 'a list -> 'b Expr0.expr list -> 'b t
  (** Substitution *)

val forget_list : ('a,'b) Env.t -> 'b t -> 'a list -> 'b t
  (** Eliminating variables *)

(*  ********************************************************************** *)
(** {3 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows etxensions). The functions above are axtually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig

  val print :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b,'c,'d,'e) Env.O.t -> Format.formatter -> 'd t -> unit

  val bottom :
    ('a,'b,'c,'d,'e) Env.O.t -> 'd t
  val top :
    ('a,'b,'c,'d,'e) Env.O.t -> 'd t
  (** Constructors *)

  val is_bottom : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> bool
  val is_top : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> bool
  val is_leq : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> bool
  val is_eq : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> bool
  val is_variable_unconstrained : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'a -> bool
  (** Tests *)

  val meet : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t
  val join : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t
  val meet_condition : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd Expr0.Bool.t -> 'd t
  (** Lattice operations *)

  val assign_lexpr :
    ?relational:bool ->
    ?nodependency:bool ->
    ('a,'b,'c,'d,'e) Env.O.t ->
    'd t -> 'a list -> 'd Expr0.expr list -> 'd t
  (** Assignement

      If [nodependency=true], which means that no expression depends on the
      assigned variables, it uses an optimized algorithm.

      If [rel=true], it is assumed that [env#bddincr=2] (checked), starting from
      a pair index. It is also advised to have paired variables in groups.

      [rel=true] is most probably much better for assignements of a few
      variables.  *)
  val substitute_lexpr :
    ('a,'b,'c,'d,'e) Env.O.t ->
    'd t -> 'a list -> 'd Expr0.expr list -> 'd t
  (** Substitution *)

  val forget_list : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'a list -> 'd t
  (** Eliminating variables *)

  module Asssub : sig
    val sort : int array -> 'a Cudd.Bdd.t array -> int array * 'a Cudd.Bdd.t array
    val is_equal : 'a Cudd.Bdd.t array -> 'a Cudd.Bdd.t array -> bool
    val post : 'a Cudd.Bdd.t -> int array -> 'a Cudd.Bdd.t array -> 'a Cudd.Bdd.t
    val postcondition : 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t array -> 'a Cudd.Bdd.t
  end
  val relation_supp_compose_of_lvarlexpr :
    ('a,'b,'c,'d,'e) Env.O.t ->
    'a list -> 'd Expr0.expr list ->
    'd Cudd.Bdd.t * 'd Cudd.Bdd.t * 'd Cudd.Bdd.t array

  val apply_change : 'a t -> 'a Env.change -> 'a t
end
