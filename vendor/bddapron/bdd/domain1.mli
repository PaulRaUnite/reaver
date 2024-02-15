(** Boolean (abstract) domain with normalized environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 Abstract domain} *)
(*  ********************************************************************** *)

type ('a,'b) t = ('a,'b) Expr1.Bool.t
  (** Abstract value *)

type 'a dt = ('a,Cudd.Man.d) t
type 'a vt = ('a,Cudd.Man.v) t

val of_domain0 : ('a,'b) Env.t -> 'b Domain0.t -> ('a,'b) t
val get_env : ('a,'b) t -> ('a,'b) Env.t
val to_domain0 : ('a,'b) t -> 'b Domain0.t
  (** Conversion operations *)

val of_expr1 : ('a,'b) Expr1.Bool.t -> ('a,'b) t
val to_expr1 : ('a,'b) t -> ('a,'b) Expr1.Bool.t
  (** Conversion operations *)

val size : ('a,'b) t -> int
  (** Size of an abstract value (number of nodes) *)
val print : Format.formatter -> ('a,'b) t -> unit

val bottom : ('a,'b) Env.t -> ('a,'b) t
val top : ('a,'b) Env.t -> ('a,'b) t
  (** Constructors *)

val is_bottom : ('a,'b) t -> bool
val is_top : ('a,'b) t -> bool
val is_leq : ('a,'b) t -> ('a,'b) t -> bool
val is_eq : ('a,'b) t -> ('a,'b) t -> bool
val is_variable_unconstrained : ('a,'b) t -> 'a -> bool
  (** Tests *)

val meet : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
val join : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
val meet_condition : ('a,'b) t -> ('a,'b) Expr1.Bool.t -> ('a,'b) t
  (** Lattice operations *)

val assign_lexpr : ?relational:bool -> ?nodependency:bool -> ('a,'b) t -> 'a list -> ('a,'b) Expr1.t list -> ('a,'b) t
val assign_listexpr : ?relational:bool -> ?nodependency:bool -> ('a,'b) t -> 'a list -> ('a,'b) Expr1.List.t -> ('a,'b) t
  (** Assignement

      If [nodependency=true], which means that no expression depends on the
      assigned variables, it uses an optimized algorithm.

      If [rel=true], it is assumed that [env#bddincr=2] (checked), starting from
      a pair index. It is also advised to have paired variables in groups.

      [rel=true] is most probably much better for assignements of a few
      variables.  *)
val substitute_lexpr : ('a,'b) t -> 'a list -> ('a,'b) Expr1.t list -> ('a,'b) t
val substitute_listexpr : ('a,'b) t -> 'a list -> ('a,'b) Expr1.List.t -> ('a,'b) t
  (** Substitution *)

val forget_list : ('a,'b) t -> 'a list -> ('a,'b) t
  (** Eliminating variables *)

val change_environment : ('a,'b) t -> ('a,'b) Env.t -> ('a,'b) t
val rename :('a,'b) t -> ('a*'a) list -> ('a,'b) t
  (** Change of environments *)

(*  ********************************************************************** *)
(** {3 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
    types (this allows etxensions). The functions above are axtually derived from
    the functions below by just constraining their types.  We provide here also
    more internal functions *)

module O : sig

  val check_value :
    ('a -> int array -> 'a) ->
    (('b, 'c, 'd, 'e, 'f) Env.O.t, 'a) Env.value ->
    ('b, 'c, 'd, 'e, 'f) Env.O.t -> 'a
  val check_lvalue :
    ('a -> int array -> 'a) ->
    (('b, 'c, 'd, 'e, 'f) Env.O.t, 'a) Env.value list ->
    ('b, 'c, 'd, 'e, 'f) Env.O.t -> 'a list

  type ('a,'b,'c) t = ('a,'b,'c) Expr1.O.Bool.t

  type ('a,'b) dt = ('a,'b,Cudd.Man.d) t
  type ('a,'b) vt = ('a,'b,Cudd.Man.v) t

  val of_domain0 : 'b -> 'c Domain0.t -> ('a,'b,'c) t
  val get_env : ('a,'b,'c) t -> 'b
  val to_domain0 : ('a,'b,'c) t -> 'c Domain0.t

  val of_expr1 : ('a,'b,'c) Expr1.O.Bool.t -> ('a,'b,'c) t
  val to_expr1 : ('a,'b,'c) t -> ('a,'b,'c) Expr1.O.Bool.t

  val size : ('a,'b,'c) t -> int
  val print : Format.formatter -> ('a,'b,'c) t -> unit

  val bottom : 'b -> ('a,'b,'c) t
  val top : 'b -> ('a,'b,'c) t
  (** Constructors *)

  val is_bottom : ('a,'b,'c) t -> bool
  val is_top : ('a,'b,'c) t -> bool
  val is_leq : ('a,'b,'c) t -> ('a,'b,'c) t -> bool
  val is_eq : ('a,'b,'c) t -> ('a,'b,'c) t -> bool
  val is_variable_unconstrained : ('a,'b,'c) t -> 'a -> bool
  (** Tests *)

  val meet : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
  val join : ('a,'b,'c) t -> ('a,'b,'c) t -> ('a,'b,'c) t
  val meet_condition : ('a,'b,'c) t -> ('a,'b,'c) Expr1.O.Bool.t -> ('a,'b,'c) t
  (** Lattice operations *)

  val assign_lexpr : ?relational:bool -> ?nodependency:bool -> ('a,'b,'c) t -> 'a list -> ('a,'b,'c) Expr1.O.t list -> ('a,'b,'c) t
  val assign_listexpr : ?relational:bool -> ?nodependency:bool -> ('a,'b,'c) t -> 'a list -> ('a,'b,'c) Expr1.O.List.t -> ('a,'b,'c) t
  (** Assignement

      If [rel=true], it is assumed that [env#bddincr=2] (checked), starting from
      a pair index. It is also advised to have paired variables in groups.

      [rel=true] is most probably much better for assignements of a few
      variables.  *)
  val substitute_lexpr : ('a,'b,'c) t -> 'a list -> ('a,'b,'c) Expr1.O.t list -> ('a,'b,'c) t
  val substitute_listexpr : ('a,'b,'c) t -> 'a list -> ('a,'b,'c) Expr1.O.List.t -> ('a,'b,'c) t
  (** Substitution *)

  val forget_list : ('a,'b,'c) t -> 'a list -> ('a,'b,'c) t
  (** Eliminating variables *)

  val change_environment : ('a,'b,'c) t -> 'b -> ('a,'b,'c) t
  val rename :('a,'b,'c) t -> ('a*'a) list -> ('a,'b,'c) t
    (** Change of environments *)

end
