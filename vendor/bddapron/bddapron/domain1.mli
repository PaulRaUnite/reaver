(** Boolean/Numerical domain with normalized environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 Generic interface} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {4 Types} *)
(*  ====================================================================== *)

type ('a,'b,'c,'d) man = ('a,'b,'c,'d) Domain0.man
(** Type of generic managers.

    - ['a]: type of symbols
    - ['b]: as in ['b Apron.Manager.t]
	    ([Box.t], [Polka.strict Polka.t], etc);
    - ['c]: type of the underlying manager;
    - ['d]: type of the underlying abstract values of level 0.
*)
type ('a,'b) mtbdd =
  (
    'a,
    'b,
    ('a,'b) Mtbdddomain0.man,
    'b Mtbdddomain0.t
  ) man
type ('a,'b) bdd =
  (
    'a,
    'b,
    ('a,'b) Bdddomain0.man,
    'b Bdddomain0.t
  ) man

type ('a,'d) t = ('a Env.t, 'd) Env.value
(** Type of generic abstract values *)

val canonicalize : ?apron:bool -> ('a,'b,'c,'d) man -> ('a,'d) t -> unit
val print :
  ?print_apron:(
    (int -> string) ->
      Format.formatter -> 'b Apron.Abstract0.t -> unit
  ) ->
  ('a,'b,'c,'d) man -> Format.formatter -> ('a,'d) t -> unit

val get_env : ('a,'d) t -> 'a Env.t
val to_level0 :  ('a,'d) t -> 'd
val of_level0 : 'a Env.t -> 'd -> ('a,'d) t

val size : ('a,'b,'c,'d) man -> ('a,'d) t -> int
val bottom : ('a,'b,'c,'d) man -> 'a Env.t -> ('a,'d) t
val top : ('a,'b,'c,'d) man -> 'a Env.t -> ('a,'d) t
val of_apron : ('a,'b,'c,'d) man -> 'a Env.t -> 'b Apron.Abstract1.t -> ('a,'d) t
val of_bddapron : ('a,'b,'c,'d) man -> 'a Env.t -> ('a Expr1.Bool.t * 'b Apron.Abstract1.t) list -> ('a,'d) t
val is_bottom : ('a,'b,'c,'d) man -> ('a,'d) t -> bool
val is_top : ('a,'b,'c,'d) man -> ('a,'d) t -> bool
val is_leq : ('a,'b,'c,'d) man -> ('a,'d) t -> ('a,'d) t -> bool
val is_eq : ('a,'b,'c,'d) man -> ('a,'d) t -> ('a,'d) t -> bool
val to_bddapron :
  ('a,'b,'c,'d) man -> ('a,'d) t -> ('a Expr1.Bool.t * 'b Apron.Abstract1.t) list
val meet : ('a,'b,'c,'d) man -> ('a,'d) t -> ('a,'d) t -> ('a,'d) t
val join : ('a,'b,'c,'d) man -> ('a,'d) t -> ('a,'d) t -> ('a,'d) t
val meet_condition :
  ('a,'b,'c,'d) man -> 'a Cond.t ->
  ('a,'d) t -> 'a Expr1.Bool.t -> ('a,'d) t
val meet_condition2 :
  ('a,'b,'c,'d) man ->
  ('a,'d) t -> 'a Expr2.Bool.t -> ('a,'d) t
val assign_lexpr :
  ?relational:bool -> ?nodependency:bool ->
  ('a,'b,'c,'d) man -> 'a Cond.t ->
  ('a,'d) t -> 'a list -> 'a Expr1.t list -> ('a,'d) t option -> ('a,'d) t
val assign_listexpr2 :
  ?relational:bool ->  ?nodependency:bool ->
  ('a,'b,'c,'d) man ->
  ('a,'d) t -> 'a list -> 'a Expr2.List.t -> ('a,'d) t option -> ('a,'d) t
val substitute_lexpr :
  ('a,'b,'c,'d) man -> 'a Cond.t ->
  ('a,'d) t -> 'a list -> 'a Expr1.t list -> ('a,'d) t option -> ('a,'d) t
val substitute_listexpr2 :
  ('a,'b,'c,'d) man ->
  ('a,'d) t -> 'a list -> 'a Expr2.List.t -> ('a,'d) t option -> ('a,'d) t
val forget_list : ('a,'b,'c,'d) man -> ('a,'d) t -> 'a list -> ('a,'d) t
val widening : ('a,'b,'c,'d) man -> ('a,'d) t -> ('a,'d) t -> ('a,'d) t
val widening_threshold : ('a,'b,'c,'d) man -> ('a,'d) t -> ('a,'d) t -> Apron.Lincons1.earray -> ('a,'d) t
val change_environment :
  ('a,'b,'c,'d) man -> ('a,'d) t -> 'a Env.t -> ('a,'d) t
val unify : ('a,'b,'c,'d) man -> ('a,'d) t -> ('a,'d) t -> ('a,'d) t
val rename : ('a,'b,'c,'d) man -> ('a,'d) t -> ('a * 'a) list -> ('a,'d) t

val man_get_apron : ('a,'b,'c,'d) man -> 'b Apron.Manager.t

(*  ********************************************************************** *)
(** {3 Implementation based on {!Mtbdddomain0}} *)
(*  ********************************************************************** *)

val mtbdd_of_mtbdddomain : ('a,'b) Mtbdddomain0.man -> ('a,'b) mtbdd
  (** Make a mtbdd manager from an underlying BDDAPRON manager *)
val make_mtbdd : ?global:bool -> 'b Apron.Manager.t -> ('a,'b) mtbdd
  (** Make a mtbdd manager from an APRON manager *)

(*  ====================================================================== *)
(** {4 Type conversion functions} *)
(*  ====================================================================== *)

val man_is_mtbdd : ('a,'b,'c,'d) man -> bool
  (** Return [true] iff the argument manager is a mtbdd manager *)
val man_of_mtbdd : ('a,'b) mtbdd -> ('a,'b,'c,'d) man
  (** Makes a mtbdd manager generic *)
val man_to_mtbdd : ('a,'b,'c,'d) man -> ('a,'b) mtbdd
  (** Instanciate the type of a mtbdd manager.
      Raises [Failure] if the argument manager is not a mtbdd manager *)

val of_mtbdd :
  ('a,'b) mtbdd * ('a,'b Mtbdddomain0.t) t ->
  ('a,'b,'c,'d) man * ('a,'d) t
  (** Makes a pair (mtbdd manager,mtbdd abstract value) generic *)
val to_mtbdd :
  ('a,'b,'c,'d) man * ('a,'d) t ->
  ('a,'b) mtbdd * ('a, 'b Mtbdddomain0.t) t
  (** Instanciate the type of a pair (mtbdd manager,mtbdd abstract value).
      Raises [Failure] if the argument manager is not a mtbdd manager *)

(*  ********************************************************************** *)
(** {3 Implementation based on {!Bdddomain0}} *)
(*  ********************************************************************** *)

val bdd_of_bdddomain : ('a,'b) Bdddomain0.man -> ('a,'b) bdd
  (** Make a bdd manager from an underlying BDDAPRON manager *)
val make_bdd : 'b Apron.Manager.t -> ('a,'b) bdd
  (** Make a bdd manager from an APRON manager *)

(*  ====================================================================== *)
(** {4 Type conversion functions} *)
(*  ====================================================================== *)

val man_is_bdd : ('a,'b,'c,'d) man -> bool
  (** Return [true] iff the argument manager is a bdd manager *)
val man_of_bdd : ('a,'b) bdd -> ('a,'b,'c,'d) man
  (** Makes a bdd manager generic *)
val man_to_bdd : ('a,'b,'c,'d) man -> ('a,'b) bdd
  (** Instanciate the type of a bdd manager.
      Raises [Failure] if the argument manager is not a bdd manager *)

val of_bdd :
  ('a,'b) bdd * ('a,'b Bdddomain0.t) t ->
  ('a,'b,'c,'d) man * ('a,'d) t
  (** Makes a pair (bdd manager,bdd abstract value) generic *)
val to_bdd :
  ('a,'b,'c,'d) man * ('a,'d) t ->
  ('a,'b) bdd * ('a,'b Bdddomain0.t) t
  (** Instanciate the type of a pair (bdd manager,bdd abstract value).
      Raises [Failure] if the argument manager is not a bdd manager *)
