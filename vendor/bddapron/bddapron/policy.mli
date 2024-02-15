(** Policies for BDDAPRON abstract values *)

(* This file is part of the APRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(** This module provides a generic BddApron lift for policies
    on Apron abstract values.
    Policies are available only for MTBDD abstract values
    ([Domain0.t] built on [Mtbdddomain0.t]). *)

val apron_policy_print :
  'a Apron.Policy.man -> 'b Env.t ->
  Format.formatter -> 'a Apron.Policy.t -> unit

module Dnf : sig
  type 'a t = 'a Cond.cond Bdd.Normalform.dnf
      (** Disjunction of conjuction of conditions.
	  [(c1 and c2) or c3] is represented by [[|[c1,c2],[c3]|]].
	  Unicity of the representation is guaranted by a lexicographic
	  ordering based on the order on conditions. *)
end
module DDDnf : sig
  type 'a t = 'a Dnf.t Cudd.Mtbdd.t
  type 'a table = 'a Dnf.t Cudd.Mtbdd.table
end
module DPolicy : sig
  type 'a t = {
    hash : int;
    dpolicy : 'a Apron.Policy.t Bdd.Normalform.disjunction
  }
  type 'a table = 'a t Cudd.Mtbdd.table
end
module PMtbdddomain0 : sig
  type ('a, 'b) man = {
    man : ('a, 'b) Mtbdddomain0.man;
    papron : 'b Apron.Policy.man;
    ptable : 'b DPolicy.table;
    betable : 'a DDDnf.table;
    symbol : 'a Env.symbol;
  }
  type 'a t = 'a DPolicy.t Cudd.Mtbdd.t
end
module PDomain0 : sig
  type ('a, 'b, 'c, 'd, 'e, 'f) man = {
    man : ('a,'b,'c,'d) Domain0.man;
    pman : 'e;
    print : 'e -> 'a Env.t -> 'a Cond.t -> Format.formatter -> 'f -> unit;
    meet_condition_apply :
      'e -> 'a Env.t -> 'a Cond.t -> 'f -> 'd -> 'a Expr0.Bool.t -> 'd;
    meet_condition_improve :
      'e -> 'a Env.t -> 'a Cond.t -> 'f option -> 'd -> 'a Expr0.Bool.t -> 'f;
  }
end

(* ********************************************************************** *)
(** {3 Policy, level 1} *)
(* ********************************************************************** *)

module Domain1 : sig
  type ('a, 'b, 'c, 'd, 'e, 'f) man = ('a, 'b, 'c, 'd, 'e, 'f) PDomain0.man = {
    man : ('a,'b,'c,'d) Domain0.man;
    pman : 'e;
    print : 'e -> 'a Env.t -> 'a Cond.t -> Format.formatter -> 'f -> unit;
    meet_condition_apply :
      'e -> 'a Env.t -> 'a Cond.t -> 'f -> 'd -> 'a Expr0.Bool.t -> 'd;
    meet_condition_improve :
      'e -> 'a Env.t -> 'a Cond.t -> 'f option -> 'd -> 'a Expr0.Bool.t -> 'f;
  }
      (** Type of generic policy managers.

	  - ['a]: type of symbols
	  - ['b]: as in ['b Apron.Manager.t] ([Box.t], [Polka.strict Polka.t], etc);
	  - ['c]: type of the underlying manager;
	  - ['d]: type of the underlying abstract values of level 0.
	  - ['e]: type of the underlying policy manager
	  - ['f]: type of the underlying policy
      *)

  type ('a, 'b) mtbdd = (
    'a,
    'b,
    ('a, 'b) Mtbdddomain0.man,
    'b Mtbdddomain0.t,
    ('a, 'b) PMtbdddomain0.man,
    'b PMtbdddomain0.t
  ) man

  val manager_get_manager : ('a,'b,'c,'d,'e,'f) man -> ('a,'b,'c,'d) Domain0.man
  val print : ('a, 'b, 'c, 'd, 'e, 'f) man -> 'a Env.t -> 'a Cond.t -> Format.formatter -> 'f -> unit
  val meet_condition_apply :
    ('a, 'b, 'c, 'd, 'e, 'f) man -> 'a Cond.t ->
    'f -> ('a, 'd) Domain1.t -> 'f Expr1.Bool.t -> ('a, 'd) Domain1.t
  val meet_condition_improve :
    ('a, 'b, 'c, 'd, 'e, 'f) man -> 'a Cond.t ->
    'f option -> ('a, 'd) Domain1.t -> 'f Expr1.Bool.t -> 'f
  val meet_condition2_apply :
    ('a, 'b, 'c, 'd, 'e, 'f) man ->
    'f -> ('a, 'd) Domain1.t -> 'a Expr2.Bool.t -> ('a, 'd) Domain1.t
  val meet_condition2_improve :
    ('a, 'b, 'c, 'd, 'e, 'f) man ->
    'f option -> ('a, 'd) Domain1.t -> 'a Expr2.Bool.t -> 'f
end

(* ********************************************************************** *)
(** {3 Policy, level 0} *)
(* ********************************************************************** *)

module Domain0 : sig
  type ('a, 'b, 'c, 'd, 'e, 'f) man = ('a, 'b, 'c, 'd, 'e, 'f) PDomain0.man = {
    man : ('a,'b,'c,'d) Domain0.man;
    pman : 'e;
    print : 'e -> 'a Env.t -> 'a Cond.t -> Format.formatter -> 'f -> unit;
    meet_condition_apply :
      'e -> 'a Env.t -> 'a Cond.t -> 'f -> 'd -> 'a Expr0.Bool.t -> 'd;
    meet_condition_improve :
      'e -> 'a Env.t -> 'a Cond.t -> 'f option -> 'd -> 'a Expr0.Bool.t -> 'f;
  }
      (** Type of generic policy managers.

	  - ['a]: type of symbols
	  - ['b]: as in ['b Apron.Manager.t] ([Box.t], [Polka.strict Polka.t], etc);
	  - ['c]: type of the underlying manager;
	  - ['d]: type of the underlying abstract values of level 0.
	  - ['e]: type of the underlying policy manager
	  - ['f]: type of the underlying policy
      *)
  type ('a, 'b) mtbdd = (
    'a,
    'b,
    ('a, 'b) Mtbdddomain0.man,
    'b Mtbdddomain0.t,
    ('a, 'b) PMtbdddomain0.man,
    'b PMtbdddomain0.t
  ) man

  val manager_get_manager : ('a,'b,'c,'d,'e,'f) man -> ('a,'b,'c,'d) Domain0.man
  val print : ('a, 'b, 'c, 'd, 'e, 'f) man -> 'a Env.t -> 'a Cond.t -> Format.formatter -> 'f -> unit
  val meet_condition_apply :
    ('a, 'b, 'c, 'd, 'e, 'f) man ->
    'a Env.t -> 'a Cond.t -> 'f -> 'd -> 'a Expr0.Bool.t -> 'd
  val meet_condition_improve :
    ('a, 'b, 'c, 'd, 'e, 'f) man ->
    'a Env.t -> 'a Cond.t -> 'f option -> 'd -> 'a Expr0.Bool.t -> 'f
  val make_mtbdd :
    ?global:bool ->
    symbol:'a Env.symbol -> 'b Apron.Policy.man -> ('a, 'b) mtbdd
  end

(* ********************************************************************** *)
(** {3 Policy, level 0, MTBDD implementation} *)
(* ********************************************************************** *)

module Mtbdddomain0 : sig
  type ('a, 'b) man = ('a,'b) PMtbdddomain0.man = {
    man : ('a, 'b) Mtbdddomain0.man;
    papron : 'b Apron.Policy.man;
    ptable : 'b DPolicy.table;
    betable : 'a DDDnf.table;
    symbol : 'a Env.symbol;
  }
  type 'a t = 'a PMtbdddomain0.t

  val manager_get_manager : ('a,'b) man -> ('a, 'b) Mtbdddomain0.man
  val make_man :
    ?global:bool ->
    symbol:'a Env.symbol -> 'b Apron.Policy.man -> ('a, 'b) man
  val equal : 'a -> 'b t -> 'b t -> bool
  val print :
    ('a, 'b) man -> 'c Env.t -> 'c Cond.t ->
    Format.formatter -> 'b t -> unit
  val meet_condition_apply :
    ('a, 'b) man -> 'a Env.t -> 'a Cond.t ->
    'b t -> 'b Mtbdddomain0.t -> 'a Expr0.Bool.t -> 'b Mtbdddomain0.t
  val meet_condition_improve :
    ('a, 'b) man -> 'a Env.t -> 'a Cond.t ->
    'b t option -> 'b Mtbdddomain0.t -> 'a Expr0.Bool.t -> 'b t
end
