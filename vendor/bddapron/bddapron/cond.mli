(** Normalized condition environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 Types} *)
(*  ********************************************************************** *)

type 'a cond = [`Apron of 'a Apronexpr.Condition.t]
  (** Conditions *)

val print_cond : ('a,'b,'c,'d) Env.O.t -> Format.formatter -> [< 'a cond ] -> unit
val compare_cond : 'a Bdd.Env.symbol -> [< 'a cond] -> [< 'a cond] -> int
val negate_cond : ('a,'b,'c,'d) Env.O.t -> 'a cond -> 'a cond
val support_cond : ('a,'b,'c,'d) Env.O.t -> [< 'a cond] -> 'a PSette.t
module O : sig
  type ('a,'b) t = ('a,'b, 'a cond, Cudd.Man.v) Bdd.Cond.t
  constraint 'b = ('a,'c,'d,'e) Env.O.t

  val make :
    symbol:'a Bdd.Env.symbol ->
    ?bddindex0:int ->
    ?bddsize:int ->
    ?bddmax:int ->
    Cudd.Man.vt -> ('a,'b) t
end

type 'a t = ('a, 'a Env.t) O.t

val make :
  symbol:'a Bdd.Env.symbol ->
  ?bddindex0:int ->
  ?bddsize:int ->
  ?bddmax:int ->
  Cudd.Man.vt -> 'a t

val copy : 'a t -> 'a t

val print : 'a Env.t -> Format.formatter -> 'a t -> unit

(*  ********************************************************************** *)
(** {3 Level 2} *)
(*  ********************************************************************** *)

type ('a,'b) value = ('a,'b) Bdd.Cond.value = {
  cond : 'a;
  val1 : 'b
}
val make_value : 'a -> 'b -> ('a,'b) value
val get_cond : ('a,'b) value -> 'a
val get_val1 : ('a,'b) value -> 'b
val get_env : ('a, ('b, 'c) Env.value) value -> 'b
val get_val0 : ('a, ('b, 'c) Env.value) value -> 'c
