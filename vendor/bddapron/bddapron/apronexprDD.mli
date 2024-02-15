(** DDs on top of arithmetic expressions (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type 'a t = 'a Apronexpr.t Cudd.Mtbdd.t
val of_expr : [> `Apron of 'a t] -> 'a t
val to_expr : 'a t -> [> `Apron of 'a t]
val of_apronexpr :  ('a,'b,'c,'d) Env.O.t -> 'a Apronexpr.t -> 'a t

val print :
  (Format.formatter -> Cudd.Bdd.vt -> unit) ->
  'a Apronexpr.symbol ->
  Format.formatter -> 'a t -> unit
val is_zero : ('a,'b,'c,'d) Env.O.t -> 'a Apronexpr.t -> bool
val is_one : ('a,'b,'c,'d) Env.O.t -> 'a Apronexpr.t -> bool
val absorbant_zero :
  ('a,'b,'c,'d) Env.O.t ->
  'a Apronexpr.t Cudd.Mtbdd.unique -> 'a Apronexpr.t Cudd.Mtbdd.unique option
val absorbant_one :
  ('a,'b,'c,'d) Env.O.t ->
  'a Apronexpr.t Cudd.Mtbdd.unique -> 'a Apronexpr.t Cudd.Mtbdd.unique option
val cst : ('a,'b,'c,'d) Env.O.t -> Apron.Coeff.t -> 'a t
val var : ('a,'b,'c,'d) Env.O.t -> 'a -> 'a t
val add :
  ('a,'b,'c,'d) Env.O.t ->
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  'a t -> 'a t -> 'a t
val sub :
  ('a,'b,'c,'d) Env.O.t ->
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  'a t -> 'a t -> 'a t
val mul :
  ('a,'b,'c,'d) Env.O.t ->
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  'a t -> 'a t -> 'a t
val div :
  ('a,'b,'c,'d) Env.O.t ->
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  'a t -> 'a t -> 'a t
val gmod :
  ('a,'b,'c,'d) Env.O.t ->
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  'a t -> 'a t -> 'a t
val negate :   ('a,'b,'c,'d) Env.O.t -> 'a t -> 'a t
val cast :
  ('a,'b,'c,'d) Env.O.t ->
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  'a t -> 'a t
val sqrt :
  ('a,'b,'c,'d) Env.O.t ->
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  'a t -> 'a t
val support_leaf : ('a,'b,'c,'d) Env.O.t -> 'a t -> 'a PSette.t
val support_cond : ('a,'b,'c,'d) Env.O.t -> 'a t -> Cudd.Bdd.vt
val substitute_linexpr :
  ('a,'b,'c,'d) Env.O.t ->
  'a Apronexpr.Lin.t -> ('a, [> `Apron of 'a t ]) PMappe.t -> 'a t
val substitute_polyexpr :
  ('a,'b,'c,'d) Env.O.t ->
  'a Apronexpr.Poly.t -> ('a, [> `Apron of 'a t ]) PMappe.t -> 'a t
val substitute_treeexpr :
  ('a,'b,'c,'d) Env.O.t ->
  'a Apronexpr.Tree.t -> ('a, [> `Apron of 'a t ]) PMappe.t -> 'a t
val substitute :
  ('a,'b,'c,'d) Env.O.t ->
  'a Apronexpr.t -> ('a, [> `Apron of 'a t ]) PMappe.t -> 'a t
module Condition :  sig
  val of_apronexpr :
    'b -> ('a,'b) Cond.O.t -> 'a Apronexpr.Condition.t -> Cudd.Bdd.vt
  val of_condition :
    'b -> ('a,'b) Cond.O.t ->
    [< `Bool of bool | `Cond of 'a Apronexpr.Condition.t] ->
    Cudd.Bdd.vt

  val make : 'b -> ('a,'b) Cond.O.t -> Apronexpr.Condition.typ -> 'a t -> Cudd.Bdd.vt
  val supeq : 'b -> ('a,'b) Cond.O.t -> 'a t -> Cudd.Bdd.vt
  val sup : 'b -> ('a,'b) Cond.O.t -> 'a t -> Cudd.Bdd.vt
  val eq : 'b -> ('a,'b) Cond.O.t -> 'a t -> Cudd.Bdd.vt
  val substitute :
    'b -> ('a,'b) Cond.O.t ->
    'a Apronexpr.Condition.t ->
    ('a, [> `Apron of 'a t]) PMappe.t ->
    Cudd.Bdd.vt
end
