(** Normalized condition environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Env

type 'a cond = [`Apron of 'a Apronexpr.Condition.t]

let print_cond (env:('a,'b,'c,'d) Env.O.t) fmt (cond:[< 'a cond]) =
  match cond with
  | `Apron x -> Apronexpr.Condition.print env.symbol fmt x

let compare_cond symbol c1 c2 = match (c1,c2) with
  (`Apron c1, `Apron c2) -> Apronexpr.Condition.compare symbol c1 c2

let negate_cond (env:('a,'b,'c,'d) Env.O.t) (c:'a cond) : 'a cond =
  match c with
  | `Apron x ->
      `Apron (Apronexpr.Condition.negate (Env.typ_of_var env) x)

let support_cond env cond = match cond with
  | `Apron x -> Apronexpr.Condition.support env.symbol x

module O = struct
  type ('a,'b) t = ('a, 'b, 'a cond, Cudd.Man.v) Bdd.Cond.t
  constraint 'b = ('a,'c,'d,'e) Env.O.t

  let make ~symbol ?bddindex0 ?bddsize ?bddmax (cudd:Cudd.Man.vt) : ('a, 'b) t =
    Bdd.Cond.make
      ~symbol
      ~compare_cond:(compare_cond symbol)
      ~negate_cond
      ~support_cond
      ~print_cond
      ?bddindex0 ?bddsize ?bddmax cudd
end

type 'a t = ('a, 'a Env.t) O.t

let make = O.make

let copy = Bdd.Cond.copy

let print = Bdd.Cond.print

(*  ********************************************************************** *)
(** {3 Level 2} *)
(*  ********************************************************************** *)

type ('a,'b) value = ('a,'b) Bdd.Cond.value = {
  cond : 'a;
  val1 : 'b
}

let make_value = Bdd.Cond.make_value
let get_cond = Bdd.Cond.get_cond
let get_val1 = Bdd.Cond.get_val1
let get_env = Bdd.Cond.get_env
let get_val0 = Bdd.Cond.get_val0
