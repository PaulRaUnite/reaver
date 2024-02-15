(** Functions common to the two implementations of Combined Boolean/Numerical domain *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Cond
open Cond
open Bdd.Env
open Env

let tcons0_array_of_cubecond env cond cubecond =
  let eapron = env.Bdd.Env.ext.Env.eapron in
  let lidcondb = Cudd.Bdd.list_of_cube cubecond in
  let tidcondb = Array.of_list lidcondb in
  let ttcons0 =
    Array.map
      (begin fun idb ->
	let `Apron condition = Bdd.Cond.cond_of_idb cond idb in
	let tcons0 = Apronexpr.Condition.to_tcons0 env.Bdd.Env.symbol eapron condition in
	tcons0
      end)
      tidcondb
  in
  ttcons0

let lvar_split env lvar =
  let eapron = env.Bdd.Env.ext.Env.eapron in
  let (lbvar,ladim) =
    List.fold_left
      (begin fun (lbvar,ladim) var ->
	match Env.typ_of_var env var with
	| #Bdd.Env.typ -> (var::lbvar,ladim)
	| _ ->
	    let avar = Apron.Var.of_string (env.Bdd.Env.symbol.Bdd.Env.marshal var) in
	    let adim = Apron.Environment.dim_of_var eapron avar in
	    (lbvar,adim::ladim)
      end)
      ([],[]) lvar
  in
  let bsupp = Bdd.Expr0.O.bddsupport env lbvar in
  let tadim = Array.of_list ladim in
  (bsupp,tadim)

let condition_of_tcons0 (env:('a,'b,'c,'d) Env.O.t) tcons0 =
  Apronexpr.Condition.of_tcons0
    env.Bdd.Env.symbol
    (fun var -> Env.typ_of_var env var)
    env.ext.Env.eapron
    tcons0

let bdd_of_tcons0 env cond tcons0 =
  let condition = condition_of_tcons0 env tcons0 in
  ApronexprDD.Condition.of_condition env cond condition

let bdd_of_tcons0_array env cond tcons0_array =
  Array.fold_left
    (fun res tcons0 ->
      Cudd.Bdd.dand res (bdd_of_tcons0 env cond tcons0))
    (Cudd.Bdd.dtrue env.cudd)
    tcons0_array
