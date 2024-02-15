(** Recursive descend on sets of diagrams (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Cond
open Cond
open Bdd.Env
open Env

let print_bdd env cond fmt bdd
    =
  Bdd.Expr0.O.print_bdd
    ~print_external_idcondb:begin fun fmt idb ->
      let condition = Bdd.Cond.cond_of_idb cond idb in
      Cond.print_cond env fmt condition
    end
    env fmt bdd


(*  ********************************************************************** *)
(** {3 Arrays of expressions} *)
(*  ********************************************************************** *)

let texpr_cofactor cofactor (texpr:'a Expr0.t array) bdd : 'a Expr0.t array =
  Array.map (fun expr -> cofactor expr bdd) texpr

let texpr_support (cond:('a,'b) Cond.O.t) (texpr: 'a Expr0.t array) =
  Array.fold_left
    (fun res expr ->
      let supp =
	Cudd.Bdd.support_inter
	  cond.supp
	  (Expr0.O.support_cond cond.Bdd.Cond.cudd expr)
      in
      Cudd.Bdd.support_union res supp
    )
    (Cudd.Bdd.dtrue cond.Bdd.Cond.cudd)
    texpr

let texpr_cofactors (env:('a,'b,'c,'d) Env.O.t) (texpr: 'a Expr0.t array) topvar =
  let bdd = Cudd.Bdd.ithvar env.cudd topvar in
  let nbdd = Cudd.Bdd.dnot bdd in
  let t1 = Array.map (fun e -> Expr0.cofactor e bdd) texpr in
  let t2 = Array.map (fun e -> Expr0.cofactor e nbdd) texpr in
  (t1,t2)

(*  ********************************************************************** *)
(** {3 Assignements and substitutions} *)
(*  ********************************************************************** *)

let split_lvar
    symbol
    (lvar:'a list)
    (lexpr:'a Expr0.t list)
    :
    'a list * Apron.Var.t array
    =
  let lbvar = ref [] in
  let lavar = ref [] in
  List.iter2
    (begin fun var expr ->
      match expr with
      | #Bdd.Expr0.t ->
	  lbvar := var :: !lbvar;
      | `Apron _ ->
	  let var = Apron.Var.of_string (symbol.marshal var) in
	  lavar := var :: !lavar;
    end)
    lvar lexpr
  ;
  (!lbvar, Array.of_list !lavar)

let split_texpr
    (texpr:'a Expr0.t array)
    :
    Cudd.Man.v Bdd.Expr0.t list * 'a ApronexprDD.t array
    =
  let lbexpr = ref [] in
  let laexpr = ref [] in
  Array.iter
    (begin fun expr ->
      match expr with
      | (#Bdd.Expr0.t) as e ->
	  lbexpr := e :: !lbexpr
      | `Apron e ->
	  laexpr := e :: !laexpr
    end)
    texpr
  ;
  (!lbexpr, Array.of_list !laexpr)

let split_lvarlexpr
    symbol
    (lvar:'a list)
    (lexpr:'a Expr0.t list)
    :
    'a list * Cudd.Man.v Bdd.Expr0.t list *
    Apron.Var.t array * 'a ApronexprDD.t array
    =
  let lbvar = ref [] in
  let lavar = ref [] in
  let lbexpr = ref [] in
  let laexpr = ref [] in
  List.iter2
    (begin fun var expr ->
      match expr with
      | (#Bdd.Expr0.t) as e ->
	  lbvar := var :: !lbvar;
	  lbexpr := e :: !lbexpr
      | `Apron e ->
	  let var = Apron.Var.of_string (symbol.marshal var) in
	  lavar := var :: !lavar;
	  laexpr := e :: !laexpr
     end)
    lvar lexpr
  ;
  (!lbvar, !lbexpr, Array.of_list !lavar, Array.of_list !laexpr)


(*  ********************************************************************** *)
(** {3 Recursive descend to eliminate conditions} *)
(*  ********************************************************************** *)

let cofactors
    (man:'a ApronDD.man)
    (env:'c)
    (cond:('b,'c) Cond.O.t)
    (t:'a ApronDD.t)
    (idcond:int)
    :
    ('a ApronDD.t * 'a ApronDD.t)
    =
  if PMappe.mem idcond env.idcondvar then begin
    let bdd = Cudd.Bdd.ithvar env.cudd idcond in
    (Cudd.Mtbddc.cofactor t bdd,
    Cudd.Mtbddc.cofactor t (Cudd.Bdd.dnot bdd))
  end
  else begin
    let eapron = env.ext.eapron in
    let `Apron cond1 = Bdd.Cond.cond_of_idb cond (idcond,true) in
    let `Apron cond2 = Bdd.Cond.cond_of_idb cond (idcond,false) in
    let tcons1 = Apronexpr.Condition.to_tcons0 env.symbol eapron cond1 in
    let tcons2 = Apronexpr.Condition.to_tcons0 env.symbol eapron cond2 in
    let t1 = ApronDD.meet_tcons_array man t [|tcons1|] in
    let t2 = ApronDD.meet_tcons_array man t [|tcons2|] in
    (t1,t2)
  end

(** Performs a recursive descend of MTBDDs [t],[tbdd], [tmtbdd]
    and [odest], until there is no arithmetic conditions in [tbdd]
    and [tmtbdd], in which case calls [f t tbdd tmtbdd
    odest]. Returns [bottom] if [t] or [odest] is bottom. *)

let rec descend_mtbdd
    (man:'a ApronDD.man)
    (env:'c)
    (cond:('b,'c) Cond.O.t)
    (f:'a ApronDD.t -> 'b Expr0.t array -> 'a ApronDD.t)
    (t:'a ApronDD.t)
    (texpr:'b Expr0.t array)
    =
  if ApronDD.is_bottom man t then t
  else begin
    let supp = texpr_support cond texpr in
    let res =
      if Cudd.Bdd.is_cst supp then begin
	f t texpr
      end
      else begin
	let topvar = Cudd.Bdd.topvar supp in
	let (texpr1,texpr2) = texpr_cofactors env texpr topvar in
	let (t1,t2) = cofactors man env cond t topvar in
	let res1 = descend_mtbdd man env cond f t1 texpr1 in
	let res2 = descend_mtbdd man env cond f t2 texpr2 in
	ApronDD.join man res1 res2
      end
    in
    res
  end

let descend = Bdd.Decompose.descend
