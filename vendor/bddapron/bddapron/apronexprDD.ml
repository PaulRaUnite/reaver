(** DDs on top of arithmetic expressions (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Env
open Env

(*  ********************************************************************** *)
(** {3 Decision diagram} *)
(*  ********************************************************************** *)

type 'a t = 'a Apronexpr.t Cudd.Mtbdd.t

let of_expr = function
| `Apron x -> x
| _ -> failwith "ApronexprDD.of_expr: Arithmetical expression expected"

let to_expr (x:'a t) = `Apron x

let print print_bdd symbol fmt (expr:'a t) =
  Cudd.Mtbdd.print
    print_bdd
    (Apronexpr.print symbol)
    fmt expr

let is_zero env expr = (Apronexpr.compare env.symbol expr Apronexpr.zero) = 0
let is_one env expr = (Apronexpr.compare env.symbol expr Apronexpr.one) = 0
let absorbant_zero env expr =
  let expr0 = Cudd.Mtbdd.get expr in
  if (Apronexpr.equal env.symbol expr0 Apronexpr.zero)
  then Some expr
  else None
let absorbant_one env expr =
  let expr0 = Cudd.Mtbdd.get expr in
  if (Apronexpr.equal env.symbol expr0 Apronexpr.one)
  then Some expr
  else None

let of_apronexpr env apronexpr =
  Cudd.Mtbdd.cst env.cudd env.ext.table apronexpr
let cst env coeff =
  of_apronexpr env (Apronexpr.cst coeff)
let var env v =
  of_apronexpr env
    (Apronexpr.var env.symbol (Env.typ_of_var env) v)
let add env ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  Cudd.User.map_op2
    ~commutative:true
    ~special:(fun dd1 dd2 ->
      if Cudd.Mtbdd.is_cst dd1 then
	if is_zero env (Cudd.Mtbdd.dval dd1) then Some dd2 else None
      else
	if is_zero env (Cudd.Mtbdd.dval dd2) then Some dd1 else None
    )
    (fun e1 e2 ->
      Cudd.Mtbdd.unique env.ext.table
	(Apronexpr.add env.symbol
	  ~typ ~round (Cudd.Mtbdd.get e1) (Cudd.Mtbdd.get e2)))
    e1 e2

let sub env ?typ ?round e1 e2 =
  Cudd.User.map_op2
    ~special:(fun dd1 dd2 ->
      if Cudd.Mtbdd.is_cst dd2 && is_zero env (Cudd.Mtbdd.dval dd2) then Some dd1
      else None
    )
    (fun e1 e2 ->
      Cudd.Mtbdd.unique env.ext.table
	(Apronexpr.sub env.symbol
	  ?typ ?round (Cudd.Mtbdd.get e1) (Cudd.Mtbdd.get e2)))
    e1 e2

let mul env ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  Cudd.User.map_op2
    ~commutative:true
    ~special:(fun dd1 dd2 ->
      if Cudd.Mtbdd.is_cst dd1 then
	let v = Cudd.Mtbdd.dval dd1 in
	if is_zero env v then Some dd1
	else if is_one env v then Some dd2
	else None
      else
	let v = Cudd.Mtbdd.dval dd2 in
	if is_zero env v then Some dd2
	else if is_one env v then Some dd1
	else None
    )
    (fun e1 e2 ->
      Cudd.Mtbdd.unique env.ext.table
	(Apronexpr.mul env.symbol
	  ~typ ~round (Cudd.Mtbdd.get e1) (Cudd.Mtbdd.get e2)))
    e1 e2

let div env ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  Cudd.User.map_op2
    ~special:(fun dd1 dd2 ->
      if Cudd.Mtbdd.is_cst dd1 then
	if is_zero env (Cudd.Mtbdd.dval dd1) then Some dd1 else None
      else
	if is_one env (Cudd.Mtbdd.dval dd2) then Some dd1 else None
    )
    (fun e1 e2 ->
      Cudd.Mtbdd.unique env.ext.table
	(Apronexpr.div env.symbol
	  ~typ ~round (Cudd.Mtbdd.get e1) (Cudd.Mtbdd.get e2)))
    e1 e2

let gmod env ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  Cudd.User.map_op2
    (fun e1 e2 ->
      Cudd.Mtbdd.unique env.ext.table
	(Apronexpr.gmod env.symbol
	  ~typ ~round (Cudd.Mtbdd.get e1) (Cudd.Mtbdd.get e2)))
    e1 e2
let negate env e =
  Cudd.User.map_op1
    (fun e -> Cudd.Mtbdd.unique env.ext.table (Apronexpr.negate (Cudd.Mtbdd.get e)))
    e

let cast env ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
  Cudd.User.map_op1
    (fun e -> Cudd.Mtbdd.unique env.ext.table (Apronexpr.cast ~typ ~round (Cudd.Mtbdd.get e)))
    e

let sqrt env ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
  Cudd.User.map_op1
    (fun e -> Cudd.Mtbdd.unique env.ext.table (Apronexpr.sqrt ~typ ~round (Cudd.Mtbdd.get e)))
    e

let support_leaf env e =
  Array.fold_left
    (fun res e -> PSette.union res (Apronexpr.support env.symbol e))
    (PSette.empty env.symbol.compare)
    (Cudd.Mtbdd.leaves e)

let support_cond env = Cudd.Mtbdd.support

let substitute_linexpr
  env
  (linexpr:'a Apronexpr.Lin.t)
  (substitution:('a, [>`Apron of 'a t]) PMappe.t)
  :
  'a t
  =
  let substitute_linterm (term:'a Apronexpr.Lin.term) : 'a t
    =
    let (mpqf,var) = term in
    let expr = of_expr (PMappe.find var substitution) in
    let coeffexpr =
      of_apronexpr env
	(Apronexpr.cst (Apron.Coeff.s_of_mpqf mpqf))
    in
    mul env coeffexpr expr
  in

  let (lterm1,lterm2) =
    List.partition
      (fun (coeff,var) -> PMappe.mem var substitution)
      linexpr.Apronexpr.Lin.lterm
  in
  if lterm1=[] then
    of_apronexpr env (Apronexpr.Lin linexpr)
  else begin
    let linexpr2 = {
      Apronexpr.Lin.cst = linexpr.Apronexpr.Lin.cst;
      Apronexpr.Lin.lterm = lterm2
    }
    in
    let expr2 =
      of_apronexpr env (Apronexpr.Lin linexpr2)
    in
    let expr =
      List.fold_left
	(begin fun res term ->
	  add env res (substitute_linterm term)
	end)
	expr2 lterm1
    in
    expr
  end

let substitute_polyexpr
  env
  (polyexpr:'a Apronexpr.Poly.t)
  (substitution:('a, [>`Apron of 'a t]) PMappe.t)
  :
  'a t
  =
  let substitute_varexp (res:'a t) (varexp:'a Apronexpr.Poly.varexp) : 'a t
    =
    let (var,exp) = varexp in
    try
      let expr = of_expr (PMappe.find var substitution) in
      let res = ref res in
      for i=1 to exp do
	res := mul env !res expr
      done;
      !res
    with Not_found ->
      let monomial = [varexp] in
      let term = ((Mpqf.of_int 1), monomial) in
      let polyexpr = [term] in
      of_apronexpr env (Apronexpr.Poly polyexpr)
  in
  let substitute_monomial (res:'a t) (monomial:'a Apronexpr.Poly.monomial) : 'a t
    =
    List.fold_left substitute_varexp res monomial
  in
  let substitute_term (term:'a Apronexpr.Poly.term) : 'a t
    =
    let (mpqf,monomial) = term in
    let res = of_apronexpr env
	(Apronexpr.cst (Apron.Coeff.s_of_mpqf mpqf))
    in
    substitute_monomial res monomial
  in
  let res = of_apronexpr env
    (Apronexpr.cst (Apron.Coeff.s_of_int 0))
  in
  List.fold_left
    (begin fun res term ->
      add env res (substitute_term term)
    end)
    res polyexpr

let substitute_treeexpr
  env
  (treeexpr:'a Apronexpr.Tree.t)
  (substitution:('a, [>`Apron of 'a t]) PMappe.t)
  :
  'a t
  =
  let rec parcours = function
    | Apronexpr.Tree.Cst coeff ->
	of_apronexpr env (Apronexpr.cst coeff)
    | Apronexpr.Tree.Var var ->
	begin
	  try of_expr (PMappe.find var substitution)
	  with Not_found ->
	    of_apronexpr env
	      (Apronexpr.Lin(Apronexpr.Lin.var var))
	end
    | Apronexpr.Tree.Unop(unop,e,t,r) ->
	let res = parcours e in
	(begin match unop with
	| Apronexpr.Tree.Neg -> negate env res
	| Apronexpr.Tree.Cast -> cast env ~typ:t ~round:r res
	| Apronexpr.Tree.Sqrt -> sqrt env ~typ:t ~round:r res
	end)
    | Apronexpr.Tree.Binop(binop,e1,e2,t,r) ->
	let res1 = parcours e1 in
	let res2 = parcours e2 in
	begin match binop with
	| Apronexpr.Tree.Add -> add
	| Apronexpr.Tree.Sub -> sub
	| Apronexpr.Tree.Mul -> mul
	| Apronexpr.Tree.Div -> div
	| Apronexpr.Tree.Mod -> gmod
        | Apronexpr.Tree.Pow -> raise Exit
	end
	  env ~typ:t ~round:r res1 res2
  in
  parcours treeexpr

let substitute
  env
  (expr:'a Apronexpr.t)
  (substitution:('a, [>`Apron of 'a t]) PMappe.t)
  :
  'a t
  =
  match expr with
  | Apronexpr.Lin x -> substitute_linexpr env x substitution
  | Apronexpr.Poly x -> substitute_polyexpr env x substitution
  | Apronexpr.Tree x -> substitute_treeexpr env x substitution

module Condition = struct
  let of_apronexpr env cond condition =
    let (id,b) = Bdd.Cond.idb_of_cond env cond (`Apron condition) in
    let bdd = Cudd.Bdd.ithvar env.cudd id in
    if b then bdd else Cudd.Bdd.dnot bdd
  let of_condition (env:'b) (cond:('a,'b) Cond.O.t) condition =
    match condition with
    | `Bool b ->
	if b then Cudd.Bdd.dtrue env.cudd else Cudd.Bdd.dfalse env.cudd
    | `Cond x ->
	of_apronexpr env cond x

  let make env cond typ e =
    Cudd.Mtbdd.fold_guardleaves
      (fun g l e ->
        let atom =
          let condition = Apronexpr.Condition.make (Env.typ_of_var env) typ l in
          of_condition env cond condition
        in
        Cudd.Bdd.dor e (Cudd.Bdd.dand g atom))
      e
      (Cudd.Bdd.dfalse (Cudd.Mtbdd.manager e))

  let supeq env cond expr = make env cond Apronexpr.Condition.SUPEQ expr
  let sup env cond expr = make env cond Apronexpr.Condition.SUP expr
  let eq env cond expr = make env cond Apronexpr.Condition.EQ expr

  let substitute
   env cond
   (condition:'a Apronexpr.Condition.t)
   (substitution:('a,[>`Apron of 'a t]) PMappe.t)
   :
   Cudd.Bdd.vt
    =
    let (typ,expr) = condition in
    let nexpr = substitute env expr substitution in
    make env cond typ nexpr

end
