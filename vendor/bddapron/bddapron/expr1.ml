(** Finite-type and arithmetical expressions with normalized environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Env
open Env

(*  ********************************************************************** *)
(** {3 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

  (*  ==================================================================== *)
  (** {4 General expressions} *)
  (*  ==================================================================== *)

  type ('a,'b) t = ('b, 'a Expr0.t) Env.value
  constraint 'b = ('a,'c,'d,'e) Env.O.t

  type ('a,'b) expr = ('a,'b) t

  let substitute ?memo (cond:('a,'b) Cond.O.t) (e:('a,'b) t) (substitution:('a * ('a,'b) t) list) : ('a,'b) t
      =
    let lvarexpr =
      Env.check_lvarvalue e.env substitution
    in
    make_value e.env (Expr0.O.substitute ?memo e.env cond e.val0 lvarexpr)
  let substitute_list ?memo cond le lvarexpr =
    match le with
    | [] -> []
    | [e] -> [substitute ?memo cond e lvarexpr]
    | _ ->
	let env = (List.hd le).env in
	let lval0 = List.map (fun e -> e.val0) le in
	let lvarexpr = Env.check_lvarvalue env lvarexpr in
	let lval0 = Expr0.O.substitute_list ?memo env cond lval0 lvarexpr in
	List.map (make_value env) lval0

  let substitute_by_var ?memo (cond:('a,'b) Cond.O.t) (e:('a,'b) t) (substitution:('a*'a) list) =
    make_value e.env (Expr0.O.substitute_by_var ?memo e.env cond e.val0 substitution)
  let substitute_by_var_list ?memo cond le lvarvar =
    match le with
    | [] -> []
    | [e] -> [substitute_by_var ?memo cond e lvarvar]
    | _ ->
	let env = (List.hd le).env in
	let lval0 = List.map (fun e -> e.val0) le in
	let lval0 = Expr0.O.substitute_by_var_list ?memo env cond lval0 lvarvar in
	List.map (make_value env) lval0

  let ddsubstitute = substitute
  let ddsubstitute_by_var = substitute_by_var

  let var env cond (var:'a) : ('a,'b) t
      =
    make_value env (Expr0.O.var env cond var)

  (*  ==================================================================== *)
  (** {4 Bdd expressions} *)
  (*  ==================================================================== *)

  module Bool = struct
    type ('a,'b) t = ('b, Cudd.Man.v Cudd.Bdd.t) Env.value
    constraint 'b = ('a,'c,'d,'e) Env.O.t

    let of_expr0 = Bdd.Expr1.O.Bool.of_expr0
    let get_env = Bdd.Expr1.O.Bool.get_env
    let to_expr0 = Bdd.Expr1.O.Bool.to_expr0
    let of_expr = Bdd.Expr1.O.Bool.of_expr
    let to_expr = Bdd.Expr1.O.Bool.to_expr
    let extend_environment = Bdd.Expr1.O.Bool.extend_environment

    let dtrue env cond = Bdd.Expr1.O.Bool.dtrue env
    let dfalse env cond = Bdd.Expr1.O.Bool.dfalse env
    let of_bool env cond = Bdd.Expr1.O.Bool.of_bool env
    let var env cond = Bdd.Expr1.O.Bool.var env
    let dnot cond = Bdd.Expr1.O.Bool.dnot
    let dand cond = Bdd.Expr1.O.Bool.dand
    let dor cond = Bdd.Expr1.O.Bool.dor
    let xor cond = Bdd.Expr1.O.Bool.xor
    let nand cond = Bdd.Expr1.O.Bool.nand
    let nor cond = Bdd.Expr1.O.Bool.nor
    let nxor cond = Bdd.Expr1.O.Bool.nxor
    let eq cond = Bdd.Expr1.O.Bool.eq
    let leq cond = Bdd.Expr1.O.Bool.leq
    let ite cond = Bdd.Expr1.O.Bool.ite
    let is_true cond = Bdd.Expr1.O.Bool.is_true
    let is_false cond = Bdd.Expr1.O.Bool.is_false
    let is_cst cond = Bdd.Expr1.O.Bool.is_cst
    let is_eq cond = Bdd.Expr1.O.Bool.is_eq
    let is_leq cond = Bdd.Expr1.O.Bool.is_leq
    let is_inter_false cond = Bdd.Expr1.O.Bool.is_inter_false
    let exist cond = Bdd.Expr1.O.Bool.exist
    let forall cond = Bdd.Expr1.O.Bool.forall
    let cofactor = Bdd.Expr1.O.Bool.cofactor
    let restrict = Bdd.Expr1.O.Bool.restrict
    let tdrestrict = Bdd.Expr1.O.Bool.tdrestrict

    let print cond fmt e = Expr0.O.Bool.print e.env cond fmt e.val0

    let substitute_by_var ?memo cond e sub =
      of_expr (ddsubstitute_by_var ?memo cond (to_expr e) sub)
    let substitute ?memo cond e sub =
      of_expr (ddsubstitute ?memo cond (to_expr e) sub)
  end

  module Bint = struct
    type ('a,'b) t = ('b, Cudd.Man.v Bdd.Int.t) Env.value
    constraint 'b = ('a,'c,'d,'e) Env.O.t

    let of_expr0 = Bdd.Expr1.O.Bint.of_expr0
    let get_env = Bdd.Expr1.O.Bint.get_env
    let to_expr0 = Bdd.Expr1.O.Bint.to_expr0
    let of_expr = Bdd.Expr1.O.Bint.of_expr
    let to_expr = Bdd.Expr1.O.Bint.to_expr
    let extend_environment = Bdd.Expr1.O.Bint.extend_environment

    let of_int env cond = Bdd.Expr1.O.Bint.of_int env
    let var env cond = Bdd.Expr1.O.Bint.var env
    let neg cond = Bdd.Expr1.O.Bint.neg
    let succ cond = Bdd.Expr1.O.Bint.succ
    let pred cond = Bdd.Expr1.O.Bint.pred
    let add cond = Bdd.Expr1.O.Bint.add
    let sub cond = Bdd.Expr1.O.Bint.sub
    let mul cond = Bdd.Expr1.O.Bint.mul
    let shift_left cond = Bdd.Expr1.O.Bint.shift_left
    let shift_right cond = Bdd.Expr1.O.Bint.shift_right
    let scale cond = Bdd.Expr1.O.Bint.scale
    let ite cond = Bdd.Expr1.O.Bint.ite

    let zero cond = Bdd.Expr1.O.Bint.zero
    let eq cond = Bdd.Expr1.O.Bint.eq
    let eq_int cond = Bdd.Expr1.O.Bint.eq_int
    let supeq cond = Bdd.Expr1.O.Bint.supeq
    let supeq_int cond = Bdd.Expr1.O.Bint.supeq_int
    let sup cond = Bdd.Expr1.O.Bint.sup
    let sup_int cond = Bdd.Expr1.O.Bint.sup_int

    let cofactor = Bdd.Expr1.O.Bint.cofactor
    let restrict = Bdd.Expr1.O.Bint.restrict
    let tdrestrict = Bdd.Expr1.O.Bint.tdrestrict

    let guard_of_int cond = Bdd.Expr1.O.Bint.guard_of_int
    let guardints cond = Bdd.Expr1.O.Bint.guardints

    let print cond fmt e = Expr0.O.Bint.print e.env cond fmt e.val0

    let substitute_by_var ?memo cond e sub =
      of_expr (ddsubstitute_by_var ?memo cond (to_expr e) sub)
    let substitute ?memo cond e sub =
      of_expr (ddsubstitute ?memo cond (to_expr e) sub)

  end
  module Benum = struct
    type ('a,'b) t = ('b, Cudd.Man.v Bdd.Enum.t) Env.value
    constraint 'b = ('a,'c,'d,'e) Env.O.t

    let of_expr0 = Bdd.Expr1.O.Benum.of_expr0
    let get_env = Bdd.Expr1.O.Benum.get_env
    let to_expr0 = Bdd.Expr1.O.Benum.to_expr0
    let of_expr = Bdd.Expr1.O.Benum.of_expr
    let to_expr = Bdd.Expr1.O.Benum.to_expr
    let extend_environment = Bdd.Expr1.O.Benum.extend_environment

    let var env cond = Bdd.Expr1.O.Benum.var env
    let ite cond = Bdd.Expr1.O.Benum.ite
    let eq cond = Bdd.Expr1.O.Benum.eq
    let eq_label cond = Bdd.Expr1.O.Benum.eq_label
    let cofactor = Bdd.Expr1.O.Benum.cofactor
    let restrict = Bdd.Expr1.O.Benum.restrict
    let tdrestrict = Bdd.Expr1.O.Benum.tdrestrict
    let guard_of_label cond = Bdd.Expr1.O.Benum.guard_of_label
    let guardlabels cond = Bdd.Expr1.O.Benum.guardlabels

    let print cond fmt e = Expr0.O.Benum.print e.env cond fmt e.val0

    let substitute_by_var ?memo cond e sub =
      of_expr (ddsubstitute_by_var ?memo cond (to_expr e) sub)
    let substitute ?memo cond e sub =
      of_expr (ddsubstitute ?memo cond (to_expr e) sub)
  end

  (*  ==================================================================== *)
  (** {4 Arith expressions} *)
  (*  ==================================================================== *)

  module Apron = struct
    type ('a,'b) t = ('b, 'a ApronexprDD.t) Env.value
    constraint 'b = ('a,'c,'d,'e) Env.O.t

    let of_expr0 = Env.make_value
    let get_env = Env.get_env
    let to_expr0 = Env.get_val0
    let of_expr e : ('a,'b) t =
      match e.val0 with
      | `Apron x -> make_value e.env x
      | _ -> failwith "Apron.of_expr: arithmetic expression expected"

    let to_expr (e:('a,'b) t) =
      make_value e.env (`Apron e.val0)

    let extend_environment e nenv =
      if Env.is_eq e.env nenv then
	e
      else begin
	if not (Env.is_leq e.env nenv) then
	  failwith "Apron.extend_environment: the given environment is not a superenvironment "
	;
	let perm = Bdd.Env.permutation12 e.env nenv in
	make_value nenv (Cudd.Mtbdd.permute e.val0 perm)
      end

    let var env cond name = make_value env (Expr0.O.Apron.var env cond name)
    let cst env cond cst = make_value env (Expr0.O.Apron.cst env cond cst)
    let add cond ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
      Env.mapbinop
	(ApronexprDD.add e1.env ~typ ~round) e1 e2
    let mul cond ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
      Env.mapbinop
	(ApronexprDD.mul e1.env ~typ ~round) e1 e2
    let sub cond ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
      Env.mapbinop
	(ApronexprDD.sub e1.env ~typ ~round) e1 e2
    let div cond ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
      Env.mapbinop
	(ApronexprDD.div e1.env ~typ ~round) e1 e2
    let gmod cond ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
      Env.mapbinop
	(ApronexprDD.gmod e1.env ~typ ~round) e1 e2
    let negate cond e = Env.mapunop (ApronexprDD.negate e.env) e
    let sqrt cond ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
      Env.mapunop (ApronexprDD.sqrt e.env ~typ ~round) e
    let cast cond ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
      Env.mapunop (ApronexprDD.cast e.env ~typ ~round) e

    let ite cond e1 e2 e3 =
      Env.mapterop Cudd.Mtbdd.ite e1 e2 e3

    let condition (cond:('a,'b) Cond.O.t) typ (e:('a,'b) t) : ('a,'b) Bool.t =
      make_value e.env (ApronexprDD.Condition.make e.env cond typ e.val0)

    let supeq cond expr = condition cond Apron.Tcons1.SUPEQ expr
    let sup cond expr = condition cond Apron.Tcons1.SUP expr
    let eq cond expr = condition cond Apron.Tcons1.EQ expr

    let cofactor e1 e2 = Env.mapbinop Cudd.Mtbdd.cofactor e1 e2
    let restrict e1 e2 = Env.mapbinop Cudd.Mtbdd.restrict e1 e2
    let tdrestrict e1 e2 = Env.mapbinop Cudd.Mtbdd.tdrestrict e1 e2

    let print cond fmt (x:('a,'b) t) =
      ApronexprDD.print (Expr0.O.print_bdd x.env cond) x.env.symbol fmt x.val0

    let substitute_by_var ?memo cond e sub =
      of_expr (ddsubstitute_by_var ?memo cond (to_expr e) sub)
    let substitute ?memo cond e sub =
      of_expr (ddsubstitute ?memo cond (to_expr e) sub)
  end

  (*  ====================================================================== *)
  (** {4 General expressions} *)
  (*  ====================================================================== *)

  let typ_of_expr e = Expr0.O.typ_of_expr e.env e.val0

  let of_expr0 = Env.make_value
  let get_env = Env.get_env
  let to_expr0 = Env.get_val0

  let permute (e:'a Expr0.t) (tab:int array) : 'a Expr0.t = match e with
    | #Bdd.Expr0.t as e ->
	let res = Bdd.Expr0.O.permute e tab in
	(res:>'a Expr0.t)
    | `Apron e -> `Apron (Cudd.Mtbdd.permute e tab)

  let extend_environment abs nenv =
    if Env.is_eq abs.env nenv then
      abs
    else begin
      if not (Env.is_leq abs.env nenv) then
	failwith "BddapronexprE.O.extend_environment: the given environment is not a superenvironment"
      ;
      let perm = Bdd.Env.permutation12 abs.env nenv in
      make_value nenv (permute abs.val0 perm)
    end

  let ite cond e1 e2 e3 =
    Env.check_value3 e1 e2 e3;
    make_value e1.env (Expr0.O.ite e1.env cond e1.val0 e2.val0 e3.val0)

  let cofactor e1 e2 = Env.mapbinop Expr0.cofactor e1 e2
  let restrict e1 e2 = Env.mapbinop Expr0.restrict e1 e2
  let tdrestrict e1 e2 = Env.mapbinop Expr0.tdrestrict e1 e2

  let eq cond e1 e2 =
    Env.check_value2 e1 e2;
    let t = Expr0.O.check_typ2 e1.env e1.val0 e2.val0 in
    match t with
    | `Bool ->
	Bool.eq cond (Bool.of_expr e1) (Bool.of_expr e2)
    | `Bint _ ->
	Bint.eq cond (Bint.of_expr e1) (Bint.of_expr e2)
    | `Benum _ ->
	Benum.eq cond (Benum.of_expr e1) (Benum.of_expr e2)
    | `Real ->
	let diff = Apron.sub cond
	  (Apron.of_expr e1)
	  (Apron.of_expr e2)
	in
	Apron.eq cond diff
    | _ -> failwith ""

  let support cond (e:('a,'b) t) = Expr0.O.support e.env cond e.val0
  let support_cond cudd (e:('a,'b) t) = Expr0.O.support_cond cudd e.val0

  let print cond fmt (e:('a,'b) t) : unit =
    Expr0.O.print e.env cond fmt e.val0

  let make = make_value

  let normalize ?reduce ?careset (cond,lexpr) =
    let (lenv,lexpr) =
      List.fold_left
	(fun (lenv,lexpr) e -> (e.env::lenv,e.val0::lexpr))
	([],[]) lexpr
    in
    let (cond,lexpr) = Expr0.O.normalize ?reduce ?careset (cond,lexpr) in
    let lexpr =
      List.rev_map2 make_value lenv lexpr
    in
    (cond,lexpr)

  (*  ====================================================================== *)
  (** {4 List of expressions} *)
  (*  ====================================================================== *)

  module List = struct
    type ('a,'b) t = ('b, 'a Expr0.t list) Env.value
    constraint 'b = ('a,'c,'d,'e) Env.O.t

    let of_lexpr0 = Env.make_value
    let get_env = Env.get_env
    let to_lexpr0 = Env.get_val0

    let of_lexpr env lexpr1 =
      let lexpr0 = Env.check_lvalue env lexpr1 in
      of_lexpr0 env lexpr0
    let to_lexpr lexpr1 =
      List.map (fun val0 -> Env.make_value lexpr1.env val0) lexpr1.val0

    let extend_environment e nenv =
      Bdd.Env.extend_environment Expr0.O.permute_list e nenv

    let normalize ?reduce ?careset (cond,list) =
      let (cond,lexpr0) =
	Expr0.O.normalize ?reduce ?careset (cond,list.val0)
      in
      (cond, of_lexpr0 list.env lexpr0)

    let print
	?(first=("(@[":(unit,Format.formatter,unit) format))
	?(sep=(",@ ":(unit,Format.formatter,unit) format))
	?(last=("@])":(unit,Format.formatter,unit) format))
	(cond:('a,'b) Cond.O.t) fmt (x:('a,'b) t)
	=
      Print.list ~first ~sep ~last
	(Expr0.O.print x.env cond) fmt x.val0

  end

end

(*  ********************************************************************** *)
(** {3 Closed signature} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {4 Operations on general expressions} *)
(*  ====================================================================== *)

type 'a t = ('a Env.t, 'a Expr0.t) Env.value

type 'a expr = 'a t
let typ_of_expr = O.typ_of_expr
let make = O.make
let of_expr0 = O.of_expr0
let get_env = O.get_env
let to_expr0 = O.to_expr0
let extend_environment = O.extend_environment
let var = O.var
let ite = O.ite
let eq = O.eq
let substitute_by_var = O.substitute_by_var
let substitute = O.substitute
let substitute_by_var_list = O.substitute_by_var_list
let substitute_list = O.substitute_list
let support = O.support
let support_cond = O.support_cond
let cofactor = O.cofactor
let restrict = O.restrict
let tdrestrict = O.tdrestrict
let print = O.print
let make = O.make
let normalize = O.normalize

(*  ====================================================================== *)
(** {4 Boolean expressions} *)
(*  ====================================================================== *)

module Bool = struct
  type 'a t = ('a Env.t, Cudd.Man.v Cudd.Bdd.t) Env.value
  let of_expr0 = O.Bool.of_expr0
  let get_env = O.Bool.get_env
  let to_expr0 = O.Bool.to_expr0
  let of_expr = O.Bool.of_expr
  let to_expr = O.Bool.to_expr
  let extend_environment = O.Bool.extend_environment
  let dtrue = O.Bool.dtrue
  let dfalse = O.Bool.dfalse
  let of_bool = O.Bool.of_bool
  let var = O.Bool.var
  let dnot = O.Bool.dnot
  let dand = O.Bool.dand
  let dor = O.Bool.dor
  let xor = O.Bool.xor
  let nand = O.Bool.nand
  let nor = O.Bool.nor
  let nxor = O.Bool.nxor
  let eq = O.Bool.eq
  let leq = O.Bool.leq
  let ite = O.Bool.ite
  let is_true = O.Bool.is_true
  let is_false = O.Bool.is_false
  let is_cst = O.Bool.is_cst
  let is_eq = O.Bool.is_eq
  let is_leq = O.Bool.is_leq
  let is_inter_false = O.Bool.is_inter_false
  let exist = O.Bool.exist
  let forall = O.Bool.forall
  let cofactor = O.Bool.cofactor
  let restrict = O.Bool.restrict
  let tdrestrict = O.Bool.tdrestrict
  let substitute_by_var = O.Bool.substitute_by_var
  let substitute = O.Bool.substitute
  let print = O.Bool.print
end

(*  ====================================================================== *)
(** {4 Bounded integer expressions} *)
(*  ====================================================================== *)

module Bint = struct
  type 'a t = ('a Env.t, Cudd.Man.v Bdd.Int.t) Env.value
  let of_expr0 = O.Bint.of_expr0
  let get_env = O.Bint.get_env
  let to_expr0 = O.Bint.to_expr0
  let of_expr = O.Bint.of_expr
  let to_expr = O.Bint.to_expr
  let extend_environment = O.Bint.extend_environment
  let of_int = O.Bint.of_int
  let var = O.Bint.var
  let neg = O.Bint.neg
  let succ = O.Bint.succ
  let pred = O.Bint.pred
  let add = O.Bint.add
  let sub = O.Bint.sub
  let mul = O.Bint.mul
  let shift_left = O.Bint.shift_left
  let shift_right = O.Bint.shift_right
  let scale = O.Bint.scale
  let ite = O.Bint.ite
  let zero = O.Bint.zero
  let eq = O.Bint.eq
  let supeq = O.Bint.supeq
  let sup = O.Bint.sup
  let eq_int = O.Bint.eq_int
  let supeq_int = O.Bint.supeq_int
  let sup_int = O.Bint.sup_int
  let cofactor = O.Bint.cofactor
  let restrict = O.Bint.restrict
  let tdrestrict = O.Bint.tdrestrict
  let substitute_by_var = O.Bint.substitute_by_var
  let substitute = O.Bint.substitute
  let guard_of_int= O.Bint.guard_of_int
  let guardints= O.Bint.guardints
  let print = O.Bint.print
end

(*  ====================================================================== *)
(** {4 Enumerated expressions} *)
(*  ====================================================================== *)

module Benum = struct
  type 'a t = ('a Env.t, Cudd.Man.v Bdd.Enum.t) Env.value
  let of_expr0 = O.Benum.of_expr0
  let get_env = O.Benum.get_env
  let to_expr0 = O.Benum.to_expr0
  let of_expr = O.Benum.of_expr
  let to_expr = O.Benum.to_expr
  let extend_environment = O.Benum.extend_environment
  let var = O.Benum.var
  let ite = O.Benum.ite
  let eq = O.Benum.eq
  let eq_label = O.Benum.eq_label
  let cofactor = O.Benum.cofactor
  let restrict = O.Benum.restrict
  let tdrestrict = O.Benum.tdrestrict
  let substitute_by_var = O.Benum.substitute_by_var
  let substitute = O.Benum.substitute
  let guard_of_label = O.Benum.guard_of_label
  let guardlabels = O.Benum.guardlabels
  let print = O.Benum.print
end

(*  ====================================================================== *)
(** {4 Arithmetic expressions} *)
(*  ====================================================================== *)
type apron_coeff = Apron.Coeff.t
type apron_typ = Apron.Texpr1.typ
type apron_round = Apron.Texpr1.round
type apron_cons_typ = Apron.Tcons1.typ

module Apron = struct
  type 'a t = ('a Env.t, 'a ApronexprDD.t) Env.value
  let of_expr0 = O.Apron.of_expr0
  let get_env = O.Apron.get_env
  let to_expr0 = O.Apron.to_expr0
  let of_expr = O.Apron.of_expr
  let to_expr = O.Apron.to_expr
  let extend_environment = O.Apron.extend_environment
  let var = O.Apron.var
  let cst = O.Apron.cst
  let add = O.Apron.add
  let mul = O.Apron.mul
  let sub = O.Apron.sub
  let div = O.Apron.div
  let gmod = O.Apron.gmod
  let negate = O.Apron.negate
  let sqrt = O.Apron.sqrt
  let cast = O.Apron.cast
  let ite = O.Apron.ite
  let condition = O.Apron.condition
  let supeq = O.Apron.supeq
  let sup = O.Apron.sup
  let eq = O.Apron.eq
  let cofactor = O.Apron.cofactor
  let restrict = O.Apron.restrict
  let tdrestrict = O.Apron.tdrestrict
  let substitute_by_var = O.Apron.substitute_by_var
  let substitute = O.Apron.substitute
  let print = O.Apron.print
end

(*  ====================================================================== *)
(** {4 List of expressions} *)
(*  ====================================================================== *)

module List = struct
  type 'a t = ('a Env.t, 'a Expr0.t list) Env.value

  let of_lexpr0 = O.List.of_lexpr0
  let get_env = O.List.get_env
  let to_lexpr0 = O.List.to_lexpr0
  let of_lexpr = O.List.of_lexpr
  let to_lexpr = O.List.to_lexpr
  let extend_environment = O.List.extend_environment
  let normalize = O.List.normalize
  let print = O.List.print
end
