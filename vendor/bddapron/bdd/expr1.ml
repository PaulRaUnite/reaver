(** Finite-type expressions with normalized environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Env

(*  ********************************************************************** *)
(** {3 Opened signatures and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

  (*  ==================================================================== *)
  (** {4 Datatypes} *)
  (*  ==================================================================== *)

  type ('a,'b,'c) t = ('b, 'c Expr0.t) Env.value
  constraint 'b = ('a,'d,'e,'c,'f) Env.O.t

  type ('a,'b,'c) expr = ('a,'b,'c) t
    (** Type of general expressions *)

  type ('a,'b) dt = ('a,'b,Cudd.Man.d) t
  type ('a,'b) vt = ('a,'b,Cudd.Man.v) t

  (*  ==================================================================== *)
  (** {4 Expressions} *)
  (*  ==================================================================== *)

  let typ_of_expr e = Expr0.O.typ_of_expr e.env e.val0

  let make = Env.make_value
  let of_expr0 = Env.make_value
  let get_env = Env.get_env
  let to_expr0 = Env.get_val0

  let extend_environment e nenv =
    Env.extend_environment Expr0.O.permute e nenv

  let ite e1 e2 e3 =
    Env.mapterop Expr0.O.ite e1 e2 e3

  let cofactor e1 e2 = Env.mapbinop Expr0.cofactor e1 e2
  let restrict e1 e2 = Env.mapbinop Expr0.restrict e1 e2
  let tdrestrict e1 e2 = Env.mapbinop Expr0.tdrestrict e1 e2

  let substitute_by_var ?memo e lvarvar =
    make_value e.env (Expr0.O.substitute_by_var ?memo e.Env.env e.Env.val0 lvarvar)
  let substitute_by_var_list ?memo le lvarvar =
    match le with
    | [] -> []
    | [e] -> [substitute_by_var ?memo e lvarvar]
    | _ ->
	let env = (List.hd le).env in
	let lval0 = List.map (fun e -> e.val0) le in
	let lval0 = Expr0.O.substitute_by_var_list ?memo env lval0 lvarvar in
	List.map (make_value env) lval0

  let substitute ?memo e lvarexpr =
    let lvarexpr = Env.check_lvarvalue e.env lvarexpr in
    make_value e.env (Expr0.O.substitute ?memo e.env e.val0 lvarexpr)
  let substitute_list ?memo le lvarexpr =
    match le with
    | [] -> []
    | [e] -> [substitute ?memo e lvarexpr]
    | _ ->
	let env = (List.hd le).env in
	let lval0 = List.map (fun e -> e.val0) le in
	let lvarexpr = Env.check_lvarvalue env lvarexpr in
	let lval0 = Expr0.O.substitute_list ?memo env lval0 lvarexpr in
	List.map (make_value env) lval0

  let eq e1 e2 = Env.mapbinope Expr0.O.eq e1 e2

  let support (e:('a,'b,'c) expr) = Expr0.O.support e.env e.val0

  let support_cond (e:('a,'b,'c) expr) = Expr0.O.support_cond e.env.cudd e.val0

  let print fmt (e:('a,'b,'c) expr) : unit = Expr0.O.print e.env fmt e.val0

  (*  -------------------------------------------------------------------- *)
  (** {5 Boolean expressions} *)
  (*  -------------------------------------------------------------------- *)

  module Bool = struct
    type ('a,'b,'c) t = ('b, 'c Cudd.Bdd.t) Env.value
    constraint 'b = ('a,'d,'e,'c,'f) Env.O.t

    type ('a,'b) dt = ('a,'b,Cudd.Man.d) t
    type ('a,'b) vt = ('a,'b,Cudd.Man.v) t

    let of_expr0 = Env.make_value
    let get_env = Env.get_env
    let to_expr0 = Env.get_val0

    let of_expr e : ('a,'b,'c) t =
      match e.val0 with
      | `Bool x -> make_value e.env x
      | _ -> failwith "Bool.of_expr: Boolean expression expected"

    let to_expr (e:('a,'b,'c) t) =
      make_value e.env (`Bool e.val0)

    let extend_environment e nenv = Env.extend_environment Cudd.Bdd.permute e nenv

    let dtrue env = make_value env (Cudd.Bdd.dtrue env.cudd)
    let dfalse env = make_value env (Cudd.Bdd.dfalse env.cudd)
    let of_bool env b = if b then dtrue env else dfalse env

    let var env (var:'a) =
      Env.check_var env var;
      make_value env (Expr0.O.Bool.var env var)

    let dnot e = Env.mapunop Cudd.Bdd.dnot e

    let dand e1 e2 = Env.mapbinop Cudd.Bdd.dand e1 e2
    let dor e1 e2 = Env.mapbinop Cudd.Bdd.dor e1 e2
    let xor e1 e2 = Env.mapbinop Cudd.Bdd.xor e1 e2
    let nand e1 e2 = Env.mapbinop Cudd.Bdd.nand e1 e2
    let nor e1 e2 = Env.mapbinop Cudd.Bdd.nor e1 e2
    let nxor e1 e2 = Env.mapbinop Cudd.Bdd.nxor e1 e2
    let eq e1 e2 = Env.mapbinop Cudd.Bdd.eq e1 e2
    let leq e1 e2 = Env.mapbinop (fun x y -> Cudd.Bdd.dor y (Cudd.Bdd.dnot x)) e1 e2
    let ite e1 e2 e3 = Env.mapterop Cudd.Bdd.ite e1 e2 e3

    let is_true e = Cudd.Bdd.is_true e.val0
    let is_false e = Cudd.Bdd.is_false e.val0
    let is_cst e = Cudd.Bdd.is_cst e.val0
    let is_eq e1 e2 =
      Env.check_value2 e1 e2;
      Cudd.Bdd.is_equal e1.val0 e2.val0

    let is_leq e1 e2 =
      Env.check_value2 e1 e2;
      Cudd.Bdd.is_leq e1.val0 e2.val0

    let is_inter_false e1 e2 =
      Env.check_value2 e1 e2;
      Cudd.Bdd.is_inter_empty e1.val0 e2.val0

    let exist (lvar:'a list) e =
      Env.check_lvar e.env lvar;
      make_value
	e.env
	(Cudd.Bdd.exist (Expr0.O.bddsupport e.env lvar) e.val0)

    let forall (lvar:'a list) e =
      Env.check_lvar e.env lvar;
      make_value
	e.env
	(Cudd.Bdd.forall (Expr0.O.bddsupport e.env lvar) e.val0)

    let cofactor e1 e2 = Env.mapbinop Cudd.Bdd.cofactor e1 e2
    let restrict e1 e2 = Env.mapbinop Cudd.Bdd.restrict e1 e2
    let tdrestrict e1 e2 = Env.mapbinop Cudd.Bdd.tdrestrict e1 e2

    let substitute_by_var ?memo e lvarvar =
      make_value e.Env.env
	(Expr0.O.Bool.substitute_by_var ?memo e.Env.env e.Env.val0 lvarvar)
    let substitute ?memo e lvarexpr =
      of_expr (substitute ?memo (to_expr e) lvarexpr)

    let print fmt (x:('a,'b,'c) t) =
      Expr0.O.print_bdd x.env fmt x.val0

  end

  (*  -------------------------------------------------------------------- *)
  (** {5 Bounded integer expressions} *)
  (*  -------------------------------------------------------------------- *)

  module Bint = struct
    type ('a,'b,'c) t = ('b, 'c Int.t) Env.value
    constraint 'b = ('a,'d,'e,'c,'f) Env.O.t

    type ('a,'b,'c) dt = ('a,'b,Cudd.Man.d) t
    type ('a,'b,'c) vt = ('a,'b,Cudd.Man.v) t

    let of_expr0 = Env.make_value
    let get_env = Env.get_env
    let to_expr0 = Env.get_val0

    let of_expr e : ('a,'b,'c) t =
      match e.val0 with
      | `Bint x -> make_value e.env x
      | _ -> failwith "Bint.of_expr: bounded integer expression expected"

    let to_expr (e:('a,'b,'c) t)  =
      make_value e.env (`Bint e.val0)

    let extend_environment e nenv = Env.extend_environment Int.permute e nenv

    let of_int env typ cst =
      match typ with
      | `Bint(sgn,size) ->
	  make_value env (Int.of_int env.cudd sgn size cst)

    let var env (var:'a) =
      make_value env (Expr0.O.Bint.var env var)

    let neg e = Env.mapunop Int.neg e
    let succ e = Env.mapunop Int.succ e
    let pred e = Env.mapunop Int.pred e

    let add e1 e2 = Env.mapbinop Int.add e1 e2
    let sub e1 e2 = Env.mapbinop Int.sub e1 e2
    let mul e1 e2 = Env.mapbinop Int.mul e1 e2
    let shift_left n e = Env.mapunop (Int.shift_left n) e
    let shift_right n e = Env.mapunop (Int.shift_right n) e
    let scale n e = Env.mapunop (Int.scale n) e
    let ite e1 e2 e3 = Env.mapterop Int.ite e1 e2 e3

    let zero e = make_value e.env (Int.zero e.env.cudd e.val0)

    let eq e1 e2 = Env.mapbinop (Int.equal e1.env.cudd) e1 e2
    let supeq e1 e2 = Env.mapbinop (Int.greatereq e1.env.cudd) e1 e2
    let sup e1 e2 = Env.mapbinop (Int.greater e1.env.cudd) e1 e2
    let eq_int e n = make_value e.env (Int.equal_int e.env.cudd e.val0 n)
    let supeq_int e n = make_value e.env (Int.greatereq_int e.env.cudd e.val0 n)
    let sup_int e n = make_value e.env (Int.greater_int e.env.cudd e.val0 n)

    let cofactor e1 e2 = Env.mapbinop Int.cofactor e1 e2
    let restrict e1 e2 = Env.mapbinop Int.restrict e1 e2
    let tdrestrict e1 e2 = Env.mapbinop Int.tdrestrict e1 e2

    let substitute_by_var ?memo e lvarvar =
      make_value e.Env.env
	(Expr0.O.Bint.substitute_by_var ?memo e.Env.env e.Env.val0 lvarvar)
    let substitute ?memo e lvarexpr =
      of_expr (substitute ?memo (to_expr e) lvarexpr)

    let guard_of_int (e:('a,'b,'c) t) (n:int) : ('a,'b,'c) Bool.t =
      make_value e.env (Int.guard_of_int e.env.cudd e.val0 n)

    let guardints (e:('a,'b,'c) t) : (('a,'b,'c) Bool.t*int) list =
      let res = Int.guardints e.env.cudd e.val0 in
      List.map (fun (bdd,n) -> (make_value e.env bdd, n)) res

    let print fmt (x:('a,'b,'c) t) =
      Int.print_minterm (Expr0.O.print_bdd x.env) fmt x.val0

  end

  (*  -------------------------------------------------------------------- *)
  (** {5 Enumerated type expressions} *)
  (*  -------------------------------------------------------------------- *)

  module Benum = struct
    type ('a,'b,'c) t = ('b, 'c Enum.t) Env.value
    constraint 'b = ('a,'d,'e,'c,'f) Env.O.t

    type ('a,'b) dt = ('a,'b,Cudd.Man.d) t
    type ('a,'b) vt = ('a,'b,Cudd.Man.v) t

    let of_expr0 = Env.make_value
    let get_env = Env.get_env
    let to_expr0 = Env.get_val0

    let of_expr e : ('a,'b,'c) t =
      match e.val0 with
      | `Benum x -> make_value e.env x
      | _ -> failwith "Benum.of_expr: bounded integer expression expected"

    let to_expr (e:('a,'b,'c) t) =
      make_value e.env (`Benum e.val0)

    let extend_environment e nenv = Env.extend_environment Enum.permute e nenv

    let var env (var:'a) =
      make_value env (Expr0.O.Benum.var env var)

    let ite e1 e2 e3 = Env.mapterop Enum.ite e1 e2 e3

    let eq e1 e2 = Env.mapbinope Enum.equal e1 e2

    let eq_label e label =
      make_value e.env (Enum.equal_label e.env e.val0 label)

    let cofactor e1 e2 = Env.mapbinop Enum.cofactor e1 e2
    let restrict e1 e2 = Env.mapbinop Enum.restrict e1 e2
    let tdrestrict e1 e2 = Env.mapbinop Enum.tdrestrict e1 e2

    let substitute_by_var ?memo e lvarvar =
      make_value e.Env.env
	(Expr0.O.Benum.substitute_by_var ?memo e.Env.env e.Env.val0 lvarvar)
    let substitute ?memo e lvarexpr =
      of_expr (substitute ?memo (to_expr e) lvarexpr)

    let guard_of_label (e:('a,'b,'c) t) (n:'a) : ('a,'b,'c) Bool.t =
      make_value e.env (Enum.guard_of_label e.env e.val0 n)

    let guardlabels (e:('a,'b,'c) t) : (('a,'b,'c) Bool.t*'a) list =
      let res = Enum.guardlabels e.env e.val0 in
      List.map (fun (bdd,n) -> (make_value e.env bdd, n)) res

    let print fmt (x:('a,'b,'c) t) =
      Enum.print_minterm (Expr0.O.print_bdd x.env) x.env fmt x.val0

  end

  (*  ==================================================================== *)
  (** {4 General expressions} *)
  (*  ==================================================================== *)

  let var env (var:'a) : ('a,'b,'c) expr
      =
    make_value env (Expr0.O.var env var)

  let make = Env.make_value
  let of_expr0 = Env.make_value
  let get_env = Env.get_env
  let to_expr0 = Env.get_val0

  (*  ====================================================================== *)
  (** {4 List of expressions} *)
  (*  ====================================================================== *)

  module List = struct
    type ('a,'b,'c) t = ('b, 'c Expr0.t list) Env.value
    constraint 'b = ('a,'d,'e,'c,'f) Env.O.t

    type ('a,'b) dt = ('a,'b,Cudd.Man.d) t
    type ('a,'b) vt = ('a,'b,Cudd.Man.v) t

(*    let of_lexpr0 : 'b -> 'c Expr0.t list -> ('a,'b,'c) t
 = make_value
*)
    let of_lexpr0 = Env.make_value
    let get_env = Env.get_env
    let to_lexpr0 = Env.get_val0

    let of_lexpr env lexpr1 =
      let lexpr0 = Env.check_lvalue env lexpr1 in
      of_lexpr0 env lexpr0
    let to_lexpr e =
      List.map (fun expr0 -> Env.make_value e.env expr0) e.val0

    let extend_environment e nenv =
      Env.extend_environment Expr0.O.permute_list e nenv

    let print ?first ?sep ?last fmt x =
      Print.list ?first ?sep ?last
	(Expr0.O.print x.env) fmt x.val0
  end

end

(*  ********************************************************************** *)
(** {3 Closed signatures} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {4 Expressions} *)
(*  ====================================================================== *)

type ('a,'b) t = (('a,'b) Env.t, 'b Expr0.t) Env.value
type ('a,'b) expr = ('a,'b) t
  (** Type of general expressions *)

type 'a dt = ('a,Cudd.Man.d) t
type 'a vt = ('a,Cudd.Man.v) t

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

module Bool = struct
  type ('a,'b) t = (('a,'b) Env.t, 'b Cudd.Bdd.t) Env.value

  type 'a dt = ('a,Cudd.Man.d) t
  type 'a vt = ('a,Cudd.Man.v) t

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
module Bint = struct
  type ('a,'b) t = (('a,'b) Env.t, 'b Int.t) Env.value

  type 'a dt = ('a,Cudd.Man.d) t
  type 'a vt = ('a,Cudd.Man.v) t

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
module Benum = struct
  type ('a,'b) t = (('a,'b) Env.t, 'b Enum.t) Env.value

  type 'a dt = ('a,Cudd.Man.d) t
  type 'a vt = ('a,Cudd.Man.v) t

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
module List = struct
  type ('a,'b) t = (('a,'b) Env.t, 'b Expr0.t list) Env.value
  type 'a dt = ('a,Cudd.Man.d) t
  type 'a vt = ('a,Cudd.Man.v) t

  let of_lexpr0 = O.List.of_lexpr0
  let get_env = O.List.get_env
  let to_lexpr0 = O.List.to_lexpr0
  let of_lexpr = O.List.of_lexpr
  let to_lexpr = O.List.to_lexpr
  let extend_environment = O.List.extend_environment
  let print = O.List.print
end
