(** Boolean (abstract) domain with normalized environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Env

(*  ********************************************************************** *)
(** {3 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

  type ('a,'b,'c) t = ('a,'b,'c) Expr1.O.Bool.t

  type ('a,'b) dt = ('a,'b,Cudd.Man.d) t
  type ('a,'b) vt = ('a,'b,Cudd.Man.v) t

  let of_domain0 = Env.make_value
  let get_env = Env.get_env
  let to_domain0 = Env.get_val0

  let id = fun x -> x
  let of_expr1 = id
  let to_expr1 = id

  let size t = Domain0.size t.val0
  let print fmt t = Domain0.O.print t.env fmt t.val0

  let bottom = Expr1.O.Bool.dfalse
  let top = Expr1.O.Bool.dtrue

  let is_bottom t = Domain0.O.is_bottom t.env t.val0
  let is_top t = Domain0.O.is_top t.env t.val0
  let is_leq = Expr1.O.Bool.is_leq
  let is_eq = Expr1.O.Bool.is_eq

  let is_variable_unconstrained t var =
    Domain0.O.is_variable_unconstrained t.env t.val0 var

  let meet = Expr1.O.Bool.dand
  let join = Expr1.O.Bool.dor

  let check_value permute t nenv =
    if Env.is_eq t.env nenv &&
      t.env.bddindex0 = nenv.bddindex0 then
      t.val0
    else if Env.is_leq t.env nenv &&
      t.env.bddindex0 = nenv.bddindex0 then
	permute t.val0 (Env.permutation12 t.env nenv)
    else
      failwith (Print.sprintf "Bdd.Domain1: the environment of the argument is not a subenvironment of the expected environment@. t.env=%a@.nenv=%a@." Env.print t.env Env.print nenv)

  let check_lvalue permute lt nenv =
    List.map (fun t -> check_value permute t nenv) lt

  let meet_condition t bexpr =
    let bexpr0 = check_value Expr0.O.Bool.permute bexpr t.env in
    make_value t.env (Expr0.O.Bool.dand t.env t.val0 bexpr0)

  let assign_lexpr ?relational ?nodependency (t:('a,'b,'c) t) lvar lexpr =
    if lvar=[] && lexpr=[] then t
    else begin
      Env.check_lvar t.env lvar;
      let lexpr0 = check_lvalue Expr0.O.permute lexpr t.env in
      make_value t.env (Domain0.O.assign_lexpr ?relational ?nodependency t.env t.val0 lvar lexpr0)
    end

  let assign_listexpr ?relational ?nodependency (t:('a,'b,'c) t) lvar lexpr =
    let lexpr0 = check_value Expr0.O.permute_list lexpr t.env in
    if lvar=[] && lexpr0=[] then t
    else begin
      Env.check_lvar t.env lvar;
      make_value t.env (Domain0.O.assign_lexpr ?relational ?nodependency t.env t.val0 lvar lexpr0)
    end

  let substitute_lexpr (t:('a,'b,'c) t) lvar lexpr =
    if lvar=[] && lexpr=[] then t
    else begin
      Env.check_lvar t.env lvar;
      let lexpr0 = check_lvalue Expr0.O.permute lexpr t.env in
      make_value t.env (Domain0.O.substitute_lexpr t.env t.val0 lvar lexpr0)
    end

  let substitute_listexpr (t:('a,'b,'c) t) lvar lexpr =
    let lexpr0 = check_value Expr0.O.permute_list lexpr t.env in
    if lvar=[] && lexpr.val0=[] then t
    else begin
      Env.check_lvar t.env lvar;
      make_value t.env (Domain0.O.substitute_lexpr t.env t.val0 lvar lexpr0)
    end

  let forget_list t lvar =
    Env.check_lvar t.env lvar;
    make_value t.env (Domain0.O.forget_list t.env t.val0 lvar)

  let rename t lvarvar =
    if lvarvar=[] then t else
      let nenv = Env.copy t.env in
      let operm = Env.rename_vars_with nenv lvarvar in
      make_value nenv
	(match operm with
	| None -> t.val0
	| Some perm -> Cudd.Bdd.permute t.val0 perm
	)

  let change_environment abs nenv =
    let change = Env.compute_change abs.Env.env nenv in
    make_value nenv (Domain0.O.apply_change abs.Env.val0 change)
end

(*  ********************************************************************** *)
(** {3 Closed signature} *)
(*  ********************************************************************** *)

type ('a,'b) t = ('a,'b) Expr1.Bool.t
  (** Abstract value *)

type 'a dt = ('a,Cudd.Man.d) t
type 'a vt = ('a,Cudd.Man.v) t

let of_domain0 = O.of_domain0
let get_env = O.get_env
let to_domain0 = O.to_domain0
let of_expr1 = O.of_expr1
let to_expr1 = O.to_expr1

let size = O.size
let print = O.print
let bottom = O.bottom
let top = O.top
let is_bottom = O.is_bottom
let is_top = O.is_top
let is_leq = O.is_leq
let is_eq = O.is_eq
let is_variable_unconstrained = O.is_variable_unconstrained
let meet = O.meet
let join = O.join
let meet_condition = O.meet_condition
let assign_lexpr = O.assign_lexpr
let substitute_lexpr = O.substitute_lexpr
let assign_listexpr = O.assign_listexpr
let substitute_listexpr = O.substitute_listexpr
let forget_list = O.forget_list
let change_environment = O.change_environment
let rename = O.rename
