(** Boolean/Numerical domain with normalized environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Cond
open Cond
open Bdd.Env
open Env

(*  ********************************************************************** *)
(** {3 Generic interface} *)
(*  ********************************************************************** *)

type ('a,'b,'c,'d) man = ('a,'b,'c,'d) Domain0.man
(** Type of generic managers.

    - ['a]: type of symbols
    - ['b]: as in ['b Apron.Manager.t]
	    ([Box.t], [Polka.strict Polka.t], etc);
    - ['c]: type of the underlying manager;
    - ['d]: type of the underlying abstract values of level 0.
*)
type ('a,'b) mtbdd =
  (
    'a,
    'b,
    ('a,'b) Mtbdddomain0.man,
    'b Mtbdddomain0.t
  ) man
type ('a,'b) bdd =
  (
    'a,
    'b,
    ('a,'b) Bdddomain0.man,
    'b Bdddomain0.t
  ) man

type ('a,'d) t = ('a Env.t, 'd) Env.value
(** Type of generic abstract values *)

let canonicalize ?apron man t = Domain0.canonicalize ?apron man t.val0
let print ?print_apron man fmt t = Domain0.print ?print_apron man t.env fmt t.val0
let get_env = Env.get_env
let to_level0 = Env.get_val0
let of_level0 = Env.make_value
let size man t = Domain0.size man t.val0
let bottom man env = make_value env (Domain0.bottom man env)
let top man env = make_value env (Domain0.top man env)
let of_apron man env abs1 =
  let eapron = env.ext.eapron in
  if not (Apron.Environment.equal eapron abs1.Apron.Abstract1.env) then
    failwith "Bddapron.Domain1.of_apron: the APRON environment of the APRON abstract value is different from the numerical part of the BDDAPRON environment"
  ;
  make_value env
    (Domain0.of_apron man env abs1.Apron.Abstract1.abstract0)

let of_bddapron man env lboolabs1 =
    let eapron = env.ext.eapron in
    let lboolabs0 =
      List.map
	(begin fun (boolexpr1,abs1) ->
	  let env1 = Expr1.Bool.get_env boolexpr1 in
	  if not (Env.is_eq env env1) then
	    failwith "Bddapron.domainlevel1.of_bddapron: the BDDAPRON environment of one Boolean expression is different from the expected environment"
	  ;
	  if not (Apron.Environment.equal eapron env1.ext.eapron) then
	    failwith "Bddapron.domainlevel1.of_apron: the APRON environment of one APRON abstract value is different from the numerical part of the expected BDDAPRON environment"
	  ;
	  (Expr1.Bool.to_expr0 boolexpr1, abs1.Apron.Abstract1.abstract0)
	end)
	lboolabs1
    in
    make_value env
      (Domain0.of_bddapron man env lboolabs0)

let is_bottom man t = Domain0.is_bottom man t.val0
let is_top man t = Domain0.is_top man t.val0
let is_leq man t1 t2 =
  Env.check_value2 t1 t2;
  Domain0.is_leq man t1.val0 t2.val0
let is_eq man t1 t2 =
  Env.check_value2 t1 t2;
  Domain0.is_eq man t1.val0 t2.val0
let to_bddapron man t =
  let eapron = t.env.ext.eapron in
  let list0 = Domain0.to_bddapron man t.val0 in
  List.map
    (fun (bdd,abs0) ->
      (Env.make_value t.Env.env bdd,
      {
	Apron.Abstract1.env = eapron;
	Apron.Abstract1.abstract0 = abs0
      }
      ))
    list0

let meet man t1 t2 =
  Env.mapbinop (Domain0.meet man) t1 t2

let join man t1 t2 =
  Env.mapbinop (Domain0.join man) t1 t2

let widening man t1 t2 =
  Env.mapbinop (Domain0.widening man) t1 t2
let widening_threshold man t1 t2 tlincons1 =
  Env.check_value2 t1 t2;
  if not (Apron.Environment.equal (Env.apron t1.Env.env) tlincons1.Apron.Lincons1.array_env) then
    failwith "Bddapron.DomainLevel1.widening_threshold: incompatible APRON environments"
    ;
  Env.make_value t1.Env.env (Domain0.widening_threshold man t1.Env.val0 t2.Env.val0 tlincons1.Apron.Lincons1.lincons0_array)

 let meet_condition man cond t condition
    =
  let condition0 =
    Bdd.Domain1.O.check_value Expr0.O.Bool.permute
      condition t.env
  in
  make_value t.env
    (Domain0.meet_condition man t.env cond t.val0 condition0)

let meet_condition2 man t condition2
    =
  let condition0 =
    Bdd.Domain1.O.check_value Expr0.O.Bool.permute
      condition2.val1 t.env
  in
  make_value t.env
    (Domain0.meet_condition man t.env condition2.cond
      t.val0 condition0)

let assign_lexpr
    ?relational ?nodependency
    man cond
    t lvar lexpr odest
    =
  let lexpr0 = Bdd.Domain1.O.check_lvalue Expr0.O.permute lexpr t.env in
  let odest0 = Env.check_ovalue t.env odest in
  make_value t.env
    (Domain0.assign_lexpr ?relational ?nodependency man
      t.env cond t.val0 lvar lexpr0 odest0)

let assign_listexpr2
    ?relational ?nodependency
    man
    t lvar listexpr2 odest
    =
  let lexpr0 =
    Bdd.Domain1.O.check_value Expr0.O.permute_list listexpr2.val1 t.env
  in
  let odest0 = Env.check_ovalue t.env odest in
  make_value t.env
    (Domain0.assign_lexpr ?relational ?nodependency man
      t.env listexpr2.cond t.val0 lvar lexpr0 odest0)

let substitute_lexpr
    man cond
    t lvar lexpr odest
    =
  let lexpr0 = Bdd.Domain1.O.check_lvalue Expr0.O.permute lexpr t.env in
  let odest0 = Env.check_ovalue t.env odest in
  make_value t.env
    (Domain0.substitute_lexpr man
      t.env cond t.val0 lvar lexpr0 odest0)

let substitute_listexpr2
    man
    t lvar listexpr2 odest
    =
  let lexpr0 =
    Bdd.Domain1.O.check_value Expr0.O.permute_list listexpr2.val1 t.env
  in
  let odest0 = Env.check_ovalue t.env odest in
  make_value t.env
    (Domain0.substitute_lexpr man
      t.env listexpr2.cond t.val0 lvar lexpr0 odest0)

let forget_list man t lvar =
  make_value t.env
    (Domain0.forget_list man t.env t.val0 lvar)

let change_environment man t nenv =
  if Env.is_eq t.env nenv then
    t
  else begin
    let change = Env.compute_change t.env nenv in
    let bottom = Domain0.bottom man nenv in
    make_value nenv
      (Domain0.apply_change ~bottom man t.val0 change)
  end

let unify man t1 t2
    =
  let nenv = Env.lce t1.env t2.env in
  let nt1 = change_environment man t1 nenv in
  let nt2 = change_environment man t2 nenv in
  let res = meet man nt1 nt2 in
  res

let rename man t lvarvar =
  let nenv = Env.copy t.env in
  let perm = Env.rename_vars_with nenv lvarvar in
  make_value nenv
    (Domain0.apply_permutation man t.val0 perm)

let man_get_apron = Domain0.man_get_apron

(*  ********************************************************************** *)
(** {3 Implementation based on {!Mtbdddomain1}} *)
(*  ********************************************************************** *)

let mtbdd_of_mtbdddomain = Domain0.mtbdd_of_mtbdddomain
let make_mtbdd = Domain0.make_mtbdd

let man_is_mtbdd = Domain0.man_is_mtbdd
let man_of_mtbdd = Domain0.man_of_mtbdd
let man_to_mtbdd = Domain0.man_to_mtbdd
let of_mtbdd (manabs:('a,'b) Domain0.mtbdd * ('a, 'b Mtbdddomain0.t) t)
    :
    ('a,'b,'c,'d) man * ('a,'d) t
    =
  Obj.magic manabs
let to_mtbdd (manabs:('a,'b,'c,'d) man * ('a,'d) t)
    :
    ('a,'b) Domain0.mtbdd * ('a, 'b Mtbdddomain0.t) t
    =
  if man_is_mtbdd (fst manabs) then
    Obj.magic manabs
  else
    failwith ""

(*  ********************************************************************** *)
(** {3 Implementation based on {!Bdddomain1}} *)
(*  ********************************************************************** *)

let bdd_of_bdddomain = Domain0.bdd_of_bdddomain
let make_bdd = Domain0.make_bdd

let man_is_bdd = Domain0.man_is_bdd
let man_of_bdd = Domain0.man_of_bdd
let man_to_bdd = Domain0.man_to_bdd
let of_bdd (manabs:('a,'b) Domain0.bdd * ('a, 'b Bdddomain0.t) t)
    :
    ('a,'b,'c,'d) man * ('a,'d) t
    =
  Obj.magic manabs
let to_bdd (manabs:('a,'b,'c,'d) man * ('a,'d) t)
    :
    ('a,'b) Domain0.bdd * ('a, 'b Bdddomain0.t) t
    =
  if man_is_bdd (fst manabs) then
    Obj.magic manabs
  else
    failwith ""
