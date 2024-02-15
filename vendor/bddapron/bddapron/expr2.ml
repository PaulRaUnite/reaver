(** Finite-type and arithmetical expressions with variable and condition environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Cond
open Cond
open Env

(*  ********************************************************************** *)
(** {3 Opened signature} *)
(*  ********************************************************************** *)

module O = struct

  (*  ==================================================================== *)
  (** {4 General expressions} *)
  (*  ==================================================================== *)

  type ('a,'b) t = (('a,'b) Cond.O.t, ('a,'b) Expr1.O.t) Bdd.Cond.value

  type ('a,'b) expr = ('a,'b) t

  let print fmt (expr:('a,'b) t) =
    Expr1.O.print expr.cond fmt expr.val1

  let of_expr0
      ?(normalize=false) ?reduce ?careset
      (env:'b) (cond:('a,'b) Cond.O.t) (expr0:'a Expr0.t)
      :
      ('a,'b) t
      =
    let (cond,expr0) =
      if normalize then begin
	let (cond,lexpr0) = Expr0.O.normalize ?reduce ?careset (cond,[expr0]) in
	(cond, List.hd lexpr0)
      end
      else
	(cond,expr0)
    in
    Bdd.Cond.make_value cond (Env.make_value env expr0)

  let of_expr1
      ?(normalize=false) ?reduce ?careset
      (cond:('a,'b) Cond.O.t) (expr1:('a,'b) Expr1.O.t)
      :
      ('a,'b) t
      =
    if normalize then
      of_expr0 ~normalize ?reduce ?careset expr1.Env.env cond expr1.Env.val0
    else
      Bdd.Cond.make_value cond expr1

  let get_env = Cond.get_env
  let get_cond = Cond.get_cond
  let to_expr0 = Cond.get_val0
  let to_expr1 = Cond.get_val1

  let extend_environment e nenv =
    Bdd.Cond.make_value
      e.cond
      (Expr1.O.extend_environment e.val1 nenv)

  (*  ==================================================================== *)
  (** {4 Boolean expressions} *)
  (*  ==================================================================== *)

  module Bool = struct
    type ('a,'b) t = (('a,'b) Cond.O.t, ('a,'b) Expr1.O.Bool.t) Bdd.Cond.value

    let print fmt (expr:('a,'b) t) =
      Expr1.O.Bool.print expr.cond fmt expr.val1

    let of_expr e =
      let cond = e.cond and expr1 = e.val1 in
      Bdd.Cond.make_value cond (Expr1.O.Bool.of_expr expr1)
    let to_expr e =
      let cond = e.cond and bexpr1 = e.val1 in
      Bdd.Cond.make_value cond (Expr1.O.Bool.to_expr bexpr1)

    let of_expr0
	?normalize ?reduce ?careset
	(env:'b) (cond:('a,'b) Cond.O.t) (bexpr0:'a Expr0.Bool.t)
	:
	('a,'b) t
	=
      let expr0 = ((Expr0.O.Bool.to_expr bexpr0):> 'a Expr0.t) in
      let expr = of_expr0 ?normalize ?reduce ?careset env cond expr0 in
      of_expr expr

    let of_expr1
	?normalize ?reduce ?careset
	(cond:('a,'b) Cond.O.t) (bexpr1:('a,'b) Expr1.O.Bool.t)
	:
	('a,'b) t
	=
      let expr1 = ((Expr1.O.Bool.to_expr bexpr1):> ('a,'b) Expr1.O.t) in
      let expr = of_expr1 ?normalize ?reduce ?careset cond expr1 in
      of_expr expr
    let get_env = Cond.get_env
    let get_cond = Cond.get_cond
    let to_expr0 = Cond.get_val0
    let to_expr1 = Cond.get_val1

    let extend_environment e nenv =
      Bdd.Cond.make_value
	e.cond
	(Expr1.O.Bool.extend_environment e.val1 nenv)

    let is_false e = Expr1.O.Bool.is_false e.cond e.val1
    let is_true e = Expr1.O.Bool.is_true e.cond e.val1

  end

  (*  ==================================================================== *)
  (** {4 List of expressions} *)
  (*  ==================================================================== *)

  module List = struct
    type ('a,'b) t = (('a,'b) Cond.O.t, ('a,'b) Expr1.O.List.t) Bdd.Cond.value

    let print fmt (listexpr:('a,'b) t) =
      Expr1.O.List.print listexpr.cond fmt listexpr.val1

    let of_lexpr0
	?(normalize=false) ?reduce ?careset
	(env:'b) (cond:('a,'b) Cond.O.t) (lexpr0:'a Expr0.t list)
	:
	('a,'b) t
	=
      let (cond,lexpr0) =
	if normalize
	then Expr0.O.normalize ?reduce ?careset (cond,lexpr0)
	else (cond,lexpr0)
      in
      Bdd.Cond.make_value cond (Env.make_value env lexpr0)

    let of_listexpr1
	?(normalize=false) ?reduce ?careset
	(cond:('a,'b) Cond.O.t) (listexpr1:('a,'b) Expr1.O.List.t)
	:
	('a,'b) t
	=
      if normalize then
	of_lexpr0 ~normalize ?reduce ?careset listexpr1.Env.env cond listexpr1.Env.val0
      else
	Bdd.Cond.make_value cond listexpr1

    let of_lexpr1
	?normalize ?reduce ?careset
	(env:'b) (cond:('a,'b) Cond.O.t) (lexpr1:('a,'b) Expr1.O.t list)
	:
	('a,'b) t
	=
      let listexpr1 = Expr1.O.List.of_lexpr env lexpr1 in
      of_listexpr1 ?normalize ?reduce ?careset cond listexpr1

    let extend_environment e nenv =
      Bdd.Cond.make_value
	e.cond
	(Expr1.O.List.extend_environment e.val1 nenv)

    let get_env = Cond.get_env
    let get_cond = Cond.get_cond
    let to_lexpr0 = Cond.get_val0
    let to_listexpr1 = Cond.get_val1
    let to_lexpr1 e = Expr1.O.List.to_lexpr e.Cond.val1
  end
end

(*  ********************************************************************** *)
(** {3 Closed signature} *)
(*  ********************************************************************** *)

type 'a t = ('a Cond.t, 'a Expr1.t) Bdd.Cond.value
type 'a expr = 'a t

let of_expr0 = O.of_expr0
let of_expr1 = O.of_expr1
let get_env = O.get_env
let get_cond = O.get_cond
let to_expr0 = O.to_expr0
let to_expr1 = O.to_expr1
let extend_environment = O.extend_environment
let print = O.print

module Bool = struct

  type 'a t = ('a Cond.t, 'a Expr1.Bool.t) Bdd.Cond.value
  let of_expr0 = O.Bool.of_expr0
  let of_expr1 = O.Bool.of_expr1
  let get_env = O.Bool.get_env
  let get_cond = O.Bool.get_cond
  let to_expr0 = O.Bool.to_expr0
  let to_expr1 = O.Bool.to_expr1
  let of_expr = O.Bool.of_expr
  let to_expr = O.Bool.to_expr
  let extend_environment = O.Bool.extend_environment
  let is_false = O.Bool.is_false
  let is_true = O.Bool.is_true
  let print = O.Bool.print
end

module List = struct
  type 'a t = ('a Cond.t, 'a Expr1.List.t) Bdd.Cond.value
  let of_lexpr0 = O.List.of_lexpr0
  let of_lexpr1 = O.List.of_lexpr1
  let of_listexpr1 = O.List.of_listexpr1
  let get_env = O.List.get_env
  let get_cond = O.List.get_cond
  let to_lexpr0 = O.List.to_lexpr0
  let to_lexpr1 = O.List.to_lexpr1
  let to_listexpr1 = O.List.to_listexpr1
  let extend_environment = O.List.extend_environment
  let print = O.List.print
end
