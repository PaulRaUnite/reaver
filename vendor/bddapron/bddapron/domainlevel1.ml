(** Functor from level 0 to level 1 (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Cond
open Cond
open Bdd.Env
open Env

module type Level0 = sig
  type ('a,'b) man
  type 'b t

  val size : ('a,'b) man -> 'b t -> int
  val print :
    ?print_apron:(
      (int -> string) ->
	Format.formatter -> 'b Apron.Abstract0.t -> unit
    ) ->
    'a Env.t -> Format.formatter -> 'b t -> unit

  val bottom : ('a,'b) man -> 'a Env.t -> 'b t
  val top : ('a,'b) man -> 'a Env.t -> 'b t
  val of_apron : ('a,'b) man -> 'a Env.t -> 'b Apron.Abstract0.t -> 'b t
  val of_bddapron : ('a,'b) man -> 'a Env.t -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list -> 'b t

  val is_bottom : ('a,'b) man -> 'b t -> bool
  val is_top : ('a,'b) man -> 'b t -> bool
  val is_leq : ('a,'b) man -> 'b t -> 'b t -> bool
  val is_eq : ('a,'b) man -> 'b t -> 'b t -> bool
  val to_bddapron :
    ('a,'b) man -> 'b t -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list
  val meet : ('a,'b) man -> 'b t -> 'b t -> 'b t
  val join : ('a,'b) man -> 'b t -> 'b t -> 'b t
  val meet_condition :
    ('a,'b) man -> 'a Env.t -> 'a Cond.t -> 'b t -> 'a Expr0.Bool.t -> 'b t
  val assign_lexpr :
    ?relational:bool -> ?nodependency:bool ->
    ('a,'b) man -> 'a Env.t -> 'a Cond.t ->
    'b t -> 'a list -> 'a Expr0.t list -> 'b t option -> 'b t
  val substitute_lexpr :
    ('a,'b) man -> 'a Env.t -> 'a Cond.t ->
    'b t -> 'a list -> 'a Expr0.t list -> 'b t option -> 'b t
  val forget_list : ('a,'b) man -> 'a Env.t -> 'b t -> 'a list -> 'b t
  val widening : ('a,'b) man -> 'b t -> 'b t -> 'b t
  val widening_threshold : ('a,'b) man -> 'b t -> 'b t -> Apron.Lincons0.t array -> 'b t
  val apply_change :
    bottom:'b t -> ('a,'b) man -> 'b t -> Env.change -> 'b t
  val apply_permutation :
    ('a,'b) man -> 'b t -> int array option * Apron.Dim.perm option -> 'b t
end

module type Level1 = sig
  type ('a,'b) man
  type 'b t0
  type ('a,'b) t = ('a Env.t, 'b t0) Env.value

  val get_env : ('a,'b) t -> 'a Env.t
  val to_level0 : ('a,'b) t -> 'b t0
  val of_level0 : 'a Env.t -> 'b t0 -> ('a,'b) t

  val size : ('a,'b) man -> ('a,'b) t -> int
  val print :
    ?print_apron:(
      (int -> string) ->
	Format.formatter -> 'b Apron.Abstract0.t -> unit
    ) ->
    Format.formatter -> ('a,'b) t -> unit
  val bottom : ('a,'b) man -> 'a Env.t -> ('a,'b) t
  val top : ('a,'b) man -> 'a Env.t -> ('a,'b) t
  val of_apron : ('a,'b) man -> 'a Env.t -> 'b Apron.Abstract1.t -> ('a,'b) t
  val of_bddapron : ('a,'b) man -> 'a Env.t -> ('a Expr1.Bool.t * 'b Apron.Abstract1.t) list -> ('a,'b) t
  val is_bottom : ('a,'b) man -> ('a,'b) t -> bool
  val is_top : ('a,'b) man -> ('a,'b) t -> bool
  val is_leq : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> bool
  val is_eq : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> bool
  val to_bddapron : ('a,'b) man -> ('a,'b) t -> ('a Expr1.Bool.t * 'b Apron.Abstract1.t) list
  val meet : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val join : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val meet_condition : ('a,'b) man -> 'a Cond.t -> ('a,'b) t -> 'a Expr1.Bool.t -> ('a,'b) t
  val meet_condition2 : ('a,'b) man -> ('a,'b) t -> 'a Expr2.Bool.t -> ('a,'b) t

  val assign_lexpr :
    ?relational:bool -> ?nodependency:bool ->
    ('a,'b) man -> 'a Cond.t ->
    ('a,'b) t -> 'a list -> 'a Expr1.t list -> ('a,'b) t option -> ('a,'b) t
  val assign_listexpr2 :
    ?relational:bool -> ?nodependency:bool ->
    ('a,'b) man ->
    ('a,'b) t -> 'a list -> 'a Expr2.List.t -> ('a,'b) t option ->
    ('a,'b) t
  val substitute_lexpr :
    ('a,'b) man -> 'a Cond.t ->
    ('a,'b) t -> 'a list -> 'a Expr1.t list -> ('a,'b) t option -> ('a,'b) t
  val substitute_listexpr2 :
    ('a,'b) man ->
    ('a,'b) t -> 'a list -> 'a Expr2.List.t -> ('a,'b) t option -> ('a,'b) t
  val forget_list :
    ('a,'b) man -> ('a,'b) t -> 'a list -> ('a,'b) t
  val widening : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val widening_threshold : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> Apron.Lincons1.earray -> ('a,'b) t
  val change_environment : ('a,'b) man -> ('a,'b) t -> 'a Env.t -> ('a,'b) t
  val rename : ('a,'b) man -> ('a,'b) t -> ('a*'a) list -> ('a,'b) t
  val unify : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
end

module Make(Level0:Level0) :
  (Level1 with type ('a,'b) man = ('a,'b) Level0.man
	  and type 'b t0 = 'b Level0.t)
  =
struct
  type ('a,'b) man = ('a,'b) Level0.man
  type 'b t0 = 'b Level0.t
  type ('a,'b) t = ('a Env.t, 'b t0) Env.value

  let get_env = Env.get_env
  let to_level0 = Env.get_val0
  let of_level0 = Env.make_value

  let print ?print_apron fmt t = Level0.print ?print_apron t.env fmt t.val0
  let size man t = Level0.size man t.val0
  let bottom man env = make_value env (Level0.bottom man env)
  let top man env = make_value env (Level0.top man env)
  let of_apron man env abs1 =
    let eapron = env.ext.eapron in
    if not (Apron.Environment.equal eapron abs1.Apron.Abstract1.env) then
      failwith "Bddapron.domainlevel1.of_apron: the APRON environment of the APRON abstract value is different from the numerical part of the BDDAPRON environment"
    ;
    make_value env
      (Level0.of_apron man env abs1.Apron.Abstract1.abstract0)
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
      (Level0.of_bddapron man env lboolabs0)

  let is_bottom man t = Level0.is_bottom man t.val0
  let is_top man t = Level0.is_top man t.val0
  let is_leq man t1 t2 =
    Env.check_value2 t1 t2;
    Level0.is_leq man t1.val0 t2.val0
  let is_eq man t1 t2 =
    Env.check_value2 t1 t2;
    Level0.is_eq man t1.val0 t2.val0
  let to_bddapron man t =
    let eapron = t.env.ext.eapron in
    let list0 = Level0.to_bddapron man t.val0 in
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
    Env.mapbinop (Level0.meet man) t1 t2

  let join man t1 t2 =
    Env.mapbinop (Level0.join man) t1 t2

  let widening man t1 t2 =
    Env.mapbinop (Level0.widening man) t1 t2
  let widening_threshold man t1 t2 tlincons1 =
    Env.check_value2 t1 t2;
    if not (Apron.Environment.equal (Env.apron t1.Env.env) tlincons1.Apron.Lincons1.array_env) then
      failwith "Bddapron.DomainLevel1.widening_threshold: incompatible APRON environments"
    ;
    Env.make_value t1.Env.env (Level0.widening_threshold man t1.Env.val0 t2.Env.val0 tlincons1.Apron.Lincons1.lincons0_array)

  let meet_condition man cond t condition
      =
    let condition0 =
      Bdd.Domain1.O.check_value Expr0.O.Bool.permute
	condition t.env
    in
    make_value t.env
      (Level0.meet_condition man t.env cond t.val0 condition0)

  let meet_condition2 man t condition2
      =
    let condition0 =
      Bdd.Domain1.O.check_value Expr0.O.Bool.permute
	condition2.val1 t.env
    in
    make_value t.env
      (Level0.meet_condition man t.env condition2.cond
	t.val0 condition0)

  let assign_lexpr
      ?relational ?nodependency
      man cond
      t lvar lexpr odest
      =
    let lexpr0 = Bdd.Domain1.O.check_lvalue Expr0.O.permute lexpr t.env in
    let odest0 = Env.check_ovalue t.env odest in
    make_value t.env
      (Level0.assign_lexpr ?relational ?nodependency man
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
      (Level0.assign_lexpr ?relational ?nodependency man
	t.env listexpr2.cond t.val0 lvar lexpr0 odest0)

  let substitute_lexpr
      man cond
      t lvar lexpr odest
      =
    let lexpr0 = Bdd.Domain1.O.check_lvalue Expr0.O.permute lexpr t.env in
    let odest0 = Env.check_ovalue t.env odest in
    make_value t.env
      (Level0.substitute_lexpr man
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
      (Level0.substitute_lexpr man
	t.env listexpr2.cond t.val0 lvar lexpr0 odest0)

  let forget_list man t lvar =
    make_value t.env
      (Level0.forget_list man t.env t.val0 lvar)

  let change_environment man t nenv =
    if Env.is_eq t.env nenv then
      t
    else begin
      let change = Env.compute_change t.env nenv in
      let bottom = Level0.bottom man nenv in
      make_value nenv
	(Level0.apply_change ~bottom man t.val0 change)
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
      (Level0.apply_permutation man t.val0 perm)

end
