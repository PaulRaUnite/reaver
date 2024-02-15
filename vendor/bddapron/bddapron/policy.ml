(** Policies for BDDAPRON abstract values *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

(* ********************************************************************** *)
(** {3 Boolean expressions as MTBDD on DNF} *)
(* ********************************************************************** *)

(* ====================================================================== *)
(** {4 Disjunctive Normal Form of conditions} *)
(* ====================================================================== *)

module Dnf = struct

  type 'a t = 'a Cond.cond Bdd.Normalform.dnf
      (** Canonicity of the representation is guaranted by a lexicographic
	  ordering based on the order on conditions. *)

  let hash _ (t:'a t) = Hashtbl.hash t

  let conjunction_equal symbol c1 c2 =
    let open Bdd.Normalform in
    match (c1,c2) with
    | (Conjunction l1),(Conjunction l2) ->
	if (List.length l1)=(List.length l2) then
	  List.for_all2
	    (begin fun cond1 cond2 ->
	      (Cond.compare_cond symbol cond1 cond2)=0 end)
	    l1 l2
	else
	  false
    | Cfalse,Cfalse -> true
    | _,_ -> false

  let equal symbol d1 d2 =
    let open Bdd.Normalform in
    match (d1,d2) with
    | (Disjunction l1),(Disjunction l2) ->
	if (List.length l1)=(List.length l2) then
	  List.for_all2
	    (fun d1 d2 -> conjunction_equal symbol d1 d2)
	    l1 l2
	else
	  false
    | Dtrue,Dtrue -> true
    | _,_ -> false

  let print env fmt dnf =
    Bdd.Normalform.print_dnf
      (Cond.print_cond env)
      fmt dnf

  (** Turns a conjunction of BddApron conditions into
      the corresponding set of Apron conditions. *)
  let to_apron_tcons0_array env (conj:'a Cond.cond Bdd.Normalform.conjunction) =
    let open Bdd.Normalform in
    match conj with
    | Conjunction l ->
	let cond_to_tcons0 (c:'a Cond.cond) =
	  match c with
	    `Apron v ->
	      Apronexpr.Condition.to_tcons0
		env.Bdd.Env.symbol env.Bdd.Env.ext.Env.eapron v
	in
	Array.map cond_to_tcons0 (Array.of_list l)
    | Cfalse -> failwith ""

end

(* ====================================================================== *)
(** {4 Boolean expressions as MTBDD with DNF leaves} *)
(* ====================================================================== *)

module DDDnf = struct

  type 'a t = 'a Dnf.t Cudd.Mtbdd.t
  type 'a table = 'a Dnf.t Cudd.Mtbdd.table

  let make_table (symbol:'a Env.symbol) : 'a table =
    Cudd.Mtbdd.make_table
      (Dnf.hash symbol)
      (Dnf.equal symbol)

  let print env cond =
    Cudd.Mtbdd.print
      (Expr0.Bool.print env cond)
      (Dnf.print env)

  let of_expr0
      (table:'a table)
      (env:'a Env.t) (cond:'a Cond.t) (expr:'a Expr0.Bool.t)
      :
      'a t
      =
    let lboolcond = Bdd.Decompose.decompose_bdd_boolcond env cond expr in
    let lbooldnf =
      List.rev_map
	(begin fun (boolexpr,condexpr) ->
	  let dnf =
	    if Cudd.Bdd.is_true condexpr then
	      Bdd.Normalform.dnf_true
	    else begin
	      assert(not (Cudd.Bdd.is_false condexpr));
	      Bdd.Decompose.dnf_of_bdd
		~first:(cond.Bdd.Cond.bddindex0)
		~last:(cond.Bdd.Cond.bddindex)
		(Bdd.Cond.cond_of_idb cond)
		condexpr
	    end
	  in
	  (boolexpr,dnf)
	end)
	lboolcond
    in
    let cudd = env.Bdd.Env.cudd in
    let resfalse = Cudd.Mtbdd.cst cudd table Bdd.Normalform.dnf_false in
    List.fold_left
      (begin fun res (boolcond,dnf) ->
	Cudd.Mtbdd.ite
	  boolcond
	  (Cudd.Mtbdd.cst cudd table dnf)
	  res
      end)
      resfalse lbooldnf

  let is_cst dd =
    Cudd.Mtbdd.is_cst dd &&
      begin
	let dnf = Cudd.Mtbdd.dval dd in
	dnf = Bdd.Normalform.dnf_true || dnf = Bdd.Normalform.dnf_false
      end
 let is_true dd =
    Cudd.Mtbdd.is_cst dd &&
      begin
	let dnf = Cudd.Mtbdd.dval dd in
	dnf = Bdd.Normalform.dnf_true
      end
  let is_false dd =
    Cudd.Mtbdd.is_cst dd &&
      begin
	let dnf = Cudd.Mtbdd.dval dd in
	dnf = Bdd.Normalform.dnf_false
      end
end

(* ====================================================================== *)
(** {4 APRON Policies (for conjunctions of Apron constraints)} *)
(* ====================================================================== *)

let apron_policy_print papron env fmt (p:'a Apron.Policy.t) =
  pp_print_string fmt (Apron.Policy.to_string papron p)

(* ====================================================================== *)
(** {4 Policy for DNF of Apron conditions} *)
(* ====================================================================== *)

module DPolicy = struct
  type 'a t = {
    hash : int;
    dpolicy : 'a Apron.Policy.t Bdd.Normalform.disjunction
  }

  type 'a table = 'a t Cudd.Mtbdd.table

  let equal papron dp1 dp2 =
    dp1.hash = dp2.hash &&
    let open Bdd.Normalform in
    match (dp1.dpolicy,dp2.dpolicy) with
    | (Disjunction l1),(Disjunction l2) ->
	(List.length l1) = (List.length l2) &&
	List.for_all2
	(fun p1 p2 -> Apron.Policy.equal papron p1 p2)
	l1 l2
    | Dtrue,Dtrue -> true
    | _,_ -> false

  let hash dp = dp.hash

  let print papron env fmt dp =
    fprintf fmt
      "(H=%i,%a)"
      dp.hash
      (Bdd.Normalform.print_disjunction
	(apron_policy_print papron env))
      dp.dpolicy

  let of_dpolicy dpolicy =
    { hash = Hashtbl.hash dpolicy; dpolicy = dpolicy }

(*
  let to_conj_list (_,t) = Array.to_list t
  let of_conj_list l =
    let t = Array.of_list l in
    hash_set t
*)
end

(* ********************************************************************** *)
(** {3 Policy, level 0} *)
(* ********************************************************************** *)

module PMtbdddomain0 = struct

  type ('a,'b) man = {
    man : ('a,'b) Mtbdddomain0.man;
    papron : 'b Apron.Policy.man;
    ptable : 'b DPolicy.table;
    betable : 'a DDDnf.table;
    symbol : 'a Env.symbol;
  }
  type 'a t = 'a DPolicy.t Cudd.Mtbdd.t

  let manager_get_manager man = man.man

  (** Builds a generic policy manager based on
      a given Apron policy manager. *)
  let make_man
      ?global
      ~(symbol:'a Env.symbol)
      (papron:'b Apron.Policy.man)
      :
      ('a,'b) man
      =
    let apron = Apron.Policy.manager_get_manager papron in
    let man = Mtbdddomain0.make_man ?global apron in
    {
      man = man;
      papron = papron;
      ptable =
	Cudd.Mtbdd.make_table
	  ~hash:DPolicy.hash ~equal:(DPolicy.equal papron);
      betable = DDDnf.make_table symbol;
      symbol = symbol;
    }

  let equal man (p1:'a t) (p2:'a t) =
    Cudd.Mtbdd.is_equal p1 p2

  let print man env cond fmt (p:'a t) =
    Cudd.Mtbdd.print
      (Expr0.Bool.print env cond)
      (DPolicy.print man.papron env)
      fmt p

  let meet_condition_apply
      (pman:('a,'b) man)
      (env:'a Env.t) (cond:'a Cond.t)
      (policy:'b t)
      (abs:'b Mtbdddomain0.t) (bexpr:'a Expr0.Bool.t)
      :
      'b Mtbdddomain0.t
      =
    let man = pman.man in
    let apron = Apron.Policy.manager_get_manager pman.papron in
    let aenv = env.Bdd.Env.ext.Env.eapron in
    let adim = Apron.Environment.dimension aenv in

    if Cudd.Bdd.is_true bexpr || Mtbdddomain0.is_bottom man abs then
      abs
    else if Cudd.Bdd.is_false bexpr then
      Mtbdddomain0.bottom man env
    else begin
      (* 1. Convert Boolean expression into DNF *)
      let dnf = DDDnf.of_expr0 pman.betable env cond bexpr in

      (* 2. Defining ternary operation *)
      let abottom = Apron.Abstract0.bottom apron adim.Apron.Dim.intd adim.Apron.Dim.reald in
      let op3 =
	Cudd.User.make_op3
	  ~special:(begin fun policy abs bexpr ->
	    if Mtbdddomain0.is_bottom man abs || DDDnf.is_true bexpr then
	      Some abs
	    else if DDDnf.is_false bexpr then
	      Some (Mtbdddomain0.of_apron man env abottom)
	    else
	      None
	  end)
	  (begin fun upolicy uabs udnf ->
	    let policy = Cudd.Mtbdd.get upolicy in
	    let abs = Cudd.Mtbddc.get uabs in
	    let dnf = Cudd.Mtbdd.get udnf in
	    let dpolicy = policy.DPolicy.dpolicy in
	    let dabs =
	      Bdd.Normalform.rev_map2_disjunction
		(begin fun policy conj ->
		  assert(conj<>Bdd.Normalform.conjunction_true);
		  let tcons0 = Dnf.to_apron_tcons0_array env conj in
		  let nabs = Apron.Policy.Abstract0.meet_tcons_array_apply pman.papron policy abs tcons0 in
		  nabs
		end)
		dpolicy dnf
	    in
	    let res = match dabs with
	      | Bdd.Normalform.Disjunction labs ->
		  if labs=[]
		  then abottom
		  else Apron.Abstract0.join_array apron (Array.of_list labs)
	      | Bdd.Normalform.Dtrue -> failwith ""
	    in
	    Cudd.Mtbddc.unique man.ApronDD.table res
	  end)
      in
      (* 3. Applying it *)
      let res = Cudd.User.apply_op3 op3 policy abs dnf in
      res
    end

  let meet_condition_improve
      (pman:('a,'b) man)
      (env:'a Env.t) (cond:'a Cond.t)
      (oddpolicy:'b t option)
      (abs:'b Mtbdddomain0.t) (bexpr:'a Expr0.Bool.t)
      :
      'b t
      =
    let cudd = env.Bdd.Env.cudd in
    let man = pman.man in

    if Cudd.Bdd.is_cst bexpr then
      Cudd.Mtbdd.cst cudd pman.ptable
	(DPolicy.of_dpolicy Bdd.Normalform.Dtrue)
    else begin
      (* 1. Convert Boolean expression into DNF *)
      let dnf = DDDnf.of_expr0 pman.betable env cond bexpr in
      match oddpolicy with
      | None ->
	  (* 2. Defining binary operation *)
	  let op2 =
	    Cudd.User.make_op2
	      ~special:(begin fun ddabs dddnf ->
		if DDDnf.is_cst dddnf then
		  Some (
		    Cudd.Mtbdd.cst cudd pman.ptable
		      (DPolicy.of_dpolicy Bdd.Normalform.Dtrue)
		  )
		else
		  None
	      end)
	      (begin fun uabs udnf ->
		let abs = Cudd.Mtbddc.get uabs in
		let dnf = Cudd.Mtbdd.get udnf in
		let dpolicy =
		  let open Bdd.Normalform in
		  rev_map_disjunction
		    (begin fun conj ->
		      assert(conj<>conjunction_true);
		      let tcons0 = Dnf.to_apron_tcons0_array env conj in
		      let npolicy = Apron.Policy.Abstract0.meet_tcons_array_improve pman.papron None abs tcons0 in
		      npolicy
		    end)
		    dnf
		in
		let dpolicy = DPolicy.of_dpolicy dpolicy in
		Cudd.Mtbdd.unique pman.ptable dpolicy
	      end)
	  in
	  (* 3. Applying it *)
	  Cudd.User.apply_op2 op2 abs dnf
      | Some(ddpolicy) ->
	  (* 2. Defining ternary operation *)
	  let op3 =
	    Cudd.User.make_op3
	      ~special:(begin fun policy abs bexpr ->
		if Mtbdddomain0.is_bottom man abs || DDDnf.is_cst bexpr then
		  Some policy
		else
		  None
	      end)
	      (begin fun upolicy uabs udnf ->
		let policy = Cudd.Mtbdd.get upolicy in
		let abs = Cudd.Mtbddc.get uabs in
		let dnf = Cudd.Mtbdd.get udnf in
		let dpolicy = policy.DPolicy.dpolicy in
		let ndpolicy =
		  Bdd.Normalform.rev_map2_disjunction
		    (begin fun policy conj ->
		      assert(conj<> Bdd.Normalform.conjunction_true);
		      let tcons0 = Dnf.to_apron_tcons0_array env conj in
		      let npolicy = Apron.Policy.Abstract0.meet_tcons_array_improve pman.papron (Some policy) abs tcons0 in
		      npolicy
		    end)
		    dpolicy dnf
		in
		let ndpolicy = DPolicy.of_dpolicy ndpolicy in
		Cudd.Mtbdd.unique pman.ptable ndpolicy
	      end)
	  in
	  (* 3. Applying it *)
	  Cudd.User.apply_op3 op3 ddpolicy abs dnf
    end
end

module PDomain0 = struct
  type ('a,'b,'c,'d,'e,'f) man = {
    man: ('a,'b,'c,'d) Domain0.man;
    pman : 'e;
    print : 'e -> 'a Env.t -> 'a Cond.t -> Format.formatter -> 'f -> unit;
    meet_condition_apply : 'e -> 'a Env.t -> 'a Cond.t -> 'f -> 'd -> 'a Expr0.Bool.t -> 'd;
    meet_condition_improve : 'e -> 'a Env.t -> 'a Cond.t -> 'f option -> 'd -> 'a Expr0.Bool.t -> 'f
  }
      (** Type of generic policy managers.

	  - ['a]: type of symbols
	  - ['b]: as in ['b Apron.Manager.t] ([Box.t], [Polka.strict Polka.t], etc);
	  - ['c]: type of the underlying manager;
	  - ['d]: type of the underlying abstract values of level 0.
	  - ['e]: type of the underlying policy manager
	  - ['f]: type of the underlying policy
      *)

  type ('a,'b) mtbdd = (
    'a,
    'b,
    ('a,'b) Mtbdddomain0.man,
    'b Mtbdddomain0.t,
    ('a,'b) PMtbdddomain0.man,
    'b PMtbdddomain0.t
  ) man


  let manager_get_manager man = man.man
  let print man = man.print man.pman
  let meet_condition_apply man = man.meet_condition_apply man.pman
  let meet_condition_improve man = man.meet_condition_improve man.pman

  let make_mtbdd ?global ~symbol (papron:'b Apron.Policy.man) : ('a,'b) mtbdd =
    let pman = PMtbdddomain0.make_man ?global ~symbol papron in
    let man = Domain0.mtbdd_of_mtbdddomain pman.PMtbdddomain0.man in
    {
      man = man;
      pman = pman;
      print = PMtbdddomain0.print;
      meet_condition_apply = PMtbdddomain0.meet_condition_apply;
      meet_condition_improve = PMtbdddomain0.meet_condition_improve;
    }
end

(* ********************************************************************** *)
(** {3 Policy, level 1} *)
(* ********************************************************************** *)
module PDomain1 = struct
  type ('a,'b,'c,'d,'e,'f) man = ('a,'b,'c,'d,'e,'f) PDomain0.man = {
    man: ('a,'b,'c,'d) Domain0.man;
    pman : 'e;
    print : 'e -> 'a Env.t -> 'a Cond.t -> Format.formatter -> 'f -> unit;
    meet_condition_apply : 'e -> 'a Env.t -> 'a Cond.t -> 'f -> 'd -> 'a Expr0.Bool.t -> 'd;
    meet_condition_improve : 'e -> 'a Env.t -> 'a Cond.t -> 'f option -> 'd -> 'a Expr0.Bool.t -> 'f
  }
      (** Type of generic policy managers.

	  - ['a]: type of symbols
	  - ['b]: as in ['b Apron.Manager.t] ([Box.t], [Polka.strict Polka.t], etc);
	  - ['c]: type of the underlying manager;
	  - ['d]: type of the underlying abstract values of level 0.
	  - ['e]: type of the underlying policy manager
	  - ['f]: type of the underlying policy
      *)
  type ('a,'b) mtbdd = (
    'a,
    'b,
    ('a,'b) Mtbdddomain0.man,
    'b Mtbdddomain0.t,
    ('a,'b) PMtbdddomain0.man,
    'b PMtbdddomain0.t
  ) man

  let make_mtbdd = PDomain0.make_mtbdd

  let manager_get_manager = PDomain0.manager_get_manager
  let print = PDomain0.print
  let meet_condition_apply man cond policy (abs1:('a,'b) Domain1.t) bexpr1
      :
      ('a,'b) Domain1.t
      =
    let env = abs1.Env.env in
    let nabs0 =
      PDomain0.meet_condition_apply man env cond policy
	abs1.Env.val0 (Expr1.Bool.to_expr0 bexpr1)
    in
    Env.make_value env nabs0

  let meet_condition_improve man cond opolicy (abs1:('a,'b) Domain1.t) bexpr1 =
    let env = abs1.Env.env in
    PDomain0.meet_condition_improve man env cond opolicy
      abs1.Env.val0 (Expr1.Bool.to_expr0 bexpr1)

  let meet_condition2_apply
      man policy (abs1:('a,'b) Domain1.t) (bexpr2:'a Expr2.Bool.t)
      :
      ('a,'b) Domain1.t
      =
    let env = abs1.Env.env in
    let bexpr0 =
      Bdd.Domain1.O.check_value Expr0.O.Bool.permute
	bexpr2.Cond.val1 env
    in
    let nabs0 =
      PDomain0.meet_condition_apply man env bexpr2.Cond.cond policy
	abs1.Env.val0 bexpr0
    in
    Env.make_value env nabs0

  let meet_condition2_improve
      man opolicy (abs1:('a,'b) Domain1.t) (bexpr2:'a Expr2.Bool.t)
      =
    let env = abs1.Env.env in
    let bexpr0 =
      Bdd.Domain1.O.check_value Expr0.O.Bool.permute
	bexpr2.Cond.val1 env
    in
    let npolicy =
      PDomain0.meet_condition_improve man env bexpr2.Cond.cond opolicy
	abs1.Env.val0 bexpr0
    in
    npolicy

end

module Mtbdddomain0 = PMtbdddomain0
module Domain0 = PDomain0
module Domain1 = PDomain1
