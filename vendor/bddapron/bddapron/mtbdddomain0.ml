(** Boolean/Numerical domain, with MTBDDs over APRON values *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open ApronDD
open Bdd.Env
open Env

type ('a,'b) man = 'b ApronDD.man
type 'b t = 'b ApronDD.t

let make_man = ApronDD.make_man


(*  ********************************************************************** *)
(** {3 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

  (*  ==================================================================== *)
  (** {4 Interface to ApronDD} *)
  (*  ==================================================================== *)

  let size man = Cudd.Mtbddc.size
  let print ?print_apron env fmt t =
    ApronDD.print
      ?print_apron
      (Bdd.Expr0.O.print_bdd env)
      (Env.string_of_aprondim env)
      fmt t

  let bottom man env =
    ApronDD.bottom
      ~cudd:(env.cudd) man (Apron.Environment.dimension env.ext.eapron)
  let top man env =
    ApronDD.top
      ~cudd:(env.cudd) man (Apron.Environment.dimension env.ext.eapron)
  let of_apron man env abs0 =
    ApronDD.cst ~cudd:(env.cudd) man abs0
  let is_bottom = ApronDD.is_bottom
  let is_top = ApronDD.is_top
  let is_leq = ApronDD.is_leq
  let is_eq = ApronDD.is_eq
  let to_bddapron (man:('a,'b) man) (x:'b t) =
    let tab = Cudd.Mtbddc.guardleafs x in
    Array.fold_left
      (begin fun res ((bdd,abs) as pair) ->
	if Apron.Abstract0.is_bottom man.apron abs
	then res
	else pair::res
      end)
      [] tab

  let meet = ApronDD.meet
  let join = ApronDD.join
  let widening = ApronDD.widening
  let widening_threshold = ApronDD.widening_threshold

  let of_bddapron man env (lbddabs0:('a Expr0.Bool.t * 'b Apron.Abstract0.t) list) =
    let bot = bottom man env in
    List.fold_left
      (fun res (bdd,abs0) ->
	join man res
	  (Cudd.Mtbddc.ite bdd (of_apron man env abs0) bot))
      bot
      lbddabs0

  (*  ==================================================================== *)
  (** {4 Meet with an elementary condition, cofactors} *)
  (*  ==================================================================== *)

  let meet_idcondb
      (man:('a,'b) man)
      (env:'c)
      (cond:('a,'c) Cond.O.t)
      (t:'b t)
      (idcondb:int*bool)
      :
      'b t
      =
    let (idcond,b) = idcondb in
    if PMappe.mem idcond env.idcondvar then begin
      let bdd = Cudd.Bdd.ithvar env.cudd idcond in
      let bdd = if b then bdd else Cudd.Bdd.dnot bdd in
      Cudd.Mtbddc.ite bdd t (bottom man env)
    end
    else begin
      let `Apron condition = Bdd.Cond.cond_of_idb cond (idcond,b) in
      let tcons0 = Apronexpr.Condition.to_tcons0 env.symbol env.ext.eapron condition in
      ApronDD.meet_tcons_array man t [|tcons0|]
    end

  (*  ==================================================================== *)
  (** {4 Meet with Boolean formula} *)
  (*  ==================================================================== *)

  let meet_cube man env cond bottom (t:'b t) (condition:'a Expr0.Bool.t) : 'b t =
    let (cubebool,cubecond) = Bdd.Decompose.cube_split cond condition in
    let tcons0_array = Common.tcons0_array_of_cubecond env cond cubecond in
    let nt = ApronDD.meet_tcons_array man t tcons0_array in
    Cudd.Mtbddc.ite cubebool nt bottom

  let meet_condition man env cond (t:'b t) (condition:'a Expr0.Bool.t) : 'b t =
    let bottom = bottom man env in
    let res =
      Bdd.Decompose.descend
	~cudd:env.cudd
	~maxdepth:max_int
	~nocare:(fun (careset,condition,abs) ->
	  Cudd.Bdd.is_false careset || is_bottom man abs
	)
	~cube_of_down:(fun (careset,condition,abs) -> Cudd.Bdd.cube_of_bdd condition)
	~cofactor:(fun (careset,condition,abs) cube ->
	  let ncareset = Cudd.Bdd.cofactor careset cube in
	  let ncondition = Cudd.Bdd.cofactor condition cube in
	  let nabs = meet_cube man env cond bottom abs cube in
	  (ncareset,ncondition,nabs)
	)
	~select:(fun (careset,condition,elt) ->
	  Bdd.Decompose.select_cond_bdd cond condition
	)
	~terminal:(fun ~depth ~newcube ~cube ~down ->
	  let (careset,condition,abs) = down in
	  let res = Cudd.Mtbddc.ite condition abs bottom in
	  if is_bottom man res then None else Some res
	)
	~ite:(fun ~depth ~newcube ~cond ~dthen ~delse ->
	  match (dthen,delse) with
	  | None,None -> None
	  | ox,None | None,ox -> ox
	  | (Some x),(Some y) -> Some (join man x y)
	)
	~down:(cond.Bdd.Cond.careset, condition, t)
    in
    match res with
    | None -> bottom
    | Some x -> x

  (*  ==================================================================== *)
  (** {4 Assignement/Substitution} *)
  (*  ==================================================================== *)

  let assign_lexpr
      ?relational ?nodependency
      (man:('a,'b) man)
      (env:'c)
      (cond:('a,'c) Cond.O.t)
      (t:'b t)
      (lvar : 'a list) (lexpr: 'a Expr0.t list)
      (odest:'b t option)
      :
      'b t
      =
    assert(List.length lvar = List.length lexpr);
    let (lbvar,tavar) = Descend.split_lvar env.symbol lvar lexpr in
    let eapron = env.ext.eapron in
    let tadim = Array.map (Apron.Environment.dim_of_var eapron) tavar in

    let texpr = Array.of_list lexpr in
    Descend.descend_mtbdd man env cond
      (begin fun t texpr ->
	let (lbexpr,taexpr) = Descend.split_texpr texpr in
	let res =
	  ApronDD.asssub_texpr_array
	    ~asssub_bdd:(fun bdd ->
	      Bdd.Domain0.O.assign_lexpr ?relational ?nodependency
		env bdd lbvar lbexpr
	    )
	    ApronDD.Assign
	    env.symbol man eapron t tadim taexpr odest
	in
	res
      end)
      t texpr

  let substitute_lexpr
      (man:('a,'b) man)
      (env:'c)
      (cond:('a,'c) Cond.O.t)
      (t:'b t)
      (lvar : 'a list) (lexpr: 'a Expr0.t list)
      (odest:'b t option)
      :
      'b t
      =
    assert(List.length lvar = List.length lexpr);
    let (lbvar,tavar) = Descend.split_lvar env.symbol lvar lexpr in
    let eapron = env.ext.eapron in
    let tadim = Array.map (Apron.Environment.dim_of_var eapron) tavar in
    let (org,dest) = match odest with
      | Some x -> (x, t)
      | None -> (top man env, t)
    in
    let texpr = Array.of_list lexpr in
    Descend.descend_mtbdd man env cond
      (begin fun org texpr ->
	let (lbexpr,taexpr) = Descend.split_texpr texpr in
	let res =
	  if tadim=[||] then
	    let compose = Bdd.Expr0.O.composition_of_lvarlexpr env lbvar lbexpr in
	    let res = Cudd.Mtbddc.vectorcompose compose dest in
	    meet man res org
	  else
	    ApronDD.asssub_texpr_array
	      ~asssub_bdd:(fun bdd ->
		Bdd.Domain0.O.substitute_lexpr env bdd lbvar lbexpr
	      )
	      ApronDD.Substitute
	      env.symbol man eapron org tadim taexpr (Some dest)
	in
	res
      end)
      org texpr

  (*  ==================================================================== *)
  (** {4 Forget} *)
  (*  ==================================================================== *)

  let forget_list (man:('a,'b) man) env (t:'b t) (lvar:'a list) =
    if lvar=[] then t
    else begin
      let (supp,tadim) = Common.lvar_split env lvar in
      if Cudd.Bdd.is_true supp then
	ApronDD.forget_array man t tadim
      else begin
	if tadim=[||] then
	  ApronDD.exist man ~supp t
	else begin
	  let op2 = ApronDD.make_op_join man in
	  let op1 = Cudd.User.make_op1
	    (begin fun tu ->
	      let t = Cudd.Mtbddc.get tu in
	      let res = Apron.Abstract0.forget_array man.apron t tadim false in
	      let resu = Cudd.Mtbddc.unique man.ApronDD.table res in
	      resu
	    end)
	  in
	  let existop1 = Cudd.User.make_existop1 ~op1 op2 in
	  let res = Cudd.User.apply_existop1 existop1 ~supp t in
	  Cudd.User.clear_op2 op2;
	  Cudd.User.clear_op1 op1;
	  Cudd.User.clear_existop1 existop1;
	  res
	end
      end
    end

  let forall_bool_list (man:('a,'b) man) env (t:'b t) (lvar:'a list) =
    if lvar=[] then t
    else begin
      let (supp,tadim) = Common.lvar_split env lvar in
      if tadim <> [| |] then
        failwith "Bddapron.mt.forall_bool_list: numerical variables in \
                  universal quantification"
      else
        if Cudd.Bdd.is_true supp then t
        else ApronDD.forall man ~supp t
    end

  let apply_change ~bottom man t change =
    if
      change.cbdd.intro = None &&
      change.cbdd.remove = None
    then
      ApronDD.apply_dimchange2 man t change.capron false
    else if
      change.capron.Apron.Dim.add=None && change.capron.Apron.Dim.remove=None
    then begin
      let bdd = change.cbdd in
      let t = match bdd.intro with
	| None -> t
	| Some perm -> Cudd.Mtbddc.permute t perm
      in
      match bdd.remove with
      | None -> t
      | Some(supp,perm) ->
	  let t = ApronDD.exist man ~supp t in
	  Cudd.Mtbddc.permute t perm
    end
    else begin
      let mtbdd = Cudd.Mapleaf.expansivemapleaf1
	~default:bottom
	~merge:(ApronDD.join man)
	(begin fun guard absu ->
	  let nguard = Bdd.Domain0.O.apply_change guard change.cbdd in
	  let abs = Cudd.Mtbddc.get absu in
	  let nabs = Apron.Abstract0.apply_dimchange2 man.apron abs change.capron false
	  in
	  let nabsu = Cudd.Mtbddc.unique man.ApronDD.table nabs in
	  (nguard,nabsu)
	end)
	t
      in
      mtbdd
    end

  let apply_permutation man (t:'b t) (operm,oapronperm) =
    let res = match oapronperm with
      | None -> t
      | Some apronperm ->
	  ApronDD.permute_dimensions man t apronperm
    in
    let res = match operm with
      | None -> res
      | Some perm ->
	  Cudd.Mtbddc.permute res perm
    in
    res

end

(*  ********************************************************************** *)
(** {3 Closed signature} *)
(*  ********************************************************************** *)

let size = O.size
let print = O.print
let bottom = O.bottom
let top = O.top
let of_apron = O.of_apron
let of_bddapron = O.of_bddapron
let is_bottom = O.is_bottom
let is_top = O.is_top
let is_leq = O.is_leq
let is_eq = O.is_eq
let to_bddapron = O.to_bddapron
let meet = O.meet
let join = O.join
let meet_condition = O.meet_condition
let assign_lexpr = O.assign_lexpr
let substitute_lexpr = O.substitute_lexpr
let forget_list = O.forget_list
let forall_bool_list = O.forall_bool_list
let widening = O.widening
let widening_threshold = O.widening_threshold
let apply_change = O.apply_change
let apply_permutation = O.apply_permutation
