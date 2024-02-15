(** *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Env

type 'a t = 'a Expr0.Bool.t
type dt = Cudd.Man.d t
type vt = Cudd.Man.v t

let size = Cudd.Bdd.size

(*  ********************************************************************** *)
(** {3 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

let print = Expr0.O.Bool.print
let bottom = Expr0.O.Bool.dfalse
let top = Expr0.O.Bool.dtrue
let is_bottom env = Cudd.Bdd.is_false
let is_top env = Cudd.Bdd.is_true
let is_leq = Expr0.O.Bool.is_leq
let is_eq = Expr0.O.Bool.is_eq

let is_variable_unconstrained env abs var =
  Env.check_var env var;
  let tid = PMappe.find var env.vartid in
  Array.fold_left
    (begin fun res id -> res && not (Cudd.Bdd.is_var_in id abs) end)
    true
    tid

let meet = Expr0.O.Bool.dand
let join = Expr0.O.Bool.dor
let meet_condition = meet

module Asssub = struct
  let sort tid tbdd =
    let tidbdd = Array.init (Array.length tid)
      (fun i -> (Array.unsafe_get tid i, Array.unsafe_get tbdd i))
    in
    let man = Cudd.Bdd.manager tbdd.(0) in
    Array.sort
      (fun (id1,_) (id2,_) ->
	(Cudd.Man.level_of_var man id1) -
	(Cudd.Man.level_of_var man id2))
      tidbdd;

    let ntid = Array.init (Array.length tid) (fun i -> fst tidbdd.(i))
    and ntbdd = Array.init (Array.length tbdd) (fun i -> snd tidbdd.(i))
    in
    (ntid,ntbdd)

  let is_equal ta tb =
    try
      for i=0 to (Array.length ta) - 1 do
	let a = Array.unsafe_get ta i
	and b = Array.unsafe_get tb i
	in
	if not (Cudd.Bdd.is_equal a b) then raise Exit;
      done;
      true
    with Exit -> false

  let post bdd tid tf =
    let manager = Cudd.Bdd.manager bdd in
    let rec parcours i tf =
      let id = tid.(i) in
      let f = tf.(0) in
      let length = Array.length tf in
      if length = 1 then begin
	if Cudd.Bdd.is_true f then
	  (Cudd.Bdd.ithvar manager id)
	else if Cudd.Bdd.is_false f then
	  (Cudd.Bdd.dnot (Cudd.Bdd.ithvar manager id))
	else
	  Cudd.Bdd.dtrue manager
      end
      else begin
	let tf = Array.sub tf 1 (length - 1) in
	if (Cudd.Bdd.is_true f) then
	  (Cudd.Bdd.dand (Cudd.Bdd.ithvar manager id) (parcours (i+1) tf))
	else if (Cudd.Bdd.is_false f) then
	  (Cudd.Bdd.dand (Cudd.Bdd.dnot (Cudd.Bdd.ithvar manager id)) (parcours (i+1) tf))
	else
	  let tfa = Array.map (fun g -> Cudd.Bdd.constrain g f) tf in
	  let nf = Cudd.Bdd.dnot f in
	  for i=0 to length-2 do
	    Array.unsafe_set tf i (Cudd.Bdd.constrain (Array.unsafe_get tf i) nf)
	  done;
	  if is_equal tf tfa then
	    parcours (i+1) tf
	  else
	    Cudd.Bdd.ite (Cudd.Bdd.ithvar manager id)
	      (parcours (i+1) tfa)
	      (parcours (i+1) tf)
      end
    in
    parcours 0
      (Array.map
	(fun f -> Cudd.Bdd.constrain f bdd)
	tf)

  let postcondition f tbdd =
    let tid = Array.init (Array.length tbdd) (fun i -> i) in
    let (tid,tbdd) = sort tid tbdd in
    post f tid tbdd
end

let relation_supp_compose_of_lvarlexpr
  env lvar (lexpr:'a Expr0.t list)
  :
  'a Cudd.Bdd.t * 'a Cudd.Bdd.t * 'a Cudd.Bdd.t array
  =
  assert (env.bddincr=2);
  let manager = env.cudd in
  let relation = ref (Cudd.Bdd.dtrue manager) in
  let supp = ref (!relation) in
  let tbdd =
    Array.init (Cudd.Man.get_bddvar_nb manager)
      (fun i -> Cudd.Bdd.ithvar manager i)
  in
  List.iter2
    (begin fun var expr ->
      let tid = Expr0.O.tid_of_var env var in
      let reg = Expr0.O.reg_of_expr expr in
      Array.iteri
	(begin fun i id ->
	  let varid = Cudd.Bdd.ithvar manager id in
	  supp := Cudd.Bdd.dand !supp varid;
	  relation := Cudd.Bdd.dand !relation
	    (Cudd.Bdd.eq (Cudd.Bdd.ithvar manager (id+1)) reg.(i));
	  tbdd.(id+1) <- varid;
	end)
	tid
    end)
    lvar lexpr
  ;
  (!relation,!supp,tbdd)

let assign_lexpr
  ?(relational=false)
  ?(nodependency=false)
  env abs lvar (lexpr:'a Expr0.t list)
  =
  assert(List.length lvar = List.length lexpr);
  if lvar=[] then abs
  else begin
    let res =
      if relational then begin
	let manager = env.cudd in
	if nodependency then begin
	  let supp = Expr0.O.bddsupport env lvar in
	  let image = ref (Cudd.Bdd.exist supp abs) in
	  List.iter2
	    (begin fun var expr ->
	      let tid = Expr0.O.tid_of_var env var in
	      let reg = Expr0.O.reg_of_expr expr in
	      Array.iteri
		(begin fun i id ->
		  let varid = Cudd.Bdd.ithvar manager id in
		  image := Cudd.Bdd.dand !image (Cudd.Bdd.eq varid reg.(i))
		end)
		tid
	    end)
	    lvar lexpr
	  ;
	  !image
	end
	else begin
	  let (relation,supp,tbdd) = 
	    relation_supp_compose_of_lvarlexpr env lvar lexpr
	  in
	  let image = Cudd.Bdd.existand supp abs relation in
	  let image = Cudd.Bdd.vectorcompose tbdd image in
	  image
	end
      end
      else begin
	let tbdd = Expr0.O.composition_of_lvarlexpr env lvar lexpr in
	let image = Asssub.postcondition abs tbdd in
	image
      end
    in
    res
  end

let substitute_lexpr env abs lvar lexpr =
  assert(List.length lvar = List.length lexpr);
  if lvar=[] then
    abs
  else begin
    let tbdd = Expr0.O.composition_of_lvarlexpr env lvar lexpr in
    Cudd.Bdd.vectorcompose tbdd abs
  end

let forget_list env abs lvar =
  Expr0.O.Bool.exist env lvar abs

let apply_change abs change =
  let nabs = match change.Env.intro with
    | None -> abs
    | Some perm -> Cudd.Bdd.permute abs perm
  in
  let nnabs = match change.Env.remove with
      | None -> nabs
      | Some(supp,perm) ->
	  let res = Cudd.Bdd.exist supp nabs in
	  Cudd.Bdd.permute res perm
  in
  nnabs

end

(*  ********************************************************************** *)
(** {3 Closed signature} *)
(*  ********************************************************************** *)

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
let forget_list = O.forget_list
