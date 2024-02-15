(** Extra-operations on formula *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Cond
open Cond
open Bdd.Env
open Env
open Bddleaf


module O = struct
  module Expr0 = struct

    module Bool = struct

      let descend
	  env cond (condition:'a Expr0.Bool.t)
	  =
	Bdd.Decompose.descend
	  ~cudd:env.cudd
	  ~maxdepth:max_int
	  ~nocare:(fun (careset,condition) -> Cudd.Bdd.is_false careset)
	  ~cube_of_down:(fun (careset,condition) -> Cudd.Bdd.cube_of_bdd condition)
	  ~cofactor:(fun (careset,condition) cube ->
	    let ncareset = Cudd.Bdd.cofactor careset cube in
	    let ncondition = Cudd.Bdd.cofactor condition cube in
	    (ncareset,ncondition)
	  )
	  ~select:(fun (careset,condition) ->
	    Bdd.Decompose.select_cond_bdd cond condition
	  )
	  ~down:(cond.Bdd.Cond.careset, condition)

    let to_lconjunction env cond (condition:'a Expr0.Bool.t) : ('a Expr0.Bool.t * 'a Expr0.Bool.t) list
	=
      let res =
	descend
	  env cond condition
	  ~terminal:(fun ~depth ~newcube ~cube ~down ->
	    let (careset,condition) = down in
	    let (cubebool,cubecond) = Bdd.Decompose.cube_split cond cube in
	    Some [(Cudd.Bdd.dand cubebool condition, cubecond)]
	  )
	  ~ite:(fun ~depth ~newcube ~cond ~dthen ~delse ->
	    match (dthen,delse) with
	    | None,None -> None
	    | None,x | x,None -> x
	    | (Some x),(Some y) -> Some(List.rev_append x y)
	  )
      in
      match res with
      | None -> []
      | Some x -> x

    let forget
	man env cond (condition:'a Expr0.Bool.t) lvar : 'a Expr0.Bool.t
	=
      let (bsupp,tadim) = Common.lvar_split env lvar in
      let apron = Domain0.man_get_apron man in
      let ores =
	Bdd.Decompose.descend
	  ~cudd:env.cudd
	  ~maxdepth:max_int
	  ~nocare:(fun (careset,condition,elt) ->
	    Cudd.Bdd.is_false careset ||
	      Apron.Abstract0.is_bottom apron elt.Bddleaf.leaf
	  )
	  ~cube_of_down:(fun (careset,condition,abs) -> Cudd.Bdd.cube_of_bdd condition)
	  ~cofactor:(fun (careset,condition,elt) cube ->
	    let ncareset = Cudd.Bdd.cofactor careset cube in
	    let ncondition = Cudd.Bdd.cofactor condition cube in
	    let nelt = Bdddomain0.O.L.meet_cube apron env cond elt cube in
	    (ncareset,ncondition,nelt)
	  )
	  ~select:(fun (careset,condition,elt) ->
	    Bdd.Decompose.select_cond_bdd cond condition
	  )
	  ~terminal:(fun ~depth ~newcube ~cube ~down ->
	    let (careset,condition,elt) = down in
	    let nguard = Cudd.Bdd.existand bsupp cube elt.Bddleaf.guard in
	    let nabs = Apron.Abstract0.forget_array apron elt.Bddleaf.leaf tadim false in
	    let tcons_array = Apron.Abstract0.to_tcons_array apron nabs in
	    let bddcond =
	      Array.fold_left
		(begin fun res tcons0 ->
		  let condition =
		    Apronexpr.Condition.of_tcons0
		      env.symbol
		      (fun var -> Bdd.Env.typ_of_var env var)
		      env.ext.eapron
		      tcons0
		  in
		  let bdd = match condition with
		    | `Bool b -> (if b then Cudd.Bdd.dtrue else Cudd.Bdd.dfalse) env.cudd
		    | `Cond condition ->
			let (id,b) = Bdd.Cond.idb_of_cond env cond (`Apron condition) in
			let bdd = Cudd.Bdd.ithvar env.cudd id in
			if b then bdd else Cudd.Bdd.dnot bdd
		  in
		  Cudd.Bdd.dand res bdd
		end)
		(Cudd.Bdd.dtrue env.cudd) tcons_array
	    in
	    Some (Cudd.Bdd.dand nguard bddcond)
	  )
	  ~ite:(fun ~depth ~newcube ~cond ~dthen ~delse ->
	    let dthen = match dthen with
	      | None -> Cudd.Bdd.dfalse env.cudd
	      | Some x -> x
	    in
	    let delse = match delse with
	      | None -> Cudd.Bdd.dfalse env.cudd
	      | Some x -> x
	    in
	    Some(Cudd.Bdd.dor dthen delse)
	  )
	  ~down:(
	    cond.Bdd.Cond.careset,
	    condition,
	    let dim = Apron.Environment.dimension env.ext.eapron in
	    {
	      Bddleaf.guard = Cudd.Bdd.dtrue env.cudd;
	      Bddleaf.leaf = Apron.Abstract0.top apron dim.Apron.Dim.intd dim.Apron.Dim.reald
	    }
	  )
      in
      let res = match ores with
	| None -> Cudd.Bdd.dfalse env.cudd
	| Some x -> x
      in
      res

    end

  end

  module Expr1 = struct
    module Bool = struct
      let to_lconjunction cond condition =
	let env = condition.Bdd.Env.env in
	let lconjunction0 = Expr0.Bool.to_lconjunction env cond condition.Bdd.Env.val0 in
	List.map
	  (begin fun (e1,e2) ->
	    (Bdd.Env.make_value env e1,
	    Bdd.Env.make_value env e2)
	  end)
	  lconjunction0
      let forget man cond condition lvar =
	let env = condition.Bdd.Env.env in
	let res0 =
	  Expr0.Bool.forget man env cond condition.Bdd.Env.val0 lvar
	in
	Bdd.Env.make_value env res0
    end
  end
end

include O
