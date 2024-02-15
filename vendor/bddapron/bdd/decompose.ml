(** Separation of Boolean formula in purely Boolean/conditional parts *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Cond
open Env

type vdd = bool Cudd.Vdd.t

let vdd_of_bdd bdd =
  Cudd.(
  let cudd = Cudd.Bdd.manager bdd in
  Cudd.Vdd.ite bdd (Cudd.Vdd.cst cudd true) (Cudd.Vdd.cst cudd false)
  )
let bdd_of_vdd vdd =
  Cudd.Vdd.guard_of_leaf vdd true

type typ = Bool | Cond | Other

type info = {
  mutable minlevelbool : int;
  mutable maxlevelbool : int;
  mutable minlevelcond : int;
  mutable maxlevelcond : int;
  varlevel : int array;
  levelvar : int array;
  vartyp : typ array;
  leveltyp : typ array
}

let make_info env cond =
  let cudd = env.cudd in
  let nb = Cudd.Man.get_bddvar_nb cudd in
  let r = {
    minlevelbool = max_int;
    maxlevelbool = min_int;
    minlevelcond = max_int;
    maxlevelcond = min_int;
    varlevel = Array.make nb 0;
    levelvar = Array.make nb 0;
    vartyp = Array.make nb Other;
    leveltyp = Array.make nb Other;
  }
  in
  for var=env.bddindex0 to pred (env.bddindex0 + env.bddindex) do
    let level = Cudd.Man.level_of_var cudd var in
    if level < r.minlevelbool then r.minlevelbool <- level;
    if level > r.maxlevelbool then r.maxlevelbool <- level;
    r.varlevel.(var) <- level;
    r.levelvar.(level) <- var;
    r.vartyp.(var) <- Bool;
    r.leveltyp.(level) <- Bool;
  done;
  for var=cond.Cond.bddindex0 to pred (cond.Cond.bddindex0 + cond.Cond.bddindex) do
    let level = Cudd.Man.level_of_var cudd var in
    if level < r.minlevelcond then r.minlevelcond <- level;
    if level > r.maxlevelcond then r.maxlevelcond <- level;
    r.varlevel.(var) <- level;
    r.levelvar.(level) <- var;
    r.vartyp.(var) <- Cond;
    r.leveltyp.(level) <- Cond;
  done;
  r

let split_level (bdd:Cudd.Bdd.vt) (level:int) : (Cudd.Bdd.vt * Cudd.Bdd.vt) list
    =
  let vdd = vdd_of_bdd bdd in
  let tcondexpr = Cudd.Vdd.nodes_below_level vdd (Some level) in
  Array.fold_left
    (begin fun res condexpr ->
      if Cudd.Vdd.is_cst condexpr && (Cudd.Vdd.dval condexpr)=false then
	res
      else
	(Cudd.Vdd.guard_of_node vdd condexpr, bdd_of_vdd condexpr)::res
    end)
    [] tcondexpr

let splitpermutation_of_envcond
    env cond typ
    :
    int * (int array * int array) option
    =
  let r = make_info env cond in
  if typ=`BoolCond && r.maxlevelbool < r.minlevelcond then
    (r.minlevelcond,None)
  else if typ=`CondBool && r.maxlevelcond < r.minlevelbool then
    (r.minlevelbool,None)
  else begin
    let permute1 = Array.make (Array.length r.levelvar) 0 in
    let permute2 = Array.make (Array.length r.levelvar) 0 in
    let (blevel,clevel) = match typ with
      | `BoolCond ->
	  let blevel = ref (-1) in
	  let clevel = ref (!blevel + (env.bddindex-env.bddindex0)) in
	  (blevel,clevel)
      | `CondBool ->
	  let clevel = ref (-1) in
	  let blevel = ref (!clevel + (cond.Cond.bddindex-cond.Cond.bddindex0)) in
	  (blevel,clevel)
    in
    Array.iteri
      (begin fun level var ->
	let typ = r.leveltyp.(level) in
	let nlevel = match typ with
	  | Bool -> incr blevel; !blevel
	  | Cond -> incr clevel; !clevel
	  | Other -> level
	in
	let nvar = r.levelvar.(nlevel) in
	permute1.(var) <- nvar;
	permute2.(nvar) <- var;
      end)
      r.levelvar
    ;
    let level = match typ with
      | `BoolCond -> !blevel+1
      | `CondBool -> !clevel+1
    in
    (level,Some(permute1,permute2))
  end

let split_bdd
    ?memo1 ?memo2
    (level,opermutations)
    (bexpr:'a Cudd.Bdd.t)
    :
    ('a Cudd.Bdd.t * 'a Cudd.Bdd.t) list
    =
  match opermutations with
  | None ->
      split_level bexpr level
  | Some(permute1,permute2) ->
      let pbexpr = Cudd.Bdd.permute ?memo:memo1 bexpr permute1 in
      let pres = split_level pbexpr level in
      let memo = match memo2 with
	| None -> Cudd.Memo.Hash(Cudd.Hash.create 1)
	| Some m -> m
      in
      let res =
	List.rev_map
	  (fun (boolexpr,condexpr) ->
	    (Cudd.Bdd.permute ~memo boolexpr permute2,
	    Cudd.Bdd.permute ~memo condexpr permute2))
	  pres
      in
      if memo2=None then Cudd.Memo.clear memo;
      res

let cube_split cond cube =
  let supp = Cudd.Bdd.support cube in
  let suppbool = Cudd.Bdd.support_diff supp cond.supp in
  let cubecond = Cudd.Bdd.exist suppbool cube in
  let cubebool = Cudd.Bdd.cofactor cube cubecond in
  (cubebool,cubecond)

let decompose_bdd_boolcond
    env cond
    (bexpr:'a Cudd.Bdd.t)
    :
    ('a Cudd.Bdd.t * 'a Cudd.Bdd.t) list
    =
  if Cudd.Bdd.is_true bexpr then
    [(bexpr,bexpr)]
  else if Cudd.Bdd.is_false bexpr then
    []
  else
    let splitperm = splitpermutation_of_envcond env cond `BoolCond in
    split_bdd splitperm bexpr

let decompose_bdd_condbool
    env cond
    (bexpr:'a Cudd.Bdd.t)
    :
    ('a Cudd.Bdd.t * 'a Cudd.Bdd.t) list
    =
  if Cudd.Bdd.is_true bexpr then
    [(bexpr,bexpr)]
  else if Cudd.Bdd.is_false bexpr then
    []
  else
    let splitperm = splitpermutation_of_envcond env cond `CondBool in
    split_bdd splitperm bexpr

let bdd_topvar dd = if Cudd.Bdd.is_cst dd then -1 else Cudd.Bdd.topvar dd
let vdd_topvar dd = if Cudd.Vdd.is_cst dd then -1 else Cudd.Vdd.topvar dd

let select_cond = bdd_topvar

let select_cond_bdd cond bdd =
  let inter = Cudd.Bdd.support_inter cond.supp (Cudd.Bdd.support bdd) in
  select_cond inter

let bdd_support_cond cond bdd =
  Cudd.Bdd.support_inter cond.supp (Cudd.Bdd.support bdd)
let vdd_support_cond cond vdd =
  Cudd.Bdd.support_inter cond.supp (Cudd.Vdd.support vdd)

let tbdd_tvdd_support_cond cond (tbdd,tvdd) =
 let cudd = cond.Cond.cudd in
 let supp = ref (Cudd.Bdd.dtrue cudd)
 in
  Array.iter
    (fun bdd ->
      supp := Cudd.Bdd.support_union !supp (bdd_support_cond cond bdd))
    tbdd;
  Array.iter
    (fun vdd ->
      supp := Cudd.Bdd.support_union !supp (vdd_support_cond cond vdd))
    tvdd;
  !supp

let tbdd_tvdd_cofactor (tbdd,tvdd) c =
  (
    Array.map (fun x -> Cudd.Bdd.cofactor x c) tbdd,
    Array.map (fun x -> Cudd.Vdd.cofactor x c) tvdd
  )

let descend
    ~(cudd:'c Cudd.Man.t)
    ~(maxdepth:int)
    ~(nocare:('a -> bool))
    ~(cube_of_down:('a -> 'c Cudd.Bdd.t))
    ~(cofactor:('a -> 'c Cudd.Bdd.t -> 'a))
    ~(select:('a -> int))
    ~(terminal:(depth:int -> newcube:'c Cudd.Bdd.t -> cube:'c Cudd.Bdd.t ->
		 down:'a -> 'b option))
    ~(ite:(depth:int -> newcube:'c Cudd.Bdd.t ->
	    cond:int -> dthen:'b option -> delse:'b option -> 'b option))
    ~(down:'a)
    :
    'b option
    =
  let rec map depth cube down =
    if nocare down then None
    else begin
      let newcube = cube_of_down down in
      let (cube,down) =
	if Cudd.Bdd.is_true newcube
	then (cube,down)
	else (Cudd.Bdd.dand cube newcube, cofactor down newcube)
      in
      if nocare down then None
      else begin
	let cond = select down in
	if (cond<0) then (* End case *)
	  terminal ~depth:max_int ~newcube ~cube ~down
	else if depth>=maxdepth then (* End case *)
	  terminal ~depth ~newcube ~cube ~down
	else begin (* Recursive case *)
	  let var = Cudd.Bdd.ithvar cudd cond in
	  let nvar = Cudd.Bdd.dnot var in
	  let dthen =
	    map (depth+1) (Cudd.Bdd.dand cube var) (cofactor down var)
	  and delse =
	    map (depth+1) (Cudd.Bdd.dand cube nvar) (cofactor down nvar)
	  in
	  ite ~depth ~newcube ~cond ~dthen ~delse
	end
      end
    end
  in
  map 0 (Cudd.Bdd.dtrue cudd) down

let decompose_dd_treecondbool
    ?careset
    ~topvar
    ~support
    ~cofactor
    env cond
    (dd:'a)
    :
    (int, 'a) Normalform.tree
    =
  let cudd = cond.Cond.cudd in
  let dtrue = Cudd.Bdd.dtrue cudd in

  let select =
    let r = make_info env cond in
    if r.maxlevelcond < r.minlevelbool then begin
      fun dd -> topvar dd
    end else begin
      fun dd -> select_cond (support dd)
    end
  in

  let otree = match careset with
    | None ->
	descend
	  ~cudd:cudd
	  ~maxdepth:max_int
	  ~nocare:(fun _ -> false)
	  ~cube_of_down:(fun _ -> dtrue)
	  ~cofactor
	  ~select
	  ~terminal:(fun ~depth ~newcube ~cube ~down ->
	    let lidb = Cudd.Bdd.list_of_cube newcube in
	    Some(lidb,Normalform.Leaf(down))
	  )
	  ~ite:(fun ~depth ~newcube ~cond ~dthen ~delse ->
	    let lidb = Cudd.Bdd.list_of_cube newcube in
	    match (dthen,delse) with
	    | (Some t1),(Some t2) -> Some(lidb,Normalform.Ite(cond,t1,t2))
	    | _ -> failwith ""
	  )
	  ~down:dd
    | Some(careset) ->
	descend
	  ~cudd:cudd
	  ~maxdepth:max_int
	  ~nocare:(fun (dd,careset) -> not (Cudd.Bdd.is_false careset))
	  ~cube_of_down:(fun _ -> dtrue)
	  ~cofactor:(fun (dd,careset) cube -> (cofactor dd cube, Cudd.Bdd.cofactor careset cube))
	  ~select:(fun (dd,careset) -> select dd)
	  ~terminal:(fun ~depth ~newcube ~cube ~down ->
	    let (dd,careset) = down in
	    let lidb = Cudd.Bdd.list_of_cube newcube in
	    Some(lidb,Normalform.Leaf(dd))
	  )
	  ~ite:(fun ~depth ~newcube ~cond ~dthen ~delse ->
	    let lidb = Cudd.Bdd.list_of_cube newcube in
	    match (dthen,delse) with
	    | (Some t1),(Some t2) -> Some(lidb,Normalform.Ite(cond,t1,t2))
	    | (Some t1),None ->
		let (lidb1,d1) = t1 in
		Some((cond,true)::(List.rev_append lidb1 lidb),d1)
	    | None,(Some t2) ->
		let (lidb2,d2) = t2 in
		Some((cond,true)::(List.rev_append lidb2 lidb),d2)
	    | None,None -> failwith ""
	  )
	  ~down:(dd,careset)
  in
  match otree with
  | Some t -> t
  | None -> failwith ""

let decompose_bdd_treecondbool
    env cond
    (bdd:'a Cudd.Bdd.t)
    :
    (int, 'a Cudd.Bdd.t) Normalform.tree
    =
  decompose_dd_treecondbool
    ~careset:bdd
    ~topvar:bdd_topvar
    ~support:(bdd_support_cond cond)
    ~cofactor:Cudd.Bdd.cofactor
    env cond bdd

let decompose_vdd_treecondbool
    ?careset
    env cond
    (vdd:'a Cudd.Vdd.t)
    :
    (int, 'a Cudd.Vdd.t) Normalform.tree
    =
  decompose_dd_treecondbool
    ?careset
    ~topvar:vdd_topvar
    ~support:(vdd_support_cond cond)
    ~cofactor:Cudd.Vdd.cofactor
    env cond vdd

let decompose_tbdd_tvdd_treecondbool
    ?careset
    env cond
    ddarray
    =
  let cudd = cond.Cond.cudd in
  decompose_dd_treecondbool
    ?careset
    ~topvar:(fun (tbdd,tvdd) ->
      let minlevel topvar dd minlevel =
	let var = topvar dd in
	if var<0 then
	  minlevel
	else
	  let level = Cudd.Man.level_of_var cudd var in
	  min minlevel level
      in
      let toplevel = Array.fold_right (minlevel bdd_topvar) tbdd max_int in
      let toplevel = Array.fold_right (minlevel vdd_topvar) tvdd toplevel in
      if toplevel=max_int then -1 else Cudd.Man.var_of_level cudd toplevel
    )
    ~support:(tbdd_tvdd_support_cond cond)
    ~cofactor:tbdd_tvdd_cofactor
    env cond ddarray

let conjunction_of_minterm ?first ?last of_idb minterm =
  let first = match first with
    | Some n -> n
    | None -> 0
  in
  let last =
    let length = Array.length minterm in
    match last with
    | Some n -> min n length
    | None -> length
  in
  let l = ref [] in
  for id=first to pred last do
    begin match minterm.(id) with
    | Cudd.Man.Top -> ()
    | _ as tb -> l := (of_idb (id, (tb=Cudd.Man.True))) :: !l
    end
  done;
  Normalform.Conjunction(!l)

let dnf_of_bdd ?first ?last of_idb bdd =
  let open Normalform in
  if Cudd.Bdd.is_false bdd then
    Disjunction []
  else if Cudd.Bdd.is_true bdd then
    Dtrue
  else begin
    let disj = ref [] in
    Cudd.Bdd.iter_cube
      (fun minterm ->
	let conj = conjunction_of_minterm ?first ?last of_idb minterm in
	disj := conj :: !disj
      )
      bdd
    ;
    Disjunction(!disj)
  end
