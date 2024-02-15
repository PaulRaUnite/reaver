(** DDs on top of Apron abstract values (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

type 'a leaf = 'a Apron.Abstract0.t
type 'a t = 'a leaf Cudd.Mtbddc.t
type 'a table = 'a leaf Cudd.Mtbddc.table

type 'a leaf_u = 'a leaf Cudd.Mtbddc.unique

type 'a global = {
  op_is_leq : ('a leaf_u, 'a leaf_u) Cudd.User.test2;
  op_join : ('a leaf_u, 'a leaf_u, 'a leaf_u) Cudd.User.op2;
  op_meet : ('a leaf_u, 'a leaf_u, 'a leaf_u) Cudd.User.op2;
  op_exist : 'a leaf_u Cudd.User.exist;
  op_forall : 'a leaf_u Cudd.User.exist;
}
type 'a man = {
  apron : 'a Apron.Manager.t;
  table : 'a table;
  oglobal : 'a global option;
}

let make_table (apron:'a Apron.Manager.t) : 'a table =
  Cudd.Mtbddc.make_table
    ~hash:Hashtbl.hash
    ~equal:(fun (x:'a Apron.Abstract0.t) (y:'a Apron.Abstract0.t) ->
      x==y ||
	(let dimx = Apron.Abstract0.dimension apron x in
	let dimy = Apron.Abstract0.dimension apron y in
	dimx=dimy && Apron.Abstract0.is_eq apron x y)
    )

let myunique = Cudd.Mtbddc.unique
let myget = Cudd.Mtbddc.get
let mydval = Cudd.Mtbddc.dval

let special_is_leq apron dd1 dd2 =
  if Cudd.Mtbddc.is_cst dd1 && Apron.Abstract0.is_bottom apron (mydval dd1) then
    Some true
  else if Cudd.Mtbddc.is_cst dd2 && Apron.Abstract0.is_bottom apron (mydval dd2) then
    Some false
  else None

let neutral_join x =
  let apron = Apron.Abstract0.manager x in
  Apron.Abstract0.is_bottom apron x

let special_join apron dd1 dd2 =
  if Cudd.Mtbddc.is_cst dd1 && Apron.Abstract0.is_bottom apron (mydval dd1) then
    Some dd2
  else if Cudd.Mtbddc.is_cst dd2 &&  Apron.Abstract0.is_bottom apron (mydval dd2) then
    Some dd1
  else
    None

let special_meet apron dd1 dd2 =
  if Cudd.Mtbddc.is_cst dd1 && Apron.Abstract0.is_bottom apron (mydval dd1) then
    Some dd1
  else if Cudd.Mtbddc.is_cst dd2 &&  Apron.Abstract0.is_bottom apron (mydval dd2) then
    Some dd2
  else
    None

let make_op_join man =
  match man.oglobal with
    | Some global -> global.op_join
    | None ->
	Cudd.User.make_op2
	  ~memo:(Cudd.Memo.Hash(Cudd.Hash.create 2))
	  ~commutative:true ~idempotent:true
	  ~special:(special_join man.apron)
	  (fun x y -> myunique man.table (Apron.Abstract0.join man.apron (myget x) (myget y)))

let make_op_meet man =
  match man.oglobal with
    | Some global -> global.op_meet
    | None ->
	Cudd.User.make_op2
	  ~memo:(Cudd.Memo.Hash(Cudd.Hash.create 2))
	  ~commutative:true ~idempotent:true
	  ~special:(special_meet man.apron)
	  (fun x y -> myunique man.table (Apron.Abstract0.meet man.apron (myget x) (myget y)))

let make_global (apron:'a Apron.Manager.t) (table:'a table) : 'a global =
  let memo = Cudd.Memo.Global in
  let op_is_leq =
    Cudd.User.make_test2 ~memo
      ~symetric:false ~reflexive:true
      ~special:(special_is_leq apron)
      (fun x y -> (Apron.Abstract0.is_leq apron (myget x) (myget y)))
  in
  let op_join =
    Cudd.User.make_op2 ~memo
      ~commutative:true ~idempotent:true
      ~special:(special_join apron)
      (fun x y -> myunique table (Apron.Abstract0.join apron (myget x) (myget y)))
  in
  let op_meet =
    Cudd.User.make_op2 ~memo
      ~commutative:true ~idempotent:true
      ~special:(special_meet apron)
      (fun x y -> myunique table (Apron.Abstract0.meet apron (myget x) (myget y)))
  in
  let op_exist = Cudd.User.make_exist ~memo op_join in
  let op_forall = Cudd.User.make_exist ~memo op_meet in
  {
    op_is_leq = op_is_leq;
    op_join = op_join;
    op_meet = op_meet;
    op_exist = op_exist;
    op_forall = op_forall;
  }

let make_man ?(global=false) (apron:'a Apron.Manager.t) =
  let table = make_table apron in
  let oglobal =
    if global then
      Some(make_global apron table)
    else
      None
  in
  let man = {
    apron = apron;
    table = table;
    oglobal = oglobal
  }
  in
  man

let bottom_of_abs man (mtbdd:'a t) : 'a Apron.Abstract0.t =
  let abs = Cudd.Mtbddc.pick_leaf mtbdd in
  let dim = Apron.Abstract0.dimension man.apron abs in
  Apron.Abstract0.bottom man.apron dim.Apron.Dim.intd dim.Apron.Dim.reald

let print
    ?(print_apron=Apron.Abstract0.print)
    print_bdd string_of_dim fmt (t:'a t)
    =
  if Cudd.Mtbddc.is_cst t then
    fprintf fmt "{ %a }"
      (print_apron string_of_dim) (mydval t)
  else
    let nb = Cudd.Mtbddc.nbpaths t in
    if nb > (float_of_int !Cudd.Man.print_limit) then
      fprintf fmt "idd with %i nodes, %i leaves and %g paths"
	(Cudd.Mtbddc.size t) (Cudd.Mtbddc.nbleaves t) nb
    else begin
      let leaves_u = Cudd.Mtbddc.leaves_u t in
      assert((Array.length leaves_u) >= 2);
      let first = ref true in
      fprintf fmt "{ @[<v>";
      for i=Array.length leaves_u - 1 downto 0 do
	let leaf_u = leaves_u.(i) in
	let leaf = myget leaf_u in
	if not (Apron.Abstract0.is_bottom (Apron.Abstract0.manager leaf) leaf) then begin
	  if !first then first := false else fprintf fmt " or@,";
	  let bdd = Cudd.Mtbddc.guard_of_leaf_u t leaf_u in
	  fprintf fmt "@[<hv>(%a) and@ %a@]"
	    print_bdd bdd (print_apron string_of_dim) leaf;
	end
      done;
      fprintf fmt "@] }"
    end

let cst ~cudd man abs : 'a t =
  Cudd.Mtbddc.cst cudd man.table abs

let bottom ~cudd man dim : 'a t =
  cst cudd man
    (Apron.Abstract0.bottom man.apron dim.Apron.Dim.intd dim.Apron.Dim.reald)
let top ~cudd man dim : 'a t =
  cst cudd man
    (Apron.Abstract0.top man.apron dim.Apron.Dim.intd dim.Apron.Dim.reald)
let is_bottom man (x:'a t) =
  Cudd.Mtbddc.is_cst x && Apron.Abstract0.is_bottom man.apron (mydval x)
let is_top man (x:'a t)  =
  Cudd.Mtbddc.is_cst x && Apron.Abstract0.is_top man.apron (mydval x)

let is_eq (man:'a man) =
  Cudd.Mtbddc.is_equal

let is_leq man (x:'a t) (y:'a t) =
  match man.oglobal with
  | None ->
      Cudd.User.map_test2
	~symetric:false ~reflexive:true
	~special:(special_is_leq man.apron)
	(fun x y -> Apron.Abstract0.is_leq man.apron (myget x) (myget y))
	x y
  | Some global ->
      Cudd.User.apply_test2 global.op_is_leq x y

let join man (x:'a t) (y:'a t) : 'a t  =
  match man.oglobal with
  | None ->
      Cudd.User.map_op2
	~commutative:true ~idempotent:true
	~special:(special_join man.apron)
	(fun x y -> myunique man.table (Apron.Abstract0.join man.apron (myget x) (myget y)))
	x y
  | Some global ->
      Cudd.User.apply_op2 global.op_join x y

let meet man (x:'a t) (y:'a t) : 'a t  =
  match man.oglobal with
  | None ->
      Cudd.User.map_op2
	~commutative:true ~idempotent:true
	~special:(special_meet man.apron)
	(fun x y -> myunique man.table (Apron.Abstract0.meet man.apron (myget x) (myget y)))
	x y
  | Some global ->
      Cudd.User.apply_op2 global.op_meet x y

let widening man (x:'a t) (y:'a t) : 'a t  =
  Cudd.User.map_op2
    ~commutative:false ~idempotent:true
    ~special:(fun dd1 dd2 ->
      if Cudd.Mtbddc.is_cst dd1 && Apron.Abstract0.is_bottom man.apron (mydval dd1)
      then Some dd2 else None
    )
    (fun x y -> myunique man.table (Apron.Abstract0.widening man.apron (myget x) (myget y)))
    x y
let widening_threshold man (x:'a t) (y:'a t) tlincons0 : 'a t  =
  Cudd.User.map_op2
    ~commutative:false ~idempotent:true
    ~special:(fun dd1 dd2 ->
      if Cudd.Mtbddc.is_cst dd1 && Apron.Abstract0.is_bottom man.apron (mydval dd1)
      then Some dd2 else None
    )
    (fun x y -> myunique man.table (Apron.Abstract0.widening_threshold man.apron (myget x) (myget y) tlincons0))
    x y

let meet_tcons_array man (x:'a t) tcons :'a t =
  Cudd.User.map_op1
    (fun x -> myunique man.table (Apron.Abstract0.meet_tcons_array man.apron (myget x) tcons))
    x

let forget_array man (x:'a t) tdim : 'a t =
  Cudd.User.map_op1
    (fun x -> myunique man.table (Apron.Abstract0.forget_array man.apron (myget x) tdim false))
    x

let permute_dimensions man (x:'a t) perm : 'a t =
  Cudd.User.map_op1
    (fun x -> myunique man.table (Apron.Abstract0.permute_dimensions man.apron (myget x) perm))
    x

let add_dimensions  man (x:'a t) change project : 'a t =
  Cudd.User.map_op1
    (fun x ->
      myunique man.table (Apron.Abstract0.add_dimensions man.apron (myget x) change project))
    x
let remove_dimensions man (x:'a t) change : 'a t =
  Cudd.User.map_op1
    (fun x ->
      myunique man.table (Apron.Abstract0.remove_dimensions man.apron (myget x) change))
    x

let apply_dimchange2 man (x:'a t) change2 project =
  Cudd.User.map_op1
    (fun x ->
      myunique man.table (Apron.Abstract0.apply_dimchange2 man.apron (myget x) change2 project))
    x



(* asssub is supposed to be either Apron.Asbstract0.assign or Apron.Asbstract0.substitute *with arguments inverted*
   if assign, we compute assign(org)/\dest
   if substitute, we compute substitute(dest)/\org *)

type asssub = Assign | Substitute

let fun_of_asssub asssub symbol man env org tdim texpr odest =
  let org = myget org in
  let texpr =
    Array.map
      (fun expr -> Apronexpr.to_texpr0 symbol env (Cudd.Mtbdd.get expr))
      texpr
  in
  let res = match asssub with
    | Assign ->
	let odest = match odest with
	  | Some x -> Some(myget x)
	  | None -> None
	in
	Apron.Abstract0.assign_texpr_array man.apron org tdim texpr odest
    | Substitute ->
	let dest = match odest with
	  | Some x -> myget x
	  | _ -> failwith ""
	in
	Apron.Abstract0.substitute_texpr_array man.apron dest tdim texpr (Some org)
  in
  myunique man.table res

let asssub_texpr
    (asssub:asssub)
    symbol man env
    (org:'a t) (dim:Apron.Dim.t) (expr:'b ApronexprDD.t)
    (odest:'a t option)
    : 'a t
    =
  let var =
    symbol.Bdd.Env.unmarshal
      (Apron.Var.to_string
	(Apron.Environment.var_of_dim env dim))
  in
  match odest with
  | None ->
      (* implies assign mode *)
      Cudd.User.map_op2
	~special:(fun ddorg ddexpr ->
	  if Cudd.Mtbddc.is_cst ddorg && Apron.Abstract0.is_bottom man.apron (mydval ddorg)
	  then Some ddorg
	  else if
	    Cudd.Mtbdd.is_cst ddexpr &&
	      match Cudd.Mtbdd.dval ddexpr with
	      | Apronexpr.Lin({
		  Apronexpr.Lin.cst=cst;
		  Apronexpr.Lin.lterm=[(coeff,varexpr)]
		}) when
		  (Mpqf.sgn cst)=0 &&
		  (Mpqf.cmp_int coeff 1)=0 &&
		  (symbol.Bdd.Env.compare var varexpr)=0
		  -> true
	      | _ -> false
	  then Some ddorg
	  else None
	)
	(fun org expr ->
	  fun_of_asssub asssub symbol man env org [|dim|] [|expr|] None
	)
	org expr
  | Some dest ->
      Cudd.User.map_op3
	~special:(fun ddorg dddest ddexpr ->
	  if Cudd.Mtbddc.is_cst ddorg && Apron.Abstract0.is_bottom man.apron (mydval ddorg) then Some ddorg
	  else if Cudd.Mtbddc.is_cst dddest && Apron.Abstract0.is_bottom man.apron (mydval dddest) then Some dddest
	  else None
	)
	(fun org dest expr ->
	  fun_of_asssub asssub symbol man env org [|dim|] [|expr|] (Some dest)
	)
	org dest expr

let asssub_texpr_array
    ?(asssub_bdd : (Cudd.Bdd.vt -> Cudd.Bdd.vt) option)
    (asssub : asssub)
    symbol
    (man:'a man) env
    (org:'a t) (tdim:Apron.Dim.t array) (texpr:'b ApronexprDD.t array)
    (odest:'a t option)
    : 'a t
    =
  let cudd = Cudd.Mtbddc.manager org in
  let bottom = bottom_of_abs man org in
  let bottom_u = myunique man.table bottom in
  let default = Cudd.Mtbddc.cst_u cudd bottom_u in
  let combine = fun (bdd,res1) res2 ->
    join man (Cudd.Mtbddc.ite bdd res1 default) res2
  in
  let absorbant x = neutral_join (myget x) in
  let tabsorbant = Array.make (Array.length texpr) None in
  if (Array.length tdim) = 1 && asssub_bdd = None then
    asssub_texpr asssub symbol man env org tdim.(0) texpr.(0) odest
  else if texpr=[||] then begin
    begin match (asssub_bdd,odest) with
    | None,None -> org
    | None,Some(dest) -> meet man org dest
    | Some (f),None ->
	assert(asssub=Assign);
	Cudd.Mapleaf.combineleaf1
	  ~default ~combine
	  (fun guard (org:'a leaf Cudd.Mtbddc.unique) -> (f guard, Cudd.Mtbddc.cst_u cudd org))
	  org
    | Some(f),Some(dest) ->
	let source = match asssub with
	  | Assign -> org
	  | Substitute -> dest
	in
	let image =
	  Cudd.Mapleaf.combineleaf1
	    ~default ~combine
	    (fun guard x -> (f guard, Cudd.Mtbddc.cst_u cudd x))
	    source
	in
	begin match asssub with
	| Assign -> meet man image dest
	| Substitute -> meet man image org
	end
    end
  end
  else begin
    match (asssub_bdd,odest) with
    | None,None ->
	Cudd.Mapleaf.combineleaf1_array
	  ~default ~combine
	  ~absorbant:absorbant ~tabsorbant
	  (fun guard org texpr ->
	    let res = fun_of_asssub asssub symbol man env org tdim texpr None in
	    (guard,Cudd.Mtbddc.cst_u cudd res)
	  )
	  org texpr
    | None,Some(dest) ->
	Cudd.Mapleaf.combineleaf2_array
	  ~default ~combine
	  ~absorbant1:absorbant ~absorbant2:absorbant ~tabsorbant
	  (fun guard org dest texpr ->
	    let res = fun_of_asssub asssub symbol man env org tdim texpr (Some dest) in
	    (guard,Cudd.Mtbddc.cst_u cudd res)
	  )
	  org dest texpr
    | Some(f),None ->
	Cudd.Mapleaf.combineleaf1_array
	  ~default ~combine ~absorbant:absorbant ~tabsorbant
	  (begin fun guard org texpr ->
	    let nguard = match asssub with
	      | Assign -> f guard
	      | Substitute -> failwith ""
	    in
	    let res = fun_of_asssub asssub symbol man env org tdim texpr None in
	    (nguard,Cudd.Mtbddc.cst_u cudd res)
	  end)
	  org texpr
    | Some(f),Some(dest) ->
	Cudd.Mapleaf.combineleaf1_array
	  ~default ~combine ~absorbant:absorbant ~tabsorbant
	  (begin fun guardorg org texpr ->
	    begin match asssub with
	    | Assign ->
		let nguard = f guardorg in
		let dest = Cudd.Mtbddc.restrict dest nguard in
		let res =
		  Cudd.Mapleaf.combineleaf1
		    ~default ~combine
		    (fun guard dest ->
		      let res = fun_of_asssub asssub symbol man env org tdim texpr (Some dest) in
		      (guard,Cudd.Mtbddc.cst_u cudd res)
		    )
		    dest
		in
		(nguard,res)
	    | Substitute ->
		let res =
		  Cudd.Mapleaf.combineleaf1
		    ~default ~combine
		    (fun guarddest dest ->
		      let nguard = Cudd.Bdd.dand guardorg (f guarddest) in
		      let res = fun_of_asssub asssub symbol man env org tdim texpr (Some dest) in
		      (nguard,Cudd.Mtbddc.cst_u cudd res)
		    )
		    dest
		in
		(Cudd.Bdd.dtrue cudd, res)
	    end
	  end)
	  org texpr
  end

let assign_texpr_array man org tdim texpr odest =
  asssub_texpr_array ?asssub_bdd:None Assign man org tdim texpr odest

(* mieux: mapvectorcomposeapply, si dest vide *)
let substitute_texpr_array man org tdim texpr odest =
  asssub_texpr_array ?asssub_bdd:None Substitute man org tdim texpr odest

let exist man ~(supp:Cudd.Man.v Cudd.Bdd.t) (t:'a t) : 'a t =
  match man.oglobal with
  | None ->
      let op2 = make_op_join man in
      let exist =
	Cudd.User.make_exist
	  ~memo:(Cudd.Memo.Hash(Cudd.Hash.create 2)) op2
      in
      let res = Cudd.User.apply_exist exist ~supp t in
      Cudd.User.clear_op2 op2;
      Cudd.User.clear_exist exist;
      res
  | Some global ->
      Cudd.User.apply_exist global.op_exist ~supp t

let existand
    (man:'a man)
    ~(bottom:'a Apron.Abstract0.t Cudd.Mtbddc.unique)
    ~(supp:Cudd.Bdd.vt) (guard:Cudd.Bdd.vt) (t:'a t)
    :
    'a t
    =
  let op2 =
    match man.oglobal with
      | None ->
	  let bottomdd = Cudd.Mtbddc.cst_u (Cudd.Bdd.manager supp) bottom in
	  let op2 =
	    Cudd.User.make_op2
	      ~memo:(Cudd.Memo.Hash(Cudd.Hash.create 2))
	      ~commutative:true ~idempotent:true
	      ~special:(fun dd1 dd2 ->
		if (Cudd.Mtbddc.is_equal dd1 bottomdd || Cudd.Mtbddc.is_equal dd2 bottomdd) then
		  Some(bottomdd)
		else
		  None
	      )
	      (fun x y -> myunique man.table (Apron.Abstract0.join man.apron (myget x) (myget y)))
	  in
	  op2
      | Some global ->
	  global.op_join
  in
  let existand = Cudd.User.make_existand
    ~memo:(Cudd.Memo.Hash(Cudd.Hash.create 3))
    ~bottom op2
  in
  let res = Cudd.User.apply_existand existand ~supp guard t in
  Cudd.User.clear_op2 op2;
  Cudd.User.clear_existand existand;
  res

let forall man ~(supp:Cudd.Man.v Cudd.Bdd.t) (t:'a t) : 'a t =
  match man.oglobal with
  | None ->
      let op2 = make_op_meet man in
      let forall =
	Cudd.User.make_exist
	  ~memo:(Cudd.Memo.Hash(Cudd.Hash.create 2)) op2
      in
      let res = Cudd.User.apply_exist forall ~supp t in
      Cudd.User.clear_op2 op2;
      Cudd.User.clear_exist forall;
      res
  | Some global ->
      Cudd.User.apply_exist global.op_forall ~supp t
