(** Output of BDDs/MTBDDs *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 Types} *)
(*  ********************************************************************** *)

(** BDD node *)
type bnode =
  | BIte of int * int * bool * int
      (** [BIte(idcond,idnodeThen,signElse,idnodeElse)] *)
  | BTrue
      (** Terminal case. Not needed in principle *)

type 'a bdd = {
  cond : int PSette.t ref;
    (** Reachable conditions *)
  mutable bdef : (int, bnode) PMappe.t;
    (** Global BDDs graph *)
  bhash : ('a Cudd.Bdd.t, int) Hashhe.t;
  mutable blastid : int;
    (** Hashtables and Counters for resp. first free BDD or IDD node *)
}

(*  ********************************************************************** *)
(** {3 BDDs} *)
(*  ********************************************************************** *)

let make_bdd ~cond = {
  cond = cond;
  bdef = PMappe.empty (-);
  bhash = Hashhe.create 23;
  blastid = -1;
}

let signid_of_bdd (db:'a bdd) (bdd:'a Cudd.Bdd.t)
    :
    bool * int
    =
  let rec iter (bdd:'a Cudd.Bdd.t) =
    let res =
      let cmpl = Cudd.Bdd.is_complement bdd in
      let bdd = if cmpl then Cudd.Bdd.dnot bdd else bdd in
      try
	let id = Hashhe.find db.bhash bdd in
	(cmpl,id)
      with Not_found ->
	let bdef =
	  begin match Cudd.Bdd.inspect bdd with
	  | Cudd.Bdd.Ite(cond,dthen,delse) ->
	      db.cond := PSette.add cond !(db.cond);
	      let (b1,id1) = iter dthen in
	      let (b2,id2) = iter delse in
	      assert(not b1);
	      BIte(cond,id1,b2,id2)
	  | Cudd.Bdd.Bool(b) ->
	      assert(b);
	      BTrue
	  end
	in
	db.blastid <- db.blastid + 1;
	Hashhe.add db.bhash bdd db.blastid;
	db.bdef <- PMappe.add db.blastid bdef db.bdef;
	(cmpl,db.blastid)
    in
    res
  in
  iter bdd

(*  ********************************************************************** *)
(** {3 VDDs} *)
(*  ********************************************************************** *)

(** VDD node *)
type 'a vnode =
  | VIte of int * int * int
      (** VIte(idcond,idnodeThen,idnodeElse) *)
  | VCst of 'a
      (** Leaf *)

(** Database *)
type 'a vdd = {
  cond : int PSette.t ref;
    (** Reachable conditions *)
  mutable vdef : (int, 'a vnode) PMappe.t;
    (** Global MTBDDs graph *)
  lhash : ('a, unit) PHashhe.t;
  vhash : ('a Cudd.Vdd.t, int) Hashhe.t;
  mutable vlastid : int;
    (** Hashtables and Counters for MTBDD nodes. *)
}

let make_vdd ~(compare:'a Cudd.PWeakke.compare) ~cond = 
  let hash = compare.Cudd.PWeakke.hash in
  let equal = compare.Cudd.PWeakke.equal in
  {
    cond = cond;
    vdef = PMappe.empty (-);
    lhash = PHashhe.create hash equal 23;
    vhash = Hashhe.create 23;
    vlastid = -1;
  }

let make_mtbdd ~(table:'a Cudd.Mtbdd.table) ~cond = 
  make_vdd ~compare:table.Cudd.PWeakke.compare ~cond

let make_mtbddc ~(table:'a Cudd.Mtbddc.table) ~cond = 
  make_vdd ~compare:table.Cudd.PWeakke.compare ~cond

let id_of_vdd 
    (db:'a vdd) (vdd:'a Cudd.Vdd.t)
    :
    int
    =
  let rec iter (vdd:'a Cudd.Vdd.t) : int
      =
    try
      Hashhe.find db.vhash vdd
    with Not_found ->
      let mdef =
	begin match Cudd.Vdd.inspect vdd with
	| Cudd.Vdd.Ite(cond,dthen,delse) ->
	    db.cond := PSette.add cond !(db.cond);
	    let idthen = iter dthen in
	    let idelse = iter delse in
	    VIte(cond,idthen,idelse)
	| Cudd.Vdd.Leaf(cst) ->
	    PHashhe.replace db.lhash cst ();
	    VCst(cst)
	end
      in
      db.vlastid <- db.vlastid + 1;
      Hashhe.add db.vhash vdd db.vlastid;
      db.vdef <- PMappe.add db.vlastid mdef db.vdef;
      db.vlastid
  in
  iter vdd

(*  ********************************************************************** *)
(** {4 ADDs} *)
(*  ********************************************************************** *)

(** ADD node *)
type anode = 
  | AIte of int * int * int 
      (** RIte(idcond,idnodeThen,idnodeElse) *)
  | ACst of float

(** Database *)
type add = {
  cond : int PSette.t ref;
  mutable adef : (int, anode) PMappe.t;
  mutable lset : float Sette.t;
  ahash : (Cudd.Add.t, int) Hashhe.t;
  mutable alastid : int;
}

let make_add ~cond = 
  {
    cond = cond;
    adef = PMappe.empty (-);
    lset = Sette.empty;
    ahash = Hashhe.create 23;
    alastid = -1;
  }

let id_of_add 
    (db:add) (add:Cudd.Add.t)
    :
    int
    =
  let rec iter (add:Cudd.Add.t) : int
      =
    try
      Hashhe.find db.ahash add
    with Not_found ->
      let mdef =
	begin match Cudd.Add.inspect add with
	| Cudd.Add.Ite(cond,dthen,delse) ->
	    db.cond := PSette.add cond !(db.cond);
	    let idthen = iter dthen in
	    let idelse = iter delse in
	    AIte(cond,idthen,idelse)
	| Cudd.Add.Leaf(cst) ->
	    db.lset <- Sette.add cst db.lset;
	    ACst(cst)
	end
      in
      db.alastid <- db.alastid + 1;
      Hashhe.add db.ahash add db.alastid;
      db.adef <- PMappe.add db.alastid mdef db.adef;
      db.alastid
  in
  iter add

(*  ********************************************************************** *)
(** {3 Iterators} *)
(*  ********************************************************************** *)

let iter_cond_ordered (cond:int PSette.t) (manager:'a Cudd.Man.t) (f:int -> unit) : unit
  =
  let size = Cudd.Man.get_bddvar_nb manager in
  for level=0 to pred size do
    let var = Cudd.Man.var_of_level manager level in
    if PSette.mem var cond then
      f var
  done;
  ()

let iter_bdef_ordered (db:'a bdd) (f:int -> bnode -> unit) : unit
  =
  PMappe.iter f db.bdef

let iter_vdef_ordered (db:'a vdd) (f:int -> 'a vnode -> unit) : unit
  =
  PMappe.iter f db.vdef
