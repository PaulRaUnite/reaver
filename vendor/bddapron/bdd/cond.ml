(** Normalized condition environments (base module) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

(*  ********************************************************************** *)
(** {3 Datatypes } *)
(*  ********************************************************************** *)

type ('a,'b,'c,'d) t = {
  symbol : 'a Env.symbol;
  compare_cond : 'c -> 'c -> int;
  negate_cond : 'b -> 'c -> 'c;
  support_cond : 'b -> 'c -> 'a PSette.t;
  mutable print_cond : 'b -> Format.formatter -> 'c -> unit;

  mutable cudd : 'd Cudd.Man.t;
    (** CUDD manager *)
  mutable bddindex0 : int;
    (** First index for conditions *)
  mutable bddsize : int;
    (** Number of indices dedicated to conditions *)
  mutable bddindex : int;
    (** Next free index in BDDs used by {!idb_of_cond}. *)
  mutable condidb : ('c,int*bool) PDMappe.t;
    (** Two-way association between a condition and a pair of a
	BDD index and a polarity *)
  mutable supp : 'd Cudd.Bdd.t;
    (** Support of conditions *)
  mutable careset : 'd Cudd.Bdd.t;
    (** Boolean formula indicating which logical combination known
	as true could be exploited for simplification.  For
	instance, [x>=1 => x>=0]. *)
  (** Maximum BDD variables used by conditions; forbids automated enlargement in
      none. *)
  bddmax : int option;
}

(*  ********************************************************************** *)
(** {3 Printing} *)
(*  ********************************************************************** *)

let print (env:'b) fmt (cond:('a,'b,'c,'d) t) =
  fprintf fmt
    "{@[<v>bddindex0 = %i; bddindex = %i; cond = %a;@ supp = %a@ careset = %a@]}"
    cond.bddindex0 cond.bddindex
    (PDMappe.print
      (cond.print_cond env)
      (fun fmt (id,b) -> fprintf fmt "(%i,%b)" id b))
    cond.condidb
    (Cudd.Bdd.print_minterm pp_print_int) cond.supp
    (Cudd.Bdd.print_minterm pp_print_int) cond.careset

(*  ********************************************************************** *)
(** {3 Constructors} *)
(*  ********************************************************************** *)

let compare_idb = Env.compare_idb

(* let setup_groups { cudd; bddindex0; bddsize } = *)
(*   Cudd.Man.group cudd bddindex0 bddsize Cudd.Man.MTR_DEFAULT; *)
(*   (\* Cudd.Man.group cudd 0 (bddindex0 + bddsize) Cudd.Man.MTR_FIXED *\) *)
(*   () *)

let make
    ~symbol
    ~(compare_cond: 'c -> 'c -> int)
    ~(negate_cond:'b -> 'c -> 'c)
    ~(support_cond:('b -> 'c -> 'a PSette.t))
    ~(print_cond: 'b -> Format.formatter -> 'c -> unit)
    ?(bddindex0=100)
    ?(bddsize=300)
    ?bddmax
    (cudd:'d Cudd.Man.t)
    :
    ('a,'b,'c,'d) t
    =
  for i = bddindex0 to bddsize - 1 do ignore (Cudd.Bdd.ithvar cudd i) done;
  {
    symbol = symbol;
    compare_cond = compare_cond;
    negate_cond = negate_cond;
    support_cond = support_cond;
    print_cond = print_cond;
    cudd = cudd;
    bddindex0 = bddindex0;
    bddsize = bddsize;
    bddindex = bddindex0;
    bddmax = bddmax;
    condidb = PDMappe.empty compare_cond compare_idb;
    supp = Cudd.Bdd.dtrue cudd;
    careset = Cudd.Bdd.dtrue cudd;
  }

let copy t = { t with cudd = t.cudd }

(*  ********************************************************************** *)
(** {3 Internal functions} *)
(*  ********************************************************************** *)

let permutation t =
  let perm = Array.init (Cudd.Man.get_bddvar_nb t.cudd) (fun i -> i) in
  let index = ref t.bddindex0 in
  PDMappe.iter
    (begin fun cond (id,b) ->
      if b then begin
	perm.(id) <- !index;
	incr index;
      end
    end)
    t.condidb
  ;
  perm

let permute_with t (perm:int array) : unit
    =
  t.condidb <- PDMappe.fold
    (begin fun cond (id,b) res ->
      PDMappe.add cond (perm.(id),b) res
     end)
    t.condidb
    (PDMappe.empty t.compare_cond compare_idb);
  t.supp <- (Cudd.Bdd.permute t.supp perm);
  t.careset <- (Cudd.Bdd.permute t.careset perm);
  (* NB: auto shrink... *)
  t.bddindex <- PDMappe.fold (fun _ (id, _) -> max id) t.condidb (pred t.bddindex0) + 1;
  ()

let normalize_with t : int array =
  let perm = permutation t in
  permute_with t perm;
  perm

let reduce_with t supp =
  let suppr = Cudd.Bdd.support_diff t.supp supp in
  t.careset <- Cudd.Bdd.exist suppr t.careset;
  t.supp <- Cudd.Bdd.cofactor t.supp suppr;
  let suppr = Cudd.Bdd.list_of_support suppr in
  t.condidb <- List.fold_left
    (fun acc id -> PDMappe.removey (id, true) (PDMappe.removey (id, false) acc))
    t.condidb suppr
  ;
(*
  let setidb =
    List.fold_left
      (fun res id ->
	PSette.add (id,true) (PSette.add (id,false) res)
      )
      (PSette.empty compare_idb)
      suppr
  in
  t.condidb <- PDMappe.diffsety t.condidb setidb;
*)
  ()

let clear t =
  let dtrue = Cudd.Bdd.dtrue t.cudd in
  t.condidb <- (PDMappe.empty t.compare_cond compare_idb);
  t.supp <- dtrue;
  t.careset <- dtrue;
  t.bddindex <- t.bddindex0

let check_normalized (env:'b) (cond:('a,'b,'c,'d) t) : bool
    =
  try
    let index = ref cond.bddindex0 in
    PDMappe.iter
      (begin fun _ (id,b) ->
	if b then begin
	  if id <> !index then begin
	    printf
	      "Bdd.Cond.check_normalized: not normalized at index %i@.env=%a@."
	      !index
	      (print env) cond
	    ;
	    raise Exit
	  end;
	  incr index;
	end
      end)
      cond.condidb
    ;
    true
  with Exit ->
    false

let extend_with (env: (('a, _, _, _, _) Env.O.t as 'b))
    ({ cudd; bddindex0; bddsize } as cond: ('a, 'b, 'c, 'd) t) size =
  cond.bddsize <- bddsize + size;
  for i = bddindex0 + bddsize to bddindex0 + bddsize + size - 1
  do ignore (Cudd.Bdd.ithvar cudd i) done;
  ()

(*  ********************************************************************** *)
(** {3 Operations} *)
(*  ********************************************************************** *)

let cond_of_idb t idb = PDMappe.x_of_y idb t.condidb
let idb_of_cond (env:'b) t cond : int*bool =
  try PDMappe.y_of_x cond t.condidb
  with Not_found ->
    let rec newcond cond =
      try
        let ncond = t.negate_cond env cond in
        let id = t.bddindex in
        if id>=t.bddindex0+t.bddsize then raise Exit;
        let b = (t.compare_cond cond ncond) < 0 in
        t.condidb <- PDMappe.add cond (id,b) t.condidb;
        t.condidb <- PDMappe.add ncond (id,not b) t.condidb;
        t.bddindex <- t.bddindex + 1;
        let bdd = (Cudd.Bdd.ithvar t.cudd id) in
        t.supp <- Cudd.Bdd.dand bdd t.supp;
        if t.bddindex >= t.bddindex0+t.bddsize then raise Exit;
        (id,b)
      with Exit -> match t.bddmax with
        | Some h when t.bddsize < h ->
            extend_with env t (min h (t.bddsize * 2));
            newcond cond
        | _ -> raise Env.Bddindex
    in
    newcond cond

let compute_careset
    (t:('a,'b,'c,'d) t)
    ~(normalized:bool)
    :
    unit
    =
  if false then begin
    let env = Obj.magic (Env.O.make t.cudd) in
    printf "cond=%a@." (print env) t
  end;
  t.careset <- (Cudd.Bdd.dtrue t.cudd);
  let list =
    PMappe.fold
      (begin fun cons idb res ->
	(cons,idb) :: res
      end)
      (PDMappe.mapx t.condidb)
      []
  in
  let list =
    if normalized then list
    else
      List.fast_sort
	(fun (cons1,idb1) (cons2,idb2) -> t.compare_cond cons1 cons2)
	list
  in
  if false then begin
    printf "list=%a@."
      (Print.list (fun fmt (_,(id,b)) -> fprintf fmt "(%i,%b)" id b)) list
  end;
  let rec parcours lblock currentblock = function
    | ((cons1,idb1) as x1)::( ((cons2,idb2)::_) as rest) ->
	let cmp = t.compare_cond cons1 cons2 in
	if cmp=(-3) then
	  let currentblock = List.rev (x1::currentblock) in
	  parcours (currentblock::lblock) [] rest
	else
	  parcours lblock (x1::currentblock) rest
    | [x1] ->
	let currentblock = List.rev (x1::currentblock) in
	currentblock::lblock
    | [] -> []
  in
  let lblock = parcours [] [] list in
  if false then begin
    printf "block=%a@."
      (Print.list (Print.list (fun fmt (_,(id,b)) -> fprintf fmt "(%i,%b)" id b))) lblock
  end;

  let implies (id1,b1) (id2,b2) =
    let bdd2 = Cudd.Bdd.ithvar t.cudd id2 in
    let bdd2 = if b2 then bdd2 else Cudd.Bdd.dnot bdd2 in
    let nbdd1 = Cudd.Bdd.ithvar t.cudd id1 in
    let nbdd1 = if not b1 then nbdd1 else Cudd.Bdd.dnot nbdd1 in
    Cudd.Bdd.dor bdd2 nbdd1
  in
  let rec parcours = function
    | (cons1,idb1)::rest ->
	List.iter
	  (begin fun (cons2,idb2) ->
	    let cmp = t.compare_cond cons1 cons2 in
	    if false then printf "idb1=(%i,%b) idb2=(%i,%b) cmp=%i@." (fst idb1) (snd idb1) (fst idb2) (snd idb2) cmp;
	    if cmp = (-1) then begin
	      t.careset <- Cudd.Bdd.dand t.careset (implies idb1 idb2)
	      ;
	      if false then printf "careset=%a@." (Cudd.Bdd.print pp_print_int) t.careset;
	    end
	  end)
	  rest
	;
	parcours rest
    | _ -> ()
  in
  List.iter parcours lblock;
  ()

let is_leq (cond1:('a,'b1,'c,'d) t) (cond2:('a,'b2,'c,'d) t) : bool =
  cond1==cond2 ||
    cond1.cudd = cond2.cudd &&
      (cond1.bddindex - cond1.bddindex0 <= cond2.bddindex - cond2.bddindex0) &&
      (cond1.condidb==cond2.condidb || PMappe.subset (fun _ _ -> true)
	(PDMappe.mapx cond1.condidb)
	(PDMappe.mapx cond2.condidb))

let is_eq (cond1:('a,'b,'c,'d) t) (cond2:('a,'b,'c,'d) t) : bool =
  cond1==cond2 ||
    cond1.cudd = cond2.cudd &&
      (cond1.bddindex - cond1.bddindex0 = cond2.bddindex - cond2.bddindex0) &&
      (cond1.condidb==cond2.condidb || (PDMappe.equaly cond1.condidb cond2.condidb))

let permutation_of_offset (low:int) (length:int) (offset:int) : int array =
  Array.init length (fun i -> if i >= low then i + offset else i)

let shift (cond:('a,'b,'c,'d) t) (offset:int) : ('a,'b,'c,'d) t =
  let perm = permutation_of_offset cond.bddindex0 cond.bddindex offset in
  let ncond = copy cond in
  ncond.bddindex0 <- ncond.bddindex0 + offset;
  permute_with ncond perm;
  ncond

let shift_with (cond:('a,'b,'c,'d) t) (offset:int) : int array =
  (* XXX: handle groups? *)
  let perm = permutation_of_offset cond.bddindex0 cond.bddindex offset in
  cond.bddindex0 <- cond.bddindex0 + offset;
  cond.bddindex <- cond.bddindex + offset;
  Array.iter (fun i -> ignore (Cudd.Bdd.ithvar cond.cudd i)) perm;
  permute_with cond perm;
  perm

let lce (cond1:('a,'b,'c,'d) t) (cond2:('a,'b,'c,'d) t) : ('a,'b,'c,'d) t =
  if is_leq cond2 cond1 then
    let offset = cond1.bddindex0 - cond2.bddindex0 in
    if offset>=0 then
      cond1
    else
      shift cond1 (-offset)
  else if is_leq cond1 cond2 then
    let offset = cond2.bddindex0 - cond1.bddindex0 in
    if offset>=0 then
      cond2
    else
      shift cond2 (-offset)
  else begin
    let mapcondkid =
      let add k cond (id,b) res =
	if b then PMappe.add cond (k,id) res else res
      in
      let map1 =
	PDMappe.fold (add 1) cond1.condidb (PMappe.empty cond1.compare_cond)
      in
      let map12 =
	PDMappe.fold (add 2) cond2.condidb map1
      in
      map12
    in
    let cond = copy cond1 in
    cond.bddindex0 <- Pervasives.max cond1.bddindex0 cond2.bddindex0;
    cond.bddsize <- Pervasives.max cond1.bddsize cond2.bddsize;
    clear cond;
    PMappe.iter
      (begin fun pcond (k,id) ->
	let ncond = cond_of_idb (if k=1 then cond1 else cond2) (id,false) in
	if cond.bddindex >= cond.bddindex0+cond.bddsize then raise Env.Bddindex;
	cond.condidb <- PDMappe.add pcond (cond.bddindex,true) cond.condidb;
	cond.condidb <- PDMappe.add ncond (cond.bddindex,false) cond.condidb;
	let bdd = (Cudd.Bdd.ithvar cond.cudd cond.bddindex) in
	cond.supp <- Cudd.Bdd.dand cond.supp bdd;
	cond.bddindex <- cond.bddindex + 1
      end)
      mapcondkid
    ;
    compute_careset cond ~normalized:true;
    cond
  end

let permutation12 (cond1:('a,'b,'c,'d) t) (cond2:('a,'b,'c,'d) t) : int array
  =
  assert(is_leq cond1 cond2);
  let perm = Array.init (Cudd.Man.get_bddvar_nb cond1.cudd) (fun i -> i) in
  let offset = ref (cond2.bddindex0 - cond1.bddindex0) in
  PMappe.iter
    (begin fun cons2 (id2,b2) ->
      if b2 then begin
	try
	  let (id1,b1) = PDMappe.y_of_x cons2 cond1.condidb in
	  assert b1;
	  perm.(id1) <- id1 + !offset
	with Not_found ->
	  incr offset;
      end
    end)
    (PDMappe.mapx cond2.condidb)
  ;
  perm

let permutation21 (cond2:('a,'b,'c,'d) t) (cond1:('a,'b,'c,'d) t) : int array
    =
  assert(is_leq cond1 cond2);
  let perm = Array.init (Cudd.Man.get_bddvar_nb cond2.cudd) (fun i -> i) in
  let offset = ref (cond2.bddindex0 - cond1.bddindex0) in
  PMappe.iter
    (begin fun cons2 (id2,b2) ->
      if b2 then begin
	try
	  let (id1,b1) = PDMappe.y_of_x cons2 cond1.condidb in
	  assert b1;
	  perm.(id1 + !offset) <- id1
	with Not_found ->
	  incr offset;
      end
    end)
    (PDMappe.mapx cond2.condidb)
  ;
  perm

(*  ********************************************************************** *)
(** {3 Facility for transient computations} *)
(*  ********************************************************************** *)

(** Facility to save/restore the state of the condition environment, when some
    computations create transient conditions that can safely be forgotten
    afterwards. *)
type ('a,'b,'c,'d) repo =
    {
      cs_bddindex: int;
      cs_condidb: ('c,int*bool) PDMappe.t;
      cs_supp: 'd Cudd.Bdd.t;
      cs_careset: 'd Cudd.Bdd.t;
    }

let save: ('a,'b,'c,'d) t -> ('a,'b,'c,'d) repo = fun c ->
  {
    cs_bddindex = c.bddindex;
    cs_condidb = c.condidb;
    cs_supp = c.supp;
    cs_careset = c.careset;
  }

let restore_with: ('a,'b,'c,'d) repo -> ('a,'b,'c,'d) t -> unit =
  fun { cs_bddindex; cs_condidb; cs_supp; cs_careset } c ->
    c.bddindex <- cs_bddindex;
    c.condidb <- cs_condidb;
    c.supp <- cs_supp;
    c.careset <- cs_careset

(*  ********************************************************************** *)
(** {3 Level 2} *)
(*  ********************************************************************** *)

type ('a,'b) value = {
  cond : 'a;
  val1 : 'b
}

let make_value cond val1 =
  { cond=cond; val1=val1 }
let get_cond v = v.cond
let get_val1 v = v.val1
let get_env v = v.val1.Env.env
let get_val0 v = v.val1.Env.val0
