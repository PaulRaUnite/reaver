(** Normalized managers/environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

(*  ********************************************************************** *)
(** {3 Datatypes } *)
(*  ********************************************************************** *)

exception Bddindex

(** Type defintion *)
type 'a typdef = [
  | `Benum of 'a array
]

(** Types *)
type 'a typ = [
  | `Bool
  | `Bint of (bool * int)
  | `Benum of 'a
]

type 'a group_tree =
  | Var of 'a * int * int
  | Node of 'a group_tree list * int * int * [ `Fixed | `Reord ]

let gidx : 'a group_tree -> int = function
  | Var (_, i, _) | Node (_, i, _, _) -> i
let glen : 'a group_tree -> int = function
  | Var (_, _, l) | Node (_, _, l, _) -> l
let rec gvar : 'a group_tree -> 'a = function
  | Var (v, _, _) -> v
  | Node (g::_, _, _, _) -> gvar g
  | _ -> assert false
let (gtvars: 'a group_tree -> 'a list), (gtlvars: 'a group_tree list -> 'a list) =
  let rec acc_gtvars acc = function
    | Var (v, _, _) -> v :: acc
    | Node (l, _, _, _) -> List.fold_left acc_gtvars acc l
  in
  (fun l -> acc_gtvars [] l),
  (fun l -> List.fold_left acc_gtvars [] l)
let (gtidxs: 'a group_tree -> int list), (gtlidxs: 'a group_tree list -> int list) =
  let rec acc_gtidxs acc = function
    | Var (_, i, _) -> i :: acc
    | Node (l, _, _, _) -> List.fold_left acc_gtidxs acc l
  in
  (fun l -> acc_gtidxs [] l),
  (fun l -> List.fold_left acc_gtidxs [] l)

let gcmpidx a b = let c = compare (gidx a) (gidx b) in assert (c <> 0); c

let insert_gt gt = List.merge gcmpidx [gt]

let pp_gt pp_symb =
  let rec pp_gt fmt = function
    | Var (v, i, 1) ->
        Format.fprintf fmt "%a (%d)" pp_symb v i
    | Var (v, i, l) ->
        Format.fprintf fmt "%a (%d:%d)" pp_symb v i (i+l-1)
    | Node (g, i, l, `Fixed) ->
        Format.fprintf fmt "{F[%d:%d]:@ @[%a@]}" i l pp_gtl g
    | Node (g, i, l, `Reord) ->
        Format.fprintf fmt "{R[%d:%d]:@ @[%a@]}" i l pp_gtl g
  and pp_gtl fmt = function
    | [] -> ()
    | [x] -> Format.fprintf fmt "%a" pp_gt x
    | x::t -> Format.fprintf fmt "%a,@ " pp_gt x; pp_gtl fmt t
  in
  pp_gt

(** Environment *)
type 'a symbol = {
  compare : 'a -> 'a -> int;
  marshal : 'a -> string;
  unmarshal : string -> 'a;
  mutable print : Format.formatter -> 'a -> unit;
}
type ('a,'b,'c,'d,'e) t0 = {
  mutable cudd : 'd Cudd.Man.t;
    (** CUDD manager *)
  mutable typdef : ('a, 'c) PMappe.t;
    (** Named types definitions *)
  mutable vartyp : ('a, 'b) PMappe.t;
    (** Associate to a var/label its type *)
  mutable bddindex0 : int;
    (** First index for finite-type variables *)
  mutable bddsize : int;
    (** Number of indices dedicated to finite-type variables *)
  mutable bddindex : int;
    (** Next free index in BDDs used by [self#add_var]. *)
  mutable bddincr : int;
    (** Increment used by {!add_var} for incrementing
	[bddindex] *)
  mutable idcondvar : (int, 'a) PMappe.t;
    (** Associates to a BDD index the variable involved by it *)
  mutable vartid : ('a, int array) PMappe.t;
  (** (Sorted) array of BDD indices associated to finite-type variables. *)
  mutable groups: 'a group_tree list;
  mutable print_external_idcondb : Format.formatter -> int*bool -> unit;
    (** Printing conditions not managed by the environment..
	By default, [pp_print_int]. *)
  mutable ext : 'e;
  symbol : 'a symbol;
  copy_ext : 'e -> 'e;
}

let compare_idb (id1,b1) (id2,b2) =
  let res = id1-id2 in
  if res!=0 then
    res
  else
    (if b1 then 1 else 0) - (if b2 then 1 else 0)

let print_typ print_symbol (fmt:Format.formatter) typ = match typ with
  | `Bool -> pp_print_string fmt "bool"
  | `Bint(sign,size) -> fprintf fmt "%cint[%i]" (if sign then 's' else 'u') size
  | `Benum s -> print_symbol fmt s
  | _ -> pp_print_string fmt "Bdd.Env.print_typ: unknown type"

let print_typdef print_symbol (fmt:Format.formatter) typdef = match typdef with
  | `Benum array ->
      fprintf fmt "benum{%a}"
	(Print.array ~first:"" ~sep:"," ~last:"" print_symbol)
	array
  | _ -> pp_print_string fmt "Bdd.Env.print_typdef: unknown type definition"

let print_tid (fmt:Format.formatter) (tid:int array) : unit =
  Print.array Format.pp_print_int fmt tid

let notfound format =
  let buffer = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buffer in
  Format.kfprintf
    (begin fun fmt ->
      Format.pp_print_flush fmt ();
      let s = Buffer.contents buffer in
      Buffer.clear buffer;
      raise (Failure s)
    end)
    fmt
    format

let string_remove_null s =
  let l = String.length s in
  let nl = ref l in
  for i=0 to l-1 do
    match String.unsafe_get s i with
    | '\\' | '\000'  -> incr nl
    | _ -> ()
  done;
  if !nl = l then
    s
  else begin
    let ns = Bytes.create !nl in
    let ni = ref 0 in
    for i=0 to l-1 do
      match String.unsafe_get s i with
      | '\\' ->
	  Bytes.set ns !ni '\\';
	  incr ni;
	  Bytes.set ns !ni '\\';
	  incr ni
      | '\000' ->
	  Bytes.set ns !ni '\\';
	  incr ni;
	  Bytes.set ns !ni '0';
	  incr ni;
      | _ as c ->
	  Bytes.set ns !ni c;
	  incr ni;
    done;
    assert(!ni == !nl);
    Bytes.to_string ns
  end

let string_add_null ns =
  let nl = String.length ns in
  let l = ref nl in
  let ni = ref 0 in
  while !ni < nl do
    if String.unsafe_get ns !ni = '\\' then begin
      incr ni;
      decr l
    end;
    incr ni
  done;
  if !l == nl then
    ns
  else begin
    let s = Bytes.create !l in
    ni := 0;
    for i=0 to !l - 1 do
      match ns.[!ni] with
      | '\\' ->
	  Bytes.set s i
	    begin match ns.[!ni + 1] with
	    | '\\' -> '\\'
	    | _ -> '\000'
	    end;
	  ni := !ni + 2
      | _ as c ->
	  Bytes.set s i c;
	  incr ni
    done;
    Bytes.to_string s
  end

let marshal s = string_remove_null (Marshal.to_string s [Marshal.No_sharing])
let unmarshal s = Marshal.from_string (string_add_null s) 0

let make_symbol
    ?(compare=Pervasives.compare)
    ?(marshal=marshal)
    ?(unmarshal=unmarshal)
    print
    =
  {
    compare = compare;
    marshal = marshal;
    unmarshal = unmarshal;
    print = print;
  }

let string_symbol =
  let id = fun x -> x in {
    compare = String.compare;
    marshal = id;
    unmarshal = id;
    print = Format.pp_print_string
  }

module O = struct
  type ('a,'b,'c,'d,'e) t = ('a,'b,'c,'d,'e) t0
  constraint 'b = [>'a typ]
  constraint 'c = [>'a typdef]

  let print print_typ print_typdef print_ext fmt (env:('a,'b,'c,'d,'e) t) =
    fprintf fmt
      "{@[<v>typdef = %a;@ vartyp = %a;@ bddindex0 = %i;@ bddindex = %i; bddincr = %i;@ bddsize = %i;@ idcondvar = %a;@ vartid = %a;@ ext = %a@]}"
      (PMappe.print env.symbol.print print_typdef) env.typdef
      (PMappe.print ~first:"[@[" env.symbol.print print_typ) env.vartyp
      env.bddindex0 env.bddindex env.bddincr env.bddsize
      (PMappe.print pp_print_int env.symbol.print) env.idcondvar
      (PMappe.print env.symbol.print print_tid) env.vartid
      print_ext env.ext

  let clear_groups { cudd; bddindex0; bddsize } =
    Cudd.Man.ungroupall cudd(* ; *)
    (* Cudd.Man.group cudd bddindex0 bddsize Cudd.Man.MTR_DEFAULT *)

  let make
      ~symbol
      ~(copy_ext:'e -> 'e)
      ?(bddindex0=0)
      ?(bddsize=100)
      ?(relational=false)
      (cudd:'d Cudd.Man.t)
      (ext:'e)
      :
      ('a,'b,'c,'d,'e) t
      =
    for i = bddindex0 to bddsize - 1 do ignore (Cudd.Bdd.ithvar cudd i) done;
    let e = {
      cudd = cudd;
      typdef = PMappe.empty symbol.compare;
      vartyp = PMappe.empty symbol.compare;
      bddindex0 = bddindex0;
      bddsize = bddsize;
      bddindex = bddindex0;
      bddincr = if relational then 2 else 1;
      idcondvar = PMappe.empty (-);
      vartid = PMappe.empty symbol.compare;
      groups = [];
      print_external_idcondb =
	begin fun fmt (id,b) ->
	  fprintf fmt "%s%i" (if b then "not " else "") id
	end;
      ext = ext;
      symbol = symbol;
      copy_ext = copy_ext;
    } in
    e

  let fix_var_bits { cudd } tid =
    Cudd.Man.group cudd tid.(0) (Array.length tid) Cudd.Man.MTR_FIXED

  let rebuild_groups ({ cudd; groups } as env) =
    clear_groups env;
    let rec setup_gt = function
      | Var (_, _, 1) -> ()
      | Var (_, i, l) -> Cudd.Man.group cudd i l Cudd.Man.MTR_FIXED
      | Node ([l], _, _, _) -> setup_gt l
      | Node (lst, i, l, k) ->
          Cudd.Man.group cudd i l (if k = `Reord then Cudd.Man.MTR_DEFAULT
            else Cudd.Man.MTR_FIXED);
          List.iter setup_gt lst
    in
    List.iter setup_gt groups

end

type ('a,'d) t = ('a,'a typ,'a typdef,'d,unit) O.t

(*  ********************************************************************** *)
(** {3 Printing} *)
(*  ********************************************************************** *)

let typ_of_var env label : 'a
    =
  try
    PMappe.find label env.vartyp
  with Not_found ->
    notfound "Bdd.Env.typ_of_var: unknwon label/variable %a" env.symbol.print label

let print_idcondb env fmt ((id,b) as idb) =
  try
    let var = PMappe.find id env.idcondvar in
    let tid = PMappe.find var env.vartid in
    begin match typ_of_var env var with
    | `Bool -> env.symbol.print fmt var
    | _ ->
	begin
	  try
	    for i=0 to pred(Array.length tid) do
	      if id = tid.(i) then begin
		fprintf fmt "%a%i" env.symbol.print var i;
		raise Exit
	      end
	    done;
	  with Exit -> ()
	end;
    end
  with Not_found ->
    env.print_external_idcondb fmt idb

let print_order env (fmt:Format.formatter) : unit
    =
  let cudd = env.cudd in
  let nb = Cudd.Man.get_bddvar_nb env.cudd in
  let tab =
    Array.init nb
      (begin fun var ->
	let level = Cudd.Man.level_of_var cudd var in
	(var,level)
      end)
  in
  Array.sort (fun (v1,l1) (v2,l2) -> Pervasives.compare l1 l2) tab;
  Print.array
    ~first:"@[<v>"
    ~sep:"@ "
    ~last:"@]"
    (begin fun fmt (id,level) ->
      fprintf fmt "%3i => %3i, %a"
	level id
	(print_idcondb env) (id,true)
    end)
    fmt
    tab;
  ()

(*  ********************************************************************** *)
(** {3 Constructors} *)
(*  ********************************************************************** *)


let print fmt (env:('a,'b,'c,'d,'e) O.t) =
  O.print
    (print_typ env.symbol.print)
    (print_typdef env.symbol.print)
    (fun fmt _ -> pp_print_string  fmt "_")
    fmt env

let make
    ~symbol
    ?bddindex0 ?bddsize ?relational cudd
    =
  O.make
    ~symbol
    ~copy_ext:(fun x -> ())
    ?bddindex0 ?bddsize ?relational cudd ()

let make_string ?bddindex0 ?bddsize ?relational cudd =
  make ~symbol:string_symbol ?bddindex0 ?bddsize ?relational cudd

let copy env =
  { env with
    symbol = { env.symbol with print = env.symbol.print };
    ext = env.copy_ext env.ext;
  }

(*  ********************************************************************** *)
(** {3 Internal functions} *)
(*  ********************************************************************** *)

let gcmpvar env a b = env.symbol.compare (gvar a) (gvar b)

let normalize_gtl env : ('a group_tree list as 'b) -> 'b =
  let rec ngtl l =
    List.fold_left (fun gtl gt -> List.merge (gcmpvar env) [ngt gt] gtl) [] l
  and ngt = function
    | Node (gtl, i, l, k) -> Node (ngtl gtl, i, l, k)
    | v -> v
  in
  ngtl

let pack_gtl ?full env (gtl: 'a group_tree list as 'b) : int array * 'b =
  let perm = Array.init (Cudd.Man.get_bddvar_nb env.cudd) (fun i -> i) in
  let rec slice index i = function
    | 0 -> index
    | l -> perm.(i) <- index; slice (index + env.bddincr) (succ i) (pred l)
  in
  let rec pack_gt allow_space (index, gtl) = function
    | Var (v, i, l) ->
        let index = if allow_space then max index i else index in
        (slice index i l, Var (v, index, l) :: gtl)
    | Node (l, i, _, k) ->
        let index = if allow_space then max index i else index in
        let index, l = pack_gtl ~full:allow_space index l in
        let minidx = gidx (List.hd l) in
        (index, Node (l, minidx, index - minidx, k) :: gtl)
  and pack_gtl ?(full=true) index gtl =
    let i, gtl, _ = List.fold_left
      (fun (i, l, a) gt -> let i, l = pack_gt a (i, l) gt in i, l, false)
      (index, [], not full) gtl
    in
    i, List.rev gtl
  in
  let index, gtl = pack_gtl ?full env.bddindex0 gtl in
  let index, gtl =
    if index >= env.bddindex0 + env.bddsize && full = Some false
    then pack_gtl ~full:true env.bddindex0 gtl
    else index, gtl
  in
  perm, gtl

let permute_indexes env perm =
  env.idcondvar <- (PMappe.fold (fun idcond -> PMappe.add perm.(idcond))
                     env.idcondvar (PMappe.empty (-)));
  env.vartid <- PMappe.map (Array.map (fun id -> perm.(id))) env.vartid

let permute_with env (perm:int array) : unit =
  permute_indexes env perm;
  O.rebuild_groups env

let normalize_with env : int array =
  let perm, groups = env.groups |> normalize_gtl env |> pack_gtl ~full:true env in
  env.groups <- groups;
  permute_with env perm;
  perm

let pack_with env : int array =
  let perm, groups = env.groups |> pack_gtl ~full:true env in
  env.groups <- groups;
  permute_with env perm;
  perm

let add_typ_with (env:('a,'b,'c,'d,'e) O.t) (typ:'a) (typdef:'c) : unit
    =
  if PMappe.mem typ env.typdef then
    failwith (Print.sprintf "Bdd.Env.add_typ: type %a already defined" env.symbol.print typ)
  ;
  env.typdef <- PMappe.add typ typdef env.typdef;
  begin match typdef with
  | `Benum labels ->
      let t = `Benum typ in
      Array.iter
	(begin fun label -> env.vartyp <- PMappe.add label t env.vartyp end)
	labels
  | _ -> ()
  end

let check_normalized (env:('a,'b,'c,'d,'e) O.t) : bool
    =
  try
    let index = ref env.bddindex0 in
    PMappe.iter
      (begin fun var tid ->
	Array.iter
	  (begin fun id ->
	    if id <> !index then begin
	      printf
		"Bdd.Env.check_normalized: not normalized at index %i@.env=%a@."
		!index
		print env
	      ;
	      raise Exit
	    end;
	    index := !index + env.bddincr;
	  end)
	  tid
      end)
      env.vartid
    ;
    true
  with Exit ->
    false

let compose_permutation (perm1:int array) (perm2:int array) : int array =
  let l1 = Array.length perm1 in
  let l2 = Array.length perm2 in
  let l = max l1 l2 in
  let perm = Array.init l (fun i -> i) in
  for i=0 to l1 - 1 do
    let j = perm1.(i) in
    if j<l2 then
      let k = perm2.(j) in
      perm.(i) <- k
  done;
  perm

let compose_opermutation (operm1:int array option) (operm2:int array option)
    :
    int array option
    =
  match operm1 with
  | None -> operm2
  | Some perm1 ->
      match operm2 with
      | None -> operm1
      | Some perm2 ->
	  Some (compose_permutation perm1 perm2)

let permutation_of_offset (length:int) (offset:int) : int array =
  Array.init length (fun i -> i + offset)

(*  ********************************************************************** *)
(** {3 Accessors} *)
(*  ********************************************************************** *)

let mem_typ env typ =
  PMappe.mem typ env.typdef
let mem_var env label =
  PMappe.mem label env.vartyp
let mem_label env label =
  let typ = PMappe.find label env.vartyp in
  match typ with
  | `Benum _ when not (PMappe.mem label env.vartid) -> true
  | _ -> false

let typdef_of_typ env typ
    =
  try
    PMappe.find typ env.typdef
  with Not_found ->
    notfound "Bdd.Env.t#typdef_of_typ: unknown type %a" env.symbol.print typ

let vars env =
  PMappe.maptoset env.vartid

let labels env =
  PMappe.fold
    (begin fun typ def res ->
      match def with
      | `Benum tlabel ->
	  Array.fold_right PSette.add tlabel res
      | _ -> res
    end)
    env.typdef
    (PSette.empty env.symbol.compare)

(*  ********************************************************************** *)
(** {3 Adding types and variables} *)
(*  ********************************************************************** *)

type 'a group_tree_spec =
  [
  | `Var of 'a
  | `Reord of 'a group_tree_spec list
  | `Fixed of 'a group_tree_spec list
  ]

let group_vars_with env (gtl: 'a group_tree_spec list) : int array =

  (* Retrieve variable indexes, and order inner groups according to those. *)
  let rec trgt = function
    | `Var v -> let tid = PMappe.find v env.vartid in
               Var (v, tid.(0), Array.length tid)
    | `Reord l -> trn l `Reord
    | `Fixed l -> trn l `Fixed
  and trn l k =
    let l = List.rev_map trgt l |> List.sort gcmpidx in
    Node (l, gidx (List.hd l), List.fold_left (fun s x -> (+) s (glen x)) 0 l, k)
  in

  let spec_gtl = List.fold_left (fun acc gt -> insert_gt (trgt gt) acc) [] gtl in
  let base_gtl = env.groups in

  let _pp_gt = pp_gt env.symbol.print in
  (* List.iter (Format.eprintf "base> @[%a@]@\n" _pp_gt) base_gtl; *)
  (* List.iter (Format.eprintf "spec> @[%a@]@\n" _pp_gt) spec_gtl; *)

  let rec assert_sorted = function
    | [_] | [] -> ()
    | x::y::tl -> assert (gcmpidx x y < 0); assert_sorted (y::tl)
  in
  assert_sorted spec_gtl;
  assert_sorted base_gtl;

  (* let rec pp_ilist fmt = function *)
  (*   | [] -> () *)
  (*   | [x] -> Format.fprintf fmt "%d" x *)
  (*   | x::t -> Format.fprintf fmt "%d,@," x; pp_ilist fmt t *)
  (* in *)

  let gsux g = gidx g + glen g in
  let (<~) g g' = gsux g <= gidx g' in
  let merge_in_gtl gtl g =
    let rec rem_idxs acc gtl = function
      | [] -> List.rev_append acc gtl
      | i::itl as idxs -> match gtl with
          | [] -> List.rev acc
          | x :: _  when i < gidx x -> rem_idxs acc gtl itl
          | x :: tl when i >= gsux x -> rem_idxs (x :: acc) tl idxs
          | Var _ :: tl -> rem_idxs acc tl itl
          | Node (gtl, i', l, k) :: tl ->
              let x = Node (rem_idxs [] gtl idxs, i', l, k) in
              rem_idxs (x :: acc) tl idxs
    in
    let rec merge_in_gtl acc = function
      | [] -> (fun tl -> List.rev_append acc tl)
      | g::tl as gtl -> function
          | [] -> List.rev_append acc gtl
          | g'::l when g <~ g' -> merge_in_gtl (g :: acc) tl (g' :: l)
          | g'::l when g' <~ g -> merge_in_gtl (g' :: acc) gtl l
          | g' :: tl' ->
              (* Format.eprintf ">> g = %a@\n" _pp_gt g; *)
              (* Format.eprintf ">> g'= %a@\n" _pp_gt g'; *)
              let gidxs = gtidxs g |> List.sort (-) in
              (* Format.eprintf "   1:  [@[%a@]]@\n" pp_ilist gidxs; *)
              let tl' = rem_idxs [] tl' gidxs in
              (* Format.eprintf "   2:  [@[%a@]]@\n" pp_ilist (gtlidxs tl' |> List.sort (-)); *)
              match g' with
                | Var _ ->
                    assert (gidx g' = gidx g); merge_in_gtl (g :: acc) tl tl'
                | Node (gtl', i, l, k) ->
                    let g'' = Node (merge_in_gtl [] gtl gtl', i, l, k) in
                    merge_in_gtl (g'' :: acc) tl tl'
    in
    merge_in_gtl [] g gtl
  in

  let new_gtl = merge_in_gtl base_gtl spec_gtl in

  assert_sorted new_gtl;
  (* List.iter (Format.eprintf "new > @[%a@]@\n" _pp_gt) new_gtl; *)

  let perm, new_gtl = pack_gtl ~full:false env new_gtl in

  assert_sorted new_gtl;
  (* List.iter (Format.eprintf "new'> @[%a@]@\n" _pp_gt) new_gtl; *)

  env.groups <- new_gtl;
  permute_with env perm;
  perm



let enum_size_of_typ env (typ:'a) : int
    =
  let labels =
    let typdef = PMappe.find typ env.typdef in
    begin match typdef with
    | `Benum tlabel -> tlabel
    | _ ->
	failwith (Print.sprintf "Bddenum.labels_of_typ: type %a not defined as an enumerated type" env.symbol.print typ)
    end
  in
  let nb = Array.length labels in
  Reg.min_size (nb-1)

let add_var_with env ?(booking_factor=1) var typ : unit
    =
  if PMappe.mem var env.vartyp then
    failwith (Print.sprintf "Bdd.Env.add_var_with: label/var %a already defined" env.symbol.print var)
  ;
  let vartid = match typ with
    | #typ as typ ->
        let tid = match typ with
	  | `Bool -> [| env.bddindex |]
	  | `Bint(b,n) -> Array.init n (fun i -> env.bddindex+(env.bddincr*i))
	  | `Benum s -> Array.init (enum_size_of_typ env s) (fun i -> env.bddindex+(env.bddincr*i))
        in
        let l = Array.length tid in
        let size = env.bddincr * booking_factor * l in
        if env.bddindex + size >= env.bddindex0 + env.bddsize then raise Bddindex; (* abort! *)
	env.bddindex <- env.bddindex + size;
	Array.iter (fun id -> env.idcondvar <- PMappe.add id var env.idcondvar) tid;
	for i = env.bddindex - size to pred env.bddindex do
          ignore (Cudd.Bdd.ithvar env.cudd i)
	done;
        let gt = Var (var, tid.(0), l) in
        if Array.length tid <> 1 then O.fix_var_bits env tid;
        env.groups <- insert_gt gt env.groups;
        PMappe.add var tid env.vartid
    | _ ->
        env.vartid
  in
  env.vartid <- vartid;
  env.vartyp <- PMappe.add var typ env.vartyp;
  ()

type packing = [ `Normalize | `Pack | `None ]

let add_vars_with ({ bddindex; idcondvar; vartyp; vartid; groups; } as env)
    ?booking_factor ?(packing=`Normalize) lvartyp : int array option =
  let packing = if booking_factor <> None then `None else packing in
  try
    List.iter (fun (var,typ) -> add_var_with ?booking_factor env var typ) lvartyp;
    if bddindex = env.bddindex || packing = `None then None
    else if packing = `Normalize then Some (normalize_with env)
    else Some (pack_with env)
  with
    | Bddindex as e ->                                    (* complete rollback *)
        env.bddindex <- bddindex;
        env.idcondvar <- idcondvar;
        env.vartyp <- vartyp;
        env.vartid <- vartid;
        env.groups <- groups;
        raise e

let remove_vars_with env lvar : int array option
    =
  let length = ref 0 in
  List.iter
    (begin fun var ->
      let typ = PMappe.find var env.vartyp in
      begin match typ with
      | #typ ->
	  begin try
	    let tid = PMappe.find var env.vartid in
	    env.vartid <- PMappe.remove var env.vartid;
	    length := !length + (Array.length tid)*env.bddincr;
	    Array.iter
	      (fun id -> env.idcondvar <- PMappe.remove id env.idcondvar)
	      tid
	    ;
	  with Not_found ->
	    failwith
	      (Print.sprintf
		"Bdd.Env.remove: trying to remove the label %a of an enumerated type"
		env.symbol.print var)
	  end
      | _ -> ()
      end;
      env.vartyp <- PMappe.remove var env.vartyp;
    end)
    lvar
  ;
  if !length = 0 then
    None
  else begin
    let perm = normalize_with env in
    env.bddindex <- (env.bddindex - !length);
    Some perm
  end

let rename_vars_with env lvarvar
    :
    int array option
    =
      (* we need to distinguish variables without indices from the
	 other one. *)
  let (lvarvartyptidoset,lvarvartyp) =
    List.fold_left
      (begin fun (res1,res2) (var,nvar) ->
	let typ = PMappe.find var env.vartyp in
	env.vartyp <- PMappe.remove var env.vartyp;
	try
	  let tid = PMappe.find var env.vartid in
	  let oset = None in
	  env.vartid <- PMappe.remove var env.vartid;
	  Array.iter
	    (begin fun id ->
	      env.idcondvar <- PMappe.remove id env.idcondvar
	     end)
	    tid
	  ;
	  ((var,nvar,typ,tid,oset)::res1,res2)
	with Not_found ->
	  (res1, (var,nvar,typ)::res2)
      end)
      ([],[])
      lvarvar
  in
  List.iter
    (begin fun (var,nvar,typ) ->
      if PMappe.mem nvar env.vartyp then
	failwith
	  (Print.sprintf
	    "Bdd.Env.rename_vars: error, variable %a renamed in already existing %a"
	    env.symbol.print var env.symbol.print nvar)
      ;
      env.vartyp <- PMappe.add nvar typ env.vartyp;
    end)
    lvarvartyp
  ;
  List.iter
    (begin fun (var,nvar,typ,tid,oset) ->
      if PMappe.mem nvar env.vartyp then
	failwith
	  (Print.sprintf
	    "Bdd.Env.rename_vars: error, variable %a renamed in already existing %a"
	    env.symbol.print var env.symbol.print nvar)
      ;
      env.vartyp <- PMappe.add nvar typ env.vartyp;
      env.vartid <- PMappe.add nvar tid env.vartid;
      Array.iter
	(begin fun id ->
	  env.idcondvar <- PMappe.add id nvar env.idcondvar
	end)
	tid
      ;
    end)
    lvarvartyptidoset
  ;
  let perm = normalize_with env in
  Some perm

let add_typ env typ typdef =
  let nenv = copy env in
  add_typ_with nenv typ typdef;
  nenv
let add_vars env lvartyp =
  let nenv = copy env in
  ignore (add_vars_with nenv lvartyp);
  nenv
let remove_vars env lvars =
  let nenv = copy env in
  ignore (remove_vars_with nenv lvars);
  nenv
let rename_vars env lvarvar =
  let nenv = copy env in
  ignore (rename_vars_with nenv lvarvar);
  nenv

let extend_with env size =
  (* for i = 0 to size do Cudd.Bdd.ithvar env.cudd (env.bddsize + i); done; *)
  env.bddsize <- env.bddsize + size

(* ********************************************************************** *)
(** {3 Operations} *)
(* ********************************************************************** *)

let iter_ordered (env:('a,'b,'c,'d,'e) O.t) (f:'a -> int array -> unit)
  :
  unit
  =
  let processed = ref (PSette.empty env.symbol.compare) in
  let size = Cudd.Man.get_bddvar_nb env.cudd in
  for level=0 to size-1 do
    let id = Cudd.Man.var_of_level env.cudd level in
    try
      let var = PMappe.find id env.idcondvar in
      if not (PSette.mem var !processed) then begin
	let tid = PMappe.find var env.vartid in
	f var tid;
	processed := PSette.add var !processed;
      end
    with Not_found ->
      ()
  done;
  ()

(* 2 cas pour lesquels not (e1 = e2):

   a) l'ens des variables n'est pas égal
   ou
   b) bddincr ou bddindex0 est différent

   Peut-être faut-il distinguer c'est deux cas ?

   Idem pour l'égalité
*)

let is_leq env1 env2 : bool =
  env1==env2 ||
    env1.cudd = env2.cudd &&
      env1.bddincr = env2.bddincr &&
      (env1.bddindex - env1.bddindex0 <= env2.bddindex - env2.bddindex0) &&
      (env1.typdef==env2.typdef || PMappe.subset (=) env1.typdef env2.typdef) &&
      (env1.vartyp==env2.vartyp || PMappe.subset (=) env1.vartyp env2.vartyp)

let is_eq env1 env2 : bool =
  env1==(env2) ||
    env1.cudd = env2.cudd &&
      env1.bddincr = env2.bddincr &&
      (env1.bddindex - env1.bddindex0 = env2.bddindex - env2.bddindex0) &&
      (env1.typdef==env2.typdef || PMappe.equal (=) env1.typdef env2.typdef) &&
      (env1.vartyp==env2.vartyp || (PMappe.equal (=) env1.vartyp env2.vartyp))

(* does not rebuild groups *)
let shift env (offset:int) : ('a,'b,'c,'d,'e) O.t =
  let perm = permutation_of_offset env.bddindex offset in
  let nenv = copy env in
  nenv.bddindex0 <- env.bddindex0 + offset;
  permute_indexes nenv perm;
  nenv

let lce env1 env2 : ('a,'b,'c,'d,'e) O.t =
  if is_leq env2 env1 then
    let offset = env1.bddindex0 - env2.bddindex0 in
    if offset>=0 then
      env1
    else
      shift env1 (-offset)
  else if is_leq env1 env2 then
    let offset = env2.bddindex0 - env1.bddindex0 in
    if offset>=0 then
      env2
    else
      shift env2 (-offset)
  else begin
    let typdef =
      PMappe.mergei
	(begin fun typ typdef1 typdef2 ->
	  if typdef1<>typdef2 then
	    failwith
	      (Print.sprintf
		"Bdd.Env.lce: two different definitions for (enumerated) type %a" env1.symbol.print typ)
	  ;
	  typdef1
	end)
	env1.typdef env2.typdef
    in
    let vartyp =
      PMappe.mergei
	(begin fun var typ1 typ2 ->
	  if typ1<>typ2 then
	    failwith
	      (Print.sprintf
		"Bdd.Env.lce: two different types for label/variable %a" env1.symbol.print var)
	  ;
	  typ1
	end)
	env1.vartyp env2.vartyp
    in
    let (labeltyp,vartyp) =
      PMappe.partition
	(begin fun varlabel typ ->
	  match typ with
	  | `Benum _ ->
	      not
		((PMappe.mem varlabel env1.vartid) ||
		  (PMappe.mem varlabel env2.vartid))
	  | _ -> false
	end)
	vartyp
    in
    let env = copy env1 in
    env.typdef <- typdef;
    env.vartyp <- labeltyp;
    env.bddindex0 <- Pervasives.max env1.bddindex0 env2.bddindex0;
    env.bddsize <- Pervasives.max env1.bddsize env2.bddsize;
    env.bddindex <- env.bddindex0;
    env.idcondvar <- PMappe.empty (-);
    env.vartid <- PMappe.empty env1.symbol.compare;
    env.groups <- [];
    if not (PMappe.is_empty vartyp) then
      ignore (add_vars_with env ~packing:`None (PMappe.bindings vartyp));
    env
  end

let permutation12 env1 env2 : int array
    =
  assert(
    if is_leq env1 env2 then true
    else begin
      printf "env1=%a@.env2=%a@."
	print env1
	print env2
      ;
      false
    end
  );
  let perm = Array.init (Cudd.Man.get_bddvar_nb env1.cudd) (fun i -> i) in
  let offset = ref (env2.bddindex0 - env1.bddindex0) in
  PMappe.iter
    (begin fun var2 tid2 ->
      try
	let tid1 = PMappe.find var2 env1.vartid in
	Array.iter
	  (fun id -> perm.(id) <- id + !offset)
	  tid1;
      with Not_found ->
	offset := !offset + ((Array.length tid2)*env2.bddincr)
    end)
    env2.vartid
  ;
  perm

let permutation21 env2 env1 : int array
    =
  assert(
    if is_leq env1 env2 then true
    else begin
      printf "env1=%a@.env2=%a@."
	print env1
	print env2
      ;
      false
    end
  );
  let perm = Array.init (Cudd.Man.get_bddvar_nb env2.cudd) (fun i -> i) in
  let offset = ref (env2.bddindex0 - env1.bddindex0) in
  PMappe.iter
    (begin fun var2 tid2 ->
      try
	let tid1 = PMappe.find var2 env1.vartid in
	Array.iter
	  (fun id -> perm.(id + !offset) <- id)
	  tid1;
      with Not_found ->
	offset := !offset + (Array.length tid2)*env2.bddincr;
    end)
    env2.vartid
  ;
  perm

(*  ********************************************************************** *)
(** {3 Precomputing change of environments} *)
(*  ********************************************************************** *)

type 'a change = {
  intro : int array option;
  remove : ('a Cudd.Bdd.t * int array) option;
}

let diffvartid env1 env2 =
  PMappe.combine (fun _ tid1 tid2 -> match tid1, tid2 with
    | Some a, Some b when a <> b -> Some (a, b)
    | _ -> None)
    env1.vartid env2.vartid

let compute_change env1 env2 =
  let lce = lce env1 env2 in
  let intro =
    let difftid = diffvartid env1 lce in
    if PMappe.is_empty difftid then None else begin
      let perm = Array.init (Cudd.Man.get_bddvar_nb env2.cudd) (fun i -> i) in
      PMappe.iter (fun _ (tid1, tid2) ->
        Array.iter2 (fun id1 id2 -> perm.(id1) <- id2) tid1 tid2)
        difftid;
      Some perm
    end
  in
  let remove =
    if is_eq env2 lce
    then None
    else
      let mapvarid =
	PMappe.diffset
	  lce.vartid
	  (PMappe.maptoset env2.vartid)
      in
      let cudd = env1.cudd in
      let supp = ref (Cudd.Bdd.dtrue cudd) in
      PMappe.iter
	(begin fun var tid ->
	  Array.iter
	    (fun id -> supp := Cudd.Bdd.dand !supp (Cudd.Bdd.ithvar cudd id))
	    tid
	end)
	mapvarid
      ;
      let difftid = diffvartid lce env2 in
      let perm = Array.init (Cudd.Man.get_bddvar_nb env2.cudd) (fun i -> i) in
      if not (PMappe.is_empty difftid) then begin
        PMappe.iter (fun _ (tid1, tid2) ->
          Array.iter2 (fun id1 id2 -> perm.(id1) <- id2) tid1 tid2)
          difftid;
      end;
      Some(!supp, perm)
  in
  { intro = intro; remove = remove }

(*  ********************************************************************** *)
(** {3 Utilities} *)
(*  ********************************************************************** *)


type ('a,'b) value = {
  env : 'a;
  val0 : 'b
}

let make_value env val0 =
  assert(
    if (PMappe.cardinal env.idcondvar) =
      ((env.bddindex - env.bddindex0)/env.bddincr)
    then
      check_normalized env
    else begin
      printf "Pb in Bdd.Env.make_value@.";
      printf "env=%a@."
	print env
      ;
      false
    end
  );
  { env=env; val0=val0 }

let get_env val1 = val1.env
let get_val0 val1 = val1.val0

let extend_environment
    (permute:'a -> int array -> 'a)
    value
    nenv
    =
  if is_eq value.env nenv then
    let offset = nenv.bddindex0 - value.env.bddindex0 in
    if offset=0 then
      value
    else
      let perm = permutation_of_offset value.env.bddindex offset in
      make_value nenv (permute value.val0 perm)
  else if is_leq value.env nenv then
    let perm = permutation12 value.env nenv in
    make_value nenv (permute value.val0 perm)
  else
    failwith "Bdd.Env.extend_environment: the given environment is not a superenvironment "

let check_var (env:('a,'b,'c,'d,'e) O.t) (var:'a) : unit =
  try
    let typ = typ_of_var env var in
    let ok =
      match typ with
      | `Benum _ -> PMappe.mem var env.vartid
      | _ -> true
    in
    if not ok then raise Not_found
  with Not_found ->
    failwith (Print.sprintf "The variable %a is unknown or has a wrong type in the environement of the value" env.symbol.print var)

let check_lvar env lvar : unit =
  List.iter (check_var env) lvar

let check_value env t =
  if not (
    is_eq env t.env &&
      env.bddindex0 = t.env.bddindex0
  )
  then
    failwith (Print.sprintf "Bdd.Env: value does not have the expected environement@.env=%a@.t.env=%a@." print env print t.env);
  ()

let check_value2 t1 t2 =
  if not (
    is_eq t1.env t2.env &&
      t1.env.bddindex0 = t2.env.bddindex0
  )
  then
    failwith (Print.sprintf "Bdd.Env: operation called with non-equal environments:@.env1=%a@.env2=%a@." print t1.env print t2.env);
  ()

let check_value3 t1 t2 t3 =
  check_value2 t1 t2;
  check_value2 t1 t3

let check_lvarvalue env lvarexpr =
  List.map
    (fun (var,e) ->
      check_var env var;
      check_value env e;
      (var,e.val0)
    )
    lvarexpr

let check_lvalue env lexpr =
  List.map
    (fun e -> check_value env e; e.val0)
    lexpr

let check_ovalue env = function
  | None -> None
  | Some x -> check_value env x; Some x.val0

let mapunop f t =
  make_value t.env (f t.val0)

let mapbinop f t1 t2 =
  check_value2 t1 t2;
  make_value t1.env (f t1.val0 t2.val0)

let mapbinope f t1 t2 =
  check_value2 t1 t2;
  make_value t1.env (f t2.env t1.val0 t2.val0)

let mapterop f t1 t2 t3 =
  check_value3 t1 t2 t3;
  make_value t1.env (f t1.val0 t2.val0 t3.val0)
