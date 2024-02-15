(** Finite-type expressions with BDDs *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(** This module allows to manipulate structured BDDs, where variables involved
  in the Boolean formula are not only Boolean variables, but also of bounded
  integer or enumerated type (such types are encoded with several Boolean
  variables).
*)


open Format
open Env

type 'a t = [
  | `Bool of 'a Cudd.Bdd.t
      (** Boolean *)
  | `Bint of 'a Int.t
      (** Bounded integer *)
  | `Benum of 'a Enum.t
      (** Enumerated *)
]
type 'a expr = 'a t

type dt = Cudd.Man.d t
type vt = Cudd.Man.v t


(*  ********************************************************************** *)
(** {3 Opened signatures and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

  (*  ====================================================================== *)
  (** {4 Internal functions} *)
  (*  ====================================================================== *)

  let register_of_var (env:('a,'b,'c,'d,'e) Env.O.t) (var:'a) : 'd Cudd.Bdd.t array
      =
    let tid = PMappe.find var env.vartid in
    let cudd = env.cudd in
    let reg = Array.map (fun id -> Cudd.Bdd.ithvar cudd id) tid in
    reg

  let bddvar (env:('a,'b,'c,'d,'e) Env.O.t) (var:'a) : 'd expr
      =
    begin match Env.typ_of_var env var with
    | #Env.typ as typ ->
	begin try
	  let reg = register_of_var env var in
	  match typ with
	  | `Bool ->  `Bool(reg.(0))
	  | `Bint (b,n) ->
	      `Bint({
		Int.signed=b;
		Int.reg=reg;
	      })
	  | `Benum s ->
	      `Benum({
		Enum.typ = env.symbol.marshal s;
		Enum.reg = reg;
	      })
	with Not_found ->
	  `Benum (Enum.of_label env var)
	end
    | _ -> Env.notfound "unknown type for variable %a" env.symbol.print var
    end

  (*  ---------------------------------------------------------------------- *)
  (** {5 Accessors} *)
  (*  ---------------------------------------------------------------------- *)

  let mem_id (env:('a,'b,'c,'d,'e) Env.O.t) (id:int) : bool =
    PMappe.mem id env.idcondvar

  let vartyptid_of_id (env:('a,'b,'c,'d,'e) Env.O.t) (id:int) =
    let var = PMappe.find id env.idcondvar in
    (var, PMappe.find var env.vartyp, PMappe.find var env.vartid)

  let tid_of_var (env:('a,'b,'c,'d,'e) Env.O.t) (var:'a) : int array
      =
    PMappe.find var env.vartid

  let reg_of_expr (expr:'a expr) : 'a Reg.t
      =
    match expr with
    | `Bool(x) -> [| x |]
    | `Bint(x) -> x.Int.reg
    | `Benum(x) -> x.Enum.reg

  (*  ====================================================================== *)
  (** {4 Conversion to expressions} *)
  (*  ====================================================================== *)

  module Expr = struct
    (** Syntax tree for printing *)

    (** Atom *)
    type 'a atom =
      | Tbool of 'a * bool
	(** variable name and sign *)
      | Tint of 'a * int list
	(** variable name and list of possible values *)
      | Tenum of 'a * 'a list
	(** variable name, possibly primed, and list of possible labels *)

    (** Basic term *)
    type 'a term =
      | Tatom of 'a atom
	(** *)
      | Texternal of (int * bool)
	(** Unregistered BDD identifier and a Boolean for possible negation *)
      | Tcst of bool
	(** Boolean constant *)

    let map_atom f = function
      | Tbool(n,b) -> Tbool(f n,b)
      | Tint(n,l) -> Tint(f n,l)
      | Tenum(n,l) -> Tenum(f n, List.map f l)

    let map_term f = function
      | Tatom atom -> Tatom (map_atom f atom)
      | Texternal _ as x -> x
      | Tcst _ as x -> x

    (*  -------------------------------------------------------------------- *)
    (** {5 Internal conversion functions} *)
    (*  -------------------------------------------------------------------- *)

    (** In the following functions, the argument minterm is the slice of a global
    minterm that concerns the argument variable. *)

    let term_of_vint
	(var:'a) (reg:'d Int.t)
	(minterm:Cudd.Man.tbool array)
	:
	'a term
	=
      let lcode =
	Reg.Minterm.map
	  (Reg.Minterm.to_int ~signed:reg.Int.signed)
	  minterm
      in
      Tatom(Tint(var,lcode))

    let term_of_venum
	(env:('a,'b,'c,'d,'e) Env.O.t)
	(var:'a) (reg:'d Enum.t)
	(minterm:Cudd.Man.tbool array)
	:
	'a term
	=
      let typ = env.symbol.unmarshal reg.Enum.typ in
      let labels = ref [] in
      Reg.Minterm.iter
	(begin fun valminterm ->
	  let code = Reg.Minterm.to_int ~signed:false valminterm in
	  if Enum.mem_typcode env typ code then begin
	    let label = Enum.label_of_typcode env typ code in
	    labels :=  label :: !labels
	  end;
	end)
	minterm
      ;
      labels := List.rev !labels;
      let length = List.length !labels in
      if length=0 then
	Tcst(false)
      else begin
	let tlabels = Enum.labels_of_typ env typ in
	if length=Array.length tlabels then
	  Tcst(true)
	else
	  Tatom(Tenum(var,!labels))
      end

    (*  -------------------------------------------------------------------- *)
    (** {5 Converts a BDD condition} *)
    (*  -------------------------------------------------------------------- *)

    let term_of_idcondb (env:('a,'b,'c,'d,'e) Env.O.t) (idcondb:int*bool) : 'a term
	=
      let (idcond,b) = idcondb in
      if mem_id env idcond then begin
	let (var,typ,tid) = vartyptid_of_id env idcond in
	begin match typ with
	| `Bool ->
	    let atom = Tbool(var,b) in
	    Tatom(atom)
	| _ ->
	    let minterm = Array.make (Array.length tid) Cudd.Man.Top in
	    begin try
	      for i=0 to pred (Array.length tid) do
		if idcond = tid.(i) then begin
		  minterm.(i) <- if b then Cudd.Man.True else Cudd.Man.False;
		  raise Exit
		end;
	      done;
	    with Exit -> ()
	    end;
	    let term =
	      begin match bddvar env var with
	      | `Bint bddint -> term_of_vint var bddint minterm
	      | `Benum bddenum -> term_of_venum env var bddenum minterm
	      | _ -> failwith ""
	      end
	    in
	    term
	end
      end
      else
	Texternal(idcondb)

    (*  -------------------------------------------------------------------- *)
    (** {5 Converts a cube to a conjunction} *)
    (*  -------------------------------------------------------------------- *)

    let bool_of_tbool = function
      | Cudd.Man.True -> true
      | Cudd.Man.False -> false
      | Cudd.Man.Top -> failwith ""

    (** Raises Exit if false value *)
    let mand (rconjunction:'a term list ref) (term:'a term) : unit
	=
      match term with
      | Tcst(true) -> ()
      | Tcst(false) -> raise Exit
      | _ -> rconjunction := term :: !rconjunction

    let conjunction_of_minterm
	(env:('a,'b,'c,'d,'e) Env.O.t)
	(gminterm:Cudd.Man.tbool array)
	:
	'a term Normalform.conjunction
	=
      try
	let res = ref [] in
	(* We iterate first on the Boolean variables *)
	PMappe.iter
	  (begin fun var tid ->
	    match Env.typ_of_var env var with
	    | `Bool ->
		let id = tid.(0) in
		let v = gminterm.(id) in
		if v <> Cudd.Man.Top then begin
		  let term = Tatom(Tbool(var, bool_of_tbool v)) in
		  mand res term
		end;
	    | _ -> ()
	  end)
	  env.vartid
	;
	(* We print then integers and enumerated variables *)
	PMappe.iter
	  (begin fun var tid ->
	    match Env.typ_of_var env var with
	    | `Bool -> ()
	    | _ ->
		let minterm = Array.map (fun id -> gminterm.(id)) tid in
		if not (Reg.Minterm.is_indet minterm) then begin
		  let term = match bddvar env var with
		    | `Bint bddint -> term_of_vint var bddint minterm
		    | `Benum bddenum -> term_of_venum env var bddenum minterm
		    | _ -> failwith ""
		  in
		  mand res term
		end;
	  end)
	  env.vartid
	;
	(* We iterate on conditions and undeclared variables *)
	Array.iteri
	  (begin fun id (v:Cudd.Man.tbool) ->
	    if v<>Cudd.Man.Top && not (mem_id env id) then begin
	      let b = bool_of_tbool v in
	      let term = Texternal(id, b) in
	      mand res term
	    end
	  end)
	  gminterm
	;
	Normalform.Conjunction(List.rev !res)
      with Exit ->
	Normalform.Cfalse

    (*  -------------------------------------------------------------------- *)
    (** {5 Converts a full BDD} *)
    (*  -------------------------------------------------------------------- *)

    let dnf_of_bdd
	(env:('a,'b,'c,'d,'e) Env.O.t)
	(bdd:'d Cudd.Bdd.t)
	:
	'a term Normalform.dnf
	=
      if Cudd.Bdd.is_true bdd then
	Normalform.Dtrue
      else if Cudd.Bdd.is_false bdd then
	Normalform.Disjunction([])
      else begin
	try
	  let res = ref [] in
	  Cudd.Bdd.iter_cube
	    (begin fun cube ->
	      let conjunction = conjunction_of_minterm env cube in
	      match conjunction with
	      | Normalform.Cfalse -> ()
	      | Normalform.Conjunction([]) -> raise Exit
	      | _ -> res := conjunction :: !res
	    end)
	    bdd;
	  Normalform.Disjunction(List.rev !res)
	with Exit ->
	  Normalform.Dtrue
      end

    (*  -------------------------------------------------------------------- *)
    (** {5 Printing} *)
    (*  -------------------------------------------------------------------- *)

    let print_term ?print_external_idcondb (env:('a,'b,'c,'d,'e) Env.O.t) fmt (term:'a term) =
      match term with
      | Tatom(atom) ->
	  begin match atom with
	  | Tbool(var,s) ->
	      fprintf fmt "%s%a"
		(if s then "" else "not ")
		env.symbol.print var
	  | Tenum(var,labels) ->
	      if List.tl labels = [] then
		fprintf fmt "%a = %a"
		  env.symbol.print var
		  env.symbol.print (List.hd labels)
	      else
		fprintf fmt "%a in {%a}"
		  env.symbol.print var
		  (Print.list
		    ~first:"@[<h>" ~sep:",@," ~last:"@]"
		    env.symbol.print)
		  labels
	  | Tint(var,codes) ->
	      if List.tl codes = [] then
		fprintf fmt "%a = %i" env.symbol.print var (List.hd codes)
	      else
		fprintf fmt "%a in {%a}"
		  env.symbol.print var
		  (Print.list
		    ~first:"@[<h>" ~sep:",@," ~last:"@]"
		    pp_print_int)
		  codes
	  end
      | Texternal idb ->
	  let print =
	    match print_external_idcondb with
	    | Some p -> p
	    | None -> env.print_external_idcondb
	  in
	  print fmt idb
      | Tcst(b) ->
	  pp_print_bool fmt b

    let print_conjunction ?print_external_idcondb env fmt (conjunction:'a term Normalform.conjunction) =
      Normalform.print_conjunction
	(print_term ?print_external_idcondb env)
	fmt conjunction

    let print_dnf ?print_external_idcondb env fmt (disjunction:'a term Normalform.dnf) =
      Normalform.print_dnf
	(print_term ?print_external_idcondb env)
	fmt disjunction

  end

  (*  ====================================================================== *)
  (** {4 Printing} *)
  (*  ====================================================================== *)

  (*  ---------------------------------------------------------------------- *)
  (** {5 Printing a minterm} *)
  (*  ---------------------------------------------------------------------- *)

  (** Here the full minterm is inspected and taken into account. *)

  let print_minterm
      ?print_external_idcondb
      (env:('a,'b,'c,'d,'e) Env.O.t)
      (fmt:Format.formatter) (gminterm:Cudd.Man.tbool array)
      :
      unit
      =
    let conjunction = Expr.conjunction_of_minterm env gminterm in
    Expr.print_conjunction ?print_external_idcondb env fmt conjunction

  (*  ---------------------------------------------------------------------- *)
  (** {5 Printing a full BDD or IDD} *)
  (*  ---------------------------------------------------------------------- *)

  let print_bdd ?print_external_idcondb (env:('a,'b,'c,'d,'e) Env.O.t) fmt (bdd:'d Cudd.Bdd.t) =
    if Cudd.Bdd.is_true bdd then
      pp_print_string fmt "true"
    else if Cudd.Bdd.is_false bdd then
      pp_print_string fmt "false"
    else
      let size = Cudd.Bdd.nbpaths bdd in
      if size > (float_of_int !Cudd.Man.print_limit) then
	fprintf fmt "(%in,%gm)" (Cudd.Bdd.size bdd) size
      else begin
	fprintf fmt "@[<hv>";
	let first = ref true in
	Cudd.Bdd.iter_cube
	  (begin fun cube ->
	    let conjunction = Expr.conjunction_of_minterm env cube in
	    if conjunction<>Normalform.Cfalse then begin
	      if !first then first := false else fprintf fmt " or@ ";
	      Expr.print_conjunction
		?print_external_idcondb env fmt conjunction
	    end;
	  end)
	  bdd;
	fprintf fmt "@]"
      end

  (*  ---------------------------------------------------------------------- *)
  (** {5 Conditions} *)
  (*  ---------------------------------------------------------------------- *)

  let print_idcondb ?print_external_idcondb (env:('a,'b,'c,'d,'e) Env.O.t) fmt (idcondb:int*bool)
      =
    let term = Expr.term_of_idcondb env idcondb in
    Expr.print_term ?print_external_idcondb env fmt term

  let print_idcond
      ?print_external_idcondb
      (env:('a,'b,'c,'d,'e) Env.O.t) fmt (idcond:int)
      =
    print_idcondb ?print_external_idcondb env fmt (idcond,true)

  (*  ---------------------------------------------------------------------- *)
  (** {5 Expressions} *)
  (*  ---------------------------------------------------------------------- *)

  let print ?print_external_idcondb (env:('a,'b,'c,'d,'e) Env.O.t) (fmt:Format.formatter) expr : unit =
    match expr with
    | `Bool x ->
	print_bdd ?print_external_idcondb env fmt x
    | `Bint x ->
	Int.print_minterm (print_bdd ?print_external_idcondb env) fmt x
    | `Benum x ->
	Enum.print_minterm (print_bdd ?print_external_idcondb env) env fmt x

  (*  ---------------------------------------------------------------------- *)
  (** {5 Miscellaneous} *)
  (*  ---------------------------------------------------------------------- *)

  (** Same as [Cudd.Bdd.cube_of_bdd], but keep only the the values of
  variables having a determinated value.

  Example: the classical [Cudd.Bdd.cube_of_bdd] could return [b and
  (x=1 or x=3)], whereas [cube_of_bdd] will return only [b] in
  such a case. *)

  let cube_of_bdd env (bdd:'a Cudd.Bdd.t) : 'a Cudd.Bdd.t
      =
    let map =
      let cube = Cudd.Bdd.cube_of_bdd bdd in
      let list = Cudd.Bdd.list_of_cube cube in
      let map =
	List.fold_left
	  (fun map (id,b) -> PMappe.add id b map)
	  (PMappe.empty (-)) list
      in
      ref map
    in
    let res = ref (Cudd.Bdd.dtrue env.cudd) in

    let process id b
	=
      if not (mem_id env id) then begin
	let var = Cudd.Bdd.ithvar env.cudd id in
	res := Cudd.Bdd.dand (if b then var else Cudd.Bdd.dnot var) !res;
	map := PMappe.remove id !map
      end
      else begin
	let (var,typ,tid) = vartyptid_of_id env id in
	begin match typ with
	| `Bool ->
	    let var = Cudd.Bdd.ithvar env.cudd tid.(0) in
	    res := Cudd.Bdd.dand (if b then var else Cudd.Bdd.dnot var) !res;
	    map := PMappe.remove id !map
	| `Bint _ | `Benum _ ->
	    let tokeep =
	      try
		for i=0 to pred(Array.length tid) do
		  if not (PMappe.mem tid.(i) !map) then raise Exit
		done;
		true
	      with Exit ->
		false
	    in
	    if tokeep then begin
	      for i=0 to pred(Array.length tid) do
		let id = tid.(i) in
		let b = PMappe.find id !map in
		let var = Cudd.Bdd.ithvar env.cudd tid.(i) in
		res := Cudd.Bdd.dand (if b then var else Cudd.Bdd.dnot var) !res;
		map := PMappe.remove id !map
	      done
	    end
	    else begin
	      for i=0 to pred(Array.length tid) do
		let id = tid.(i) in
		map := PMappe.remove id !map
	      done;
	    end
	| _ -> ()
	end
      end
    in

    while not (PMappe.is_empty !map) do
      try
	PMappe.iter
	  (begin fun id b ->
	    process id b;
	    raise Exit
	  end)
	  !map
      with Exit ->
	()
    done;
    !res

  (* ********************************************************************** *)
  (** {4 Expressions} *)
  (*  ====================================================================== *)

  let bddsupport (env:('a,'b,'c,'d,'e) Env.O.t) (set:'a list)  : 'd Cudd.Bdd.t
      =
    List.fold_left
      (fun supp var ->
        let tid = tid_of_var env var in
        Array.fold_left
          (fun acc id -> Cudd.Bdd.dand acc (Cudd.Bdd.ithvar env.cudd id))
          supp tid)
      (Cudd.Bdd.dtrue env.cudd)
      set

  let typ_of_expr (env:('a, 'b, 'c, 'd, 'e) Env.O.t) (expr:'d expr) : [>'a Env.typ]
      =
    match expr with
    | `Bool _ -> `Bool
    | `Bint(x) -> `Bint(x.Int.signed, Array.length x.Int.reg)
    | `Benum(x) -> `Benum(env.symbol.unmarshal x.Enum.typ)

  let mapmonop
      (monop : 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t)
      (expr:'a expr)
      :
      'a expr
      =
    match expr with
    | `Bool(x) -> `Bool(monop x)
    | `Bint(x) -> `Bint
	{
	  Int.signed = x.Int.signed;
	  Int.reg = Array.map monop x.Int.reg
	}
    | `Benum(x) -> `Benum
	{
	  Enum.typ = x.Enum.typ;
	  Enum.reg = Array.map monop x.Enum.reg
	}

  let permutation_of_rename
      (env:('a,'b,'c,'d,'e) Env.O.t)
      (substitution : ('a * 'a) list)
      :
      int array
      =
    let res = Array.init (Cudd.Man.get_bddvar_nb env.cudd) (fun i -> i) in
    List.iter
      (begin fun (var,nvar) ->
	assert ((Env.typ_of_var env var) = (Env.typ_of_var env nvar));
	let tid = tid_of_var env var in
	let ntid = tid_of_var env nvar in
	Array.iteri
	  (begin fun i id ->
	    let nid = ntid.(i) in
	    res.(id) <- nid;
	  end)
	  tid
      end)
      substitution
    ;
    res

  let composition_of_lvarexpr
      (env:('a,'b,'c,'d,'e) Env.O.t)
      (lvarexpr : ('a * 'd expr) list)
      :
      'd Cudd.Bdd.t array
      =
    let manager = env.cudd in
    let res =
      Array.init (Cudd.Man.get_bddvar_nb env.cudd)
	(fun i -> Cudd.Bdd.ithvar manager i)
    in
    List.iter
      (begin fun (var,expr) ->
	assert ((Env.typ_of_var env var) = (typ_of_expr env expr));
	let tid = tid_of_var env var in
	let reg = reg_of_expr expr in
	Array.iteri
	  (begin fun i id ->
	    let bdd = reg.(i) in
	    res.(id) <- bdd
	  end)
	  tid
      end)
      lvarexpr
    ;
    res

  let composition_of_lvarlexpr
      (env:('a,'b,'c,'d,'e) Env.O.t)
      (lvar:'a list) (lexpr:'d expr list)
      :
      'd Cudd.Bdd.t array
      =
    let manager = env.cudd in
    let res =
      Array.init (Cudd.Man.get_bddvar_nb env.cudd)
	(fun i -> Cudd.Bdd.ithvar manager i)
    in
    List.iter2
      (begin fun var expr ->
	assert ((Env.typ_of_var env var) = (typ_of_expr env expr));
	let tid = tid_of_var env var in
	let reg = reg_of_expr expr in
	Array.iteri
	  (begin fun i id ->
	    let bdd = reg.(i) in
	    res.(id) <- bdd
	  end)
	  tid
      end)
      lvar lexpr
    ;
    res

  let varmap (expr:'a expr) : 'a expr
      =
    mapmonop Cudd.Bdd.varmap expr

  let permute ?memo (expr:'a expr) (permutation:int array) : 'a expr
      =
    match expr with
    | `Bool x -> `Bool(Cudd.Bdd.permute ?memo x permutation)
    | `Bint x -> `Bint(Int.permute ?memo x permutation)
    | `Benum x -> `Benum(Enum.permute ?memo x permutation)

  let compose ?memo (expr:'a expr) (composition:'a Cudd.Bdd.t array) : 'a expr
      =
    match expr with
    | `Bool x -> `Bool(Cudd.Bdd.vectorcompose ?memo composition x)
    | `Bint x -> `Bint(Int.vectorcompose ?memo composition x)
    | `Benum x -> `Benum(Enum.vectorcompose ?memo composition x)

  let permute_list ?memo lexpr (permutation:int array) =
    match memo with
    | Some memo ->
	List.map (fun x -> permute ~memo x permutation) lexpr
    | None ->
	let hash = Cudd.Hash.create 1 in
	let memo = Cudd.Memo.Hash hash in
	let res = List.map (fun x -> permute ~memo x permutation) lexpr in
	Cudd.Hash.clear hash;
	res

  let compose_list ?memo (lexpr:'a expr list) (composition:'a Cudd.Bdd.t array) : 'a expr list
      =
    match memo with
    | Some memo ->
	List.map (fun x -> compose ~memo x composition) lexpr
    | None ->
	let hash = Cudd.Hash.create 1 in
	let memo = Cudd.Memo.Hash hash in
	let res = List.map (fun x -> compose ~memo x composition) lexpr in
	Cudd.Hash.clear hash;
	res

  let substitute_by_var ?memo env expr substitution =
    let perm = permutation_of_rename env substitution in
    permute ?memo expr perm

  let substitute
      ?memo
      (env:('a,'b,'c,'d,'e) Env.O.t)
      (expr:'d expr)
      (substitution: ('a * 'd expr) list)
      : 'd expr
      =
    let composition = composition_of_lvarexpr env substitution in
    compose ?memo expr composition

  let substitute_by_var_list
      ?memo
      (env:('a,'b,'c,'d,'e) Env.O.t)
      (lexpr:'d expr list)
      (substitution: ('a * 'a) list)
      : 'd expr list
      =
    if lexpr=[] then []
    else
      let perm = permutation_of_rename env substitution in
      permute_list ?memo lexpr perm

  let substitute_list
      ?memo
      (env:('a,'b,'c,'d,'e) Env.O.t)
      (lexpr:'d expr list)
      (substitution: ('a * 'd expr) list)
      : 'd expr list
      =
    if lexpr=[] then []
    else
      let composition = composition_of_lvarexpr env substitution in
      compose_list ?memo lexpr composition

  let cofactor expr bdd = mapmonop (fun x -> Cudd.Bdd.cofactor x bdd) expr
  let restrict expr bdd = mapmonop (fun x -> Cudd.Bdd.restrict x bdd) expr
  let tdrestrict expr bdd = mapmonop (fun x -> Cudd.Bdd.tdrestrict x bdd) expr

  (*  ---------------------------------------------------------------------- *)
  (** {5 Boolean expressions} *)
  (*  ---------------------------------------------------------------------- *)

  module Bool = struct

    type 'd t = 'd Cudd.Bdd.t
    type dt = Cudd.Man.d t
    type vt = Cudd.Man.v t

    let of_expr = function
      | `Bool x -> x
      | _ -> failwith "Bool.of_expr: Boolean expression expected"

    let to_expr bdd = `Bool bdd

    let dtrue (env:('a,'b,'c,'d,'e) Env.O.t) = Cudd.Bdd.dtrue env.cudd
    let dfalse (env:('a,'b,'c,'d,'e) Env.O.t) = Cudd.Bdd.dfalse env.cudd
    let of_bool (env:('a,'b,'c,'d,'e) Env.O.t) b =
      if b then Cudd.Bdd.dtrue env.cudd else Cudd.Bdd.dfalse env.cudd

    let var (env:('a,'b,'c,'d,'e) Env.O.t) (var:'a) =
      match bddvar env var with
      | `Bool x -> x
      | _ -> failwith (Print.sprintf "Formula.Bool.var: variable %a of type %a instead of Boolean"
	  env.symbol.print var
	  (Env.print_typ env.symbol.print) (Env.typ_of_var env var))

    let dnot env = Cudd.Bdd.dnot
    let dand env = Cudd.Bdd.dand
    let dor env = Cudd.Bdd.dor
    let xor env = Cudd.Bdd.xor
    let nand env = Cudd.Bdd.nand
    let nor env = Cudd.Bdd.nor
    let nxor env = Cudd.Bdd.nxor
    let eq env = Cudd.Bdd.eq
    let leq env x y = Cudd.Bdd.dor y (Cudd.Bdd.dnot x)
    let ite env = Cudd.Bdd.ite

    let is_true env = Cudd.Bdd.is_true
    let is_false env = Cudd.Bdd.is_false
    let is_cst env = Cudd.Bdd.is_cst
    let is_leq env = Cudd.Bdd.is_included_in
    let is_eq env = Cudd.Bdd.is_equal
    let is_and_false env = Cudd.Bdd.is_inter_empty

    let exist env (lvar:'a list) expr =
      Cudd.Bdd.exist (bddsupport env lvar) expr

    let forall env (lvar:'a list) expr =
      Cudd.Bdd.forall (bddsupport env lvar) expr

    let cofactor = Cudd.Bdd.cofactor
    let restrict = Cudd.Bdd.restrict
    let tdrestrict = Cudd.Bdd.tdrestrict
    let permute = Cudd.Bdd.permute
    let varmap = Cudd.Bdd.varmap

    let substitute_by_var ?memo (env:('a,'b,'c,'d,'e) Env.O.t) expr (substitution:('a*'a) list)
	=
      let perm = permutation_of_rename env substitution in
      Cudd.Bdd.permute ?memo expr perm

    let substitute ?memo (env:('a,'b,'c,'d,'e) Env.O.t) expr (substitution: ('a * 'd expr) list)
	=
      let composition = composition_of_lvarexpr env substitution in
      Cudd.Bdd.vectorcompose ?memo composition expr

    let print ?print_external_idcondb (env:('a,'b,'c,'d,'e) Env.O.t) fmt (x:'d t) =
      print_bdd env fmt x

  end

  (*  ---------------------------------------------------------------------- *)
  (** {5 Bounded integer expressions} *)
  (*  ---------------------------------------------------------------------- *)

  module Bint = struct

    type 'd t = 'd Int.t
    type dt = Cudd.Man.d t
    type vt = Cudd.Man.v t

    let of_expr = function
      | `Bint x -> x
      | _ -> failwith "Bint.of_expr: bounded integer expression expected"

    let to_expr (bddint:'d t) = `Bint bddint

    let of_int (env:('a,'b,'c,'d,'e) Env.O.t) typ cst =
      match typ with
      | `Bint(sgn,size) ->
	  Int.of_int env.cudd sgn size cst
      | _ -> failwith ""

    let var (env:('a,'b,'c,'d,'e) Env.O.t) (var:'a) =
      match bddvar env var with
      | `Bint x -> x
      | _ -> failwith (Print.sprintf "Formula.Bint.var: variable %a of type %a instead of bounded integer"
	  env.symbol.print var (Env.print_typ env.symbol.print) (Env.typ_of_var env var))

    let neg (env:('a,'b,'c,'d,'e) Env.O.t) = Int.neg
    let succ (env:('a,'b,'c,'d,'e) Env.O.t) = Int.succ
    let pred (env:('a,'b,'c,'d,'e) Env.O.t) = Int.pred
    let add (env:('a,'b,'c,'d,'e) Env.O.t) = Int.add
    let sub (env:('a,'b,'c,'d,'e) Env.O.t) = Int.sub
    let mul (env:('a,'b,'c,'d,'e) Env.O.t) = Int.mul
    let shift_left (env:('a,'b,'c,'d,'e) Env.O.t) = Int.shift_left
    let shift_right (env:('a,'b,'c,'d,'e) Env.O.t) = Int.shift_right
    let scale (env:('a,'b,'c,'d,'e) Env.O.t) = Int.scale
    let ite (env:('a,'b,'c,'d,'e) Env.O.t) = Int.ite

    let zero (env:('a,'b,'c,'d,'e) Env.O.t) = Int.zero env.cudd
    let eq (env:('a,'b,'c,'d,'e) Env.O.t) = Int.equal env.cudd
    let eq_int (env:('a,'b,'c,'d,'e) Env.O.t) = Int.equal_int env.cudd
    let supeq (env:('a,'b,'c,'d,'e) Env.O.t) = Int.greatereq env.cudd
    let supeq_int (env:('a,'b,'c,'d,'e) Env.O.t) = Int.greatereq_int env.cudd
    let sup (env:('a,'b,'c,'d,'e) Env.O.t) = Int.greater env.cudd
    let sup_int (env:('a,'b,'c,'d,'e) Env.O.t) = Int.greater_int env.cudd

    let cofactor = Int.cofactor
    let restrict = Int.restrict
    let tdrestrict = Int.tdrestrict
    let permute = Int.permute
    let varmap = Int.varmap

    let substitute_by_var ?memo env expr (substitution:('a*'a) list)
	=
      let perm = permutation_of_rename env substitution in
      Int.permute ?memo expr perm

    let substitute ?memo env expr (substitution: ('a * 'd expr) list)
	=
      let composition = composition_of_lvarexpr env substitution in
      Int.vectorcompose ?memo composition expr

    let guard_of_int (env:('a,'b,'c,'d,'e) Env.O.t) = Int.guard_of_int env.cudd
    let guardints (env:('a,'b,'c,'d,'e) Env.O.t) = Int.guardints env.cudd

    let print ?print_external_idcondb (env:('a,'b,'c,'d,'e) Env.O.t) fmt (x:'d t) =
      Int.print_minterm (Bool.print env) fmt x
  end

  (*  ---------------------------------------------------------------------- *)
  (** {5 Enumerated expressions} *)
  (*  ---------------------------------------------------------------------- *)

  module Benum = struct
    type 'd t = 'd Enum.t
    type dt = Cudd.Man.d t
    type vt = Cudd.Man.v t

    let of_expr = function
      | `Benum x -> x
      | _ -> failwith "Benum.of_expr: bounded integer expression expected"

    let to_expr (bddenum:'d t) = `Benum bddenum

    let var (env:('a,'b,'c,'d,'e) Env.O.t) (var:'a) =
      match bddvar env  var with
      | `Benum x -> x
      | _ -> failwith (Print.sprintf "Formula.Bint.var: variable %a of type %a instead of enumerated type"
	  env.symbol.print var (Env.print_typ env.symbol.print) (Env.typ_of_var env var))

    let ite (env:('a,'b,'c,'d,'e) Env.O.t) bdd x y = Enum.ite bdd x y

    let eq (env:('a,'b,'c,'d,'e) Env.O.t) x y = Enum.equal env x y
    let eq_label (env:('a,'b,'c,'d,'e) Env.O.t) x y = Enum.equal_label env x y

    let cofactor = Enum.cofactor
    let restrict = Enum.restrict
    let tdrestrict = Enum.tdrestrict
    let permute = Enum.permute
    let varmap = Enum.varmap

    let substitute_by_var ?memo env expr (substitution:('a*'a) list)
	=
      let perm = permutation_of_rename env substitution in
      Enum.permute ?memo expr perm

    let substitute ?memo env expr (substitution: ('a * 'd expr) list)
	=
      let composition = composition_of_lvarexpr env substitution in
      Enum.vectorcompose ?memo composition expr

    let guard_of_label = Enum.guard_of_label
    let guardlabels = Enum.guardlabels

    let print ?print_external_idcondb (env:('a,'b,'c,'d,'e) Env.O.t) fmt (x:'d t) =
      Enum.print_minterm (Bool.print env) env fmt x

  end

  (*  ---------------------------------------------------------------------- *)
  (** {5 General (typed) expressions} *)
  (*  ---------------------------------------------------------------------- *)

  let var = bddvar
  let ite (bdd:'a Cudd.Bdd.t) (e1:'a expr) (e2:'a expr) : 'a expr
      =
    match (e1,e2) with
    | (`Bool e1, `Bool e2) -> `Bool(Cudd.Bdd.ite bdd e1 e2)
    | (`Bint e1, `Bint e2) -> `Bint(Int.ite bdd e1 e2)
    | (`Benum e1, `Benum e2) -> `Benum(Enum.ite bdd e1 e2)
    | _ ->
	failwith "Bddexpr.ite: typing error"

  let eq (env:('a,'b,'c,'d,'e) Env.O.t) (e1:'d expr) (e2:'d expr) : 'd Cudd.Bdd.t
      =
    match (e1,e2) with
    | (`Bool e1, `Bool e2) -> Cudd.Bdd.eq e1 e2
    | (`Bint e1, `Bint e2) -> Int.equal env.cudd e1 e2
    | (`Benum e1, `Benum e2) -> Enum.equal env e1 e2
    | _ ->
	failwith "Bddexpr.eq: typing error"

  let support_cond cudd (expr:'d expr) : 'd Cudd.Bdd.t
      =
    match expr with
    | `Bool(x) -> Cudd.Bdd.support x
    | `Bint(x) ->
	if x.Int.reg=[||] then
	  Cudd.Bdd.dtrue cudd
	else
	  Cudd.Bdd.vectorsupport x.Int.reg
    | `Benum(x) ->
	if x.Enum.reg=[||] then
	  Cudd.Bdd.dtrue cudd
	else
	  Cudd.Bdd.vectorsupport x.Enum.reg

  (** Concatenates in an array the BDDs involved in the expressions *)
  let tbdd_of_texpr (texpr:'d expr array) : 'd Cudd.Bdd.t array
      =
    let nb =
      Array.fold_left
	(begin fun nb expr ->
	  begin match expr with
	  | `Bool(x) -> nb+1
	  | `Bint(x) -> nb + (Array.length x.Int.reg)
	  | `Benum(x) -> nb + (Array.length x.Enum.reg)
	  end
	end)
	0 texpr
    in
    let tb = Array.make nb (Obj.magic ()) in
    let b =
      Array.fold_left
	(begin fun b expr ->
	  begin match expr with
	  | `Bool(x) ->
	      tb.(b) <- x;
	      b+1
	  | `Bint(x) ->
	      let reg = x.Int.reg in
	      let length = Array.length reg in
	      Array.blit reg 0 tb b length;
	      b+length
	  | `Benum(x) ->
	      let reg = x.Enum.reg in
	      let length = Array.length reg in
	      Array.blit reg 0 tb b length;
	      b+length
	  end
	end)
	0 texpr
    in
    assert(b=nb);
    tb


  (** Inverse operation: rebuild an array of expressions from the old array of
  expressions (for the types) and the array of BDDs.
*)
  let texpr_of_tbdd (oldtexpr:'a expr array) (tbdd:'a Cudd.Bdd.t array) : 'a expr array
      =
    let index = ref 0 in
    Array.map
      (begin fun oldexpr ->
	match oldexpr with
	| `Bool x ->
	    let res = tbdd.(!index) in
	    incr index;
	    `Bool res
	| `Bint x ->
	    let res =
	      { x with Int.reg =
		  Array.mapi (fun i bdd -> tbdd.(!index + i)) x.Int.reg
	      }
	    in
	    index := !index + (Array.length x.Int.reg);
	    `Bint res
	| `Benum x ->
	    let res =
	      { x with Enum.reg =
		  Array.mapi (fun i bdd -> tbdd.(!index + i)) x.Enum.reg
	      }
	    in
	    index := !index + (Array.length x.Enum.reg);
	    `Benum res
      end)
      oldtexpr

  let vectorsupport_cond (texpr:'a expr array) : 'a Cudd.Bdd.t
      =
    Cudd.Bdd.vectorsupport (tbdd_of_texpr texpr)

  let support_of_bdd env (supp:'d Cudd.Bdd.t) : 'a PSette.t
      =
    let list = Cudd.Bdd.list_of_support supp in
    List.fold_left
      (begin fun res id ->
	let var = PMappe.find id env.idcondvar in
	PSette.add var res
      end)
      (PSette.empty env.symbol.compare)
      list

  let support env (expr:'d expr) : 'a PSette.t =
    let supp = support_cond env.cudd expr in
    support_of_bdd env supp

  let register_of_expr = function
    | `Bool x -> [|x|]
    | `Bint x -> x.Int.reg
    | `Benum x -> x.Enum.reg
end

(*  ********************************************************************** *)
(** {3 Closed signatures} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {4 Expressions} *)
(*  ====================================================================== *)

let typ_of_expr = O.typ_of_expr
let var = O.var
let ite = O.ite
let eq = O.eq
let substitute_by_var = O.substitute_by_var
let substitute = O.substitute
let substitute_by_var_list = O.substitute_by_var_list
let substitute_list = O.substitute_list
let cofactor = O.cofactor
let restrict = O.restrict
let tdrestrict = O.tdrestrict
let support = O.support
let support_cond = O.support_cond
let vectorsupport_cond = O.vectorsupport_cond
let cube_of_bdd = O.cube_of_bdd


let print = O.print
let print_minterm = O.print_minterm
let print_bdd = O.print_bdd
let print_idcondb = O.print_idcondb
let print_idcond = O.print_idcond

module Bool = struct
  include O.Bool
end
module Bint = struct
  include O.Bint
end
module Benum = struct
  include O.Benum
end
