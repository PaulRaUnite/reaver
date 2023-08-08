(******************************************************************************)
(* NbacSyntaxCheck *)
(* Checks on the syntax of an NBac file *)
(* author: Bertrand Jeannet, Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

open Format
open NbacExpr


(*  ********************************************************************** *)
(** {2 Type checking} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Looking for a variable in the declarations} *)
(*  ====================================================================== *)

let typ_of_label decl s
  =
  let (typ,found) = PMappe.fold 
    (fun typ def ret -> 
      let (_,oldfound) = ret in
      if oldfound then ret 
      else begin
        match def with 
        |`Benum(edef) -> (`Benum(typ),List.mem s (Array.to_list edef))
      end
    )
    decl.Program.typdef
    (`Benum(symtype_null),false)
  in
  if found then typ
  else raise Not_found


let typ_of_var decl s
  =
  try Mappe.find s decl.Program.state
  with Not_found -> begin
    try Mappe.find s decl.Program.input
    with Not_found -> begin
      try Mappe.find s decl.Program.local 
      with Not_found -> begin
        typ_of_label decl s
      end
    end
  end

(*  ====================================================================== *)
(** {3 Guessing the type of an expression} *)
(*  ====================================================================== *)

let istyp_arith typ = match typ with
| `Int | `Real -> true
| _ -> false

let norm_typ typ = 
  match typ with
  | `Int -> `Real
  | _ -> typ

let guesstyp decl expr 
  =
  let rec parcours = function
    | Cst cst ->
	begin match cst with
	| `Bool _ -> `Bool
	| `Bint(typ,n) -> `Bint typ
	| `Apron _ -> `Real
	end
    | Ref(s) ->
        begin try typ_of_var decl s
        with Not_found ->
	  raise (NbacParseError (sprintf "Error: in expression, undeclared variable or label %s" s))
        end
    | Unop(op,e) ->
	begin match op with
	| `Not -> `Bool
	| `Up -> `Bool
	| `Apron _ -> `Real
	end
    | Binop(op,e1,e2) ->
	begin match op with
	| `Bool _ -> `Bool
	| `Apron _ -> parcours e1 
	end
    | If(e1,e2,e3) ->
	parcours e2
    | Excl _ | In _ -> `Bool
  in
  let typ = parcours expr in
  norm_typ typ

(*  ====================================================================== *)
(** {3 Checking that the type of the expression is the one given} *)
(*  ====================================================================== *)

let checktyp decl typ expr
  =
  let rec parcours typ expr = match expr with
    | Cst cst ->
	let t = guesstyp decl expr in
	if typ <> t then
	  raise (NbacParseError (sprintf "constant %s:%s has not the expected type %s"
	    (string_of_cst cst) (string_of_typ t) (string_of_typ typ)))
    | Ref(var) ->
	let t = guesstyp decl expr in
	if typ <> t then
	  raise (NbacParseError (sprintf "symbol %s:%s has not the expected type %s"
	    var (string_of_typ t) (string_of_typ typ)))
    | Unop(op,e) ->
	begin match op with
	| `Not ->
	    if typ<>`Bool then
	      raise (NbacParseError (
		fprintf str_formatter
		"Boolean expression %a has not the expected type %s"
		print_expr expr (string_of_typ typ);
		flush_str_formatter()));
	    parcours typ e
	| `Up ->
	    if typ<>`Bool then
	      raise (NbacParseError (
		fprintf str_formatter
		"Boolean expression %a has not the expected type %s"
		print_expr expr (string_of_typ typ);
		flush_str_formatter()))
	| `Apron _ ->
	    if typ<>`Real then
		raise (NbacParseError (
		  fprintf str_formatter
		  "Numerical expression %a has not the expected type %s"
		  print_expr expr (string_of_typ typ);
		  flush_str_formatter()))
	end
    | Binop(op,e1,e2) ->
	begin match op with
	| `Apron(_,_,_) ->
	    begin match typ with
	    | `Benum _ | `Bint _ | `Int | `Real -> ()
	    | _ ->
		raise (NbacParseError (
		  fprintf str_formatter
		  "Numerical expression %a has not the expected type %s"
		  print_expr expr (string_of_typ typ);
		  flush_str_formatter()))
	    end;
	    parcours typ e1; parcours typ e2
	| `Bool _ ->
	    if typ<>`Bool then
	      raise (NbacParseError (
		fprintf str_formatter
		"Boolean expression %a has not the expected type %s"
		print_expr expr (string_of_typ typ);
		flush_str_formatter()))
	    ;
	    (* Cautious: Polymorph operator *)
	    let typ1 = guesstyp decl e1 in
	    parcours typ1 e1;
	    parcours typ1 e2
	end
    | If(e1,e2,e3) ->
	parcours `Bool e1;
	parcours typ e2;
	parcours typ e3;
    | Excl(le) ->
	if typ<>`Bool then
	  raise (NbacParseError (
	    fprintf str_formatter
	    "Boolean expression %a has not the expected type %s"
	    print_expr expr (string_of_typ typ);
	    flush_str_formatter()))
	;
	List.iter (parcours `Bool) le
    | In(e,le) ->
	if typ<>`Bool then
	  raise (NbacParseError (
	    fprintf str_formatter
	    "Boolean expression %a has not the expected type %s"
	    print_expr expr (string_of_typ typ);
	    flush_str_formatter()))
	;
	let typ1 = guesstyp decl e in
	parcours typ1 e;
	List.iter (parcours typ1) le
  in
  parcours typ expr



(*  ********************************************************************** *)
(** {2 Checking variables and label declarations} *)
(*  ********************************************************************** *)

(** We check here that variables of different kind (state, input, local) have
 different names, and also that there is no clash with names of labels. *)

let check_declaration decl =
  let set = ref (Mappe.maptoset decl.Program.state) in

  let checkset (nset:string Sette.t) (msg:string)  =
    let inter = Sette.inter !set nset in
    if not (Sette.is_empty inter) then begin
      let elt = Sette.choose inter in
      raise (NbacParseError (sprintf "Error: %s variable %s already declared" msg elt))
    end
    else
      set := Sette.union !set nset
  in

  checkset (Mappe.maptoset decl.Program.input) "input";
  checkset (Mappe.maptoset decl.Program.local) "local";
  ()

(*  ********************************************************************** *)
(** {2 Checking definitions and transitions} *)
(*  ********************************************************************** *)

(** We check here that every state variable has a corresponding
   transition function and that every local variables get a
   definition. We also perform type checking.
*)

let check_definition_transition prog
  =
  let decl = prog.decl in

  let check_typequations (map: (string,NbacExpr.symtype NbacExpr.expr) Mappe.t) : unit
    =
    Mappe.iter
      (begin fun var expr ->
	let typ = typ_of_var decl var in
	let ntyp = norm_typ typ in
	checktyp decl ntyp expr
      end)
      map
  in

  (* State variables and transition functions *)
  let setsymbol1 = Mappe.maptoset decl.Program.state
  and setsymbol2 = Mappe.maptoset prog.jump
  in
  if not (Sette.subset setsymbol1 setsymbol2) then begin
    let symbol = Sette.choose (Sette.diff setsymbol1 setsymbol2) in
    raise (NbacParseError (sprintf "Error: declared state variable %s is not given a transition function" symbol))
  end else if not (Sette.subset setsymbol2 setsymbol1) then begin
    let symbol = Sette.choose (Sette.diff setsymbol2 setsymbol1) in
    raise (NbacParseError (sprintf"transition function specified for the undeclared state variable %s" symbol))
  end;
  check_typequations prog.jump;
  check_typequations prog.flow;

  (* Local variables and definitions *)
  let setsymbol1 = Mappe.maptoset decl.Program.local
  and setsymbol2 = Mappe.maptoset prog.defs
  in
  if not (Sette.subset setsymbol1 setsymbol2) then begin
    let symbol = Sette.choose (Sette.diff setsymbol1 setsymbol2) in
    raise (NbacParseError (sprintf "Error: declared local variable %s is not given a definition" symbol))
  end else if not (Sette.subset setsymbol2 setsymbol1) then begin
    let symbol = Sette.choose (Sette.diff setsymbol2 setsymbol1) in
    raise (NbacParseError (sprintf"definition specified for the undeclared local variable %s" symbol))
  end;
  check_typequations prog.defs;

  ()

(*  ********************************************************************** *)
(** {2 Checking formulas} *)
(*  ********************************************************************** *)

let check_formulas prog
  =
  let decl = prog.decl in
  checktyp decl `Bool prog.assertion;
  checktyp decl `Bool prog.initial;
  checktyp decl `Bool prog.final;
  ()

(*  ********************************************************************** *)
(** {2 Checking optional automaton specification} *)
(*  ********************************************************************** *)

(* We check here that edges refer to declared location and that a location or
   an edge is not declared twice. We also perform type checking.
*)

(*
let check_abstract (decl:Syntax.declaration) (abstract:Syntax.abstract) : unit
  =
  let (expr,lexpr) = abstract in
  checktyp decl `Bool expr;
  List.iter (checktyp decl `Bool) lexpr;
  ()
*)

(*  ********************************************************************** *)
(** {2 Sorting of definitions} *)
(*  ********************************************************************** *)

(* The following procedure sorts definitions in such a way that a definitions
   uses only local variables that have already be defined. If a cycle is
   detected, an exception is raised.  the existence of at least one definition
   is supposed.

   The order found by the sort is given by the field [prog.order]. [prog.defs]
   is not modified. *)

let sort_definitions prog
  =
  if prog.defs<>Mappe.empty then begin
    (* Collects set of local variables (from the definitions) *)
    let (locals : string Sette.t) = Mappe.maptoset prog.defs in

    (* Returns the support (restricted only to local variables) of an expression *)
    let support expr
      =
      let rec parcours supp = function
	| Cst(_) -> supp
	| Ref(nom) ->
	    if (Sette.mem nom locals)
	    then Sette.add nom supp
	    else supp
	| Unop(_,e) -> parcours supp e
	| Binop(_,e1,e2) -> parcours (parcours supp e1) e2
	| If(e1,e2,e3) -> parcours (parcours (parcours supp e1) e2) e3
	| Excl(le) -> List.fold_left parcours supp le
	| In(e,le) -> List.fold_left parcours (parcours supp e) le
      in
      parcours Sette.empty expr
    in

    (* make dependency graph *)
    let (g:(string,unit,unit,unit) FGraph1.t ref) = ref (FGraph1.empty ()) in
    (* vertices *)
    Mappe.iter
      (fun symbol _ -> g := FGraph1.add_vertex !g symbol ())
      prog.defs;
    (* arcs *)
    Mappe.iter
      (begin fun symbol expr ->
	let supp = support expr in
	if Sette.mem symbol supp then
	  failwith ("Syntax.sort_definitions: the definition of "^symbol^" is self recursive !");
	Sette.iter
	  (fun var -> g := FGraph1.add_edge !g (symbol,var) ())
	  supp
      end)
      prog.defs;

    (* topological sort *)
    let sroot = ref Sette.empty in
    FGraph1.iter_vertex !g
      (begin fun vertex _ _ ->
	if (Sette.is_empty (FGraph1.pred !g vertex)) then
	  sroot := Sette.add vertex !sroot;
	()
      end);
    if Sette.is_empty !sroot then
      failwith "Syntax.sort_definitions: some definitions are mutually recursive !";

end



(*  ********************************************************************** *)
(** {2 Checking full program} *)
(*  ********************************************************************** *)

let check_prog prog
  =
  check_declaration prog.decl;
  check_definition_transition prog;
  check_formulas prog;
  sort_definitions prog;
  ()
