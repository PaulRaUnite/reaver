(******************************************************************************)
(* NbacExpr *)
(* Abstract syntax tree for expressions and datatypes *)
(* author: Peter Schrammel, Bertrand Jeannet *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

open Format

exception NbacParseError of string

type symtype = string
let symtype_compare = String.compare
let symtype_print = Format.pp_print_string
let symtype_null = ""

type typ = symtype Bddapron.Env.typ
type typdef = symtype Bddapron.Env.typdef

(*  ********************************************************************** *)
(** {2 Expressions} *)
(*  ********************************************************************** *)

(** Constant *)
type cst = [
  | `Bool of bool
  | `Bint of (bool * int) * int
  | `Apron of Apron.Coeff.t
]
type unop = [
| `Not
| `Up
| `Apron of Apron.Texpr1.unop * Apron.Texpr1.typ * Apron.Texpr1.round
]
type bbinop = Or | And | EQ | NEQ | GT | GEQ | LEQ | LT
type binop = [
| `Bool of bbinop
| `Apron of Apron.Texpr1.binop * Apron.Texpr1.typ * Apron.Texpr1.round
]
type 'a expr =
  | Cst of cst
  | Ref of 'a
  | Unop of unop * 'a expr
  | Binop of binop * 'a expr * 'a expr
  | If of 'a expr * 'a expr * 'a expr
  | Excl of 'a expr list
  | In of 'a expr * 'a expr list

type prog = {
  decl: Program.declaration_t;
  defs: (symtype,symtype expr) Mappe.t;
  jump: (symtype,symtype expr) Mappe.t;
  flow: (symtype,symtype expr) Mappe.t;
  assertion: symtype expr;
  initial: symtype expr;
  final: symtype expr;
}


(*  ********************************************************************** *)
(** {2 Printing} *)
(*  ********************************************************************** *)

let print_typ 
  (fmt:Format.formatter) 
  (typ:typ) 
  :
  unit
  =
  match typ with
  | `Bool -> pp_print_string fmt "bool"
  | `Bint(sign,size) -> fprintf fmt "%cint[%i]" (if sign then 's' else 'u') size
  | `Benum s -> pp_print_string fmt s
  | `Int -> pp_print_string fmt "int"
  | `Real -> pp_print_string fmt "real"

let string_of_typ = Print.string_of_print print_typ

let print_typdef (fmt:Format.formatter) (typdef:typdef) = 
  match typdef with
  | `Benum array ->
      fprintf fmt "enum { %a }"
      (Print.array ~first:"@[" ~sep:",@ " ~last:"@]" pp_print_string)
      array
let print_cst (fmt:Format.formatter) (cst:cst) 
  =
  match cst with
  | `Bool(b) -> pp_print_bool fmt b
  | `Apron(x) -> Apron.Coeff.print fmt x
  | `Bint((sign,size),c) -> 
      fprintf fmt "%cint[%i](%i)" (if sign then 's' else 'u') size c

let string_of_cst = Print.string_of_print print_cst

let print_unop fmt (op:unop) : unit =
  pp_print_string fmt
    begin match op with
    | `Not ->  "not"
    | `Up ->  "up"
    | `Apron (unop,typ,round) ->
	Apron.Texpr0.print_sprint_unop unop typ round
    end
let print_bbinop fmt (op:bbinop) : unit =
  pp_print_string fmt 
    begin match op with
    | Or -> "or"
    | And -> "and"
    | EQ -> "=="
    | NEQ -> "<>"
    | GT -> ">"
    | GEQ -> ">="
    | LEQ -> "<="
    | LT -> "<"
    end

let print_binop fmt (op:binop) =
  match op with
  | `Bool op -> print_bbinop fmt op
  | `Apron (op,typ,round) ->
      pp_print_string fmt
      (Apron.Texpr0.print_sprint_binop op typ round)

let is_zero (e:'a expr) =
  match e with
  | Cst(`Apron x) -> Apron.Coeff.is_zero x
  | _ -> false

let precedence_of_unop = function
  | `Not -> 4
  | `Up -> 5
  | `Apron(_,_,_) -> 8
let precedence_of_binop = function
  | `Bool op -> 
      begin match op with
      | Or -> 1
      | And -> 2
      | EQ | NEQ -> 3
      | GT | GEQ | LEQ | LT -> 5
      end
  | `Apron(op,_,_) -> 
      begin match op with
      | Apron.Texpr1.Add | Apron.Texpr1.Sub -> 6
      | Apron.Texpr1.Mul | Apron.Texpr1.Div | Apron.Texpr1.Mod -> 7
      end
let precedence_of_expr = function
  | Cst _ 
  | Ref _ -> 8
  | Unop(op,_) -> precedence_of_unop op
  | Binop(op,_,_) -> precedence_of_binop op
  | If(_,_,_) -> 0
  | In(_,_) -> 3
  | Excl(_) -> 3

let print_expr (fmt:Format.formatter) (expr:'ref expr) 
  =
  (* priority: correspondance with priorities in [parser.mly] *)
  let rec print_expr ?(specialif=false) fmt (expr:'a expr) = match expr with
    | Cst(cst) -> print_cst fmt cst
    | Ref(var) -> pp_print_string fmt var
    | Unop(op,e) ->
	let prec = precedence_of_unop op in
	let prec1 = precedence_of_expr e in
	let par = prec1<=prec in
	fprintf fmt "%a %s%a%s"
	  print_unop op
	  (if par then "(" else "")
	  (print_expr ~specialif:false) e
	  (if par then ")" else "")
    | Binop(op,e1,e2) ->
	let prec = precedence_of_binop op in
	let prec1 = precedence_of_expr e1 in
	let prec2 = precedence_of_expr e2 in
	let par1 = prec1<prec in
	let par2 = prec2<=prec in
	fprintf fmt "@[<hov>%s%a%s %a@ %s%a%s@]"
  	  (if par1 then "(" else "")
	  (print_expr ~specialif:false) e1
  	  (if par1 then ")" else "")
	  print_binop op
  	  (if par2 then "(" else "")
	  (print_expr ~specialif:false) e2
  	  (if par2 then ")" else "")
    | If(e1,e2,e3) ->
	let nif = match e3 with
	  | If _ -> true
	  | _ -> false
	in
	let prec = 0 in
	let prec1 = precedence_of_expr e1 in
	let par1 = prec1<=prec in
	if not specialif then fprintf fmt "@[<hov>";
	fprintf fmt "if %s%a%s@ then %a@ else %a"
  	  (if par1 then "(" else "")
	  (print_expr ~specialif:false) e1 
  	  (if par1 then ")" else "")
	  (print_expr ~specialif:false) e2 
	  (print_expr ~specialif:nif) e3
	;
	if not specialif then fprintf fmt "@]";
    | In(expr,lexpr) ->
	let prec = 3 in
	let prec1 = precedence_of_expr expr in
	let par1 = prec1<=prec in
	fprintf fmt "%s%a%s in {%a}"
  	  (if par1 then "(" else "")
	  (print_expr ~specialif:false) expr
  	  (if par1 then ")" else "")
	  (Print.list
	    ~first:"@[<hov>" ~sep:",@," ~last:"@]"
	    (fun fmt expr ->
	      let prec1 = precedence_of_expr expr in
	      let par1 = prec1<=prec in
	      fprintf fmt "%s%a%s"
		(if par1 then "(" else "")
		(print_expr ~specialif:false) expr
  		(if par1 then ")" else "")
	    )
	  )
	  lexpr
    | Excl(expr) ->
 	let prec = 3 in
	fprintf fmt "#(%a)"
	  (Print.list
	    ~first:"@[<hov>" ~sep:",@," ~last:"@]"
	    (fun fmt expr ->
	      let prec1 = precedence_of_expr expr in
	      let par1 = prec1<=prec in
	      fprintf fmt "%s%a%s"
		(if par1 then "(" else "")
		(print_expr ~specialif:false) expr
  		(if par1 then ")" else "")
	    )
	  )
          expr
  in
  print_expr fmt expr

let string_of_expr a = Print.string_of_print (print_expr) a

let print_equation (fmt:Format.formatter) (equation:symtype * symtype expr) : unit =
  let (var,expr) = equation in
  Format.fprintf fmt "%s'=%a" var print_expr expr

(*  ********************************************************************** *)
(** {2 Utilities} *)
(*  ********************************************************************** *)

let etrue = Cst(`Bool true)
let efalse = Cst(`Bool false)
let is_true = function
  | Cst(`Bool true) 
  | Unop(`Not,Cst(`Bool false)) -> true
  | _ -> false
let is_false = function
  | Cst(`Bool false) 
  | Unop(`Not,Cst(`Bool true)) -> true
  | _ -> false

let rec support ?filter res expr =
  match expr with
  | Cst _ -> res
  | Ref var -> 
      let b = match filter with
	| Some f -> f var
	| None -> true
      in
      if b
      then Sette.add var res
      else res
  | Unop(_,e) -> support ?filter res e
  | Binop(_,e1,e2) -> support ?filter (support ?filter res e1) e2
  | If(e1,e2,e3) -> support ?filter (support ?filter (support ?filter res e1) e2) e3
  | In(e,le) ->
      List.fold_left
	(support ?filter)
	(support ?filter res e) le
  | Excl(e) ->
      match e with 
      | [] -> res
      | hd::tl ->  List.fold_left
	             (support ?filter)
 	             (support ?filter res hd) tl

let print_declarations
  (fmt:Format.formatter)
  (decl:Program.declaration_t)
  : 
  unit
  =
  (* Type definitions *)
  if PMappe.is_empty decl.Program.typdef then begin
    PMappe.print
      ~first:"typedef@.   @[<v>" ~sep:"@ " ~last:"@]@."
      ~firstbind:"" ~sepbind:" = " ~lastbind:";"
      pp_print_string print_typdef
      fmt
      decl.Program.typdef
  end;
  (* State variables *)
  Mappe.print
    ~first:"state@.   @[<v>" ~sep:"@ " ~last:"@]@."
    ~firstbind:"" ~sepbind:" : " ~lastbind:";"
    pp_print_string print_typ
    fmt
   decl.Program.state
  ;
  (* Input variables *)
  if decl.Program.input <> Mappe.empty then begin
    Mappe.print
      ~first:"input@.   @[<v>" ~sep:"@ " ~last:"@]@."
      ~firstbind:"" ~sepbind:" : " ~lastbind:";"
      pp_print_string print_typ
      fmt
      decl.Program.input
  end;
  (* Local variables *)
  if decl.Program.local <> Mappe.empty then begin
    Mappe.print
      ~first:"local@.   @[<v>" ~sep:"@ " ~last:"@]@."
      ~firstbind:"" ~sepbind:" : " ~lastbind:";"
      pp_print_string print_typ
      fmt
      decl.Program.local
  end;
  ()

let print_definitions
  (fmt:Format.formatter)
  (defs:(string,'a expr) Mappe.t)
  : 
  unit
  =
  if defs <> Mappe.empty then begin
    Mappe.print
      ~first:"definition@.  @[<v>" ~sep:"@ " ~last:"@]@."
      ~firstbind:"" ~sepbind:" = " ~lastbind:";"
      pp_print_string print_expr
      fmt
      defs
  end;
  ()
  
let print_transitions
  (fmt:Format.formatter)
  (jump:(string,'a expr) Mappe.t)
  (flow:(string,'a expr) Mappe.t)
  : 
  unit
  =
  Mappe.print
    ~first:"transition@.  @[<v>" ~sep:"@ " ~last:"@]@."
    ~firstbind:"" ~sepbind:"' = " ~lastbind:";"
    pp_print_string print_expr
    fmt
    jump;
  Mappe.print
    ~first:"  @[<v>" ~sep:"@ " ~last:"@]@."
    ~firstbind:"." ~sepbind:" = " ~lastbind:";"
    pp_print_string print_expr
    fmt
    flow;
  ()

let print
  (fmt:Format.formatter)
  (prog:prog)
  =
  print_declarations fmt prog.decl;
  print_definitions fmt prog.defs;
  print_transitions fmt prog.jump prog.flow;
  fprintf fmt "assertion %a;@."
    print_expr prog.assertion;
  fprintf fmt "initial %a;@."
    print_expr prog.initial;
  fprintf fmt "final %a;@."
    print_expr prog.final;
  ()

