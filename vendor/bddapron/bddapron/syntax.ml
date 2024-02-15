(** Building BDDAPRON expressions from Abstract Syntax Trees *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Env
open Env

(*  ********************************************************************** *)
(** {3 Types} *)
(*  ********************************************************************** *)

(** Constant *)
type cst = [
  | `Bool of bool
  | `Bint of (bool * int) * int
  | `Apron of Apron.Coeff.t
]

(** Unary operators *)
type unop = [
| `Not
| `Apron of Apron.Texpr1.unop * Apron.Texpr1.typ * Apron.Texpr1.round
]

(** Boolean/finite-type binary operators *)
type bbinop = [
  | `Or
  | `And
  | `EQ
  | `NEQ
  | `GT
  | `GEQ
  | `LEQ
  | `LT
]
(** Binary operators *)
type binop = [
| `Bool of bbinop
| `Apron of Apron.Texpr1.binop * Apron.Texpr1.typ * Apron.Texpr1.round
]

(** Expressions *)
type 'a expr = [
  | `Cst of cst
  | `Ref of 'a
  | `Unop of unop * 'a expr
  | `Binop of binop * 'a expr * 'a expr
  | `If of 'a expr * 'a expr * 'a expr
  | `In of 'a expr * 'a expr list
]

exception Error of string

(*  ********************************************************************** *)
(** {3 Error and printing functions} *)
(*  ********************************************************************** *)

let error format =
  let buffer = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buffer in
  Format.kfprintf
    (begin fun fmt ->
      Format.pp_print_flush fmt ();
      let s = Buffer.contents buffer in
      Buffer.clear buffer;
      raise (Error s)
    end)
    fmt
    format

let print_cst fmt (cst:cst)
  =
  match cst with
  | `Bool(b) -> pp_print_bool fmt b
  | `Apron(x) -> Apron.Coeff.print fmt x
  | `Bint((sign,size),c) ->
      fprintf fmt "%cint[%i](%i)" (if sign then 's' else 'u') size c

let print_unop fmt (op:unop) : unit =
  pp_print_string fmt
    begin match op with
    | `Not ->  "not"
    | `Apron (unop,typ,round) ->
	Apron.Texpr0.print_sprint_unop unop typ round
    end

let print_bbinop fmt (op:bbinop) : unit =
  pp_print_string fmt
    begin match op with
    | `Or -> "or"
    | `And -> "and"
    | `EQ -> "=="
    | `NEQ -> "!="
    | `GT -> ">"
    | `GEQ -> ">="
    | `LEQ -> "<="
    | `LT -> "<"
    end

let print_binop fmt (op:binop) =
  match op with
  | `Bool op -> print_bbinop fmt op
  | `Apron (op,typ,round) ->
      pp_print_string fmt
	(Apron.Texpr0.print_sprint_binop op typ round)

let is_zero (e:'a expr) =
  match e with
  | `Cst(`Apron x) -> Apron.Coeff.is_zero x
  | _ -> false

let precedence_of_unop = function
  | `Not -> 4
  | `Apron(_,_,_) -> 8
let precedence_of_binop = function
  | `Bool op ->
      begin match op with
      | `Or -> 1
      | `And -> 2
      | `EQ | `NEQ -> 3
      | `GT | `GEQ | `LEQ | `LT -> 5
      end
  | `Apron(op,_,_) ->
      begin match op with
      | Apron.Texpr1.Add | Apron.Texpr1.Sub -> 6
      | Apron.Texpr1.Mul | Apron.Texpr1.Div
      | Apron.Texpr1.Mod | Apron.Texpr1.Pow -> 7
      end
let precedence_of_expr = function
  | `Cst _
  | `Ref _ -> 8
  | `Unop(op,_) -> precedence_of_unop op
  | `Binop(op,_,_) -> precedence_of_binop op
  | `If(_,_,_) -> 0
  | `In(_,_) -> 3

let print_expr
  (print_symbol: Format.formatter -> 'a -> unit)
  (fmt:Format.formatter) (expr:'a expr)
  =
  (* priority: correspondance with priorities in [parser.mly] *)
  let rec print_expr ?(specialif=false) fmt (expr:'a expr) = match expr with
    | `Cst(cst) -> print_cst fmt cst
    | `Ref(var) -> print_symbol fmt var
    | `Unop(op,e) ->
	let prec = precedence_of_unop op in
	let prec1 = precedence_of_expr e in
	let par = prec1<=prec in
	fprintf fmt "%a %s%a%s"
	  print_unop op
	  (if par then "(" else "")
	  (print_expr ~specialif:false) e
	  (if par then ")" else "")
    | `Binop(op,e1,e2) ->
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
    | `If(e1,e2,e3) ->
	let nif = match e3 with
	  | `If _ -> true
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
    | `In(expr,lexpr) ->
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
  in
  print_expr fmt expr

(*  ********************************************************************** *)
(** {3 Translation functions} *)
(*  ********************************************************************** *)

let cst_to_expr0 env cond cst
    =
  match cst with
  | `Bool b ->
      Expr0.Bool.to_expr
	((if b then Expr0.Bool.dtrue else Expr0.Bool.dfalse)
	  env cond)
  | `Bint (typ,n) ->
      Expr0.Bint.to_expr
	(Expr0.Bint.of_int env cond (`Bint typ) n)
  | `Apron coeff ->
      Expr0.Apron.to_expr
	(Expr0.Apron.cst env cond coeff)

let apply_bbinop env cond op e1 e2
    =
  match op with
  | `Or | `And ->
      let e1 = Expr0.Bool.of_expr e1 in
      let e2 = Expr0.Bool.of_expr e2 in
      begin match op with
      | `Or -> Expr0.Bool.dor env cond e1 e2
      | `And -> Expr0.Bool.dand env cond e1 e2
      | _ -> failwith ""
      end
  | `EQ | `NEQ ->
      let typexpr1 = Expr0.typ_of_expr env e1 in
      let typexpr2 = Expr0.typ_of_expr env e2 in
      if typexpr1<>typexpr2 then begin
	(error
	  "arithmetic test %a applied to expressions of different types %a and %a"
	  print_bbinop op
	  (Env.print_typ env.symbol.print) typexpr1
	  (Env.print_typ env.symbol.print) typexpr2)
      end;
      let res = Expr0.eq env cond e1 e2 in
      if op = `EQ
      then res
      else Expr0.Bool.dnot env cond res
  | `GT | `GEQ | `LEQ | `LT ->
      let typexpr1 = Expr0.typ_of_expr env e1 in
      let typexpr2 = Expr0.typ_of_expr env e2 in
      if typexpr1<>typexpr2 then begin
	(error
	  "arithmetic test %a applied to expressions of different types %a and %a"
	  print_bbinop op
	  (Env.print_typ env.symbol.print) typexpr1
	  (Env.print_typ env.symbol.print) typexpr2)
      end;
      begin match typexpr1 with
      | `Bint(b,size) ->
	  let e1 = Expr0.Bint.of_expr e1 in
	  let e2 = Expr0.Bint.of_expr e2 in
	  begin match op with
	  | `GT ->  Expr0.Bint.sup env cond e1 e2
	  | `GEQ -> Expr0.Bint.supeq env cond e1 e2
	  | `LEQ -> Expr0.Bint.supeq env cond e2 e1
	  | `LT ->  Expr0.Bint.sup env cond e2 e1
	  | _ -> failwith ""
	  end
      | `Real ->
	  let e1 = Expr0.Apron.of_expr e1 in
	  let e2 = Expr0.Apron.of_expr e2 in
	  begin match op with
	  | `GT ->
	      let e = Expr0.Apron.sub env cond e1 e2 in
	      Expr0.Apron.sup env cond e
	  | `GEQ ->
	      let e = Expr0.Apron.sub env cond e1 e2 in
	      Expr0.Apron.supeq env cond e
	  | `LEQ ->
	      let e = Expr0.Apron.sub env cond e2 e1 in
	      Expr0.Apron.supeq env cond e
	  | `LT ->
	      let e = Expr0.Apron.sub env cond e2 e1 in
	      Expr0.Apron.sup env cond e
	  | _ -> failwith ""
	  end
      | _ ->
	  (error
	    "arithmetic test %a applied to non arithmetic expressions of type %a"
	    print_bbinop op
	    (Env.print_typ env.symbol.print) typexpr1)
      end

let apply_binop
    env cond
    (binop:binop)
    (e1:'a Expr0.t)
    (e2:'a Expr0.t)
    :
    'a Expr0.t
    =
  match binop with
  | `Bool op ->
      let e = apply_bbinop env cond op e1 e2 in
      Expr0.Bool.to_expr e
  | `Apron(op,typ,round) ->
      let typexpr1 = Expr0.typ_of_expr env e1 in
      let typexpr2 = Expr0.typ_of_expr env e2 in
      if typexpr1<>typexpr2 then begin
	(error
	  "arithmetic operation %a applied to expressions of different types %a and %a"
	  Apron.Texpr0.print_binop op
	  (Env.print_typ env.symbol.print) typexpr1
	  (Env.print_typ env.symbol.print) typexpr2
	)
      end;
      begin match typexpr1 with
      | `Bint(b,size) ->
	  let e1 = Expr0.Bint.of_expr e1 in
	  let e2 = Expr0.Bint.of_expr e2 in
	  let fop = match op with
	    | Apron.Texpr1.Add -> Expr0.Bint.add
	    | Apron.Texpr1.Sub -> Expr0.Bint.sub
	    | Apron.Texpr1.Mul -> Expr0.Bint.mul
	    | Apron.Texpr1.Div
	    | Apron.Texpr1.Mod
            | Apron.Texpr1.Pow ->
		error
		  "operation %a not permitted on bounded integer expressions"
		  Apron.Texpr0.print_binop op
	  in
	  let e = fop env cond e1 e2 in
	  Expr0.Bint.to_expr e
      | `Real ->
	  let e1 = Expr0.Apron.of_expr e1 in
	  let e2 = Expr0.Apron.of_expr e2 in
	  let fop = match op with
	    | Apron.Texpr1.Add -> Expr0.Apron.add
	    | Apron.Texpr1.Sub -> Expr0.Apron.sub
	    | Apron.Texpr1.Mul -> Expr0.Apron.mul
	    | Apron.Texpr1.Div -> Expr0.Apron.div
	    | Apron.Texpr1.Mod -> Expr0.Apron.gmod
            | Apron.Texpr1.Pow ->
		error
		  "unsupported operation %a on real expressions"
		  Apron.Texpr0.print_binop op
	  in
	  let e = fop env cond ~typ ~round e1 e2 in
	  Expr0.Apron.to_expr e
      | _ ->
	  error
	    "arithmetic operation %a applied to non-arithmetic expressions of type %a"
	    Apron.Texpr0.print_binop op
	    (Env.print_typ env.symbol.print) typexpr1
      end

let rec to_expr0 (env:'a Env.t) (cond:'a Cond.t) (expr:string expr) : 'a Expr0.t
    =
  try
    let res = match expr with
      | `Cst cst -> cst_to_expr0 env cond cst
      | `Ref var -> let var = env.symbol.unmarshal var in
	  if not (Env.mem_var env var) then
	    (error
	      "unknown label/variable %a" env.symbol.print var)
	  else
	    Expr0.var env cond var
      | `Unop(op,e) ->
	  let e = to_expr0 env cond e in
	  begin match op with
	  | `Not ->
	      Expr0.Bool.to_expr
		(Expr0.Bool.dnot env cond
		  (Expr0.Bool.of_expr e))
	  | `Apron (op,typ,round) ->
	      let typexpr = Expr0.typ_of_expr env e in
	      begin match typexpr with
	      | `Bint(b,size) ->
		  let e = Expr0.Bint.of_expr e in
		  let e = begin match op with
		    | Apron.Texpr1.Neg ->
			if not b then
			  error
			    "@[<v>@ negation cannot be applied to the expression@ %a@ of type %a (unsigned integer)@ @]"
			    (Expr0.Bint.print env cond) e
			    (Env.print_typ env.symbol.print) typexpr
			;
			Expr0.Bint.neg env cond e
		    | Apron.Texpr1.Cast
		    | Apron.Texpr1.Sqrt ->
			error
			  "@[<v>@ cast or sqrt operators cannot be applied to the expression@ %a@ of type %a@ @]"
			  (Expr0.Bint.print env cond) e
			  (Env.print_typ env.symbol.print) typexpr
		  end
		  in
		  Expr0.Bint.to_expr e
	      | `Real ->
		  let e = Expr0.Apron.of_expr e in
		  let e = match op with
		    | Apron.Texpr1.Neg -> Expr0.Apron.negate env cond e
		    | Apron.Texpr1.Cast -> Expr0.Apron.cast env cond ~typ ~round e
		    | Apron.Texpr1.Sqrt -> Expr0.Apron.sqrt env cond ~typ ~round e
		  in
		  Expr0.Apron.to_expr e
	      | _ ->
		  error
		    "@[<v>@ neg, cast or sqrt operators cannot be applied to the expression@ %a@ of type %a@ @]"
		    (Expr0.print env cond) e
		    (Env.print_typ env.symbol.print) typexpr
	      end
	  end
      | `Binop(op,e1,e2) ->
	  let e1 = to_expr0 env cond e1 in
	  let e2 = to_expr0 env cond e2 in
	  apply_binop env cond op e1 e2
      | `If(e1,e2,e3) ->
	  let e1 = to_expr0 env cond e1 in
	  let e1 = Expr0.Bool.of_expr e1 in
	  let e2 = to_expr0 env cond e2 in
	  let e3 = to_expr0 env cond e3 in
	  Expr0.ite env cond e1 e2 e3
      | `In(e0,le) ->
	  let e0 = to_expr0 env cond e0 in
	  let acc = Expr0.Bool.dfalse env cond in
	  let res =
	    List.fold_left
	      (begin fun acc e ->
		let e = to_expr0 env cond e in
		Expr0.Bool.dor env cond
		  acc
		  (Expr0.eq env cond e0 e)
	      end)
	      acc le
	  in
	  Expr0.Bool.to_expr res
    in
    res
  with Error(s) ->
    error "@[<v>%s@ in expression@   %a@]"
      s (print_expr pp_print_string) expr

let to_expr1 env cond expr =
  Env.make_value env (to_expr0 env cond expr)

let to_listexpr1 env cond lexpr =
  let lexpr0 = List.map (to_expr0 env cond) lexpr in
  Expr1.List.of_lexpr0 env lexpr0

let to_listexpr2 ?normalize ?reduce ?careset env cond lexpr =
  let lexpr0 = List.map (to_expr0 env cond) lexpr in
  Expr2.List.of_lexpr0 ?normalize ?reduce ?careset env cond lexpr0

let to_boolexpr2 ?normalize ?reduce ?careset env cond expr =
  let expr0 = to_expr0 env cond expr in
  let bexpr0 = Expr0.Bool.of_expr expr0 in
  Expr2.Bool.of_expr0 ?normalize ?reduce ?careset env cond bexpr0
