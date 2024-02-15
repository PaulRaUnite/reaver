(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

{
open Syntax
open Yacc
exception Error of int * int

(* Keywords *)
let keywords = Hashtbl.create 53
let _ =
  Array.iter
    (fun (keyword,token) -> Hashtbl.add keywords keyword token)
    [|
      ("typedef", TK_TYPEDEF);
      ("enum", TK_ENUM);
      ("true", TK_TRUE);
      ("false",TK_FALSE);
      ("if",   TK_IF);
      ("then",   TK_THEN);
      ("else",   TK_ELSE);
      ("not",  TK_NOT);
      ("and",  TK_AND);
      ("or",   TK_OR);
      ("in", TK_IN);
      ("bool", TK_BOOL);
      ("sint", TK_SINT);
      ("uint", TK_UINT);
      ("int",  TK_INT);
      ("real",  TK_REAL);
    |]

let newline (lexbuf:Lexing.lexbuf) : unit
  =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  };
  ()

let attributes_of_string (pos:int) lexbuf =
  let str = Lexing.lexeme lexbuf in
  let length = (String.length str) - pos in
  let suffix = String.sub str pos length in
  let index = ref 0 in
  if length=0 then
    (Apron.Texpr1.Real, Apron.Texpr1.Rnd)
  else begin
    if String.get suffix !index <> '_' || length = 1 then
      raise (Syntax.Error "")
    ;
    incr index;
    let round_typ =
      match String.get suffix !index with
      | 'i' -> Apron.Texpr1.Int
      | 'f' -> Apron.Texpr1.Single
      | 'd' -> Apron.Texpr1.Double
      | 'l' -> Apron.Texpr1.Extended
      | 'q' -> Apron.Texpr1.Quad
      | _ -> raise (Syntax.Error "");
    in
    if length = 2 then
      (round_typ, Apron.Texpr1.Rnd)
    else begin
      incr index;
      if (String.get suffix !index) <> ',' || length = 3 then
	raise (Syntax.Error "")
      ;
      incr index;
      let round_dir =
	match String.sub suffix !index (length - !index) with
	| "n" -> Apron.Texpr1.Near
	| "0" -> Apron.Texpr1.Zero
	| "+oo" -> Apron.Texpr1.Up
	| "-oo" -> Apron.Texpr1.Down
	| "?" -> Apron.Texpr1.Rnd
	|  _ -> raise (Syntax.Error "")
      in
      (round_typ, round_dir)
    end
  end
}

rule lex = parse
  (* ignore spaces, tabs, and \r *)
  ['\r' ' ' '\t']+   { lex lexbuf }

  (* line count *)
  | '\n'
      { newline lexbuf; lex lexbuf }

  (* numbers *)
  | (['0'-'9'])+ ('/'['0'-'9']+)?
      {
	let str = Lexing.lexeme lexbuf in
	TK_MPQF(Mpqf.of_string str)
      }
  | ['0'-'9']* ('.' ['0'-'9']+) (['e' 'E'] ['+' '-']? ['0'-'9']+)?
      {
	let str = Lexing.lexeme lexbuf in
	TK_FLOAT(float_of_string str)
      }

  (* keywords *)
  | "{"    { TK_LBRACE }
  | "}"    { TK_RBRACE }
  | "["    { TK_LBRACKET }
  | "]"    { TK_RBRACKET }
  | "("    { TK_LPAR }
  | ")"    { TK_RPAR }
  | ":"    { TK_COLON }
  | ";"    { TK_SEMICOLON }
  | ","    { TK_COMMA }

  | "mod"  { TK_MOD }
  | "V:"   { TK_VERTEX }
  | "R:"   { TK_RAY }
  | "L:"   { TK_LINE }
  | "RM:"  { TK_RAYMOD }
  | "LM:"  { TK_LINEMOD }

  | "<="     { TK_LEQ }
  | ">="     { TK_GEQ }
  | "<"      { TK_LT }
  | ">"      { TK_GT }
  | "!="     { TK_NEQ }
  | "=="      { TK_EQ }

  (* Arithmetic operations *)
  | "+"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
      { TK_ADD(attributes_of_string 1 lexbuf) }
  | "-"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
      { TK_SUB(attributes_of_string 1 lexbuf) }
  | "*"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
      { TK_MUL(attributes_of_string 1 lexbuf) }
  | "/"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
      { TK_DIV(attributes_of_string 1 lexbuf) }
  | "%"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
      { TK_MODULO(attributes_of_string 1 lexbuf) }
  | "cast"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
      { TK_CAST(attributes_of_string 4 lexbuf) }
  | "sqrt"(('_'['i''f''d''l''q'])(','("n"|"0"|"+oo"|"-oo"|"?"))?)?
      { TK_SQRT(attributes_of_string 4 lexbuf) }

    (* identifiers *)
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
      {
	let id = Lexing.lexeme lexbuf in
	try Hashtbl.find keywords id
	with Not_found -> TK_ID id }

    (* end of file *)
  | eof         { TK_EOF }
  | '\004'      { TK_EOF }
  | _ { raise (Error(Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }
      
