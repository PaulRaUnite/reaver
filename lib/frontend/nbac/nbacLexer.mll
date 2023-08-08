(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

{
open NbacParser
open Lexing

let pow10 n = 
  let res = ref 1 in
  for i=0 to n-1 do
    res := !res * 10
  done;
  !res

(*c Keywords *)
let keywords = Hashtbl.create 53
let _ =
  Array.iter
    (fun (keyword,token) -> Hashtbl.add keywords keyword token)
    [| 
      ("false",TK_FALSE);
      ("true", TK_TRUE);
      ("not", TK_NOT);
      ("or", TK_OR);
      ("and", TK_AND);
      ("xor", TK_NEQUAL);
      ("eq", TK_EQUAL);
      ("in",TK_IN);
      ("up",TK_UP);
      ("if", TK_IF);
      ("then", TK_THEN);
      ("else", TK_ELSE);
      ("typedef", TK_TYPEDEF);
      ("bool",TK_BOOL);
      ("int",TK_INT);
      ("real", TK_REAL);
      ("sint",TK_SINT);
      ("uint",TK_UINT);
      ("enum",TK_ENUM);
      ("state", TK_STATE);
      ("input", TK_INPUT);
      ("local", TK_LOCAL);
      ("transition", TK_TRANS);
      ("definition", TK_DEFINITION);
      ("invariant", TK_INVARIANT);
      ("initial", TK_INITIAL);
      ("final", TK_FINAL);
      ("assertion", TK_ASSERTION);
      ("automaton", TK_AUTOMATON);
     |]
} 

rule lexBac = parse
  eof    { TK_EOF }
| [' ' '\t'] +  { lexBac lexbuf }
| ['\r' '\n'] { Parse.lex_eol lexbuf; lexBac lexbuf }
| ['\n'] { Parse.lex_eol lexbuf; lexBac lexbuf }

(* Delimiters *)
| ","   { TK_COMMA }
| ";"   { TK_SEMI }
| '''   { TK_PRIME }
| '.'   { TK_DOT }
| "("   { TK_LPAREN }
| ")"   { TK_RPAREN }
| "{"   { TK_LBRACE }
| "}"   { TK_RBRACE }
| "["   { TK_LBRACKET }
| "]"   { TK_RBRACKET }
| ":"   { TK_COLON }

(* Boolean operations *)
| "=>"      {TK_IMPLY}
| "#"       {TK_SHARP}

(* Arithmetic operations *)
| "+"   { TK_PLUS }
| "-"   { TK_MINUS }
| "*"   { TK_MUL }
| "/"   { TK_DIV }

(* Mixed operations *)
| "<>"  { TK_NEQUAL }
| ">"   { TK_GREATER }
| "<"   { TK_LESS }
| ">="  { TK_GREATEREQUAL }
| "<="  { TK_LESSEQUAL }
| "="   { TK_EQUAL }

(* Numbers *)
| ['0'-'9']+ 
    { TK_NUM(int_of_string (lexeme lexbuf)) }
| ['0'-'9']+'/'['0'-'9']+
  {
    let str = lexeme lexbuf in
    TK_MPQF(Mpqf.of_string str)
  }
| ['0'-'9']* ('.' ['0'-'9']+) (['e' 'E'] ['+' '-']? ['0'-'9']+)?
  { 
    let str = lexeme lexbuf in
    let str = String.uppercase_ascii str in
    let (eindex, exponent) = 
      try 
	let eindex = String.index str 'E' in
	let eindex1 =
	  if str.[eindex+1] = '+' then eindex+2 else eindex+1
	in
	let substr = String.sub str eindex1 (String.length str - eindex1) in
	(eindex, int_of_string substr)
      with Not_found ->
	(String.length str, 0)
    in
    let dotindex = String.index str '.' in
    let nbaftercomma = eindex - (dotindex+1) in
    let num1 = Mpzf.of_string (String.sub str 0 dotindex) in
    let num2 = Mpzf.of_string (String.sub str (dotindex+1) nbaftercomma) in
    let mpz = Mpz.init () in
    Mpz.ui_pow_ui mpz 10 nbaftercomma;
    let den = Mpzf.of_mpz mpz in
    let num = Mpzf.add (Mpzf.mul num1 den) num2 in
    let r = Mpqf.of_mpz2 num den in
    let exponent = 
      if exponent = 0 then 
	Mpqf.of_int 1
      else begin 
	Mpz.ui_pow_ui mpz 10 (abs exponent);
	let exp = Mpzf.of_mpz mpz in
	if exponent > 0 then
	  Mpqf.of_mpz exp
	else
	  Mpqf.of_mpz2 (Mpzf.of_int 1) exp
      end
    in
    TK_MPQF(Mpqf.mul r exponent)
  }

(* Identifiers *)
| ['A'-'Z' 'a'-'z' '_'] ( ['_' 'A'-'Z' 'a'-'z' '.' '0'-'9'] ) *
    { let id = lexeme lexbuf in
      try Hashtbl.find keywords id 
      with Not_found -> TK_ID (id) }
| "(*" {comment 0 lexbuf; lexBac lexbuf}
| _         
    { raise Parse.Lex_error }

and comment level = parse
| "*)" { if level>0 then comment (level-1) lexbuf }
| "(*" { comment (level+1) lexbuf }
| _   { comment level lexbuf }
| eof { raise Parse.Lex_error }
