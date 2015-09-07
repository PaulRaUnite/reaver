
{
open Parser
open Lexing

(* Keywords *)
let keywords = Hashtbl.create 53
let _ =
  Array.iter
    (fun (keyword,token) -> Hashtbl.add keywords keyword token)
    [|
      ("true",TK_TRUE);
      ("guard",TK_GUARD);
      ("and",TK_AND);
      ("assign",TK_ASSIGN);
      ("jordan",TK_JORDAN);
      ("jordansage",TK_JORDANSAGE);
      ("matrix",TK_MATRIX);
      ("var",TK_VAR);
      ("initial",TK_INITIAL);
      ("I",TK_IMAGINARY)
    |]

let newline (lexbuf:Lexing.lexbuf) : unit
  =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  };
  ()

}

rule token = parse
(* ignore spaces, tabs, and \r *)
  ['\r' ' ' '\t']+   { token lexbuf }
(* line count *)
| '\n'
    { newline lexbuf; token lexbuf }
(* comments: nested /* */, and // are allowed *)
| "/*" { Syntax.start_of_comment := Lexing.lexeme_start_p lexbuf;
	 comment lexbuf ;
	 token lexbuf }
| "//"([^'\n']*)     { token lexbuf }

| "["      { TK_LBRACKET }
| "]"      { TK_RBRACKET }
| "{"      { TK_LBRACE }
| "}"      { TK_RBRACE }
| "("      { TK_LPAR }
| ")"      { TK_RPAR }
| ":="    { TK_ASS }
| "->"    { TK_ARROW }
| ":"    { TK_COLON }
| ";"    { TK_SEMICOLON }
| ","    { TK_COMMA }
| "%i"   { TK_IMAGINARY }
(* Arithmetic operations *)
| "+" { TK_ADD }
| "-" { TK_SUB }
| "*" { TK_MUL }

(* Boolean operations *)
| ">"  { TK_SUP }
| "<"   { TK_INF }
| ">="  { TK_SUPEG }
| "<="   { TK_INFEG }
| "="   { TK_EG }
| (['0'-'9'])+ ('/'['0'-'9']+)?
  {
    let str = lexeme lexbuf in
    let res = TK_MPQF(Mpqf.of_string str) in
    res
  }
| ['0'-'9']* ('.' ['0'-'9']*['?']?) (['e' 'E'] ['+' '-']? ['0'-'9']+)?
  {
    let str = lexeme lexbuf in
    let str =
      try
	let i = String.index str '?' in
	(String.sub str 0 i)^(String.sub str (i+1) ((String.length str)-i-1))
      with Not_found ->
	str
    in
    TK_FLOAT(float_of_string str)
  }
(* Identifiers *)
| ['A'-'Z' 'a'-'z' '_']
    ( ['_' 'A'-'Z' 'a'-'z' '0'-'9' '.'] ) *
    ( ['\''] ) *
      {
	let id = Lexing.lexeme lexbuf in
	try Hashtbl.find keywords id
	with Not_found -> TK_ID id
}
(* end of file *)
| eof         { TK_EOF }
| '\004'      { TK_EOF }

and comment = parse
    "/*"   { comment lexbuf; comment lexbuf }
  | "*/"   { () }
  | '\n'
      { newline lexbuf; comment lexbuf }
  | [^'\n' '*' '/']+ { comment lexbuf }
  | _              { comment lexbuf }
  | eof            { raise (Syntax.Unterminated_comment !Syntax.start_of_comment) }
