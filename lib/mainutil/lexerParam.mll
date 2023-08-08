(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

{
open ParserParam
open Lexing

(*c Keywords *)
let keywords = Hashtbl.create 53
let _ =
  Array.iter
    (fun (keyword,token) -> Hashtbl.add keywords keyword token)
    [||]
} 

rule param = parse
  eof    { TK_EOF }
| ","   { TK_COMMA }
| ":"   { TK_COLON }
| "="   { TK_EQUAL }
| "{"   { str 0 "" lexbuf }

| ( ['_' 'A'-'Z' 'a'-'z' '0'-'9'] ) *
    { let id = lexeme lexbuf in
      try Hashtbl.find keywords id 
      with Not_found -> TK_ID (id) }
| _ { raise Parse.Lex_error } 

and str level s = parse
| "}" { if level=0 then TK_STR s
        else str (level-1) (s^"}") lexbuf }
| "{" { str (level+1) (s^"{") lexbuf }
| _   { str level (s^(lexeme lexbuf)) lexbuf }
| eof { raise Parse.Lex_error }


