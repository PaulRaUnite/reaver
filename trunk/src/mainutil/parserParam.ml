type token =
  | TK_EOF
  | TK_COMMA
  | TK_COLON
  | TK_EQUAL
  | TK_ID of (string)
  | TK_STR of (string)

open Parsing;;
let yytransl_const = [|
  257 (* TK_EOF *);
  258 (* TK_COMMA *);
  259 (* TK_COLON *);
  260 (* TK_EQUAL *);
    0|]

let yytransl_block = [|
  261 (* TK_ID *);
  262 (* TK_STR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\004\000\001\000\003\000\001\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\008\000\001\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\006\000\007\000\004\000"

let yydgoto = "\002\000\
\004\000\008\000\009\000"

let yysindex = "\006\000\
\252\254\000\000\255\254\000\000\000\000\003\255\005\255\009\255\
\010\255\254\254\000\000\003\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\255\000\000\
\012\255\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\000\000"

let yytablesize = 13
let yytable = "\005\000\
\003\000\006\000\013\000\014\000\005\000\005\000\001\000\007\000\
\010\000\011\000\015\000\012\000\003\000"

let yycheck = "\001\001\
\005\001\003\001\005\001\006\001\001\001\002\001\001\000\005\001\
\004\001\001\001\012\000\002\001\001\001"

let yynames_const = "\
  TK_EOF\000\
  TK_COMMA\000\
  TK_COLON\000\
  TK_EQUAL\000\
  "

let yynames_block = "\
  TK_ID\000\
  TK_STR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 15 "parserParam.mly"
                    ( (_1,Mappe.empty) )
# 75 "parserParam.ml"
               : string * (string, string) Mappe.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'options) in
    Obj.repr(
# 16 "parserParam.mly"
                                ( (_1,_3) )
# 83 "parserParam.ml"
               : string * (string, string) Mappe.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'option) in
    Obj.repr(
# 18 "parserParam.mly"
                ( let (v,e) = _1 in Mappe.add v e Mappe.empty )
# 90 "parserParam.ml"
               : 'options))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'option) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'options) in
    Obj.repr(
# 19 "parserParam.mly"
                          ( let (v,e) = _1 in  Mappe.add v e _3 )
# 98 "parserParam.ml"
               : 'options))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 21 "parserParam.mly"
              ( (_1,"") )
# 105 "parserParam.ml"
               : 'option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 22 "parserParam.mly"
                       ( (_1,_3) )
# 113 "parserParam.ml"
               : 'option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 23 "parserParam.mly"
                        ( (_1,_3) )
# 121 "parserParam.ml"
               : 'option))
(* Entry param *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let param (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : string * (string, string) Mappe.t)
