(** Lexer *)

exception Error of int * int
val lex : Lexing.lexbuf -> Yacc.token
