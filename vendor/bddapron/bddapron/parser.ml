(** Parsing BDDAPRON expressions from strings (or lexing buffers) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Syntax
open Format

exception Error of string

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

(*  ********************************************************************** *)
(** {3 Parsing} *)
(*  ********************************************************************** *)

let expr0_of_lexbuf env cond lexbuf =
  let x = Yacc.expr Lex.lex lexbuf in
  Syntax.to_expr0 env cond x

let expr0_of_string env cond str =
  try
    let lexbuf = Lexing.from_string str in
    try expr0_of_lexbuf env cond lexbuf
    with Parsing.Parse_error ->
      error
	"Syntaxical error, characters %d-%d in expression %s"
	(Lexing.lexeme_start lexbuf)
	(Lexing.lexeme_end lexbuf)
	str
  with Lex.Error (s,e) ->
    error
      "Lexical error, characters %d-%d in expression %s"
      s e str

let expr1_of_string env cond str =
  Env.make_value env (expr0_of_string env cond str)

let listexpr1_of_lstring env cond lstr =
  let lexpr0 = List.map (expr0_of_string env cond) lstr in
  Expr1.List.of_lexpr0 env lexpr0

let listexpr2_of_lstring ?normalize ?reduce ?careset env cond lstr =
  let lexpr0 = List.map (expr0_of_string env cond) lstr in
  Expr2.List.of_lexpr0 ?normalize ?reduce ?careset env cond lexpr0

let boolexpr2_of_string ?normalize ?reduce ?careset env cond str =
  let expr0 = expr0_of_string env cond str in
  let bexpr0 = Expr0.Bool.of_expr expr0 in
  Expr2.Bool.of_expr0 ?normalize ?reduce ?careset env cond bexpr0
