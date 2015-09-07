(* This file is part of the APRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(** Lexical analysis of expressions, constraints, generators *)

val token: Lexing.lexbuf -> Parser.token
