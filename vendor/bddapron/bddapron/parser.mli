(** Parsing BDDAPRON expressions from strings (or lexing buffers) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(** The grammar is indicated below *)

(** {3 From strings} *)

val expr0_of_string : 'a Env.t -> 'a Cond.t -> string -> 'a Expr0.t

val expr1_of_string : 'a Env.t -> 'a Cond.t -> string -> 'a Expr1.t
val listexpr1_of_lstring :
  'a Env.t -> 'a Cond.t -> string list -> 'a Expr1.List.t

val listexpr2_of_lstring :
  ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
  'a Env.t -> 'a Cond.t -> string list -> 'a Expr2.List.t
val boolexpr2_of_string :
  ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
  'a Env.t -> 'a Cond.t -> string -> 'a Expr2.Bool.t

(** {3 Misc.} *)

val expr0_of_lexbuf : 'a Env.t -> 'a Cond.t -> Lexing.lexbuf -> 'a Expr0.t

(** {3 Grammar of expressions} *)

(**
{v
<expr> ::= <bexpr>      Boolean expression
	|  <iexpr>      bounded integer expression
	|  <eexpr>      enumerated type expression
	|  <nexpr>      numerical expression

Boolean expressions
<bexpr> ::= true | false
	 | id                            variable
	 | <constraint>
	 | not <bexpr>
	 | <bexpr> (or | and) <bexpr>
	 | ( <bexpr> )
	 | if <bexpr> then <bexpr> else <bexpr>
<constraint> ::= id == <expr>
		 <iexpr> (== | >= | > | <= | <) <iexpr>
		 <nexpr> (== | >= | > | <= | <) <nexpr>

Bounded integer expressions
<iexpr> ::= uint[<integer>][-]<integer>  constant (snd integer) of given type
	 |  sint[<integer>][-]<integer>  constant (snd integer) of given type
	 |  id                           variable
	 |  <iexpr> ( +|-|* ) <iexpr>
	 |  ( <iexpr> )
	 |  if <bexpr> then <iexpr> else <iexpr>

Enumerated type expressions
<eexpr> ::= id                           variable or constant (label)
	 |  ( <eexpr> )
	 |  if <bexpr> then <eexpr> else <eexpr>

Numerical expressions
<nexpr> ::= <coeff>
	 |  id                           variable
	 |  <unop> <nexpr>
	 |  <nexpr> <binop> <nexpr>
	 |  ( <nexpr> )
	 |  if <bexpr> then <nexpr> else <nexpr>

<binop>    ::= (+|-|*|/|%)[_(i|f|d|l|q)[,(n|0|+oo|-oo)]]
<unop>     ::= -
	    |  (cast|sqrt)[_(i|f|d|l|q)[,(n|0|+oo|-oo)]]
<coeff>    ::= <float>
	    |  <rational>
<float>    ::= C/OCaml floating-point number syntax
<rational> ::= <integer>  | <integer>/<integer>
v}

Here are is an example ([bddapron/test2.ml] file).
{[open Bddapron;;

let cudd = Cudd.Man.make_v ();;
let env = Env.make cudd;;
let cond = Cond.make cudd;;
Env.add_typ_with "e0" (`Benum [|"l0";"l1";"l2"|]);;
Env.add_vars_with [
  ("b0",`Bool);
  ("b1",`Bool);
  ("i0",`Bint(false,3));
  ("i1",`Bint(false,3));
  ("e0",`Benum("e0"));
  ("x0",`Real);
  ("x1",`Real);
  ("x2",`Real);
];;

let bexpr1 = Parser.boolexpr2_of_string env cond
  "(i0==uint[3](3) + i1 or e0==(if b0 then l0 else l1)) and x0 +_f,oo 2 *_i,0 x1 >=0";;

let apron = Polka.manager_alloc_loose ();;
let man = Domain1.make_man ~global:false apron;;
let top = Domain1.top man env;;
let abs = Domain0.meet_condition man cond top bexpr1;;
]}
*)
