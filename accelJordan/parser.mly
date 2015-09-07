/* $Id$ */

/* Syntaxical analysis to convert strings to objects. */

%{
(* This file is part of the APRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Apron
open Syntax

let rec neg acc = function
  | [] -> acc
  | (var,coeff)::l ->
      let nacc =
      if Coeff.is_zero coeff then
	acc
      else
	(var,(Coeff.neg coeff))::acc
      in
      neg nacc l

let neg sum1 sum2 =
  let sum1 =
    List.filter
      (fun (var,coeff) -> not (Coeff.is_zero coeff))
      sum1
  in
  neg sum1 sum2

%}
/* ====================================================================== */
/* Lexems */
/* ====================================================================== */

%token TK_EOF
%token TK_SUPEG TK_INFEG TK_SUP TK_INF TK_EG

%token TK_LBRACKET TK_RBRACKET TK_LBRACE TK_RBRACE TK_LPAR TK_RPAR TK_ARROW TK_COLON TK_SEMICOLON TK_COMMA TK_ASS TK_IMAGINARY

%token TK_ADD TK_SUB TK_MUL

%token TK_TRUE TK_GUARD TK_AND TK_ASSIGN TK_JORDAN TK_JORDANSAGE TK_MATRIX TK_VAR TK_INITIAL

%token <Mpqf.t> TK_MPQF
%token <float> TK_FLOAT
%token <string> TK_ID

%start auto

%type <Syntax.t> auto

%%

/* ====================================================================== */
/* Rules */
/* ====================================================================== */

auto:
  TK_VAR idlist TK_SEMICOLON
  TK_INITIAL id TK_SEMICOLON
  ltransition TK_EOF
{ ($2,$5,$7) }

ltransition:
  transition { [$1] }
| ltransition transition { $2::$1 }

transition:
  id TK_ARROW id TK_COLON
  TK_GUARD guard
  TK_ASSIGN TK_LBRACE lassign TK_RBRACE loop TK_SEMICOLON
{ ($1,$3,($6,($9,$11))) }

loop:
| TK_JORDAN matrix jordan matrix
    { `Jordan($2,$3,$4) }
| TK_JORDANSAGE matrixsage jordansage matrixsage
    { `Jordansage($2,$3,$4) }
| { `None }

jordan:
    TK_LBRACKET lblock TK_RBRACKET
{
  Jordan.(
    let (lblock,dim) =
      List.fold_left
	(fun (lblock,dim) block ->
	  (block::lblock, dim+block.sumk))
	([],0)
	$2
    in
    { lblock; dim }
  )
}
lblock:
  block { [$1] }
| lblock TK_COMMA block { $3::$1 }
block:
  TK_LBRACKET complex TK_COMMA linteger TK_RBRACKET
  { Jordan.(
    let (lk,sumk) =
      List.fold_left
	(fun (lk,sumk) k ->
	  (k::lk, sumk+k))
	([],0)
	$4
    in
    { eigenvalue=$2; lk; sumk }
    )
  }

linteger:
  integer { [$1] }
| linteger TK_COMMA integer { $3::$1 }
integer:
  TK_MPQF
{
  let num = Mpqf.get_num $1 and den = Mpqf.get_den $1 in
  if Mpzf.to_float den <> 1. then failwith "integer and not rational expected in list of integers";
  int_of_float (Mpzf.to_float num)
}
| TK_FLOAT
{
  if $1 = ceil $1 then
    int_of_float $1
  else
    failwith "integer and not fractional floating-point number expected in list of integers"
}

matrix:
  TK_MATRIX TK_LPAR lvector TK_RPAR { Array.of_list (List.rev $3) }
lvector:
| lvector TK_COMMA vector { $3::$1 }
| vector { [$1] }
vector:
  TK_LBRACKET lcomplex TK_RBRACKET { Array.of_list (List.rev $2) }
lcomplex:
| lcomplex TK_COMMA complex { $3::$1 }
| complex { [$1] }

lassign:
| lassign assign { $2::$1 }
| { [] }

assign:
  id TK_ASS linexpr0 TK_SEMICOLON { ($1,$3) }

guard:
  TK_TRUE { [||] }
| llincons0 { Array.of_list $1 }

llincons0:
| llincons0 TK_AND lincons0 { $3::$1 }
| lincons0 { [$1] }

lincons0:
  linexpr0 TK_EG linexpr0 { (Lincons0.EQ, neg $1 $3) }
| linexpr0 TK_SUP linexpr0 { (Lincons0.SUP, neg $1 $3) }
| linexpr0 TK_SUPEG linexpr0 { (Lincons0.SUPEQ, neg $1 $3) }
| linexpr0 TK_INFEG linexpr0 { (Lincons0.SUPEQ, neg $3 $1) }
| linexpr0 TK_INF linexpr0 { (Lincons0.SUP, neg $3 $1) }

linexpr0:
  linexpr0 TK_ADD term
    { $3::$1 }
| linexpr0 TK_SUB term
{ let (var,coeff) = $3 in (var,Coeff.neg coeff)::$1 }
| term { [$1] }
term:
  coeff id { ($2,$1) }
| coeff TK_MUL id { ($3,$1) }
| coeff { ("",$1) }
| id { ($1, Coeff.s_of_int 1) }
| TK_SUB id { ($2, Coeff.s_of_int (-1)) }

idlist:
  id { [$1] }
| idlist TK_COMMA id { $3::$1 }

id:
  TK_ID { $1 }

scalar0:
  TK_MPQF { Scalar.Mpqf($1) }
| TK_FLOAT { Scalar.Float($1) }
scalar:
| TK_SUB scalar0 { Scalar.neg $2 }
| scalar0 { $1 }
coeff:
| TK_LBRACKET scalar TK_SEMICOLON scalar TK_RBRACKET
    { Coeff.Interval(Interval.of_infsup $2 $4) }
| scalar { Coeff.Scalar $1 }

imscalar0:
  TK_IMAGINARY { Scalar.Float(1.0) }
| scalar0 TK_MUL TK_IMAGINARY { $1 }

imscalar:
  TK_SUB imscalar0 { Scalar.neg $2 }
| imscalar0 { $1 }

complex:
| scalar TK_ADD imscalar0
    { { Complex.re = Matrix.FF.of_scalar $1;
	Complex.im = Matrix.FF.of_scalar $3; } }
| scalar TK_SUB imscalar0
   { { Complex.re = Matrix.FF.of_scalar $1;
	Complex.im = Matrix.FF.of_scalar (Scalar.neg $3); } }
| imscalar TK_ADD scalar
    { { Complex.re = Matrix.FF.of_scalar $3;
	Complex.im = Matrix.FF.of_scalar $1; } }
| imscalar TK_SUB scalar
    { { Complex.re = Matrix.FF.of_scalar (Scalar.neg $3);
	Complex.im = Matrix.FF.of_scalar $1; } }
| imscalar
    { { Complex.re = 0.;
	Complex.im = Matrix.FF.of_scalar $1; } }
| scalar { { Complex.re = Matrix.FF.of_scalar $1; Complex.im=0. } }

real: scalar
  { Matrix.FF.of_scalar $1 }

jordansage:
    TK_LBRACKET lblocksage TK_RBRACKET
{
  Jordan.(
    let (lblock,dim) =
      List.fold_left
	(fun (lblock,dim) block ->
	  (block::lblock, dim+block.sumk))
	([],0)
	$2
    in
    { lblock; dim }
  )
}

lblocksage:
  blocksage { [$1] }
| lblocksage TK_COMMA blocksage { $3::$1 }

blocksage:
  TK_LPAR complex TK_COMMA TK_LBRACKET linteger TK_RBRACKET TK_RPAR
  { Jordan.(
    let (lk,sumk) =
      List.fold_left
	(fun (lk,sumk) k ->
	  (k::lk, sumk+k))
	([],0)
	$5
    in
    { eigenvalue=$2; lk; sumk }
    )
  }

matrixsage:
  TK_MATRIX TK_LPAR TK_LBRACKET lvectorsage TK_RBRACKET TK_RPAR { Array.of_list (List.rev $4) }
lvectorsage:
| lvectorsage TK_COMMA vectorsage { $3::$1 }
| vectorsage { [$1] }
vectorsage:
  TK_LBRACKET lreal TK_RBRACKET { Array.of_list (List.rev $2) }
lreal:
| lreal TK_COMMA real { $3::$1 }
| real { [$1] }
