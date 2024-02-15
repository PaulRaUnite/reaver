/*
  This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution
*/

%{
open Syntax
%}

/* tokens */
%token TK_LBRACKET TK_RBRACKET TK_SEMICOLON TK_COLON TK_LPAR TK_RPAR TK_LBRACE TK_RBRACE
%token TK_BOOL TK_UINT TK_SINT TK_INT TK_REAL
%token TK_IN TK_COMMA
%token TK_TYPEDEF TK_ENUM TK_IF TK_THEN TK_ELSE
%token TK_VERTEX TK_RAY TK_LINE TK_MOD TK_RAYMOD TK_LINEMOD
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_MUL
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_ADD
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_SUB
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_DIV
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_MODULO
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_CAST
%token <(Apron.Texpr1.typ * Apron.Texpr1.round)> TK_SQRT
%token <Mpqf.t> TK_MPQF
%token <float> TK_FLOAT

%token TK_LEQ TK_GEQ TK_LT TK_GT TK_EQ TK_NEQ
%token TK_AND TK_OR TK_NOT
%token <string> TK_ID
%token TK_TRUE TK_FALSE
%token TK_EOF

/* precedence */
/*
%nonassoc TK_SEMICOLON
%nonassoc TK_IF TK_THEN TK_ELSE
%nonassoc TK_LPAR TK_RPAR
%left TK_OR
%left TK_AND
%nonassoc TK_NOT
%nonassoc TK_LEQ TK_GEQ TK_L TK_G TK_EQ TK_NEQ
*/

/* types */
%type <string Syntax.expr> expr
/* %type <(string * Bddapron.Env.typ) list> declarations */
%start expr
%%

coeff:
  scalar { Apron.Coeff.Scalar $1 }
| interval { Apron.Coeff.Interval $1 }
scalar:
  TK_MPQF { Apron.Scalar.Mpqf($1) }
| TK_FLOAT { Apron.Scalar.Float($1) }
interval:
  TK_LBRACKET scalar TK_COMMA scalar TK_RBRACKET
   { Apron.Interval.of_scalar $2 $4 }
cst:
  TK_TRUE  { `Bool(true) }
| TK_FALSE { `Bool(false) }
| bint num {
    `Bint($1,$2) }
| bint TK_SUB num { `Bint($1,-$3) }
| coeff { `Apron $1 }
num: TK_MPQF
{
  let mpqf = $1 in
  if Mpzf.cmp_int (Mpqf.get_den mpqf) 1 = 0 then
    let mpz = Mpz.init() in
    Mpq.get_num mpz mpqf;
    Mpz.get_int mpz
  else
    raise (Error (Print.sprintf "Error: expecting integer here"))
}
expr: expr0 TK_EOF { $1 }

list_expr0:
  expr0 { [$1] }
| expr0 TK_COMMA list_expr0 { $1::$3 }

expr0:
  expr1 { $1 }
| TK_IF expr0 TK_THEN expr0 TK_ELSE expr0 { `If($2,$4,$6) }
expr1:
  expr2 { $1 }
| expr1 TK_OR expr2   { `Binop(`Bool `Or,$1,$3) }
expr2:
  expr3 { $1 }
| expr2 TK_AND expr3  { `Binop(`Bool `And,$1,$3) }
expr3:
  expr4 { $1 }
| expr3 TK_NEQ expr4  { `Binop((`Bool `NEQ),$1,$3) }
| expr3 TK_EQ expr4   { `Binop((`Bool `EQ),$1,$3) }
| expr6 TK_IN TK_LBRACE list_expr0 TK_RBRACE  { `In($1,$4) }
expr4:
  expr5 { $1 }
| TK_NOT expr4 { `Unop(`Not,$2) }
expr5:
  expr6 { $1 }
| expr6 TK_GEQ expr6 { `Binop(`Bool `GEQ,$1,$3) }
| expr6 TK_GT expr6 { `Binop(`Bool `GT,$1,$3) }
| expr6 TK_LEQ expr6 { `Binop(`Bool `LEQ,$1,$3) }
| expr6 TK_LT expr6 { `Binop(`Bool `LT,$1,$3) }
expr6:
  expr7 { $1 }
| expr6 TK_ADD expr7
    { let (t,r) = $2 in `Binop(`Apron(Apron.Texpr1.Add,t,r), $1,$3) }
| expr6 TK_SUB expr7
    { let (t,r) = $2 in `Binop(`Apron(Apron.Texpr1.Sub,t,r), $1,$3) }
expr7:
  expr8 { $1 }
| expr7 TK_MUL expr8
    { let (t,r) = $2 in `Binop(`Apron(Apron.Texpr1.Mul,t,r), $1,$3) }
| expr7 TK_DIV expr8
    { let (t,r) = $2 in `Binop(`Apron(Apron.Texpr1.Div,t,r), $1,$3) }
| expr7 TK_MODULO expr8
    { let (t,r) = $2 in `Binop(`Apron(Apron.Texpr1.Mod,t,r), $1,$3) }
expr8:
  TK_CAST expr8
    { let (t,r) = $1 in `Unop(`Apron(Apron.Texpr1.Cast,t,r),$2) }
| TK_SQRT expr8
    { let (t,r) = $1 in `Unop(`Apron(Apron.Texpr1.Sqrt,t,r),$2) }
| TK_SUB expr8
    { let (t,r) = $1 in `Unop(`Apron(Apron.Texpr1.Neg,t,r),$2) }
| TK_LPAR expr0 TK_RPAR
    { $2 }
| cst
   { `Cst $1 }
| TK_ID
    { `Ref $1 }

typ:
  TK_BOOL { `Bool }
| TK_INT { (`Int) }
| TK_REAL { (`Real) }
| bint { (`Bint $1) }
| TK_ID { (`Benum $1) }
bint:
  TK_UINT TK_LBRACKET num TK_RBRACKET
{
  if $3 < 0 then
    raise (Error (Print.sprintf "Error: in type uint[x], x should be positive"))
  ;
  (false,$3)
}
| TK_SINT TK_LBRACKET num TK_RBRACKET
{
  if $3 < 0 then
    raise (Error (Print.sprintf "Error: in type sint[x], x should be positive"))
  ;
  (true,$3)
}

/*
variables: revvariables { List.rev $1 }
revvariables:
  revvariables TK_COMMA TK_ID { $3::$1 }
| TK_ID { [$1] }
| { [] }


declaration:
  revvariables TK_COLON typ
  {
    let lvar = $1 and typ = $3 in
    List.map (fun var -> (var,typ)) lvar
  }

revdeclarations:
  revdeclarations TK_COMMA declaration { $3 @ $1 }
| declaration { $1 }
| { [] }

declarations: revdeclarations { List.rev $1 }

typedef:
  TK_ID TK_EQ TK_ENUM TK_LBRACE variables TK_RBRACE TK_SEMICOLON
    { ($1,Array.of_list $5) }

ltypedef:
  ltypedef typedef {$2::$1}
| typedef {[$1]}

typedefs:
  TK_TYPEDEF ltypedef { $2 }
| {[]}
*/
