type token =
  | TK_EOF
  | TK_LPAREN
  | TK_RPAREN
  | TK_LBRACE
  | TK_RBRACE
  | TK_LBRACKET
  | TK_RBRACKET
  | TK_COMMA
  | TK_SEMI
  | TK_PRIME
  | TK_DOT
  | TK_COLON
  | TK_TRUE
  | TK_FALSE
  | TK_NOT
  | TK_OR
  | TK_AND
  | TK_NEQUAL
  | TK_EQUAL
  | TK_SHARP
  | TK_IMPLY
  | TK_IN
  | TK_MUL
  | TK_DIV
  | TK_PLUS
  | TK_MINUS
  | TK_GREATEREQUAL
  | TK_LESSEQUAL
  | TK_GREATER
  | TK_LESS
  | TK_UP
  | TK_IF
  | TK_THEN
  | TK_ELSE
  | TK_TYPEDEF
  | TK_ENUM
  | TK_BOOL
  | TK_INT
  | TK_REAL
  | TK_CLOCK
  | TK_UINT
  | TK_SINT
  | TK_INVARIANT
  | TK_INITIAL
  | TK_FINAL
  | TK_ASSERTION
  | TK_INPUT
  | TK_LOCAL
  | TK_STATE
  | TK_CONST
  | TK_TRANS
  | TK_DEFINITION
  | TK_AUTOMATON
  | TK_NUM of (int)
  | TK_MPQF of (Mpqf.t)
  | TK_ID of (string)

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> NbacExpr.prog
