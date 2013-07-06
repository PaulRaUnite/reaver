type token =
  | TK_EOF
  | TK_COMMA
  | TK_COLON
  | TK_EQUAL
  | TK_ID of (string)
  | TK_STR of (string)

val param :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string * (string, string) Mappe.t
