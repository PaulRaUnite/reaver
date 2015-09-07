
type linexpr = (string * Apron.Coeff.t) list
type lincons = Apron.Lincons0.typ * linexpr
type loop = [
| `Jordan of Matrix.C.t * Complex.t Jordan.t * Matrix.C.t
| `Jordansage of Matrix.F.t * Complex.t Jordan.t * Matrix.F.t
| `None
]
type transformation =
    (string * linexpr) list * loop
type guardedtransformation = (lincons array) * transformation

type transition = string * string * guardedtransformation

type t = string list * string * transition list

val start_of_comment : Lexing.position ref
exception Unterminated_comment of Lexing.position
  (** Raised during lexical analysis *)
exception Error of string
exception ErrorLoc of Lexing.position * Lexing.position * string
  (** Lexical or syntaxical analysis *)
val error : ('a, Format.formatter, unit, 'b) format4 -> 'a
val errorloc : Lexing.position -> Lexing.position -> string -> 'a
  (** Raises an error *)
