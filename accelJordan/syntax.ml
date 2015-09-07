
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
    (* [(list of variables, initial location, transitions)] *)

let start_of_comment = ref Lexing.dummy_pos

exception Unterminated_comment of Lexing.position
exception Error of string
exception ErrorLoc of Lexing.position * Lexing.position * string
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

let errorloc pos1 pos2 s = raise (ErrorLoc(pos1,pos2,s))
