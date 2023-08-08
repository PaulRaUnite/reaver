(******************************************************************************)
(* parseParams *)
(* utilities to parse parameters and options *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="ParseParams";
              Log.level=Log.Debug3}

type options_t = (string, string) Mappe.t
type param_t = string * options_t


let parse_param str =
  Parse.parse_string ~lexer:LexerParam.param ~parser:ParserParam.param str

let get_option options defaults key converter =
  try converter (Mappe.find key options)
  with Not_found -> converter (Mappe.find key defaults)

let semicolon_str_to_strlist str = (Str.split (Str.regexp_string ";") str)
let comma_str_to_strlist str = (Str.split (Str.regexp_string ",") str)

(* parses a string list into a list of boolean BDD APRON expressions *)
let strlist_to_boolexprlist env strlist =
  List.map 
    (fun expr -> 
       match expr with 
         |`Bool(e) -> e 
         |_ -> Bddapron.Expr0.Bool.dtrue env.Env.env env.Env.cond)
    (List.filter
       (fun expr -> 
          match expr with
	   |`Bool(e) -> true
	   |ex -> Log.warn_o logger 
                    (Bddapron.Expr0.print env.Env.env env.Env.cond) 
                    "expression ignored: " ex; false)
       (List.map 
         (fun str -> 
            Bddapron.Parser.expr0_of_string env.Env.env env.Env.cond str)
         strlist))
