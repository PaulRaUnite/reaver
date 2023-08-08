(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** ReaVer main utilities: parsing of command line options *)

type options_t = (string, string) Mappe.t (** option map *) 
type param_t = string * options_t (** parameter with options *)

(** parse a parameter *)
val parse_param : string -> param_t 

(** get option with given default values *) 
val get_option : options_t -> options_t -> string -> (string -> 'a) -> 'a

(** parse semicolon-separated string list *) 
val semicolon_str_to_strlist : string -> string list

(** parse comma-separated string list *) 
val comma_str_to_strlist : string -> string list

(** parse list of Boolean expressions *) 
val strlist_to_boolexprlist : Env.t -> string list -> Env.boolexpr_t list
