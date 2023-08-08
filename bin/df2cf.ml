(******************************************************************************)
(* Df2cf *)
(* DF to CF program transformation dispatcher *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Df2cf";
              Log.level=Log.Info}


exception InvalidDf2cf of string

let methods =  (* identifier * short description * long description list *)
  [("d",("discrete program","discrete program"))
  ]

(******************************************************************************)
let print_df2cf fmt () = 
  List.iter
    (fun (s,(_,longdesc)) -> 
      Format.fprintf fmt "@[ ";
      Util.print_fixed fmt 5 s;
      Format.fprintf fmt "@[<hov>";
      Util.print_breakable fmt longdesc;
      Format.fprintf fmt "@]@]@.")
    methods

let get_short_description s =
  let (desc,_) = List.assoc s methods in
  desc

(******************************************************************************)
(** parses the parameters and calls the corresponding function:
    add new Df2Cf methods here *)
let run env dfprog params = 
  let (df2cf,desc) = 
    try
      match ParseParams.parse_param params with
      (* ********** *)
      |("d",_) -> (Df2cfDiscrete.transform,get_short_description "d")
      (* ********** *)
      |_ -> raise (InvalidDf2cf params)
    with
     Exit | Parse.Lex_error | Parsing.Parse_error -> raise (InvalidDf2cf params)
  in
  Log.info_o logger (Format.pp_print_string) "Preprocessing: " desc;
  df2cf env dfprog
