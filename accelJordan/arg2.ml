(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: arg.ml,v 1.1.1.1 2003/04/18 19:20:36 jeannet Exp $ *)

(** Modified parsing of command line arguments *)

open Format

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

type key = string
type doc = (unit,Format.formatter,unit) format
type usage_msg = (unit,Format.formatter,unit) format
type anon_fun = (string -> unit)

type t = usage_msg * int * entry list * anon_fun
and entry = key*spec*string*doc
and spec =
  | Unit of (unit -> unit)     (** Call the function with unit argument *)
  | Bool of (bool -> unit)     (** Call the function with a bool argument *)
  | Set_bool of bool ref       (** Set the reference to the bool argument *)
  | Toggle of bool ref
  | String of (string -> unit) (** Call the function with a string argument *)
  | Set_string of string ref   (** Set the reference to the string argument *)
  | Int of (int -> unit)       (** Call the function with an int argument *)
  | Set_int of int ref         (** Set the reference to the int argument *)
  | Float of (float -> unit)   (** Call the function with a float argument *)
  | Set_float of float ref     (** Set the reference to the float argument *)
  | Tuple of spec list         (** Take several arguments according to the
				  spec list *)
  | Symbol of string list * (string -> unit)
                               (** Take one of the symbols as argument and
                                  call the function with the symbol. *)
  | Rest of (string -> unit)   (** Stop interpreting keywords and call the
				  function with each remaining argument *)
  | Recursive of t

(*  ********************************************************************** *)
(** {2 Exceptions} *)
(*  ********************************************************************** *)

exception Bad of string

type error =
  | Unknown of string
  | Wrong of string * string * string  (** [option, actual, expected] *)
  | Missing of string
  | Message of string

exception Stop of error (* used internally *)

(*  ********************************************************************** *)
(** {2 Command line} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Printing and messages} *)
(*  ====================================================================== *)

let rec assoc4 (x:string) (l:entry list) : spec =
  match l with
  | [] -> raise Not_found
  | (y1, y2, y3, y4)::t when y1 = x -> 
      y2
 | _::t -> 
      assoc4 x t

let help_action () = raise (Stop (Unknown "-help"));;

let add_help (t:t) : t = 
  let (usage_msg,tab,entries,anonfun) = t in
  let nentries =
    let add1 =
      try ignore (assoc4 "-help" entries); []
      with Not_found ->
	["help", Unit help_action, "", ("Display this list of options":doc)]
    and add2 =
      try ignore (assoc4 "--help" entries); []
      with Not_found ->
	["-help", Unit help_action, "", ("Display this list of options":doc)]
    in
    entries @ (add1 @ add2)
  in
  (usage_msg,tab,nentries,anonfun)

let print_offset (fmt:Format.formatter) (offset:int) =
  let str = String.make offset ' ' in
  pp_print_string fmt str

let rec print_entry 
  (offset:int)
  (tab:int option)
  (fmt:Format.formatter) ((key,spec,arg,doc):entry) 
  = 
  begin match (tab,spec) with
  | ((Some _),(Recursive _)) ->
      print_offset fmt offset;
      pp_print_string fmt (String.make (72-offset) '-');
      pp_print_space fmt ();
  | (_,_) -> ()
  end;
  print_offset fmt offset;
  begin match spec with
  | Toggle _ ->
      fprintf fmt "(+/-)%s %s" key arg;
  | Symbol(l,_) ->
      fprintf fmt "-%s %a" key
	(Print.list ~first:"{@["~sep:"|@," ~last:"@]}" pp_print_string)
	l
  | _ ->
      fprintf fmt "-%s %s" key arg;
  end;
  begin match tab with
  | Some tab ->
      let str = String.make tab ' ' in
      pp_print_string fmt str;
      pp_set_tab fmt ()
  | None ->
      pp_print_tab fmt (); 
  end;
  fprintf fmt doc;
  pp_print_space fmt ();
  begin match spec with
  | Recursive(t) -> 
      print (offset+3) fmt t;
      print_offset fmt offset;
      pp_print_string fmt (String.make (72-offset) '-');
      pp_print_space fmt ();
  | _ -> ()
  end;
  ()

and print (offset:int) (fmt:Format.formatter) (t:t) =
  let (usage_msg,tab,entries,anonfun) = t in
  print_offset fmt offset;
  pp_open_tbox fmt ();
  fprintf fmt usage_msg;
  pp_print_space fmt ();
  print_entry (offset+1) (Some tab) fmt (List.hd entries);
  List.iter (print_entry (offset+1) None fmt) (List.tl entries);
  pp_close_tbox fmt ();
  ()

let usage_aux (fmt:Format.formatter) (t:t) =
  let t = add_help t in
  fprintf fmt "@[<v>";
  print 0 fmt t;
  fprintf fmt "@.";
  ()

let usage (t:t) =
  let b = Buffer.create 200 in
  let fmt = Format.formatter_of_buffer b in
  usage_aux fmt t;
  fprintf Format.err_formatter "%s" (Buffer.contents b)

(*  ====================================================================== *)
(** {3 Parsing} *)
(*  ====================================================================== *)

let decompose (key:key) (str:string) : string array
  =
  (* Split str into a list *)
  let list = ref [key]
  and index = ref 0
  in
  begin
    try
      while true do
	let last = String.index_from str !index ' ' in
	let substr = String.sub str !index (last - !index) in
	list := substr :: !list;
	index := last+1
      done
    with Not_found ->
      let substr = String.sub str !index (String.length str - !index) in
      list := substr :: !list;
  end;
  Array.of_list (List.rev !list)

let current = ref 0

let rec parse_argv 
    ?(current=current) 
    (argv:string array) 
    (t:t) 
    :
    unit
    =
  let l = Array.length argv in
  let initpos = !current in
  let (usage_msg,tab,entries,anon_fun) = t in

  let stop (error:error) : unit
      =
    let progname =
      if initpos < l then argv.(initpos) else "(?)" in
    usage t;
    begin match error with
    | Unknown "-help" -> ()
    | Unknown "--help" -> ()
    | Unknown s ->
	fprintf Format.err_formatter "%s: unknown option `%s'.@." progname s
    | Missing s ->
	fprintf Format.err_formatter "%s: option `%s' needs an argument.@." progname s
    | Wrong (opt, arg, expected) ->
	fprintf Format.err_formatter "%s: wrong argument `%s'; option `%s' expects %s.@."
	  progname arg opt expected
    | Message s ->
	fprintf Format.err_formatter "%s: %s.@." progname s
    end;
    exit 2
  in

  let rec treat_action (s:string) (spec:spec) : unit
      =
    match spec with
    | Unit f -> f ();
    | Bool f when !current + 1 < l ->
	let arg = argv.(!current + 1) in
	begin try f (bool_of_string arg)
	with Invalid_argument "bool_of_string" ->
	  raise (Stop (Wrong (s, arg, "a boolean")))
	end;
	incr current;
    | Set_bool r when !current + 1 < l ->
	let arg = argv.(!current + 1) in
	begin try r := (bool_of_string arg)
	with Failure "bool_of_string" ->
	  raise (Stop (Wrong (s, arg, "a Boolean")))
	end;
	incr current;
    | Toggle r ->
	begin match String.get argv.(!current) 0 with
	| '+' -> r := true
	| '-' -> r := false
	| _ -> failwith ""
	end
    | String f when !current + 1 < l ->
	f argv.(!current + 1);
	incr current;
    | Set_string r when !current + 1 < l ->
	r := argv.(!current + 1);
	incr current;
    | Symbol (symb, f) when !current + 1 < l ->
        let arg = argv.(!current + 1) in
        if List.mem arg symb then begin
          f argv.(!current + 1);
          incr current;
        end else begin
          raise (Stop (Wrong (s, arg, Print.sprintf "one of: %a"
            (Print.list ~first:"@[" ~sep:"@ " pp_print_string) symb)))
        end
    | Int f when !current + 1 < l ->
	let arg = argv.(!current + 1) in
	begin try f (int_of_string arg)
	with Failure "int_of_string" ->
	  raise (Stop (Wrong (s, arg, "an integer")))
	end;
	incr current;
    | Set_int r when !current + 1 < l ->
	let arg = argv.(!current + 1) in
	begin try r := (int_of_string arg)
	with Failure "int_of_string" ->
	  raise (Stop (Wrong (s, arg, "an integer")))
	end;
	incr current;
    | Float f when !current + 1 < l ->
	let arg = argv.(!current + 1) in
	begin try f (float_of_string arg);
	with Failure "float_of_string" ->
	  raise (Stop (Wrong (s, arg, "a float")))
	end;
	incr current;
    | Set_float r when !current + 1 < l ->
	let arg = argv.(!current + 1) in
	begin try r := (float_of_string arg);
	with Failure "float_of_string" ->
	  raise (Stop (Wrong (s, arg, "a float")))
	end;
	incr current;
    | Tuple specs ->
	List.iter (treat_action s) specs;
    | Rest f ->
	while !current < l-1 do
	  f argv.(!current+1);
	  incr current;
	done;
    | Recursive(nt)when !current + 1 < l ->
	let arg = argv.(!current + 1) in
	let nargv = decompose s arg in
	let ncurrent = ref 0 in
	parse_argv ~current:ncurrent nargv nt;
	incr current
    | _ -> raise (Stop (Missing s))
  in

  (* main loop *)
  begin try
    incr current;
    while !current < l do
      let s = argv.(!current) in
      let (option:bool option) =
	if String.length s >= 1 then begin
	  match String.get s 0 with
	  | '+' -> Some true
	  | '-' -> Some false
	  | _ -> None
	end
	else None
      in
      begin match option with
      | Some b ->
	  let ns = String.sub s 1 (pred(String.length s)) in
	  let action =
	    try assoc4 ns entries
	    with Not_found -> raise (Stop (Unknown s))
	  in
	  if b then
	    begin match action with
	    | Toggle r -> ()
	    | _  -> raise (Stop (Unknown s))
	    end
	  ;
	  treat_action ns action;
	  incr current;
	  ()
      | None ->
	  anon_fun s;
	  incr current;
	  ()
      end;
      ()
    done;
  with 
  | Stop error -> stop error
  | Bad m -> stop (Message m)
  end;
  ()

let parse (t:t) : unit
  =
  parse_argv Sys.argv t

(*  ********************************************************************** *)
(** {2 Printing options} *)
(*  ********************************************************************** *)

let print (fmt:Format.formatter) (t:t) =
  let first = ref true in

  let rec print_aux (prefix:string) (t:t) 
    =
    let (_,_,entries,_) = t in
    List.iter
      (begin fun (key,spec,_,_) ->
	let is_print = 
	  begin match spec with
	  | Toggle _ | Set_bool _ | Set_int _ | Set_float _ | Set_string _ ->
	      true
	  | _ ->
	      false
	  end
	in
	if is_print then begin
	  if !first then first:=false else pp_print_space fmt ();
	  fprintf fmt "%s%s" prefix key;
	  pp_print_tab fmt ();
	  pp_print_string fmt " = ";
	  begin match spec with 
	  | Toggle r | Set_bool r -> pp_print_bool fmt !r
	  | Set_int r -> pp_print_int fmt !r
	  | Set_float r -> pp_print_float fmt !r
	  | Set_string r -> pp_print_string fmt !r
	  | _ -> failwith ""
	  end;
	end
	else begin
	  match spec with
	  | Recursive t ->
	      print_aux (key^".") t
	  | _ -> ()
	end;
      end)
      entries;
    ()
  in
  fprintf fmt "@[<v>";
  pp_open_tbox fmt ();
  pp_print_string fmt (String.make 18 ' ');
  pp_set_tab fmt ();
  pp_print_space fmt ();
  print_aux "" t;
  fprintf fmt "@]";
  ()

