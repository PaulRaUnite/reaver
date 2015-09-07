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

(*  ********************************************************************** *)
(** {2 Functions} *)
(*  ********************************************************************** *)

val parse_argv : ?current:int ref -> string array -> t -> unit
(** [Arg.parse_argv ~current args (usage_msg,tab,speclist,anon_fun)] parses
  the array [args] as if it were the command line.  It uses and updates
  the value of [~current] (if given), or [Arg.current].  You must set
  it before calling [parse_argv].  The initial value of [current]
  is the index of the program name (argument 0) in the array.
  If an error occurs, [Arg.parse_argv] raises [Arg.Bad] with
  the error message as argument.  If option [-help] or [--help] is
  given, [Arg.parse_argv] raises [Arg.Help] with the help message
  as argument.
*)

val parse : t -> unit
(** [Arg.parse (usage_msg,tab,speclist,anon_fun)] parses the command line.
    [speclist] is a list of triples [(key, spec, doc)].
    [key] is the option keyword, it must start with a ['-'] character.
    [spec] gives the option type and the function to call when this option
    is found on the command line.
    [doc] is a one-line description of this option.
    [anon_fun] is called on anonymous arguments.
    The functions in [spec] and [anon_fun] are called in the same order
    as their arguments appear on the command line.

    If an error occurs, [Arg.parse] exits the program, after printing
    an error message as follows:
-   The reason for the error: unknown option, invalid or missing argument, etc.
-   [usage_msg]
-   The list of options, each followed by the corresponding [doc] string.

    For the user to be able to specify anonymous arguments starting with a
    [-], include for example [("-", String anon_fun, doc)] in [speclist].

    By default, [parse] recognizes two unit options, [-help] and [--help],
    which will display [usage_msg] and the list of options, and exit
    the program.  You can override this behaviour by specifying your
    own [-help] and [--help] options in [speclist].
*)

val usage : t -> unit
(** [Arg.usage t] prints an error message including
    the list of valid options.  This is the same message that
    {!Arg.parse} prints in case of error.
    [speclist] and [usage_msg] are the same as for [Arg.parse]. *)

val current : int ref
(** Position (in {!Sys.argv}) of the argument being processed.  You can
    change this value, e.g. to force {!Arg.parse} to skip some arguments.
    {!Arg.parse} uses the initial value of {!Arg.current} as the index of
    argument 0 (the program name) and starts parsing arguments
    at the next element. *)

val print : Format.formatter -> t -> unit
  (** Prints the value of all variables of reference type reffered to in [t],
    in the same format than an option file. *)
