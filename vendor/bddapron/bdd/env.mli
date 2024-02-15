(** Normalized managers/environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 Types} *)
(*  ********************************************************************** *)

exception Bddindex

(** Type defintions. ['a] is the type of symbols (typically, [string]). *)
type 'a typdef = [
  | `Benum of 'a array
      (** An enumerated type is defined by an ordered list of labels *)
]

(** Types. ['a] is the type of symbols (typically, [string]. *)
type 'a typ = [
  | `Bool                 (** Boolean. *)
  | `Bint of (bool * int) (** [`Bint(s,n)]: bounded integer on [n] bits, signed iff [s=true]. *)
  | `Benum of 'a          (** Named enumerated type *)
]

type 'a group_tree

(** Manager for manipulating symbols.

    DO NOT USE [Marshal.to_string] and [Marshal.from_string], as
    they generate strings with NULL character, which is not
    handled properly when converted to C strings.

    You may use instead {!marshal} and {!unmarshal}. *)
type 'a symbol = {
  compare : 'a -> 'a -> int; (** Total order *)
  marshal : 'a -> string;    (** Conversion to string.  The
				 generated strings SHOULD NOT
				 contain NULL character, as they
				 may be converted to C strings. *)
  unmarshal : string -> 'a;  (** Conversion from string *)
  mutable print : Format.formatter -> 'a -> unit; (** Printing *)
}

(** Environment

- ['a] is the type of symbols;
- ['b] is the type of variables type;
- ['c] is the type of type definitions;
- ['d] is the type of CUDD managers and BDDs ([Cudd.Man.d] or [Cudd.Man.v]);
- ['e] is the type of "extension"
 *)
type ('a,'b,'c,'d,'e) t0 = {
  mutable cudd : 'd Cudd.Man.t;
    (** CUDD manager *)
  mutable typdef : ('a, 'c) PMappe.t;
    (** Named types definitions *)
  mutable vartyp : ('a, 'b) PMappe.t;
    (** Associate to a var/label its type *)
  mutable bddindex0 : int;
    (** First index for finite-type variables *)
  mutable bddsize : int;
    (** Number of indices dedicated to finite-type variables *)
  mutable bddindex : int;
    (** Next free index in BDDs used by [self#add_var]. *)
  mutable bddincr : int;
    (** Increment used by {!add_var} for incrementing
	[bddindex] *)
  mutable idcondvar : (int, 'a) PMappe.t;
    (** Associates to a BDD index the variable involved by it *)
  mutable vartid : ('a, int array) PMappe.t;
    (** (Sorted) array of BDD indices associated to finite-type variables. *)
  mutable groups: 'a group_tree list;
  mutable print_external_idcondb : Format.formatter -> int*bool -> unit;
    (** Printing conditions not managed by the environment..
	By default, [pp_print_int]. *)
  mutable ext : 'e;
  symbol : 'a symbol;
  copy_ext : 'e -> 'e;
}

(** {4 Opened signature} *)

module O : sig
  type ('a,'b,'c,'d,'e) t = ('a,'b,'c,'d,'e) t0
  constraint 'b = [>'a typ]
  constraint 'c = [>'a typdef]

  val make :
    symbol:'a symbol ->
    copy_ext:('e -> 'e) ->
    ?bddindex0:int -> ?bddsize:int -> ?relational:bool ->
    'd Cudd.Man.t -> 'e -> ('a,'b,'c,'d,'e) t

  val print :
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'c -> unit) ->
    (Format.formatter -> 'e -> unit) ->
    Format.formatter -> ('a,'b,'c,'d,'e) t -> unit
      (** Print an environment *)
end

type ('a,'d) t = ('a,'a typ,'a typdef,'d,unit) O.t

(*  ********************************************************************** *)
(** {3 Printing} *)
(*  ********************************************************************** *)

val print_typ :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> [> 'a typ] -> unit
  (** Print a type *)
val print_typdef :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> [> 'a typdef] -> unit
  (** Print a type definition *)

val print_tid : Format.formatter -> int array -> unit
val print_idcondb : ('a,'b,'c,'d,'e) O.t -> Format.formatter -> int * bool -> unit
val print_order : ('a,'b,'c,'d,'e) O.t -> Format.formatter -> unit
    (** Print the BDD variable ordering *)

val print : Format.formatter -> ('a,'b,'c,'d,'e) O.t -> unit
    (** Print an environment *)

(*  ********************************************************************** *)
(** {3 Constructors} *)
(*  ********************************************************************** *)

val marshal : 'a -> string
    (** Safe marshalling function, generating strings without NULL
	characters.

	(Based on [Marshal.to_string] with [Marshal.No_sharing] option.) *)
val unmarshal : string -> 'a
    (** Companion unmarshalling function *)

val make_symbol :
  ?compare:('a -> 'a -> int) ->
  ?marshal:('a -> string) ->
  ?unmarshal:(string -> 'a) ->
  (Format.formatter -> 'a -> unit) ->
  'a symbol
      (** Generic function for creating a manager for symbols.
	  Default values are [Pervasives.compare], {!marshal} and {!unmarshal}.

	  DO NOT USE [Marshal.to_string] and [Marshal.from_string], as they
	  generate strings with NULL character, which is not handled
	  properly when converted to C strings. *)

val string_symbol : string symbol
      (** Standard manager for symbols of type [string] *)

val make :
  symbol:'a symbol ->
  ?bddindex0:int ->
  ?bddsize:int -> ?relational:bool -> 'd Cudd.Man.t -> ('a,'d) t
      (** Create a new environment.

	  - [symbol] is the manager for manipulating symbols;
	  - [bddindex0]: starting index in BDDs for finite-type variables;
	  - [bddsize]: number of indices booked for finite-type
	  variables.  If at some point, there is no such
	  available index, a [Failure] exception is raised.
	  - [relational]: if true, primed indices (unprimed
	  indices plus one) are booked together with unprimed
	  indices. [bddincr] is initialized to 1 if
	  [relational=false], 2 otherwise.

	  Default values for [bddindex0,bddsize,relational] are
	  [0,100,false]. *)

val make_string :
  ?bddindex0:int -> ?bddsize:int -> ?relational:bool -> 'd Cudd.Man.t -> (string,'d) t
      (** [make_string XXX = make ~symbol:string_symbol XXX] *)

val copy : ('a,'b,'c,'d,'e) O.t -> ('a,'b,'c,'d,'e) O.t
      (** Copy *)

(*  ********************************************************************** *)
(** {3 Accessors} *)
(*  ********************************************************************** *)

val mem_typ : ('a,'b,'c,'d,'e) O.t -> 'a -> bool
    (** Is the type defined in the database ? *)
val mem_var : ('a,'b,'c,'d,'e) O.t -> 'a -> bool
    (** Is the label/var defined in the database ? *)
val mem_label : ('a,'b,'c,'d,'e) O.t -> 'a -> bool
    (** Is the label a label defined in the database ? *)

val typdef_of_typ : ('a,'b,'c,'d,'e) O.t -> 'a -> 'c
    (** Return the definition of the type *)

val typ_of_var : ('a,'b,'c,'d,'e) O.t -> 'a -> 'b
    (** Return the type of the label/variable *)

val vars : ('a,'b,'c,'d,'e) O.t -> 'a PSette.t
    (** Return the list of variables (not labels) *)
val labels : ('a,'b,'c,'d,'e) O.t -> 'a PSette.t
    (** Return the list of labels (not variables) *)

(*  ********************************************************************** *)
(** {3 Adding types and variables} *)
(*  ********************************************************************** *)

val add_typ_with : ('a,'b,'c,'d,'e) O.t -> 'a -> 'c -> unit
    (** Declaration of a new type *)

type packing = [ `Normalize | `Pack | `None ]

val add_vars_with : ('a,'b,'c,'d,'e) O.t ->
  ?booking_factor:int -> ?packing:packing -> ('a * 'b) list -> int array option
    (** Add the set of variables, possibly normalize the
	environment and return the applied permutation (that
	should also be applied to expressions defined in this
	environment) *)
val remove_vars_with : ('a,'b,'c,'d,'e) O.t -> 'a list -> int array option
    (** Remove the set of variables, and possibly normalize the
	environment and return the applied permutation. *)
val rename_vars_with : ('a,'b,'c,'d,'e) O.t -> ('a * 'a) list -> int array option
    (** Rename the variables, possibly normalize the environment
	and return the applied permutation. *)

val add_typ : ('a,'b,'c,'d,'e) O.t -> 'a -> 'c -> ('a,'b,'c,'d,'e) O.t
val add_vars : ('a,'b,'c,'d,'e) O.t -> ('a * 'b) list -> ('a,'b,'c,'d,'e) O.t
val remove_vars : ('a,'b,'c,'d,'e) O.t -> 'a list -> ('a,'b,'c,'d,'e) O.t
val rename_vars : ('a,'b,'c,'d,'e) O.t -> ('a * 'a) list -> ('a,'b,'c,'d,'e) O.t
    (** Functional versions of the previous functions *)

val add_var_with : ('a,'b,'c,'d,'e) O.t -> ?booking_factor:int -> 'a -> 'b -> unit
  (** Addition without normalization (internal) *)

val extend_with: ('a,'b,'c,'d,'e) O.t -> int -> unit
    (** Add BDD space (given in number of variables) *)


type 'a group_tree_spec =
  [
  | `Var of 'a
  | `Reord of 'a group_tree_spec list
  | `Fixed of 'a group_tree_spec list
  ]

val group_vars_with: ('a,'b,'c,'d,'e) O.t -> 'a group_tree_spec list -> int array

(* ********************************************************************** *)
(** {3 Operations} *)
(* ********************************************************************** *)

val iter_ordered :
  ('a,'b,'c,'d,'e) O.t -> ('a -> int array -> unit) -> unit
    (** Iter on all finite-state variables declared in the database *)

val is_leq : ('a,'b,'c,'d,'e) O.t -> ('a,'b,'c,'d,'e) O.t -> bool
    (** Test inclusion of environments in terms of types and
	variables (but not in term of indexes) *)
val is_eq : ('a,'b,'c,'d,'e) O.t -> ('a,'b,'c,'d,'e) O.t -> bool
    (** Test equality of environments in terms of types and
	variables (but not in term of indexes) *)
val shift : ('a,'b,'c,'d,'e) O.t -> int -> ('a,'b,'c,'d,'e) O.t
    (** Shift all the indices by the offset *)
val lce : ('a,'b,'c,'d,'e) O.t -> ('a,'b,'c,'d,'e) O.t -> ('a,'b,'c,'d,'e) O.t
    (** Least common environment *)
val permutation12 : ('a,'b,'c,'d,'e) O.t -> ('a,'b,'c,'d,'e) O.t -> int array
    (** Permutation for going from a subenvironment to a superenvironment *)
val permutation21 : ('a,'b,'c,'d,'e) O.t -> ('a,'b,'c,'d,'e) O.t -> int array
    (** Permutation from a superenvironment to a subenvironment *)

(*  ********************************************************************** *)
(** {3 Precomputing change of environments} *)
(*  ********************************************************************** *)

(** Contain the computed information to switch from one
    environment to another one. *)
type 'a change = {
  intro : int array option;
    (** Permutation to apply for making space for new BDD
	variables *)
  remove : ('a Cudd.Bdd.t * int array) option;
    (** BDD variables to existentially quantify out, and
	permutation to apply *)
}
val compute_change : ('a,'b,'c,'d,'e) O.t -> ('a,'b,'c,'d,'e) O.t -> 'd change

(*  ********************************************************************** *)
(** {3 Utilities} *)
(*  ********************************************************************** *)

val notfound : ('a, Format.formatter, unit, 'b) format4 -> 'a

(** Type of pairs [(environment, value)] *)
type ('a, 'b) value = { env : 'a; val0 : 'b; }

val make_value :
  ('a,'b,'c,'d,'e) O.t ->
  'f -> (('a,'b,'c,'d,'e) O.t, 'f) value
  (** Constructor *)
val get_env : ('a,'b) value -> 'a
val get_val0 : ('a,'b) value -> 'b

val extend_environment :
  ('f -> int array -> 'f) ->
  (('a,'b,'c,'d,'e) O.t, 'f) value ->
  ('a,'b,'c,'d,'e) O.t -> (('a,'b,'c,'d,'e) O.t, 'f) value
    (** [extend_environment permute value env] embed [value] in
	the new (super)environment [env], by computing the
	permutation transformation and using [permute] to apply it
	to the value. *)

(*  ********************************************************************** *)
(** {3 Internal functions} *)
(*  ********************************************************************** *)

val compare_idb : int*bool -> int*bool -> int
  (** Comparison *)

(* ====================================================================== *)
(** {4 Normalisation} *)
(* ====================================================================== *)

val pack_with : ('a,'b,'c,'d,'e) O.t -> int array
val normalize_with : ('a,'b,'c,'d,'e) O.t -> int array
(** Combine the two previous functions, and return the permutation *)
val check_normalized : ('a,'b,'c,'d,'e) O.t -> bool
(** Prints error message and returns [false] if not normalized *)

(* ====================================================================== *)
(** {4 Permutations} *)
(* ====================================================================== *)

val compose_permutation : int array -> int array -> int array
val compose_opermutation :
  int array option -> int array option -> int array option
val permutation_of_offset : int -> int -> int array

(* ====================================================================== *)
(** {4 Used by level1 APIs} *)
(* ====================================================================== *)

val check_var : ('a,'b,'c,'d,'e) O.t -> 'a -> unit
val check_lvar : ('a,'b,'c,'d,'e) O.t -> 'a list -> unit
val check_value :
  ('a,'b,'c,'d,'e) O.t ->
  (('a,'b,'c,'d,'e) O.t, 'f) value -> unit
val check_value2 :
  (('a,'b,'c,'d,'e) O.t, 'f) value ->
  (('a,'b,'c,'d,'e) O.t, 'g) value -> unit
val check_value3 :
  (('a,'b,'c,'d,'e) O.t, 'f) value ->
  (('a,'b,'c,'d,'e) O.t, 'g) value ->
  (('a,'b,'c,'d,'e) O.t, 'h) value -> unit

val check_lvarvalue :
  ('a,'b,'c,'d,'e) O.t ->
  ('a * (('a,'b,'c,'d,'e) O.t, 'f) value) list -> ('a * 'f) list
val check_lvalue :
  ('a,'b,'c,'d,'e) O.t ->
  (('a,'b,'c,'d,'e) O.t, 'f) value list -> 'f list
val check_ovalue :
  ('a,'b,'c,'d,'e) O.t ->
  (('a,'b,'c,'d,'e) O.t, 'f) value option -> 'f option

val mapunop :
  ('f -> 'g) ->
  (('a,'b,'c,'d,'e) O.t, 'f) value ->
  (('a,'b,'c,'d,'e) O.t, 'g) value

val mapbinop :
  ('f -> 'g -> 'h) ->
  (('a,'b,'c,'d,'e) O.t, 'f) value ->
  (('a,'b,'c,'d,'e) O.t, 'g) value ->
  (('a,'b,'c,'d,'e) O.t, 'h) value
val mapbinope :
  (('a,'b,'c,'d,'e) O.t -> 'f -> 'g -> 'h) ->
  (('a,'b,'c,'d,'e) O.t, 'f) value ->
  (('a,'b,'c,'d,'e) O.t, 'g) value -> (('a,'b,'c,'d,'e) O.t, 'h) value
val mapterop :
  ('f -> 'g -> 'h -> 'i) ->
  (('a,'b,'c,'d,'e) O.t, 'f) value ->
  (('a,'b,'c,'d,'e) O.t, 'g) value ->
  (('a,'b,'c,'d,'e) O.t, 'h) value ->
  (('a,'b,'c,'d,'e) O.t, 'i) value
