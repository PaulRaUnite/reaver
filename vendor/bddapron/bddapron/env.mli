(** Normalized variable managers/environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 Types} *)
(*  ********************************************************************** *)

(** Type definitions. ['a] is the type of symbols (typically, [string]). *)
type 'a typdef = 'a Bdd.Env.typdef

(** Types. ['a] is the type of symbols (typically, [string]). *)
type 'a typ = [
  | 'a Bdd.Env.typ
  | Apronexpr.typ
]

(** Manager for manipulating symbols.

    DO NOT USE [Marshal.to_string] and [Marshal.from_string], as they
    generate strings with NULL character, which is not handled
    properly when converted to C strings.

    You may use instead {!marshal} and {!unmarshal}. *)
type 'a symbol = 'a Bdd.Env.symbol = {
  compare : 'a -> 'a -> int; (** Total order *)
  marshal : 'a -> string;    (** Conversion to string.  The
				 generated strings SHOULD NOT
				 contain NULL character, as they
				 may be converted to C strings. *)
  unmarshal : string -> 'a;  (** Conversion from string *)
  mutable print : Format.formatter -> 'a -> unit; (** Printing *)
}

(** Environment extension. ['a] is the type of symbols, ['b] is
    the type of further extension. *)
type ('a,'b) ext = {
  mutable table : 'a Apronexpr.t Cudd.Mtbdd.table;
  mutable eapron : Apron.Environment.t;
  mutable aext : 'b;
}

(** Environment.

    - ['a] is the type of symbols;
    - ['b] is the type of variables type;
    - ['c] is the type of type definitions;
    - ['d] is the type of further extension

    See {!Bdd.Env.t0} for more (internal) details.
 *)
type ('a,'b,'c,'d) t0 = ('a,'b,'c,Cudd.Man.v,('a,'d) ext) Bdd.Env.t0

(** {4 Opened signature} *)
module O : sig
  type ('a,'b,'c,'d) t = ('a,'b,'c,'d) t0
  constraint 'b = [>'a typ]
  constraint 'c = [>'a typdef]

  val make :
    symbol:'a symbol ->
    copy_aext:('d -> 'd) ->
    ?bddindex0:int -> ?bddsize:int -> ?relational:bool ->
    Cudd.Man.vt -> 'd -> ('a,'b,'c,'d) t
      (** Create a new database.  Default values for
	  [bddindex0,bddsize,relational] are [0,100,false].
	  [bddincr] is initialized to 1 if [relational=false], 2
	  otherwise. *)
  val print :
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'c -> unit) ->
    (Format.formatter -> 'd -> unit) ->
    Format.formatter -> ('a,'b,'c,'d) t -> unit
      (** Print an environment *)
end

type 'a t = ('a, 'a typ,'a typdef,unit) O.t

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

val print_idcondb : ('a,'b,'c,'d) O.t -> Format.formatter -> int * bool -> unit
val print_order : ('a,'b,'c,'d) O.t -> Format.formatter -> unit
    (** Print the BDD variable ordering *)

val print : Format.formatter -> ('a,'b,'c,'d) O.t -> unit
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
      (** Generic function for creating a manager for symbols
	  Default values are [Pervasives.compare], {!marshal} and {!unmarshal}.

	  DO NOT USE [Marshal.to_string] and [Marshal.from_string], as they
	  generate strings with NULL character, which is not handled
	  properly when converted to C strings.  *)

val string_symbol : string symbol
      (** Standard manager for symbols of type [string] *)

val make :
  symbol:'a symbol ->
  ?bddindex0:int -> ?bddsize:int -> ?relational:bool -> Cudd.Man.vt -> 'a t
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
  ?bddindex0:int -> ?bddsize:int -> ?relational:bool -> Cudd.Man.vt -> string t
      (** [make_string XXX = make ~symbol:string_symbol XXX] *)

val copy : ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t
      (** Copy *)

val copy_ext : copy_aext:('b -> 'c) -> ('a,'b) ext -> ('a,'c) ext

(*  ********************************************************************** *)
(** {3 Accessors} *)
(*  ********************************************************************** *)

val mem_typ : ('a,'b,'c,'d) O.t -> 'a -> bool
    (** Is the type defined in the database ? *)
val mem_var : ('a,'b,'c,'d) O.t -> 'a -> bool
    (** Is the label/var defined in the database ? *)
val mem_label : ('a,'b,'c,'d) O.t -> 'a -> bool
    (** Is the label a label defined in the database ? *)

val typdef_of_typ : ('a,'b,'c,'d) O.t -> 'a -> 'c
    (** Return the definition of the type *)

val typ_of_var : ('a,'b,'c,'d) O.t -> 'a -> 'b
    (** Return the type of the label/variable *)

val vars : ('a,'b,'c,'d) O.t -> 'a PSette.t
    (** Return the list of variables (not labels) *)
val labels : ('a,'b,'c,'d) O.t -> 'a PSette.t
    (** Return the list of labels (not variables) *)
val apron :  ('a,'b,'c,'d) O.t -> Apron.Environment.t
    (** return the APRON sub-environment *)

(*  ********************************************************************** *)
(** {3 Adding types and variables} *)
(*  ********************************************************************** *)

val add_typ_with : ('a,'b,'c,'d) O.t -> 'a -> 'c -> unit
    (** Declaration of a new type *)

val add_vars_with :
  ('a,'b,'c,'d) O.t -> ?booking_factor:int -> ?packing:Bdd.Env.packing -> ('a * 'b) list ->
  int array option
    (** Add the set of variables, possibly normalize the
	environment and return the applied permutation (that
	should also be applied to expressions defined in this
	environment) *)
val remove_vars_with :
  ('a,'b,'c,'d) O.t -> 'a list ->
  int array option
    (** Remove the set of variables, as well as all constraints,
	and possibly normalize the environment and return the
	applied permutation. *)
val rename_vars_with :
  ('a,'b,'c,'d) O.t -> ('a * 'a) list ->
  int array option * Apron.Dim.perm option
    (** Rename the variables, and remove all constraints,possibly
	normalize the environment and return the applied
	permutation. *)

val add_typ : ('a,'b,'c,'d) O.t -> 'a -> 'c -> ('a,'b,'c,'d) O.t
val add_vars : ('a,'b,'c,'d) O.t -> ('a * 'b) list -> ('a,'b,'c,'d) O.t
val remove_vars : ('a,'b,'c,'d) O.t -> 'a list -> ('a,'b,'c,'d) O.t
val rename_vars : ('a,'b,'c,'d) O.t -> ('a * 'a) list -> ('a,'b,'c,'d) O.t
  (** Functional versions of the previous functions *)

(* ********************************************************************** *)
(** {3 Operations} *)
(* ********************************************************************** *)

val is_leq : ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t -> bool
    (** Test inclusion of environments in terms of types and
	variables (but not in term of indexes) *)
val is_eq : ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t -> bool
    (** Test equality of environments in terms of types and
	variables (but not in term of indexes) *)
val lce : ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t
    (** Least common environment *)

(*  ********************************************************************** *)
(** {3 Precomputing change of environments} *)
(*  ********************************************************************** *)

type change = {
  cbdd : Cudd.Man.v Bdd.Env.change;
  capron : Apron.Dim.change2;
}

val compute_change : ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t -> change

(*  ********************************************************************** *)
(** {3 Utilities} *)
(*  ********************************************************************** *)

(** Type of pairs [(environment, value)] *)
type ('a, 'b) value = ('a, 'b) Bdd.Env.value = { env : 'a; val0 : 'b; }

val make_value :
  ('a,'b,'c,'d) O.t ->
  'e -> (('a,'b,'c,'d) O.t, 'e) value
  (** Constructor *)
val get_env : ('a,'b) value -> 'a
val get_val0 : ('a,'b) value -> 'b


val check_var : ('a,'b,'c,'d) O.t -> 'a -> unit
val check_lvar : ('a,'b,'c,'d) O.t -> 'a list -> unit
val check_value :
  ('a,'b,'c,'d) O.t ->
  (('a,'b,'c,'d) O.t, 'e) value -> unit
val check_value2 :
  (('a,'b,'c,'d) O.t, 'e) value ->
  (('a,'b,'c,'d) O.t, 'f) value -> unit
val check_value3 :
  (('a,'b,'c,'d) O.t, 'e) value ->
  (('a,'b,'c,'d) O.t, 'f) value ->
  (('a,'b,'c,'d) O.t, 'g) value -> unit

val check_lvarvalue :
  ('a,'b,'c,'d) O.t ->
  ('a * (('a,'b,'c,'d) O.t, 'e) value) list -> ('a * 'e) list
val check_lvalue :
  ('a,'b,'c,'d) O.t ->
  (('a,'b,'c,'d) O.t, 'e) value list -> 'e list
val check_ovalue :
  ('a,'b,'c,'d) O.t ->
  (('a,'b,'c,'d) O.t, 'e) value option -> 'e option

val mapunop :
  ('e -> 'f) ->
  (('a,'b,'c,'d) O.t, 'e) value ->
  (('a,'b,'c,'d) O.t, 'f) value

val mapbinop :
  ('e -> 'f -> 'g) ->
  (('a,'b,'c,'d) O.t, 'e) value ->
  (('a,'b,'c,'d) O.t, 'f) value ->
  (('a,'b,'c,'d) O.t, 'g) value
val mapbinope :
  (('a,'b,'c,'d) O.t -> 'e -> 'f -> 'g) ->
  (('a,'b,'c,'d) O.t, 'e) value ->
  (('a,'b,'c,'d) O.t, 'f) value -> (('a,'b,'c,'d) O.t, 'g) value
val mapterop :
  ('e -> 'f -> 'g -> 'h) ->
  (('a,'b,'c,'d) O.t, 'e) value ->
  (('a,'b,'c,'d) O.t, 'f) value ->
  (('a,'b,'c,'d) O.t, 'g) value ->
  (('a,'b,'c,'d) O.t, 'h) value

val var_of_aprondim :
  ('a,'b,'c,'d) O.t -> Apron.Dim.t -> 'a
val aprondim_of_var :
  ('a,'b,'c,'d) O.t -> 'a -> Apron.Dim.t

val string_of_aprondim :
  ('a,'b,'c,'d) O.t -> Apron.Dim.t -> string
