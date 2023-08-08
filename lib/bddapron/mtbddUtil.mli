(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** utilities for manipulating products of Mtbdds *)

(** {2 Generic types} *)

type 'b base_t = 'b (** leaf element of the base MTBDDs *)
type 'b array_t = 'b base_t array (** array of leaf elements *)
type 'b arrset_t = 'b array_t PSette.t (** set of arrays of leaf elements *)

type 'b base_mtbdd_t = 'b base_t Cudd.Mtbdd.t (** base MTBDD *)
type 'b array_mtbdd_t = 'b array_t Cudd.Mtbdd.t (** array MTBDD *)
type 'b arrset_mtbdd_t = 'b arrset_t Cudd.Mtbdd.t (** array set MTBDD *)

type 'b array_table_t = 'b array_t Cudd.Mtbdd.table 
                                          (** hash table for array MTBDDs *)
type 'b arrset_table_t = 'b arrset_t Cudd.Mtbdd.table 
                                        (** hash table for array set MTBDDs *)

type 'a compare_t = 'a -> 'a -> int


(** {2 Generic operations} *)

(** converts an MTBDD into an array MTBDD  *)
val basemtbdd_to_arraymtbdd : 'b array_table_t -> 'b base_mtbdd_t -> 
  'b array_mtbdd_t

(** computes the product of two MTBDDs 
    by concatenating the leaves in an array *)
val arraymtbdd_product : 'b array_table_t -> 'b array_mtbdd_t -> 
  'b array_mtbdd_t -> 'b array_mtbdd_t

(** converts an array MTBDD to an arrset MTBDD *)
val arraymtbdd_to_arrsetmtbdd : compare_array:'b array_t compare_t -> 
  'b arrset_table_t -> 'b array_mtbdd_t -> 'b arrset_mtbdd_t

(** computes the product of two arrset MTBDDs,
    assuming that the leaf sets contain both exactly one element *)
val arrsetmtbdd_product : 'b arrset_table_t -> 'b arrset_mtbdd_t -> 
  'b arrset_mtbdd_t -> 'b arrset_mtbdd_t 

(** quantifies supp in the arrayset MTBDD *)
val arrsetmtbdd_exists : 'b arrset_table_t -> 'b arrset_mtbdd_t -> 
  Cudd.Bdd.vt -> 'b arrset_mtbdd_t

(** returns the list of guards of the given MTBDD *)
val get_guards_mtbdd : 'a Cudd.Mtbdd.t -> Cudd.Bdd.vt list

(** returns the arrayset MTBDD with an empty leaf *)
val arrsetmtbdd_empty : compare_array:'b array_t compare_t -> 'b arrset_table_t -> Cudd.Man.vt -> 'b arrset_mtbdd_t

(** returns the list of conjunctions (paths) of the given mtbdd *)
val mtbdd_to_dnf : Cudd.Man.vt -> 'a Cudd.Mtbdd.t ->
  (Cudd.Bdd.vt * 'a) list


(** {2 Generic printing} *)
val print_base_mtbdd : 'a Bddapron.Env.t -> 'a Bddapron.Cond.t -> 
  (Format.formatter -> 'b base_t -> unit) -> Format.formatter -> 
  'b base_mtbdd_t -> unit

val print_array : (Format.formatter -> 'b base_t -> unit) -> Format.formatter ->
  'b array_t -> unit

val print_array_mtbdd : 'a Bddapron.Env.t -> 'a Bddapron.Cond.t -> 
  (Format.formatter -> 'b base_t -> unit) -> Format.formatter -> 
  'b array_mtbdd_t -> unit

val print_arrset : (Format.formatter -> 'b base_t -> unit) -> Format.formatter ->
  'b arrset_t -> unit

val print_arrset_mtbdd : 'a Bddapron.Env.t -> 'a Bddapron.Cond.t -> 
  (Format.formatter -> 'b base_t -> unit) -> Format.formatter -> 
  'b arrset_mtbdd_t -> unit


(** {2 Instantiation: Numerical actions} *)

val make_table_action_array : 'a Bddapron.Env.t -> 
  'a Bddapron.Apronexpr.t array_table_t
val make_table_action_arrset : 'a Bddapron.Env.t ->
  'a Bddapron.Apronexpr.t arrset_table_t

val print_action : 'a Bddapron.Env.t -> Format.formatter -> 
  'a Bddapron.Apronexpr.t -> unit

val equal_action_array : 'a Bddapron.Env.t -> 'a Bddapron.Apronexpr.t array_t
  -> 'a Bddapron.Apronexpr.t array_t -> bool


(** {2 Instantiation: Polymorphic actions} *)

(** polymorphic leaf of numerical actions/labels/truth values  *)
type 'a poly_t = 
    | Apron of 'a Bddapron.Apronexpr.t
    | Bool of 'a Bddapron.Expr0.Bool.t
    | Benum of 'a
    | Bint of int * bool * int (* value * signed * length *)

val make_table_poly_array : 'a Bddapron.Env.t ->
  'a poly_t array_table_t
val make_table_poly_arrset : 'a Bddapron.Env.t ->
  'a poly_t arrset_table_t

val print_poly : 'a Bddapron.Env.t -> 'a Bddapron.Cond.t -> 
  Format.formatter -> 'a poly_t -> unit 

(** converts a (BDD,leaf) list to an MTBDD *)
val bddleafarr_to_mtbdd : ('a -> 'b Cudd.Mtbdd.unique) ->
  'c Bddapron.Env.t -> ('c Bddapron.Expr0.Bool.t * 'a) array -> 'b base_mtbdd_t

(** converts a Bddapron Expr0 into an array MTBDD *)
val bddapronexpr_to_arraymtbdd : 'a Bddapron.Env.t 
  -> 'a poly_t array_table_t -> 'a Bddapron.Expr0.t -> 
  'a poly_t array_mtbdd_t 

(** converts a Bddapron Expr0 into an arrset MTBDD *)
val bddapronexpr_to_arrsetmtbdd : 'a Bddapron.Env.t ->  
  'a poly_t arrset_table_t -> 'a Bddapron.Expr0.t ->
  'a poly_t arrset_mtbdd_t 
 
