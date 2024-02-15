(** Output of BDDs/MTBDDs *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 Types} *)
(*  ********************************************************************** *)

(** BDD node *)
type bnode =
  | BIte of int * int * bool * int
      (** [BIte(idcond,idnodeThen,signElse,idnodeElse)] *)
  | BTrue
      (** Terminal case. Not needed in principle *)

(** Database *)
type 'a bdd = {
  cond : int PSette.t ref;
    (** Reachable conditions *)
  mutable bdef : (int, bnode) PMappe.t;
    (** Global BDDs graph *)
  bhash : ('a Cudd.Bdd.t, int) Hashhe.t;
  mutable blastid : int;
    (** Hashtables and Counters for resp. first free BDD or IDD node *)
}

(** MTBDD node *)
type 'a vnode =
  | VIte of int * int * int
      (** VIte(idcond,idnodeThen,idnodeElse) *)
  | VCst of 'a
      (** Leaf *)

(** Database *)
type 'a vdd = {
  cond : int PSette.t ref;
    (** Reachable conditions *)
  mutable vdef : (int, 'a vnode) PMappe.t;
    (** Global MTBDDs graph *)
  lhash : ('a, unit) PHashhe.t;
  vhash : ('a Cudd.Vdd.t, int) Hashhe.t;
  mutable vlastid : int;
    (** Hashtables and Counters for MTBDD nodes. *)
}

(** ADD node *)
type anode =
  | AIte of int * int * int
      (** AIte(idcond,idnodeThen,idnodeElse) *)
  | ACst of float

(** Database *)
type add = {
  cond : int PSette.t ref;
  mutable adef : (int, anode) PMappe.t;
  mutable lset : float Sette.t;
  ahash : (Cudd.Add.t, int) Hashhe.t;
  mutable alastid : int;
}

(*  ********************************************************************** *)
(** {3 Functions} *)
(*  ********************************************************************** *)

val make_bdd : cond:int PSette.t ref -> 'a bdd
      (** Create a database for printing BDDs

	  [cond] allows to share the same set of conditions between
	  BDDs and MTBDDs. *)

val signid_of_bdd : 'a bdd -> 'a Cudd.Bdd.t -> bool * int
      (** Output the BDD and return its identifier *)

val make_vdd :
  compare:'a Cudd.PWeakke.compare ->
  cond:int PSette.t ref ->
  'a vdd
val make_mtbdd :
  table:'a Cudd.Mtbdd.table ->
  cond:int PSette.t ref ->
  'a Cudd.Mtbdd.unique vdd
val make_mtbddc :
  table:'a Cudd.Mtbddc.table ->
  cond:int PSette.t ref ->
  'a Cudd.Mtbddc.unique vdd
      (** Create a database for printing MTBDDs

	  [cond] allows to share the same set of conditions between
	  BDDs and MTBDDs. *)

val id_of_vdd : 'a vdd -> 'a Cudd.Vdd.t -> int
      (** Output the MTBDD and return its identifier *)

val iter_cond_ordered : int PSette.t -> 'a Cudd.Man.t -> (int -> unit) -> unit
      (** Iterate the function on all the registered conditions, from level 0
      to higher levels. *)
val iter_bdef_ordered : 'a bdd -> (int -> bnode -> unit) -> unit
      (** Iterate on definitions of BDD identifiers, in a topological order. *)
val iter_vdef_ordered : 'a vdd -> (int -> 'a vnode -> unit) -> unit
      (** Iterate on definitions of MTBDD identifiers, in a topological order. *)
