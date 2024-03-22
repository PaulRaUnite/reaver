(***********************************************************************)
(*                                                                     *)
(*                    The Lattice Automata Library                     *)
(*                                                                     *)
(*                Bertrand Jeannet and Tristan Le Gall                 *)
(*                                                                     *)
(*  Copyright 2008 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(** Partitioned lattice with an alphabet *)

(** This modules defines types and functions for {e partitioned lattice}.

  A partitioned element of such a lattice is a map from an alphabet
  (denoting equivalence classes) to an element of the lattice, which is
  supposed to be included in a partition of the lattice.

  A partition is a partitioned element such that the merge of all the images of
  the corresponding map is the top element of the lattice.

*)

(*  ********************************************************************** *)
(** {2 Type} *)
(*  ********************************************************************** *)

(** Manager for operations on basic lattice *)

type ('letter,'abstract) glattice = {
  mutable sep : 'abstract;
      (** Any value different from bottom for which equality comparison is
	cheap *)
  mutable is_bottom : 'letter -> 'abstract -> bool;
  mutable is_leq: 'letter -> 'abstract -> 'abstract -> bool;
  mutable is_eq: 'letter -> 'abstract -> 'abstract -> bool;
  mutable join: 'letter -> 'abstract -> 'abstract -> 'abstract;
  mutable meet: 'letter -> 'abstract -> 'abstract -> 'abstract;
  mutable widening: 'letter -> 'abstract -> 'abstract -> 'abstract;
}

(*  ********************************************************************** *)
(** {2 Parameter module type} *)
(*  ********************************************************************** *)

(** We use the term alphabet to denote the identifiers of equivalence
  classes. The alphabet is provided with set and map modules.  *)

module type Alphabet = sig
  type t
    (** Type of letter *)

  val sep : t
    (** Special separation letter, not to be used by user *)
    
  module Set : (Sette.S with type elt=t)
  module Map : (Mappe.S with type key=t and module Setkey=Set)
    (** Sets and Maps over letters *)
end

(** Functor for making an alphabet module from a totally ordered type *)

module MakeAlphabet(Ord:sig include Set.OrderedType val sep : t end) :
  (Alphabet with type t = Ord.t
	    and type Map.Setkey.Ord.t = Ord.t)


(*  ********************************************************************** *)
(** {2 Type for the generated module} *)
(*  ********************************************************************** *)

module type S = sig

  (*  ==================================================================== *)
  (** {3 Modules and types} *)
  (*  ==================================================================== *)

  module A:Alphabet
    (** The alphabet with its total ordering *)

  module SetAA : (Sette.S with type elt=A.t*A.t)
  module MapAA : (Mappe.S with type key=A.t*A.t and module Setkey=SetAA)
    (** Sets and Maps over pairs of letters *)

  type 'abstract lattice = (A.t,'abstract) glattice
    (** Type of manager for operations on basic lattice *)

  type 'abstract t = 'abstract A.Map.t
    (** Type of partitioned elements and type of partitions *)


  val sep : 'a lattice -> 'a t
  val is_sep : 'a t -> bool

  (*  ==================================================================== *)
  (** {3 Functions} *)
  (*  ==================================================================== *)

  val print :
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    ?firstbind:(unit, Format.formatter, unit) format ->
    ?sepbind:(unit, Format.formatter, unit) format ->
    ?lastbind:(unit, Format.formatter, unit) format ->
    (Format.formatter -> A.t -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a t -> unit
      (** Printing function.
	The optional arguments have the same meaning as the optional arguments
	of the [Mappe.print] function *)

  val bottom : 'a t
    (** Bottom value *)

  val singleton : A.t -> 'a -> 'a t
    (** Partitioned value created with one class *)

  val normalize : 'a lattice -> 'a t -> 'a t
    (** Normalize a partitionned element by removing the bindings [(a,elt)]
      such that [L.is_bottom elt = true]. *)

  val is_bottom : 'a lattice -> 'a t -> bool
  val is_leq : 'a lattice -> 'a t -> 'a t -> bool
  val is_eq : 'a lattice -> 'a t -> 'a t -> bool
    (** Emtiness, Inclusion and Equality tests *)

  val meet : 'a lattice -> 'a t -> 'a t -> 'a t
  val join : 'a lattice -> 'a t -> 'a t -> 'a t
    (** Meet and join of two partitioned element (on the same partition) *)

  val widening : 'a lattice -> partition:'a t -> 'a t -> 'a t -> 'a t
    (** Extends L.widening on partitioned elements,
      assuming that these elements are defined on the partition
      [part]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (A.t -> 'a -> 'b) -> 'a t -> 'b t
  val iter : (A.t -> 'a -> unit) -> 'a t -> unit
  val fold : (A.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val project :
    project:('a -> A.t * 'b -> 'b) -> 
    'a -> 'b lattice -> 'b t -> 
    'b t
    (** [project ~project elt lattice partition] projects the non-partitioned
      element [elt] on the partition [partition] (with corresponding lattice
      [lattice]), using the elementary projection function [~project]. 
      
      [~project elt (letter,abs)] projects [elt] on the equivalence class
      [(letter,abs)] of the partition [partition].
    *)
  val merge :
    empty:'b -> 
    merge:(A.t * 'a -> 'b -> 'b) ->
    'a t -> 'b
    (** Merge the elements of the partition with [~merge] *)

  val lcm :
    ?coherent:bool ->
    meet:(A.t * 'a -> A.t * 'a -> 'a option) ->
    'a t -> 'a t -> 'a MapAA.t
    (** [lcm ~meet part1 part2] combines the two partition by returning a map
      [(letter1,letter2) -> meet (letter1,abs1) (letter2,abs2)].  Whenever
      [~meet] returns [None], it means [bottom]. *)

  val transfer :
    ?coherent:bool ->
    meet:(A.t * 'a -> A.t * 'a -> 'a option) ->
    'a lattice -> 'a t -> 'a t -> 'a t
    (** [transfer meet man pelt newpartition] allows to transfer 
      a partitioned element [pelt] to
      a new partition [newpartition]. 
      [coherent] and [meet] have the same meaning as before. *)



end

(*  ********************************************************************** *)
(** {2 Functor for building partitioned lattices} *)
(*  ********************************************************************** *)

module Make(A:Alphabet) : (S with module A=A)
