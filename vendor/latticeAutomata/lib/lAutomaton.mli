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

(** Lattice automata and their operations: main file *)


(*  ********************************************************************** *)
(** {2 Modules} *)
(*  ********************************************************************** *)

(** Same modules as in [LautomatonRep] *)

module SetII : (Sette.S with type elt=int*int)
module MapII : (Mappe.S with type key=int*int and module Setkey=SetII)
module HashII : (Hashhe.S with type key=int*int and type 'a t='a LAutomatonRep.HashII.t)
module Graph : (FGraph.S with type vertex = int
			 and module SetV=MappeI.Setkey
			 and module SetE=MapII.Setkey
			 and module MapV=MappeI
			 and module MapE=MapII
			 and type ('a,'b,'c) t = ('a,'b,'c) LAutomatonRep.Graph.t)

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

(** Same types as in [LautomatonRep] *)

type ('a,'b) info = ('a,'b) LAutomatonRep.info = {
  lattice : 'a;
  partition : 'b;
  initial : SetteI.t array;
  final : SetteI.t array;
  mutable det : bool;
  mutable min : bool;
  mutable counter : int;
}
val print_info :
  (Format.formatter -> 'b -> unit) -> Format.formatter -> ('a,'b) info -> unit

type pinfo = LAutomatonRep.pinfo = {
  pinitial : SetteI.t array;
  pfinal : SetteI.t array;
  hashPI : (SetteI.t,int) Hashhe.t;
  mutable pcounter : int;
}
val make_pinfo : int -> pinfo
type partition = LAutomatonRep.partition = {
  mutable map : int MappeI.t;
  mutable classes : SetteI.t MappeI.t;
}
val partition_of_map : int MappeI.t -> partition
val partition_of_classes : SetteI.t MappeI.t -> partition
val partition_print : Format.formatter -> partition -> unit

(*  ********************************************************************** *)
(** {2 Module type for the generated module} *)
(*  ********************************************************************** *)

module type S = sig
  include LAutomatonRep.S

  (*  ==================================================================== *)
  (** {3 Normalisation} *)
  (*  ==================================================================== *)
    
  val minimise: 'a t -> 'a t
  val canonicalise: 'a t -> 'a t
      (** [minimise auto] returns the least minimal deterministic automaton (in
	terms of language inclusion) recognizing all words accepted by
	[auto]. *)
    
  (*  ==================================================================== *)
  (** {3 Logical operations} *)
  (*  ==================================================================== *)

  val union: 'a t -> 'a t -> 'a t
      (** [union auto1 auto2] returns an automaton corresponding to the union
	of the two automata [auto1] and [auto2]. 
	The result is neither deterministic nor minimal in general 
      *)

  val complementary : 'a t -> 'a t
    (** [complementary auto] computes the complementary of auto
	WARNING: this function only works in the case of FINITE AUTOMATA
    *)


  module Inter : sig
    val inter: 'a t -> 'a t -> 'a t * int HashII.t
    end
  val inter: 'a t -> 'a t -> 'a t
    (** [inter auto1 auto2] returns an automaton corresponding to the
	intersection of the two automata [auto1] and [auto2]. 
	The result is deterministic but not minimal in general
    *)

  val union_list : 'a t list -> 'a t
  val union_array : 'a t array -> 'a t
  val inter_list : 'a t list -> 'a t
  val inter_array : 'a t array -> 'a t
    (** n-ary versions of previous operations *)

  (*  ==================================================================== *)
  (** {3 Language operations} *)
  (*  ==================================================================== *)

  (** See the module Language for the description of the
      operations. Those operations are defined for multi-dimensional
      automata; the integer parameter specifies the dimension where
      the operation is done. 

      We said that A is an x-dimensional automaton if it recognizes
      words that are a concatenation of x subwords, separated by a special
      letter. Dimensions are indexed from 0 to x-1. 
      
  *)

  val cons_right : 'a t -> int -> 'a PL.t -> 'a t
  val deriv_right : ?cond:'a PL.t -> 'a t -> int -> 'a t
  val look_right : ?cond:'a PL.t -> 'a t -> int -> 'a PL.t
  val cons_left : 'a t -> int -> 'a PL.t -> 'a t
  val deriv_left : ?cond:'a PL.t -> 'a t -> int -> 'a t
  val look_left : ?cond:'a PL.t -> 'a t -> int -> 'a PL.t
  val of_regexp : lattice:'a PL.lattice -> partition:'a PL.t -> int -> 'a PL.t Regexp.t -> 'a t
  val to_regexp: 'a t -> 'a PL.t Regexp.t

  (** module Language encodes the operations for one-dimentional automata *)
  module Language : sig
    val look_right : ?cond:'a PL.t -> 'a t -> 'a PL.t
    (** [L -> X] such that [w.x in L <==> x in X] *)
    val cons_right : 'a t -> 'a PL.t -> 'a t
    (** [L -> X -> L.X].
      Result is not deterministic in general. *)
    val deriv_right : ?cond:'a PL.t -> 'a t -> 'a t
    (** [L -> X -> L'] such that 
        - [w in L' <==> w.x in L /\ x in X] if [cond=Some X]
        - [w in L' <==> w.x in L] if [cond=None]
      Result is deterministic if argument is. *)
    val look_left : ?cond:'a PL.t -> 'a t -> 'a PL.t
    (** [L -> X] such that [x.w in L <==> x in X] *)
    val cons_left : 'a t -> 'a PL.t -> 'a t
    (** [X -> L -> X.L].
      Result is deterministic if argument is. *)
    val deriv_left : ?cond:'a PL.t -> 'a t -> 'a t
    (** [X -> L -> L'] such that 
      - [w in L' <==> x.w in L /\ x in X] if [cond=Some X]
      - [w in L' <==> x.w in L] if [cond=None]
      Result is not deterministic in general. *)
    val empty : lattice:'a PL.lattice -> partition:'a PL.t -> 'a t
      (** Empty word *)
    val letter : lattice:'a PL.lattice -> partition:'a PL.t -> 'a PL.t -> 'a t
      (** Create a partitioned automaton recognizing a single letter.
        The letter is supposed included in the partition.
	Result is deterministic. *)
    val word : lattice:'a PL.lattice -> partition:'a PL.t -> 'a PL.t list -> 'a t
      (** Create a partitioned automaton recognizing a single word.
        The letters are supposed included in the partition.
	Result is deterministic. *)

    val transpose : 'a t -> 'a t
      (** Language transposition (reversal) *)
    module Concat : sig
      val merge_trans : 'a t -> Graph.vertex * Graph.vertex -> 'a PL.t -> 'a t
      val merge_final_initial : 'a t -> SetteI.t -> SetteI.t -> 'a t
    end
    val concat : 'a t -> 'a t -> 'a t
      (** concatenation. the dimension of [concat auto1 auto2] is
	  auto1.dim + auto2.dim -1
      *)
    val concat_list : 'a t list -> 'a t
    val concat_array : 'a t array -> 'a t
      (** Language concatenation, respecting the left to right order of
	  concatenation. Result is not deterministic in general. *)

    val star : 'a t -> 'a t
    val plus : 'a t -> 'a t
      (** Standard and strict Kleene closure.  Result is not
	  deterministic in general.  The automaton must be
	  one-dimentional.  *)
      
    val of_regexp : lattice:'a PL.lattice -> partition:'a PL.t -> 'a PL.t Regexp.t -> 'a t
      (** Constructor from a regular expressions *)

  end

  (*  ==================================================================== *)
  (** {3 is_prefix} *)
  (*  ==================================================================== *)
  val is_prefix :
    projection:('a -> PL.A.t * 'b -> 'b) -> 'a list -> 'b t -> int -> bool 
    (** [is_prefix projection w auto i] checks whether the word [w] is
	a prefix of a word in the queue [i] of [auto]. The elementary
	projection must be given. WARNING: it works in fact on
	[det(auto)], not on [auto]. *)
    
  (*  ==================================================================== *)
  (** {3 Widening} *)
  (*  ==================================================================== *)

  val widening_shape : ('a t -> 'a t) -> 'a t -> 'a t -> 'a t
    (** Widening parameterized by a partition.  Assumes inclusion of the first
      automaton in the second one.

      In [widening abstract auto1 auto2]:

      - If shape1 and shape2 have twoway bisimilar shapes, 
        then applies L.widening pairwise on labels;

      - Otherwise, return [abstract auto2]

      It is assumed that the set of image automata of the [abstract] function
      has a finite set of shapes (this guarantess termination).  *)

  val widening_shape2 : ('a t -> 'a t) -> 'a t -> 'a t -> 'a t
    (**  Same function, different implementation. This one minimise the automaton before applying the widening *)


  module Abstract : sig
     val partition_std : ?initial:bool -> ?final:bool -> 'a t -> partition
      (** Standard partition, separating in each dimension initial,
	  final, initial and final states from others. If [initial=false]
	  (resp. [final=false]), then initial states (resp. final states)
	  are not distinguished from others.  *)

    val partition_refine : ?forward:bool -> ?backward:bool -> 'a t -> 
      partition -> partition
	(** Refines a partition by separating states according to the ingoing
	  ([backward]) and/or outgoing ([forward]) letters. *) 

    val standard : ?initial:bool ->  ?final:bool -> forward:int -> backward:int -> 'a t -> 'a t
    (** Standard abstraction, based on an initial partition separating
	in each dimension initial, final, initial and final states from
	others (if [initial=false] (resp. [final=false]), then initial
	states (resp. final states) are not distinguished from others),
	further refined by a bounded bisimulation of depth [forward] in
	forward direction, and depth [backward] in backward direction.  *)

    val  construct_partition_bounded : ?initial:bool -> ?final:bool -> forward:int -> backward:int -> 'a t -> partition

    (** Gives a partition of the states of the automaton, based on
	bounded languages (forward) and bounded reverse languages
	(backward). If [initial=true] the partition takes into account
	compute the backward language, and if [final=true] the partition
	takes into acount the backward language.
    *)

    val bounded_languages : ?initial:bool -> ?final:bool -> forward:int -> backward:int -> 'a t -> 'a t
  (** Quotient of the automaton by the previous partition. *)
      


  end

end

(*  ********************************************************************** *)
(** {2 Functor} *)
(*  ********************************************************************** *)

module Make (PL:PLattice.S) : 
  (S with module A = PL.A
     and module PL = PL)
