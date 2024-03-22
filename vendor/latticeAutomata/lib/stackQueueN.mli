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

(** Non-Relational N-Stack and/or queue abstraction based on lattice automata *)

(*  ********************************************************************** *)
(** {2 Module type for the generated module} *)
(*  ********************************************************************** *)

module type S = sig
  module PL : PLattice.S
  module LAuto : (LAutomaton.S with module A=PL.A
			       and module PL=PL)

  type 'a t = ('a LAuto.t) array

  val print : 
    (Format.formatter -> 'a PL.t -> unit) ->
    Format.formatter -> 'a t -> unit
  val print_dot : 
    ?margin:int ->
    ?titlestyle:string ->
    ?vertexstyle:string ->
    ?edgestyle:string ->
    ?title:string ->
    (Format.formatter -> 'a PL.t -> unit) ->
    Format.formatter -> 'a t -> unit

  val print_regexp : 
    (Format.formatter -> 'a PL.t -> unit) ->
    Format.formatter -> 'a t -> unit

  val get_lattice : 'a t -> 'a PL.lattice
  val get_partition : 'a t -> 'a PL.t
  (* give the lattice and the partition of the lattice automaton
     This function assumes the dimension of the lattice automaton is 1
     at least, and that all dimensions use the same partition and the
     same lattice.  
  *)
  val get_size : 'a t -> int


  val minimise : 'a t -> unit
  val canonicalize : 'a t -> unit
  (* Normalization *)

  (* complementary *)
  val complementary : 'a t -> 'a t  
  (* is_prefix *)
  val is_prefix :
    projection:('a -> PL.A.t * 'b -> 'b) -> 'a list -> 'b t -> int -> bool 
  (** [is_prefix projection w auto i] checks whether the word [w] is
      a prefix of a word in the queue [i] of [auto]. The elementary
      projection must be given. WARNING: it works in fact on
      [det(auto)], not on [auto]. *)


  (*  ==================================================================== *)
  (** {3 Constructors} *)
  (*  ==================================================================== *)

  val bottom   : lattice:'a PL.lattice -> partition:'a PL.t -> int -> 'a t
  val top      : lattice:'a PL.lattice -> partition:'a PL.t -> int -> 'a t
  val allempty : lattice:'a PL.lattice -> partition:'a PL.t -> int -> 'a t

  val of_regexp : lattice:'a PL.lattice -> partition:'a PL.t -> int -> 'a PL.t Regexp.t -> 'a t

  (*  ==================================================================== *)
  (** {3 Tests} *)
  (*  ==================================================================== *)

  val is_bottom : 'a t -> bool
  val is_top    : 'a t -> bool
  val is_allempty  : 'a t -> bool
  val is_leq : ?det:bool -> 'a t -> 'a t -> bool
  val is_eq  : ?det:bool -> 'a t -> 'a t -> bool
  val is_empty  : 'a t -> int -> bool

  (*  ==================================================================== *)
  (** {3 Lattice operations} *)
  (*  ==================================================================== *)

  val join       : 'a t -> 'a t -> 'a t
  val meet       : 'a t -> 'a t -> 'a t

  val widening_shape : ('a LAuto.t -> 'a LAuto.t) -> 'a t -> 'a t -> 'a t

  (*  ==================================================================== *)
  (** {3 Queue and stack operations} *)
  (*  ==================================================================== *)

  module type Operations = sig
    val push : 'a t -> int -> 'a PL.t -> 'a t
    val top : ?cond:'a PL.t -> 'a t -> int -> 'a PL.t
    val pop : ?cond:'a PL.t -> 'a t -> int -> 'a t * 'a PL.t
    val pop_partitioned :
      ?cond:'a PL.t -> 'a t -> int -> ('a t * 'a) PL.A.Map.t
    val drop : 'a t -> int -> 'a t
    val queue : 'a t -> int -> 'a PL.t -> 'a t
    val firstout : ?cond:'a PL.t -> 'a t -> int -> 'a PL.t
    val dequeue : ?cond:'a PL.t -> 'a t -> int -> 'a t * 'a PL.t
    val dequeue_partitioned :
      ?cond:'a PL.t -> 'a t -> int -> ('a t * 'a) PL.A.Map.t
  end
  module Left :Operations
  module Right : Operations

  (*  ==================================================================== *)
  (** {3 Map and iteration on the trantisitions} *)
  (*  ==================================================================== *)

  val map_trans : ('a PL.t -> 'a PL.t) -> 'a t -> 'a t
    (** [map_trans f auto] applies f to all transitions of auto and
	returns a new automaton *)

  val iter_trans : ('a PL.t -> unit) -> 'a t -> unit
    (** [iter_trans f auto] applies f to all transitions of auto  *)

end

(*  ********************************************************************** *)
(** {2 Functor} *)
(*  ********************************************************************** *)

module Make
  (LAuto:LAutomaton.S)
  :
  (S with module PL=LAuto.PL
     and module LAuto=LAuto)
