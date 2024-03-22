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

(** Internal representation of lattice automata and basic operations *)

(*  ********************************************************************** *)
(** {2 Modules} *)
(*  ********************************************************************** *)

module SetII : (Sette.S with type elt=int*int)
module MapII : (Mappe.S with type key=int*int and module Setkey=SetII)
module HashII : (Hashhe.S with type key=int*int)
  (** Sets, Maps, and Hashtables over pairs of integers *)

module Graph : (FGraph.S with type vertex = int
			 and module SetV=MappeI.Setkey
			 and module SetE=MapII.Setkey
			 and module MapV=MappeI
			 and module MapE=MapII)
  (** Graphs with integer vertices identifiers, representing automata. *)

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

(*  ==================================================================== *)
(** {3 Auxiliary information associated to the graph of an automaton} *)
(*  ==================================================================== *)

type ('a,'b) info = {
  lattice : 'a;
      (** Manager of type ['a PLattice.S.manager] *)
  partition : 'b;
      (** Partition of type ['a PLattice.S.t] *)
  initial : SetteI.t array;
      (** (array of) Initial states *)
  final : SetteI.t array;
      (** (array of) Final states, of same size than [initial] *)
  mutable det : bool;
      (** Is the automaton deterministic ? *)
  mutable min : bool;
      (** Is the automaton minimal ? *)
  mutable counter : int;
      (** First free state identifier *)
}
val print_info :
  (Format.formatter -> 'b -> unit) -> Format.formatter -> ('a,'b) info -> unit
  (** Printing function *)

(*  ==================================================================== *)
(** {3 Auxiliary information associated to the graph of a powerset automaton}*)
(*  ==================================================================== *)

type pinfo = {
  pinitial : SetteI.t array;
    (* set of initial states in pauto *)
  pfinal : SetteI.t array;
    (* set of final states in pauto *)
  hashPI : (SetteI.t,int) Hashhe.t;
    (* association pvertex -> id in pauto *)
  mutable pcounter : int;
    (* next free vertex identifier in pauto *)
}
val make_pinfo : int -> pinfo
  (* Return an empty pinfo *)

(*  ==================================================================== *)
(** {3 Partition of a set of states} *)
(*  ==================================================================== *)

type partition = {
  mutable map : int MappeI.t;
      (** Map [state -> id of its equivalence class] *)
  mutable classes : SetteI.t MappeI.t;
      (** Map [id of an equivalence class -> set of corresponding states] *)
}
val partition_of_map : int MappeI.t -> partition
val partition_of_classes : SetteI.t MappeI.t -> partition
  (** Constructors for partitions *)

val partition_print : Format.formatter -> partition -> unit
  (** Printing function (for debugging...) *)

(*  ==================================================================== *)
(** {3 Simulation relations} *)
(*  ==================================================================== *)

type simulation = SetteI.t MappeI.t
  (** Binary relation:
     [v1 R v2 <=> v2 in map v1] *)

val simulate : simulation -> int -> int -> bool
val simulation_inv : simulation -> simulation
val simulation_print : Format.formatter -> simulation -> unit

(*  ********************************************************************** *)
(** {2 Module type for the generated module} *)
(*  ********************************************************************** *)

module type S = sig

  (*  ==================================================================== *)
  (** {3 Modules} *)
  (*  ==================================================================== *)

  module A : PLattice.Alphabet
    (** The alphabet with its total ordering *)
  module PL : (PLattice.S with module A=A)
    (** Partitioned lattice *)

  module SetAV : (Sette.S with type elt=A.t*int)
  module MapAV : (Mappe.S with type key=A.t*int and module Setkey=SetAV)
    (** Sets and Maps over pairs of a letter and an integer *)

  (*  ==================================================================== *)
  (** {3 Types} *)
  (*  ==================================================================== *)

  type 'a t = (int, 'a PL.t, ('a PL.lattice, 'a PL.t) info) Graph.t
      (** Type of lattice automata *)

  type shape = unit t
      (** Type of shape automata *)

  (** Invariant of [t] and [shape]: states, between operations,
      are supposed to be reachable and coreachable *)

  val lunit : unit PL.lattice

  (*  ==================================================================== *)
  (** {3 Utility functions} *)
  (*  ==================================================================== *)

  val mapV_A_L_of_mapAV_L : 'a MapAV.t -> 'a A.Map.t MappeI.t
  val mapV_A_L_of_mapA_LxV : ('a * int) A.Map.t -> 'a A.Map.t MappeI.t
  val mapV_A_L_of_mapA_LxV_list : ('a * int) list A.Map.t -> 'a A.Map.t MappeI.t    (** Change of representation of functions *)

  val check_sep :  'a t -> string -> unit
    (** [check_sep auto msg] checks that:
      - x is in initial.(i) iff there is a transition y--sep-> x or i=0
      - x is in final.(i) iff there is a transition x--sep-> y or i=dim(auto)
      and raises [Failure msg] if not. *)

  val auto_check : 'a t -> 'a t -> string -> unit
    (** [auto_check auto1 auto2 msg] checks that
      - the array of initial/final states are of same length
      - the two partitions are the same
      and raises [Failure msg] if not. *)

  (** WARNING : the following functions have side-effects on info *)

  val add_state :
    'a t -> int -> ?initial:bool -> ?final:bool -> int -> 'a t
    (** [add_state auto nbqueue num_state] adds a new state, which may be initial and/of final, with a queue number *)

  val del_state : 'a t -> int -> 'a t

  val restore_sep_consistance : 'a t -> 'a t
    (** rebuild the information about initial and final states
	according to the transitions. Warning: do not use it if
	possible. *)

  val reachability : 'a t -> 'a t
    (** Simplifies an automaton by reachability analysis from initial states *)
  val coreachability : 'a t -> 'a t
    (** Simplifies an automaton by coreachability analysis from final states *)

  val is_trim : 'a t -> bool
    (** [is_trim auto] checks that all states of auto are reachable
	from the initial states and coreachable from the final states 

	Warning: this function is not efficiently implemented
    *)


  (*  ==================================================================== *)
  (** {3 Printing} *)
  (*  ==================================================================== *)

  val print :
    (Format.formatter -> 'a PL.t -> unit) ->
    Format.formatter -> 'a t -> unit
      (** Printing function for automata, using a printing function for
	partitioned lattice elements *)

  val print_dot :
    ?margin:int ->
    ?titlestyle:string ->
    ?vertexstyle:string ->
    ?edgestyle:string ->
    ?title:string ->
    (Format.formatter -> 'a PL.t -> unit) ->
    Format.formatter -> 'a t -> unit
      (** Printing to DOT file *)

  (*  ==================================================================== *)
  (** {3 Constructor } *)
  (*  ==================================================================== *)

  (** The following constructors takes a basic lattice manager (on
      basic type ['a]), a partition of this lattice, and a positive
      number defining the dimensionality of the result. *)

  val bottom : lattice:'a PL.lattice -> partition:'a PL.t -> int -> 'a t
  val top    : lattice:'a PL.lattice -> partition:'a PL.t -> int -> 'a t
  val allempty  : lattice:'a PL.lattice -> partition:'a PL.t -> int -> 'a t
    (** Resp. bottom value, top value, and empty word (in each
	dimension) value *)

  (*  ==================================================================== *)
  (** {3 Tests} *)
  (*  ==================================================================== *)

  val is_bottom : 'a t -> bool
  val is_top    : 'a t -> bool
    (** Emptiness and universality test *)
    (** warning : those are structural tests, not inclusion of
	languages *)

  val is_empty  : 'a t -> int -> bool
    (** Does the given dimension recognize only the empty word ? *)
  val is_allempty  : 'a t -> bool
    (** Same for all dimensions *)

  val is_deterministic : ?check:bool -> 'a t -> bool
    (** Determinism test, using the flag. If [check=true], then really
	test each state for determinism, and update [info.det]
	accordingly. *)

  val is_minimal : 'a t -> bool
    (** Minimality test (using the flag [info.min]). *)

  val shape : 'a t -> unit t
    (** Shape automaton *)

  val dimension : 'a t -> int
    (** Dimensionality of the automaton *)

  val nb_states : 'a t -> int
    (**  Number of states of the automaton *)

  (*  ==================================================================== *)
  (** {3 Module Powerset: powerset automata} *)
  (*  ==================================================================== *)

  module Powerset : sig
    type 'a pauto = (int, 'a PL.t, pinfo) Graph.t
      (** Powerset automaton *)
    val add_pvertex : 'a t -> 'a pauto ref -> ?id:int -> SetteI.t -> int
      (** Adds the pvertex to [pauto] and returns its index.  If
	  id=None, use pinfo.counter as new id; If id=Some(id), use id
	  Return the id.  *)
  end

  (*  ==================================================================== *)
  (** {3 Determinisation} *)
  (*  ==================================================================== *)

  module Det : sig
    val raise_exit_if_vertex_not_deterministic :
      'a t -> int -> succ:SetteI.t -> unit

    val is_vertex_deterministic : 'a t -> int -> bool
    val is_pvertex_deterministic : 'a t -> SetteI.t -> bool

    val succ_pvertex : 'a t -> SetteI.t -> ('a * SetteI.t) A.Map.t
      (** Returns a map [possible letter after a state of the set in
	  auto -> (merge of the associated lattice elements, set of
	  successor states) *)
    val determinise : 'a t -> 'a Powerset.pauto
    val determinise_partial : 'a t -> 'a Powerset.pauto
  end

  val determinise : 'a t -> 'a t
    (** Determinisation guided by the shape, with merging of
	partitioned elements *)

  val determinise_partial : 'a t -> 'a t
    (** Partial determinisation, where only transitions carrying equal values
	are merged. *)

  (*  ==================================================================== *)
  (** {3 Bisimulation} *)
  (*  ==================================================================== *)

  (** Assumes deterministic automata. *)
  module Bisim : sig
    val letters_after_pvertex : 'a t -> SetteI.t -> A.Set.t
      (** Returns the set of outgoing letters of a pvertex *)
    val succ_pvertex_by_letter :
      'a t -> partition -> SetteI.t -> A.t ->
      SetteI.t ref MappeI.t * SetteI.t
	(** [succ_pvertex auto partition pvertex letter] Returns a map
	    [identifier of a successor equivalence class by the letter
	    -> set of vertices in pvertex leading to classid with this
	    letter] and the set of vertices that cannot move by this
	    letter.  *)
  end
  val bisimulation_shape : ?depth:int -> 'a t -> partition -> partition
    (** Compute the weakest bisimulation of depth [depth] (default: unbounded)
	included in the given partition. *)

  (*  ==================================================================== *)
  (** {3 Simulation} *)
  (*  ==================================================================== *)

  (** Assumes deterministic automata *)
  module Simulation : sig
    val succ_vertex_det : 'a t -> int ->  ('a * int) A.Map.t
      (** Associates with a vertex a map [outgoing letter ->
	  associated lattice element and successor vertex] *)
    val simulate_initial :
      ((A.t -> 'a*int -> 'a*int -> bool) -> ('a*int) A.Map.t -> ('a*int) A.Map.t -> bool) ->
      (A.t -> 'a -> 'a -> bool) ->
      'a t -> 'a t -> bool

    val succ_vertex_nondet : 'a t -> int ->  ('a * int) list A.Map.t
      (** Associates with a vertex a map
	  [outgoing letter ->
	  list of associated lattice element and successor vertex]
      *)
    exception Abort
    val simulation_initial : 'a t -> 'a t -> simulation
    val simulation_nondet :
      ?depth:int ->
      'a t -> 'a t -> simulation ->
      simulation
  end

  val is_leq : 'a t -> 'a t -> bool
  val is_eq : 'a t -> 'a t -> bool
    (** Inclusion and equality checks.  Not complete if automata are not
    deterministic. *)

  (*  ==================================================================== *)
  (** {3 Quotient} *)
  (*  ==================================================================== *)

  module Quotient : sig
    val succ_pvertex : 'a t -> int MappeI.t -> SetteI.t -> 'a MapAV.t
      (** Returns a map [(possible letter after a state in pvertex in
	  auto, identifier of a successor equivalence class) -> merge
	  of the associated lattice elements] *)
    val quotient : 'a t -> partition -> 'a Powerset.pauto
  end

  val quotient : 'a t -> partition -> 'a t
    (** Computes the quotient of an automaton by a partition of
	states *)

  (*  ==================================================================== *)
  (** {3 Map and iteration on the trantisitions} *)
  (*  ==================================================================== *)

  val map_trans : ((int * int) -> 'a PL.t -> 'a PL.t) -> 'a t -> 'a t
    (** [map_trans f auto] applies f to all transitions of auto and
	returns a new automaton *)

  val iter_trans : ((int * int) -> 'a PL.t -> unit) -> 'a t -> unit
    (** [iter_trans f auto] applies f to all transitions of auto  *)

  end

(*  ********************************************************************** *)
(** {2 Functor} *)
(*  ********************************************************************** *)

module Make (PL:PLattice.S) :
  (S with module A = PL.A
     and module PL = PL)
