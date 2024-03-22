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


(** Stack and queue abstraction based on lattice automata *)

(*  ********************************************************************** *)
(** {2 Module type for the generated module} *)
(*  ********************************************************************** *)

module type S = sig
  module PL : PLattice.S
  module LAuto : (LAutomaton.S with module A=PL.A
			       and module PL=PL)

  type 'a t = 'a LAuto.t ref

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
    (* give the lattice and the partition of the lattice automaton *)


  val get_size : 'a t -> int

  val canonicalize : 'a t -> unit
    (* Normalization *)

   (* complementary *)
  val complementary : 'a t -> 'a t


  (*  ==================================================================== *)
  (** {3 Constructors} *)
  (*  ==================================================================== *)

  val bottom : lattice:'a PL.lattice -> partition:'a PL.t -> 'a t
  val top    : lattice:'a PL.lattice -> partition:'a PL.t -> 'a t
  val empty  : lattice:'a PL.lattice -> partition:'a PL.t -> 'a t

  (*  ==================================================================== *)
  (** {3 Tests} *)
  (*  ==================================================================== *)

  val is_bottom : 'a t -> bool
  val is_top    : 'a t -> bool
  val is_empty  : 'a t -> bool
  val is_leq : ?det:bool -> 'a t -> 'a t -> bool
  val is_eq  : ?det:bool -> 'a t -> 'a t -> bool

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
    val push : 'a t -> 'a PL.t -> 'a t
    val top : ?cond:'a PL.t -> 'a t -> 'a PL.t
    val pop : ?cond:'a PL.t -> 'a t -> 'a t * 'a PL.t
    val pop_partitioned : ?cond:'a PL.t -> 'a t -> ('a t * 'a) PL.A.Map.t
    val drop : 'a t -> 'a t

    val queue : 'a t -> 'a PL.t -> 'a t
    val firstout : ?cond:'a PL.t -> 'a t -> 'a PL.t
    val dequeue : ?cond:'a PL.t -> 'a t -> 'a t * 'a PL.t
    val dequeue_partitioned : ?cond:'a PL.t -> 'a t -> ('a t * 'a) PL.A.Map.t
  end

  module Left : Operations
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

module Make(LAuto:LAutomaton.S) = struct
  module PL = LAuto.PL
  module LAuto = LAuto

  type 'a t = 'a LAuto.t ref

  let print f fmt auto = LAuto.print f fmt !auto
  let print_dot ?margin ?titlestyle ?vertexstyle ?edgestyle ?title 
    f fmt auto
    =
    LAuto.print_dot ?margin ?titlestyle ?vertexstyle ?edgestyle ?title
      f fmt !auto


  let print_regexp f fmt auto = Regexp.print f fmt (Regexp.simplify (=)(LAuto.to_regexp (LAuto.determinise !auto)))

  let determinise auto =
    auto := LAuto.determinise !auto

  let get_lattice auto =
    let info = LAutomatonRep.Graph.info !auto in
      info.LAutomatonRep.lattice

  let get_partition auto =
    let info = LAutomatonRep.Graph.info !auto in
      info.LAutomatonRep.partition

  let get_size auto =
     LAuto.nb_states !auto

  let canonicalize auto =
    auto := LAuto.minimise !auto

  let complementary auto =
    ref (LAuto.complementary !auto)

  let bottom ~lattice ~partition = ref (LAuto.bottom ~lattice ~partition 1)
  let top ~lattice ~partition = ref (LAuto.top ~lattice ~partition 1)
  let empty ~lattice ~partition = ref (LAuto.allempty ~lattice ~partition 1)
  let is_bottom auto =
    LAuto.is_bottom !auto
  let is_top auto =
    canonicalize auto;
    LAuto.is_top !auto
  let is_empty auto =
    LAuto.is_empty !auto 0

  let is_leq ?(det=true) auto1 auto2 =
    if det then begin
      auto1 := LAuto.determinise !auto1;
      auto2 := LAuto.determinise !auto2;
    end;
    LAuto.is_leq !auto1 !auto2
  let is_eq ?(det=true) auto1 auto2 =
    if det then begin
      auto1 := LAuto.determinise !auto1;
      auto2 := LAuto.determinise !auto2;
    end;
    LAuto.is_eq !auto1 !auto2
  let join auto1 auto2 = 
    ref (LAuto.union !auto1 !auto2)
  let meet auto1 auto2 =
    ref (LAuto.inter !auto1 !auto2)
  let widening_shape abstract auto1 auto2 =
    ref (LAuto.widening_shape abstract !auto1 !auto2)

  (*  ==================================================================== *)
  (** {3 Queue and stack operations} *)
  (*  ==================================================================== *)

  module type T_Operations = sig
    val cons : 'a LAuto.t -> 'a PL.t -> 'a LAuto.t
    val look : ?cond:'a PL.t -> 'a LAuto.t -> 'a PL.t
    val deriv : ?cond:'a PL.t -> 'a LAuto.t -> 'a LAuto.t
    val look_dual : ?cond:'a PL.t -> 'a LAuto.t -> 'a PL.t
    val deriv_dual : ?cond:'a PL.t -> 'a LAuto.t -> 'a LAuto.t
  end
  module type Operations = sig
    val push : 'a t -> 'a PL.t -> 'a t
    val top : ?cond:'a PL.t -> 'a t -> 'a PL.t
    val pop : ?cond:'a PL.t -> 'a t -> 'a t * 'a PL.t
    val pop_partitioned : ?cond:'a PL.t -> 'a t -> ('a t * 'a) PL.A.Map.t
    val drop : 'a t -> 'a t

    val queue : 'a t -> 'a PL.t -> 'a t
    val firstout : ?cond:'a PL.t -> 'a t -> 'a PL.t
    val dequeue : ?cond:'a PL.t -> 'a t -> 'a t * 'a PL.t
    val dequeue_partitioned : ?cond:'a PL.t -> 'a t -> ('a t * 'a) PL.A.Map.t
  end


  module Make(T:T_Operations) = struct
    let push auto letter = ref (T.cons !auto letter)
    let top ?cond auto = T.look ?cond !auto
    let pop ?cond auto =
      let resauto = ref (T.deriv ?cond !auto) in
      let reselt = T.look ?cond !auto in
      (resauto,reselt)
    let pop_partitioned ?cond auto =
      let mapA_L = T.look ?cond !auto in
      PL.A.Map.mapi
	(begin fun letter elt ->
	  let resauto =
	    ref (T.deriv ~cond:(PL.A.Map.add letter elt PL.A.Map.empty) !auto)
	  in
	  (resauto,elt)
	end)
	mapA_L

    let drop auto = ref (T.deriv !auto)

    let queue = push
    let firstout ?cond auto = T.look_dual ?cond !auto
    let dequeue ?cond auto =
      let resauto = ref (T.deriv_dual ?cond !auto) in
      let reselt = T.look_dual ?cond !auto in
      (resauto,reselt)
    let dequeue_partitioned ?cond auto =
      let mapA_L = T.look_dual ?cond !auto in
      PL.A.Map.mapi
	(begin fun letter elt ->
	  let resauto =
	    ref (T.deriv_dual ~cond:(PL.A.Map.add letter elt PL.A.Map.empty) !auto)
	  in
	  (resauto,elt)
	end)
	mapA_L
  end

  module Left = Make(struct
    let cons = LAuto.Language.cons_left
    let look = LAuto.Language.look_left
    let deriv = LAuto.Language.deriv_left
    let look_dual = LAuto.Language.look_right
    let deriv_dual = LAuto.Language.deriv_right
  end)
  module Right = Make(struct
    let cons = LAuto.Language.cons_right
    let look = LAuto.Language.look_right
    let deriv = LAuto.Language.deriv_right
    let look_dual = LAuto.Language.look_left
    let deriv_dual = LAuto.Language.deriv_left
  end)

  (*  ==================================================================== *)
  (** {3 Map and iteration on the trantisitions} *)
  (*  ==================================================================== *)

  let map_trans (f_apply : 'a PL.t -> 'a PL.t) (auto : 'a t) : 'a t =
    ref (LAuto.map_trans (fun (x,y) p -> f_apply p) !auto)

  (** [map_trans f auto] applies f to all transitions of auto and
      returns a new automaton *)

  let iter_trans (f_iter : 'a PL.t -> unit) (auto : 'a t) : unit =
    LAuto.iter_trans (fun (x,y) p -> f_iter p) !auto
      (** [iter_trans f auto] applies f to all transitions of auto  *)


end
