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

(** Relational N-Stack and/or queue abstraction based on lattice automata *)

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

  val f_determinise : 'a t -> 'a t   
  val f_minimise : 'a t -> 'a t
  val determinise : 'a t -> unit
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
  module A = PL.A
  type 'a t = 'a LAuto.t ref

  let dimension auto = LAuto.dimension !auto

  let check_dim auto i msg =
    let dim = dimension auto in
     if (i>=dim)||(i<0) then
      failwith (Format.sprintf "%s: the dimension %i is out of range of the dimensionality %i of the decision automaton " msg i dim)

  let print f fmt auto = LAuto.print f fmt !auto
  let print_dot ?margin ?titlestyle ?vertexstyle ?edgestyle ?title 
    f fmt auto
    =
    LAuto.print_dot ?margin ?titlestyle ?vertexstyle ?edgestyle ?title
      f fmt !auto

  let print_regexp f fmt auto = Regexp.print f fmt (Regexp.simplify (=) (LAuto.to_regexp (LAuto.determinise !auto)))

  let get_lattice auto =
    let info = LAutomatonRep.Graph.info !auto in
      info.LAutomatonRep.lattice


  let get_partition auto =
    let info = LAutomatonRep.Graph.info !auto in
      info.LAutomatonRep.partition

 let get_size auto =
    LAuto.nb_states !auto

  let f_determinise auto =
    ref( LAuto.determinise !auto)

  let f_minimise auto =
    ref( LAuto.minimise !auto)


  let determinise auto =
    auto := LAuto.determinise !auto

  let canonicalize auto =
    auto := LAuto.minimise !auto

  let minimise = canonicalize

  let complementary auto =
    ref (LAuto.complementary !auto)

  let is_prefix ~projection w auto i =
    check_dim auto i "is_prefix";
    LAuto.is_prefix ~projection:projection w !auto 0


  (*  ==================================================================== *)
  (** {3 Constructors} *)
  (*  ==================================================================== *)

  let bottom ~lattice ~partition dim = 
    ref (LAuto.bottom ~lattice ~partition dim)
  let top ~lattice ~partition dim = 
    ref (LAuto.top ~lattice ~partition dim)
  let allempty ~lattice ~partition dim = 
    ref (LAuto.allempty ~lattice ~partition dim)

 let of_regexp ~lattice ~partition dim regexp = 
    ref (LAuto.of_regexp ~lattice ~partition dim regexp)

  (*  ==================================================================== *)
  (** {3 Tests} *)
  (*  ==================================================================== *)

  let is_bottom auto = LAuto.is_bottom !auto
  let is_top auto =
    canonicalize auto;
    LAuto.is_top !auto

  let is_leq ?(det=true) auto1 auto2 =  
    if det then 
      begin
	determinise auto1 ;
	determinise auto2 
      end ;
    LAuto.is_leq !auto1 !auto2

  let is_eq ?(det=true) auto1 auto2 =
    if det then 
      begin
	determinise auto1 ;
	determinise auto2 
      end ;
    LAuto.is_eq !auto1 !auto2

  let is_empty auto i = LAuto.is_empty !auto i

  let is_allempty auto = LAuto.is_allempty !auto

  (*  ==================================================================== *)
  (** {3 Lattice operations} *)
  (*  ==================================================================== *)

  let join auto1 auto2 = ref (LAuto.union !auto1 !auto2)

  let meet auto1 auto2 = ref (LAuto.inter !auto1 !auto2)

  let widening_shape (abstract:'a LAuto.t->'a LAuto.t) auto1 auto2 =
    ref (LAuto.widening_shape abstract !auto1 !auto2)

  (*  ==================================================================== *)
  (** {3 Operations} *)
  (*  ==================================================================== *)

  module type T_Operations = sig
    val cons : 'a LAuto.t -> int -> 'a PL.t -> 'a LAuto.t
    val look : ?cond:'a PL.t -> 'a LAuto.t -> int -> 'a PL.t
    val deriv : ?cond:'a PL.t -> 'a LAuto.t -> int -> 'a LAuto.t
    val look_dual : ?cond:'a PL.t -> 'a LAuto.t -> int -> 'a PL.t
    val deriv_dual : ?cond:'a PL.t -> 'a LAuto.t -> int -> 'a LAuto.t
  end
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

  module Make(T:T_Operations) = struct
    let push auto i letter =
      check_dim auto i "StackQueueNRel.push";
      ref (T.cons !auto i letter)

    let top ?cond auto i =
      check_dim auto i "StackQueueNRel.top";
      T.look ?cond !auto i

    let pop ?cond auto i =
      check_dim auto i "StackQueueNRel.pop";
      let resauto = ref (T.deriv ?cond !auto i) in
      let reselt = T.look ?cond !auto i in
      (resauto,reselt)

    let pop_partitioned ?cond auto i =
      check_dim auto i "StackQueueNRel.pop_partitioned";
      let mapA_L = T.look ?cond !auto i in
      PL.A.Map.mapi
	(begin fun letter elt ->
	  let resauto =
	    ref (T.deriv ~cond:(PL.A.Map.add letter elt PL.A.Map.empty) !auto i)
	  in
	  (resauto,elt)
	end)
	mapA_L

    let drop auto i =
      check_dim auto i "StackQueueNRel.drop";
      ref (T.deriv !auto i)

    let queue = push
    let firstout ?cond auto i =
      check_dim auto i "StackQueueNRel.firstout";
      T.look_dual ?cond !auto i

    let dequeue ?cond auto i =
      check_dim auto i "StackQueueNRel.dequeue";
      let resauto = ref (T.deriv_dual ?cond !auto i) in
      let reselt = T.look_dual ?cond !auto i in
      (resauto,reselt)

    let dequeue_partitioned ?cond auto i =
      check_dim auto i "StackQueueNRel.dequeue_partitioned";
      let mapA_L = T.look_dual ?cond !auto i in
      PL.A.Map.mapi
	(begin fun letter elt ->
	  let resauto =
	    ref (T.deriv_dual ~cond:(PL.A.Map.add letter elt PL.A.Map.empty) !auto i)
	  in
	  (resauto,elt)
	end)
	mapA_L
  end


  module Left = Make(struct
    let cons = LAuto.cons_left
    let look = LAuto.look_left
    let deriv = LAuto.deriv_left
    let look_dual = LAuto.look_right
    let deriv_dual = LAuto.deriv_right
  end)
  module Right = Make(struct
    let cons = LAuto.cons_right
    let look = LAuto.look_right
    let deriv = LAuto.deriv_right
    let look_dual = LAuto.look_left
    let deriv_dual = LAuto.deriv_left
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
