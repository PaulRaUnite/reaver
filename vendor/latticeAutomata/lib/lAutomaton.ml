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



module SetII = LAutomatonRep.SetII
module MapII = LAutomatonRep.MapII
module HashII = LAutomatonRep.HashII
module Graph = LAutomatonRep.Graph

type ('a,'b) info = ('a,'b) LAutomatonRep.info = {
  lattice : 'a;
  partition : 'b;
  initial : SetteI.t array;
  final : SetteI.t array;
  mutable det : bool;
  mutable min : bool;
  mutable counter : int;
}
let print_info = LAutomatonRep.print_info
type pinfo = LAutomatonRep.pinfo = {
  pinitial : SetteI.t array;
  pfinal : SetteI.t array;
  hashPI : (SetteI.t,int) Hashhe.t;
  mutable pcounter : int;
}
let make_pinfo = LAutomatonRep.make_pinfo
type partition = LAutomatonRep.partition = {
  mutable map : int MappeI.t;
  mutable classes : SetteI.t MappeI.t;
}
let partition_of_map = LAutomatonRep.partition_of_map
let partition_of_classes = LAutomatonRep.partition_of_classes
let partition_print = LAutomatonRep.partition_print

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

 val complementary : 'a t  -> 'a t
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

  val cons_right : 'a t -> int -> 'a PL.t -> 'a t
  val deriv_right : ?cond:'a PL.t -> 'a t -> int -> 'a t
  val look_right : ?cond:'a PL.t -> 'a t -> int -> 'a PL.t
  val cons_left : 'a t -> int -> 'a PL.t -> 'a t
  val deriv_left : ?cond:'a PL.t -> 'a t -> int -> 'a t
  val look_left : ?cond:'a PL.t -> 'a t -> int -> 'a PL.t
  val of_regexp : lattice:'a PL.lattice -> partition:'a PL.t -> int -> 'a PL.t Regexp.t -> 'a t
  val to_regexp: 'a t -> 'a PL.t Regexp.t

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
    val letter : lattice:'a PL.lattice -> partition:'a PL.t -> 'a PL.t -> 'a t
    val word : lattice:'a PL.lattice -> partition:'a PL.t -> 'a PL.t list -> 'a t
    (** Create a partitioned automaton recognizing a single letter.
      The letter is supposed included in the partition.
      Result is deterministic. *)

    val transpose : 'a t -> 'a t
    module Concat : sig
      val merge_trans : 'a t -> Graph.vertex * Graph.vertex -> 'a PL.t -> 'a t
      val merge_final_initial : 'a t -> SetteI.t -> SetteI.t -> 'a t
    end
    val concat : 'a t -> 'a t -> 'a t
    val concat_list : 'a t list -> 'a t
    val concat_array : 'a t array -> 'a t
    (** Language concatenation.
      Result is not deterministic in general. *)

    val star : 'a t -> 'a t
    val plus : 'a t -> 'a t
    (** Standard and strict Kleene closure.
      Result is not deterministic in general. *)

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

      - If auto1 and auto2 have bisimilar shapes, then applies L.widening
	pairwise on labels;

      - Otherwise, return [abstract auto2]

      It is assumed that the set of image automata of the [abstract] function
      has a finite set of shapes (this guarantees termination).  *)

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

    val standard : ?initial:bool -> ?final:bool -> forward:int -> backward:int -> 'a t -> 'a t
      (** Standard abstraction, based on an initial partition separating
	  in each dimension initial, final, initial and final states from
	  others (if [initial=false] (resp. [final=false]), then initial
	  states (resp. final states) are not distinguished from others),
	  further refined by a bounded bisimulation of depth [forward] in
	  forward direction, and depth [backward] in backward direction.  *)


    val  construct_partition_bounded : ?initial:bool -> ?final:bool -> forward:int -> backward:int -> 'a t -> partition

  (** Gives a partition of the states of the automaton, based on
      bounded languages (forward) and bounded reverse languages
      (backward). 

  *)

    val bounded_languages : ?initial:bool -> ?final:bool -> forward:int -> backward:int -> 'a t -> 'a t
      (** Quotient of the automaton by the previous partition. *)

    end
end

(*  ********************************************************************** *)
(** {2 Implementation} *)
(*  ********************************************************************** *)
open Format

module Make
    (PL:PLattice.S)
= struct

  module M = LAutomatonRep.Make(PL)
  include M

  (*  ==================================================================== *)
  (** {3 Minimisation} *)
  (*  ==================================================================== *)

  let minimise (auto:'a t) : 'a t
    =
    check_sep auto "LAutomaton.minimise";
    let info = Graph.info auto in
    assert(if info.min then info.det else true);
    if info.min then
      auto
    else begin
      let auto =
	if info.det then
	  auto
	else
	  determinise auto
      in
	(* auto is now deterministic *)
      let info = Graph.info auto in
      let dim = Array.length info.initial in
      (* Create the partition map separating final states from others, for
	 each dimension *)
      let map =
	Graph.fold_vertex auto MappeI.empty
	  (begin fun vertex i ~pred ~succ map ->
	    let id =
	      if SetteI.mem vertex info.final.(i) then dim+i else i
	    in
	    MappeI.add vertex id map
	  end)

      in
      let partition = partition_of_map map in
      (* largest bisimulation *)
      let partition = bisimulation_shape auto partition in
      (* quotient if needed *)
      let auto = quotient auto partition in
      let info = Graph.info auto in
      info.det <- true;
      info.min <- true;
      check_sep auto "LAutomaton.minimise-end";
      auto
    end

  let canonicalise = minimise

  (*  ==================================================================== *)
  (** {3 Union and intersection} *)
  (*  ==================================================================== *)

  (*  -------------------------------------------------------------------- *)
  (** {4 Union} *)
  (*  -------------------------------------------------------------------- *)

  module Union = struct
    let rec sette_offset (set:int Sette.t) (offset:int) : int Sette.t
      =
      match set with
      | Sette.Empty -> set
      | Sette.Node(left,elt,right,height) ->
	  Sette.Node(
	    sette_offset left offset,
	    elt+offset,
	    sette_offset right offset,
	    height
	  )
    let rec mappe_offsetmap (map:(int,'a) Mappe.t) (offset:int) (f:'a -> 'a) : (int,'a) Mappe.t
      =
      match map with
      | Mappe.Empty -> map
      | Mappe.Node(left,key,data,right,height) ->
	  Mappe.Node(
	    mappe_offsetmap left offset f,
	    key+offset,(f data),
	    mappe_offsetmap right offset f,
	    height
	  )
    let rec mappe2_offset (map:(int*int,'a) Mappe.t) (offset:int) : (int*int,'a) Mappe.t
      =
      match map with
      | Mappe.Empty -> map
      | Mappe.Node(left,(key1,key2),data,right,height) ->
	  Mappe.Node(
	    mappe2_offset left offset,
	    (key1+offset,key2+offset),data,
	    mappe2_offset right offset,
	    height
	  )

    let setI_offset set offset =
      SetteI.obj (sette_offset (SetteI.repr set) offset)
    let mapI_offsetmap map offset f =
      MappeI.obj (mappe_offsetmap (MappeI.repr map) offset f)
    let mapII_offset map offset =
      MapII.obj (mappe2_offset (MapII.repr map) offset)

    let info_offset (info:('a,'b) info) offset : ('a,'b) info = { info with
      initial = Array.map (fun set -> setI_offset set offset) info.initial;
      final = Array.map (fun set -> setI_offset set offset) info.final;
      counter = info.counter+offset;
    }
    let node_offset (node:(int,'b) FGraph.node) (offset : int) : (int,'b) FGraph.node =
      { node with
	FGraph.succ = sette_offset node.FGraph.succ offset;
	FGraph.pred = sette_offset node.FGraph.pred offset;
    }
    let auto_offset (auto:'a t) (offset : int) : 'a t =
      let auto = Graph.repr auto in
      let auto = {
	FGraph.nodes = begin
	  mappe_offsetmap auto.FGraph.nodes
	    offset
	    (fun node -> node_offset node offset)
	end;
	FGraph.arcs = mappe2_offset auto.FGraph.arcs offset;
	FGraph.info = info_offset auto.FGraph.info offset
      }
      in
      Graph.obj auto

    let merge (auto1:'a t) (auto2:'a t) : 'a t =
      let auto1 = Graph.repr auto1 in
      let auto2 = Graph.repr auto2 in
      let auto = {
	FGraph.nodes = Mappe.Compare.addmap MappeI.Setkey.Ord.compare auto1.FGraph.nodes auto2.FGraph.nodes;
	FGraph.arcs = Mappe.Compare.addmap MapII.Setkey.Ord.compare auto1.FGraph.arcs auto2.FGraph.arcs;
	FGraph.info = {
	  lattice = auto1.FGraph.info.lattice;
	  partition = auto1.FGraph.info.partition;
	  initial = begin
	    Array.mapi
	      (begin fun i initial1 ->
		SetteI.union initial1 auto2.FGraph.info.initial.(i)
	      end)
	      auto1.FGraph.info.initial
	  end;
	  final = begin
	    Array.mapi
	      (begin fun i final1 ->
		 SetteI.union final1 auto2.FGraph.info.final.(i)
	      end)
	      auto1.FGraph.info.final
	  end;
	  det = false;
	  min = false;
	  counter = Pervasives.max
	  auto1.FGraph.info.counter
	  auto2.FGraph.info.counter;
	}
      }
      in
      Graph.obj auto

    (* Assume non bottom automata *)
    let union (auto1:'a t) (auto2:'a t) : 'a t =
       let info1 = Graph.info auto1 in
       let auto2 = auto_offset auto2 info1.counter in
(*       let info2 = Graph.info auto2 in *)
       let auto = merge auto1 auto2 in
(*
       if info1.det && info2.det then begin
	 (* Check if one can merge initial states in a deterministic way *)
	 let info = Graph.info auto in
	 let initial1 = SetteI.choose info1.initial.(0) in
	 let initial2 = SetteI.choose info2.initial.(0) in
	 let mapA_LxV_1 = Simulation.succ_vertex_det auto1 initial1 in
	 let mapA_LxV_2 = Simulation.succ_vertex_det auto2 initial2 in
	 let mapA_unit = A.Map.common (fun _ _ -> ()) mapA_LxV_1 mapA_LxV_2 in
	 if mapA_unit=A.Map.empty then begin
	   (* YES ! : we merge the initial states *)
	   info.det <- true;
	   let auto = del_state auto initial2 in
	   let mapV_A_L_1 = mapV_A_L_of_mapA_LxV mapA_LxV_1 in
	   let mapV_A_L_2 = mapV_A_L_of_mapA_LxV mapA_LxV_2 in
	   let mapV_A_L = MappeI.merge A.Map.addmap mapV_A_L_1 mapV_A_L_2 in
	   MappeI.fold
	     (begin fun succ mapA_L auto ->
	       Graph.add_edge auto (initial1,succ) mapA_L
	     end)
	     mapV_A_L
	     auto
	 end
	 else
	   auto
       end
       else
*)
	 check_sep auto "LAutomaton.union";
	 auto

     let union_auto (auto1:'a t) (auto2:'a t) : 'a t =
       auto_check auto1 auto2 "LAutomaton.union";
       if is_bottom auto1 then auto2
       else if is_bottom auto2 then auto1
       else begin
	 let info1 = Graph.info auto1
	 and info2 = Graph.info auto2
	 in
	 if info1.counter > info2.counter (* auto1 probably bigger *)
	 then union auto1 auto2
	 else union auto2 auto1
       end

  end

  let union = Union.union_auto

  (*  -------------------------------------------------------------------- *)
  (** {3 Complementary - restricted to finite automata} *)
  (*  -------------------------------------------------------------------- *)
    
  let complementary (auto:'a t) : 'a t =
    check_sep auto "LAutomaton.complementary";
   let auto =  determinise auto in
    let info = Graph.info auto in
    let lattice = info.lattice 
    and partition = info.partition in
    let dim = dimension auto 
    and cmpt_init = info.counter
    in
      (* convert_mappe converts a boolean version of the partition to a ('letter,'abstract) mappe *) 
    let convert_mappe_bool mappe_bool =
      PL.A.Map.fold 
	(fun l b m_res -> 
	  if b 
	  then PL.A.Map.add l (PL.A.Map.find l partition) m_res 
	  else m_res
	)
	mappe_bool
	PL.A.Map.empty
    in
      if is_bottom auto
      then
	(* special case : if bottom, returns top *)
	begin
	  top ~lattice:lattice ~partition:partition dim
	end
      else
	begin
	  (* dealing with sink states *)
	  let auto_ref = ref auto 
	  and mappe_bool_init = PL.A.Map.mapi (fun l _ -> l <> PL.A.sep) partition
	  in
	    for i = 0 to dim-1 do
	      auto_ref := add_state !auto_ref i ~initial:false ~final:false (cmpt_init+i);
	      auto_ref := Graph.add_edge !auto_ref (cmpt_init+i,cmpt_init+i) (PL.A.Map.remove PL.A.sep partition);
	      (* adds all transitions, except the one labelled by sep *)
	      if i>0
	      then
		auto_ref := Graph.add_edge !auto_ref (cmpt_init+i-1,cmpt_init+i) (PL.sep lattice)
	    done ;
	    (* sink states and separation edges are now added to the graph *)
	    let auto = !auto_ref 
	      (* underlying graph *)
	    in

	    let f_fold_vertex num_vertex nb_queue ~pred ~succ graph_res =
	      let mappe_bool = ref mappe_bool_init
		(* all bindings are true, except for the separation letter *)
	      in
		Graph.SetV.iter 
		  (fun num_new_vertex -> 
		    let mappe_trans = 
		      PL.A.Map.mapi 
			(fun l _ -> l <> PL.A.sep) 
			(Graph.attredge auto (num_vertex,num_new_vertex)) 
		    in
		      mappe_bool := PL.A.Map.merge (fun b1 b2 -> (not b1) && b2) mappe_trans !mappe_bool
		  ) 
		  succ ;
		let graph_res =
		  if (nb_queue < dim-1) && not (SetteI.mem num_vertex info.final.(nb_queue) )
		  then
		    (* add a separation transition from non-final states to the sink state of queue i+1 *)
		    Graph.add_edge graph_res (num_vertex,cmpt_init+nb_queue+1) (PL.sep lattice)
		  else
		    graph_res
		in
		(* now, mappe_bool contains all letters that label the transition leading to the sink state *)
		let mappe_to_be_added = convert_mappe_bool !mappe_bool in
		  if PL.A.Map.is_empty mappe_to_be_added
		  then graph_res
		  else Graph.add_edge graph_res (num_vertex,cmpt_init+nb_queue) mappe_to_be_added	    	    
	    in
	    let auto = Graph.fold_vertex auto auto f_fold_vertex in
	      (* all transitions leading to the sink states are now added to the graph *)
	    let auto = Graph.repr auto in

	    let auto_res = {
		FGraph.nodes = auto.FGraph.nodes;
		FGraph.arcs = auto.FGraph.arcs;
		FGraph.info = {
		    lattice = lattice;
		    partition = partition;
		    initial = 
		    Array.mapi
		      (begin fun i initiali ->
			if i>0
			then
			  SetteI.add (cmpt_init+i) initiali
			    (* sink states are initial states, except for the first queue *)
			else 
			  initiali
			end)
		      auto.FGraph.info.initial;
		    final = begin
			Array.mapi
			  (begin fun i finali ->
			    if i<dim-1
			    then
			      FGraph.fold_vertex 
				(FGraph.obj auto)
				SetteI.empty 
				(fun nbs nbq ~pred ~succ setteI -> 
				  if (nbq = i && Sette.exists (fun x-> FGraph.attrvertex (FGraph.obj auto) x = i+1) succ) 
				  then 
				    SetteI.add nbs setteI
				  else setteI
				)
			
			    else
			      begin
				(* inverse final states and non-final states *)
				FGraph.fold_vertex 
				  (FGraph.obj auto) 
				  SetteI.empty 
				  (fun num_vertex nb_queue ~pred ~succ sette_res ->
				    if nb_queue = dim-1 && (not (SetteI.mem num_vertex finali))
				      (* NB: in the else branch, finali is the final states of the last queue *)
				    then SetteI.add num_vertex sette_res
				    else sette_res
				  )
			      end
			    end)
			  auto.FGraph.info.final
		      end;
		    det = auto.FGraph.info.det;
		    min = auto.FGraph.info.min;
		    counter = cmpt_init+dim;
		  }
	      }
	    in
	    let auto=  Graph.obj auto_res in
	      check_sep auto "LAutomaton.complementary-end";
	      reachability(coreachability auto)
		(* auto is trim *)
	end

  (*  -------------------------------------------------------------------- *)
  (** {4 Intersection} *)
  (*  -------------------------------------------------------------------- *)

  module Inter = struct

    (* add a transition in a graph where edges are labelled by a couple of integer *)
    let add_trans (auto : 'a t) (edge:int*int) (letter:A.t) (elt:'a)
      :
      'a t
      =
      let mapA_L =
	try Graph.attredge auto edge
	with Not_found -> A.Map.empty
      in
      Graph.add_edge auto edge (A.Map.add letter elt mapA_L)



    (* associate to the vertex the map
       letter ->
       list of pair (elt,successor) of the vertex by the letter *)
    let succ_vertex (auto:'a t) (vertex:int) : ('a * int) list ref A.Map.t
      =
      let succ = Graph.succ auto vertex in
      SetteI.fold
	(begin fun succ (map:('a * int) list ref A.Map.t) ->
	  let mapA_L = Graph.attredge auto (vertex,succ) in
	  A.Map.fold
	    (begin fun (letter:A.t) (elt:'a) map ->
	      try
		let rlist = A.Map.find letter map in
		rlist := (elt,succ) :: !rlist;
		map
	      with Not_found ->
		A.Map.add letter (ref [(elt,succ)]) map
	    end)
	    mapA_L
	    map
	end)
	succ
	A.Map.empty

    let inter (auto1:'a t) (auto2:'a t) : 'a t * int HashII.t =
      let info1 = Graph.info auto1
      and info2 = Graph.info auto2 in
      let dim = Array.length info1.initial in
      let counter = ref 0 in
      let auto = ref (bottom ~lattice:info1.lattice ~partition:info1.partition dim) in
      let (explored:int HashII.t) = HashII.create 11 in

      (* Assume it has not been explored *)
      let rec explore_pair_vertex (vertex1:int) (vertex2:int) : int
	=
	let i = Graph.attrvertex auto1 vertex1 in
	(* Add the new state *)
	let vertex = !counter in
	HashII.add explored (vertex1,vertex2) vertex;
	auto := begin
	  add_state !auto
	    i
	    ~initial:
	    (SetteI.mem vertex1 info1.initial.(i) &&
	    SetteI.mem vertex2 info2.initial.(i))
	    ~final:
	    (SetteI.mem vertex1 info1.final.(i) &&
	    SetteI.mem vertex2 info2.final.(i))
	    vertex
	end;
	incr counter;
	(* combine outgoing transitions *)
	let mapA_LxV_1 = succ_vertex auto1 vertex1 in
	let mapA_LxV_2 = succ_vertex auto2 vertex2 in
	let (mapA_LxV : (('a * int) list * ('a * int) list) A.Map.t) =
	  A.Map.common
	    (fun rlist1 rlist2 -> (!rlist1,!rlist2))
	    mapA_LxV_1 mapA_LxV_2
	in
	A.Map.iter
	  (begin fun letter (list1,list2) ->
	    (* Iterate on pairs of successors *)
	    List.iter
	    (begin fun (elt1,succ1) ->
	      List.iter
	      (begin fun (elt2,succ2) ->
		let elt = info1.lattice.PLattice.meet letter elt1 elt2 in
		if not (info1.lattice.PLattice.is_bottom letter elt) then begin
		  let succ =
		    try HashII.find explored (succ1,succ2)
		    with Not_found ->
		      explore_pair_vertex succ1 succ2
		  in
		  auto := add_trans !auto (vertex,succ) letter elt
		end
	      end)
	      list2
	    end)
	    list1
	  end)
	  mapA_LxV
	;
	vertex
      in

      (* body of intersection *)
      SetteI.iter
	(begin fun vertex1 ->
	  SetteI.iter
	  (begin fun vertex2 ->
	    let _ =
	      try HashII.find explored (vertex1,vertex2)
	      with Not_found ->
		explore_pair_vertex vertex1 vertex2
	    in
	    ()
	  end)
	  info2.initial.(0)
	end)
	info1.initial.(0)
      ;
      auto := reachability (coreachability !auto);
      auto :=
      if is_bottom !auto then
	!auto
      else
	Graph.map_info !auto
	  (begin fun info -> { info with
	    det = info1.det && info2.det;
	    min = false;
	    counter = !counter;
	  }
	  end)
      ;
      (!auto,explored)
  end

  let inter auto1 auto2 = 
    auto_check auto1 auto2 "LAutomaton.inter";
    let auto = fst (Inter.inter auto1 auto2) in
      (* result is trim *)
      check_sep auto "LAutomaton.inter-end";
      auto

  let listop_of_binop msg (binop:'a t -> 'a t -> 'a t) (list:'a t list) =
    match list with
    | [] -> failwith (sprintf "%s: the list must contain at least one element" msg)
    | x::l -> List.fold_left binop x l

  let arrayop_of_binop (msg:string) (binop:'a t -> 'a t -> 'a t) (tab:'a t array) =
    if tab=[||] then
      failwith (sprintf "%s: the array must contain at least one element" msg)
    else
      let res = ref tab.(0) in
      for i=1 to pred (Array.length tab) do
	res := binop !res tab.(i)
      done;
      !res

  let union_list list = listop_of_binop "LAutomaton.union_list" union list
  let inter_list list = listop_of_binop "LAutomaton.inter_list" union list

  let union_array tab = arrayop_of_binop "LAutomaton.union_array" union tab
  let inter_array tab = arrayop_of_binop "LAutomaton.inter_array" inter tab

  (*  ==================================================================== *)
  (** {3 Language operations} *)
  (*  ==================================================================== *)

  let dprint_pl fmt (pl:'a PL.t) =
    if PL.is_sep pl
    then
      Format.pp_print_string fmt "#"      
    else
    PL.print
      (fun fmt c -> Format.pp_print_char fmt (Obj.magic c))
      (fun fmt _ -> ())
      fmt pl

  let dprint fmt (auto:'a t) =
    print dprint_pl fmt auto


  let dprint_pls fmt (pl:'a PL.t) =
    if PL.is_sep pl
    then
      Format.pp_print_string fmt "#"      
    else
    PL.print
      (fun fmt _ -> Format.pp_print_char fmt '-')
      (fun fmt _ -> ())
      fmt pl

  let dprints fmt (auto:'a t) =
    print dprint_pls fmt auto

  module Redirect = struct

      (* OLD VERSION
    let change_org_of_outgoing_sep
      (sep:'a PL.t) (old_auto : 'a t) (auto:'a t ref) (i:int) (final:int) (nfinal:SetteI.t)
      =
      let succ = Graph.succ old_auto final in
      let new_g =
      SetteI.fold
	(begin fun succ g ->
	  let attr = Graph.attrvertex old_auto succ in
	  if attr<>i then begin
	    assert(attr=i+1);
	    assert(PL.is_sep (Graph.attredge old_auto (final,succ)));
	    let g = Graph.remove_edge g (final,succ) in
	      if  SetteI.is_emty nfinal 
		then
	    SetteI.fold
	      (begin fun nfinal g ->
		Graph.add_edge g (nfinal,succ) sep
	      end)
	      nfinal
	      (begin 
		 let g = Graph.remove_edge g (final,succ) in
		   if SetteI

	       end)
	    end
	  else g
	end)
	succ
	!auto
      in 
	auto := new_g
      *)


    (* re-assign sep transitions *)
    let change_org_of_outgoing_sep
	(sep:'a PL.t) 
	(old_auto : 'a t) 
	(auto:'a t ref) 
	(i:int) 
	(final:int) 
	(nfinal:SetteI.t) 
	(ladd:(int*int) list ref)
	(lsuppr:(int*int) list ref)
	: unit
	=
      let succ = Graph.succ old_auto final in
	SetteI.iter
	  (begin fun succ ->
	     let attr = Graph.attrvertex old_auto succ in
	       if attr<>i then begin
		 assert(attr=i+1);
		 assert(PL.is_sep (Graph.attredge old_auto (final,succ)));
		 if Graph.is_edge !auto (final,succ) then
		   lsuppr := (final,succ)::!lsuppr
		 ;
		 SetteI.iter
		   (begin fun nfinal ->
		      ladd := (nfinal,succ)::!ladd
		    end)
		   nfinal;
		 ()
		   (*
		     if SetteI.is_empty nfinal  
		     
		     then
		     begin
		     let is_still_initial = 
		     SetteI.fold 
		     (fun psucc b-> b ||  (PL.is_sep (Graph.attredge !auto (psucc,succ))))
		     (Graph.pred !auto succ)
		     false
		     in
		     if not is_still_initial
		     then (Graph.info !auto).initial.(i+1) <- SetteI.remove succ  (Graph.info !auto).initial.(i+1)
		     end
		   *)
	       end
	   end)
	  succ
	;
	()

     (* re-assign sep transitions *)
    let change_dest_of_incoming_sep
	(sep:'a PL.t) 
	(old_auto : 'a t) 
	(auto:'a t ref) 
	(i:int) 
	(initial:int) 
	(ninitial:SetteI.t)
	(ladd: (int*int) list ref)
	(lsuppr:(int*int) list ref)
	: unit
	=
      let pred = Graph.pred old_auto initial in
	SetteI.iter
	  (begin fun pred ->
	     let attr = Graph.attrvertex old_auto pred in
	       if attr<>i then begin
		 assert(attr=i-1);
		 assert(PL.is_sep (Graph.attredge old_auto (pred,initial)));
		 if Graph.is_edge !auto (pred,initial) then
		   lsuppr := (pred,initial)::!lsuppr
		 ;
		 SetteI.iter
		   (begin fun ninitial ->
		      ladd := (pred,ninitial)::!ladd
		    end)
		   ninitial;
		 ()
		   (*
		     if SetteI.is_empty ninitial  	       
		     then
		     begin
		     let is_still_final = 
		     SetteI.fold 
		     (fun spred b-> b ||  (PL.is_sep (Graph.attredge !auto (pred, spred))))
		     (Graph.succ !auto pred)
		     false
		     in
		     if not is_still_final
		     then (Graph.info !auto).final.(i-1) <- SetteI.remove pred  (Graph.info !auto).final.(i-1)
		     end
		   *)
	       end
	   end)
	  pred
	;
	()
  end

  let cons_right
      (auto:'a t) (i:int) (letter:'a PL.t)
      :
      'a t
      = 
    check_sep auto "LAutomaton.cons_right";
    if is_bottom auto then
      auto
    else begin
      let info = Graph.info auto in
      let dim = Array.length info.initial in
      let counter = ref info.counter in
      let ninfo = { info with
		      final = Array.copy info.final;
		      min = false;
		  }
      in
	ninfo.final.(i) <- SetteI.empty;
	let nauto = ref (Graph.set_info auto ninfo) in
	  if i=dim-1 then begin
	    (* Simple case (but important in case of one dimensional automaton):
	       optimisation: one add only one final state *)
	    nauto := add_state !nauto i ~final:true !counter;
	    SetteI.iter
	      (begin fun final ->
		 nauto := Graph.add_edge !nauto (final,!counter) letter
	       end)
	      info.final.(i)
	    ;
	    incr counter
	  end
	  else begin
	    let sep = PL.sep info.lattice in
	    let old_auto = !nauto in (* freeze the value of auto *)
	    let ladd = ref [] 
	    and lsuppr = ref []
	    in
	      SetteI.iter
		(begin fun final ->
		   (* For each former final state, add a new state *)
		   nauto := add_state !nauto i ~final:true !counter;
		   Redirect.change_org_of_outgoing_sep sep old_auto nauto i
		     final
		     (SetteI.singleton !counter)
		     ladd
		     lsuppr	  
		   ;
		   nauto := Graph.add_edge !nauto (final,!counter) letter;
		   incr counter
		 end)
		info.final.(i)
	      ;
	      (*ladd and lsuppr now contain the transitions to be added or to be deleted *)
	      List.iter (fun (x,y) -> nauto := Graph.remove_edge !nauto (x,y)) !lsuppr;
	      List.iter (fun (x,y) -> nauto := Graph.add_edge !nauto (x,y) sep) !ladd;
	      ()
	  end;
	  ninfo.det <- info.det &&
	    Det.is_pvertex_deterministic !nauto info.final.(i);
	  ninfo.counter <- !counter;
	  if PL.is_sep letter then
	    (* It might be the case that letter is the separator, e.g. when called from
	       of_regexp.  In this case, check_sep will not pass... *)
	    ()
	  else
	    check_sep !nauto "LAutomaton.cons_right-end"
	  ;
	  
	  !nauto
    end

  let deriv_right
      ?(cond:'a PL.t option)
      (auto:'a t) 
      (i:int)
      :
      'a t
      =
    check_sep auto "LAutomaton.deriv_right";
    let info = Graph.info auto in
    let dim = Array.length info.initial in
      if is_bottom auto || is_empty auto i then
	bottom ~lattice:info.lattice ~partition:info.partition dim
      else begin
	let sep = PL.sep info.lattice in
	let ninfo = { info with
			initial = Array.copy info.initial;
			final = Array.copy info.final;
			min = false;
		    }
	in
	  ninfo.final.(i) <- SetteI.empty;
	  let nauto = ref (Graph.set_info auto ninfo) in
	  let old_auto = !nauto in (* freeze the value of auto *)
	  let ladd = ref [] 
	  and lsuppr = ref []
	  in
	    SetteI.iter
	      (begin fun final ->
		 let pred = Graph.pred auto final in
		 let nfinal = match cond with
		   | None -> 
		       SetteI.filter
			 (begin fun pred ->
			    let attredge = Graph.attredge auto (pred,final) in
			      not (PL.is_sep attredge)
			  end)
			 pred
		   | Some cond ->
		       SetteI.filter
			 (begin fun pred ->
			    let attredge = Graph.attredge auto (pred,final) in
			      not 
				((PL.is_sep attredge) ||
				   let inter = PL.meet info.lattice attredge cond 
				   in
				     PL.is_bottom info.lattice inter
				)
			  end)
			 pred
		 in
		   assert(i<dim);
		     begin
		       Redirect.change_org_of_outgoing_sep 
			 sep 
			 old_auto 
			 nauto 
			 i
			 final
			 nfinal
			 ladd
			 lsuppr
		     end
		   ;
		   ninfo.final.(i) <- SetteI.union nfinal ninfo.final.(i);
	       end)
	      info.final.(i)
	    ;
	    (*ladd and lsuppr now contain the transitions to be added or to be deleted *)
	    List.iter (fun (x,y) -> nauto := Graph.remove_edge !nauto (x,y)) !lsuppr;
	    List.iter (fun (x,y) -> nauto := Graph.add_edge !nauto (x,y) sep) !ladd;

	    (* check whether the states are still initial in queue i+1 *)
	    if i<dim-1 
	    then
	      begin
		ninfo.initial.(i+1) <-
		  SetteI.filter
		  (fun x -> 
		     let pred = Graph.pred !nauto x in
		       SetteI.fold
			 (fun px b ->
			    b ||
			    (PL.is_sep (Graph.attredge !nauto (px,x)))
			 )
			 pred
			 false
		  )
		  ninfo.initial.(i+1)
	      end		
	    ;


	    if ninfo.final.(i)=SetteI.empty then
	      bottom ~lattice:info.lattice ~partition:info.partition dim
	    else
	      begin
		nauto := coreachability (reachability !nauto);
		nauto := restore_sep_consistance !nauto;
		check_sep !nauto "LAutomaton.deriv_right-end";
		!nauto
	      end
		(* auto is trim *)
      end

  let look_right
    ?(cond : 'a PL.t option)
    (auto : 'a t) (i:int)
    :
    'a PL.t
    =
    check_sep auto "LAutomaton.look_right";
    let info = Graph.info auto in
    SetteI.fold
      (begin fun final letter ->
	let pred = Graph.pred auto final in
	SetteI.fold
	  (begin fun pred letter ->
	    let attredge = Graph.attredge auto (pred,final) in
	    let attredge = match cond with
	      | None -> attredge
	      | Some x -> PL.meet info.lattice attredge x
	    in
	    PL.join info.lattice letter attredge
	  end)
	  pred letter
      end)
      info.final.(i) PL.bottom

  let cons_left
    (auto:'a t) (i:int) (letter:'a PL.t)
    :
    'a t
    =
    check_sep auto "LAutomaton.cons_left";
    let res =
      if is_bottom auto then
	auto
      else begin
	let info = Graph.info auto in
	let counter = ref info.counter in
	let ninfo = { info with
	  initial = Array.copy info.initial;
	  min = false;
	}
	in
	ninfo.initial.(i) <- SetteI.empty;
	let nauto = ref (Graph.set_info auto ninfo) in
	if i=0 then begin
	  (* Simple case (but important in case of one dimensional automaton):
	     optimisation: one add only one initial state *)
	  nauto := add_state !nauto i ~initial:true !counter;
	  SetteI.iter
	    (begin fun initial ->
	      nauto := Graph.add_edge !nauto (!counter,initial) letter
	    end)
	    info.initial.(i)
	  ;
	  incr counter;
	end
	else begin
	  let sep = PL.sep info.lattice in
	  let old_auto = !nauto in (* freeze the value of auto *)
	  let ladd = ref []
	  and lsuppr = ref [] 
	  in
	  SetteI.iter
	    (begin fun initial ->
	      (* For each former initial state, add a new state *)
	      nauto := add_state !nauto i ~initial:true !counter;
	      Redirect.change_dest_of_incoming_sep 
		sep 
		old_auto 
		nauto 
		i
		initial
		(SetteI.singleton !counter)
		ladd
		lsuppr
	      ;
	      nauto := Graph.add_edge !nauto (!counter,initial) letter;
	      incr counter
	    end)
	    info.initial.(i)
	  ;
	  (*ladd and lsuppr now contain the transitions to be added or to be deleted *)
	  List.iter (fun (x,y) -> nauto := Graph.remove_edge !nauto (x,y)) !lsuppr;
	  List.iter (fun (x,y) -> nauto := Graph.add_edge !nauto (x,y) sep) !ladd;

	end;
	 (* deterministic if it was deterministic *)
	ninfo.counter <- !counter;
	!nauto
      end
    in
      if PL.is_sep letter then
	(* It might be the case that letter is the separator, e.g. when called from
	       of_regexp.  In this case, check_sep will not pass... *)
	    ()
	  else
	    check_sep res "LAutomaton.cons_left-end"
	  ;
	  res

  let deriv_left
      ?(cond:'a PL.t option)
      (auto:'a t) (i:int)
      :
      'a t
      =
 (*   Format.printf "@. deriv_left, automaton : @[%a@]@." (dprint auto;*)
    check_sep auto "LAutomaton.deriv_left";
    let info = Graph.info auto in
    let dim = Array.length info.initial in
      if is_bottom auto || is_empty auto i then
	bottom ~lattice:info.lattice ~partition:info.partition dim
      else begin
	let sep = PL.sep info.lattice in
	let ninfo = { info with
			initial = Array.copy info.initial;
			final = Array.copy info.final;
			min = false;
		    }
	in
	  ninfo.initial.(i) <- SetteI.empty;
	  let nauto = ref (Graph.set_info auto ninfo) in
	  let det = ref ninfo.det in
	  let old_auto = !nauto in (* freeze the value of nauto *)
	  let ladd = ref []
	  and lsuppr = ref []
	  in
	    SetteI.iter
	      (begin fun initial ->
		 let succ = Graph.succ auto initial in
		 let ninitial = match cond with
		   | None -> 
		       SetteI.filter
			 (begin fun succ ->
			    let attredge = Graph.attredge auto (initial,succ) in
			      not (PL.is_sep attredge)
			  end)
			 succ
		   | Some cond ->
		       SetteI.filter
			 (begin fun succ ->
			    let attredge = Graph.attredge auto (initial,succ) in
			      not 
				(
				  (PL.is_sep attredge) ||
				    let inter = PL.meet info.lattice attredge cond in
				      PL.is_bottom info.lattice inter
				)
			  end)
			 succ
		 in
		   Redirect.change_dest_of_incoming_sep 
		     sep 
		     old_auto 
		     nauto 
		     i
		     initial
		     ninitial
		     ladd
		     lsuppr
		   ;
		   ninfo.initial.(i) <- SetteI.union ninitial ninfo.initial.(i);
		   det := !det && (SetteI.cardinal ninitial) <= 1;
	       end)
	      info.initial.(i)
	    ;
	    (*ladd and lsuppr now contain the transitions to be added or to be deleted *)
	    List.iter (fun (x,y) -> nauto := Graph.remove_edge !nauto (x,y)) !lsuppr;
	    List.iter (fun (x,y) -> nauto := Graph.add_edge !nauto (x,y) sep) !ladd;

	    (* check whether the states are still final in queue i-1 *)
	    if i>0
	    then
	      begin
		ninfo.final.(i-1) <-
		  SetteI.filter
		  (fun x -> 
		     let succ = Graph.succ !nauto x in
		       SetteI.fold
			 (fun sx b ->
			    b ||
			    (PL.is_sep (Graph.attredge !nauto (x,sx)))
			 )
			 succ
			 false
		  )
		  ninfo.final.(i-1)
	      end		
	    ;


	    ninfo.det <- !det;
	    if ninfo.initial.(i)=SetteI.empty then
	      bottom ~lattice:info.lattice ~partition:info.partition dim
	    else begin
(*	      Format.printf "@. deriv_left-END, automaton : @[%a@]@." (dprint !nauto;*)
	      check_sep !nauto "LAutomaton.deriv_left-end";
	      nauto := coreachability (reachability !nauto);
	      !nauto
	    (*  is trim *)
	    end
      end

  let look_left
    ?(cond : 'a PL.t option)
    (auto : 'a t) (i:int)
    :
    'a PL.t
    =
    check_sep auto "LAutomaton.look_left";
    let info = Graph.info auto in
    SetteI.fold
      (begin fun initial map ->
	let succ = Graph.succ auto initial in
	SetteI.fold
	  (begin fun succ map ->
	    let attredge = Graph.attredge auto (initial,succ) in
	    let attredge = match cond with
	      | None -> attredge
	      | Some x -> PL.meet info.lattice attredge x
	    in
	    PL.join info.lattice map attredge
	  end)
	  succ map
      end)
      info.initial.(i) A.Map.empty


(** to_regexp converts an automaton into a regular expression *)


(* [to_regexp] implements the method that removes states of an automaton, then applies the classical Kleene's method. *)

  let to_regexp (auto : 'a t) : 'a PL.t Regexp.t =
    if is_bottom auto
    then Regexp.Empty
    else 
      begin
	(* convert the automaton into a graph where transitions are labelled by regular expressions *)
	let graph : (int,'a PL.t Regexp.exp, ('a PL.lattice,'a PL.t) info) Graph.t = 
	  Graph.map_edge auto (fun _ l -> Regexp.Letter l)
	in
	let info = Graph.info graph in
	let dim = Array.length info.initial in

	(* set of all vertices *)
	let set_vertices = Graph.vertices graph in
	  
	let graph = ref graph in
	(* assertion - check it for debugging *)
	Graph.iter_vertex 
	  !graph
	  (fun x _ ~pred ~succ ->
	    SetteI.iter (fun s -> assert (Graph.is_edge !graph (x,s))) succ;
	    SetteI.iter (fun p -> assert (Graph.is_edge !graph (p,x))) pred;
	  );
	
	let redirect_edges (self_loop:bool) (v:int) (pred:int) (succ:int): unit = 
	  
	  let exp1 = Graph.attredge !graph (pred,v) in
	  let exp2 = Graph.attredge !graph (v,succ) in
	  let new_regexp : 'a PL.t Regexp.exp= 
	    if self_loop 
	    then (* if there is a self-loop on v, add a star regular expression *)
	      let exp_v = Graph.attredge !graph (v,v) in
		Regexp.Concat [ exp1 ; Regexp.Star  exp_v ; exp2 ] 
	    else
	      Regexp.Concat [ exp1 ; exp2 ] 
	  in
	  let new_regexp =
	    if Graph.is_edge !graph (pred,succ) 
	    then (* if there is already an edge (pred, succ) , do the union of the two regular expressions *)
	      Regexp.Union [ Graph.attredge  !graph (pred,succ) ; new_regexp]
	    else 
	      new_regexp
	  in 
	    graph:= Graph.add_edge !graph (pred,succ) new_regexp 

	in

	let work_on_vertex (v:int) : unit =  
	  let succ: SetteI.t = Graph.succ !graph v in
	  let pred: SetteI.t = Graph.pred !graph v in
	    if 
	      (SetteI.is_empty (SetteI.remove v succ))
	      || (SetteI.is_empty (SetteI.remove v pred))
	      || (SetteI.mem v info.initial.(0))
	      || (SetteI.mem v info.final.(dim-1))
	    then (* if v has no predecessor or successor except itself, or is an initial or final state, do nothing *)
	      ()
	    else
	      begin
		(* debug*)
		SetteI.iter (fun p -> assert (Graph.is_edge !graph (p,v))) pred;
		SetteI.iter (fun s -> assert (Graph.is_edge !graph (v,s))) succ;

		let self_loop = SetteI.mem v succ in
		  (* test whether there is a self-loop on v *)
		SetteI.iter
		  (fun p ->
		    if p <> v then
		      SetteI.iter
			(fun s ->
			  if s <> v then
			    redirect_edges self_loop v p s
			)
			succ
		  )
		  pred;
		graph:=Graph.remove_vertex !graph v;
		(* debug *)
		SetteI.iter (fun p -> if p<>v then assert (not (Graph.is_edge !graph (p,v) || SetteI.mem v (Graph.succ !graph p)))) pred;
		SetteI.iter (fun s -> if s<>v then assert (not (Graph.is_edge !graph (v,s) || SetteI.mem v (Graph.pred !graph s)))) succ;
	      end ;
	in
	SetteI.iter work_on_vertex set_vertices ;
      (* end of state reduction *)


      (* implementation of Kleene's method on the reduced set of vertices *)
	let nb_v = Graph.size_vertex !graph in
	let tab_v = Array.make nb_v 0 in
	let k= ref 0 in Graph.iter_vertex !graph (fun x l ~pred ~succ -> tab_v.(!k) <- x ; incr k);
	(* tab_v is needed because the identifier of the vertices may be greater than nb_v *)
	let v i : int= tab_v.(i) in

	let mat_v = Array.make_matrix nb_v nb_v Regexp.Empty in	

	(* initialization *)
	for i= 0 to nb_v -1 do
	    for j = 0 to nb_v-1 do
	      try mat_v.(i).(j) <- Regexp.Exp (if i = j then Regexp.Union [Regexp.Epsilon; Graph.attredge !graph (v i,v j)] else Graph.attredge !graph (v i,v j) )
	      with
		  Not_found -> if i = j then mat_v.(i).(j) <- Regexp.Exp Regexp.Epsilon
	    done
	done;


	(* loop *)
	for k =0 to nb_v-1 do
	  let new_mat_v = Array.init nb_v (fun k -> Array.copy mat_v.(k)) in
	  for i= 0 to nb_v -1 do
	    for j = 0 to nb_v-1 do
	      let r_i_j = mat_v.(i).(j)
	      and r_i_k = mat_v.(i).(k)
	      and r_k_k = mat_v.(k).(k)
	      and r_k_j = mat_v.(k).(j)
	      in
	      let exp_res : 'a PL.t Regexp.t = 
	      match (r_i_k,r_k_k,r_k_j) with
		  (Regexp.Empty,_,_) -> Regexp.Empty
		| (_,Regexp.Empty,_) -> Regexp.Empty 
		| (_,_,Regexp.Empty) -> Regexp.Empty
		| (Regexp.Exp exp1, Regexp.Exp Regexp.Epsilon, Regexp.Exp exp2) -> Regexp.Exp (Regexp.Concat [exp1 ; exp2 ])
		| (Regexp.Exp exp1, Regexp.Exp exp2, Regexp.Exp exp3) -> Regexp.Exp (Regexp.Concat [exp1 ; Regexp.Star exp2 ; exp3 ])
	      in
	      let exp_res : 'a PL.t Regexp.t =
	      match (exp_res, r_i_j) with
		  (Regexp.Empty, _) -> r_i_j
		| (_, Regexp.Empty) -> exp_res
		| (Regexp.Exp exp1, Regexp.Exp exp2) -> Regexp.Exp (Regexp.Union [exp1;exp2])
	      in
	      new_mat_v.(i).(j) <- exp_res
	    done
	  done;

	  (* erase the old values*)
	  for i = 0 to nb_v -1 do
	    for j = 0 to nb_v -1 do
	      mat_v.(i).(j) <- new_mat_v.(i).(j) 
	    done
	  done
	done;
      (* mav_v.(i).(j) now cointains the regular expression of all path from i to j *)

	(* big sum of regular expression to any inital state to any final state *)
	let initial_states = info.initial.(0)
	and final_states = info.final.(dim-1)
	in

	let list_res =
	  let fold_final initial final list_acc = 
	    let i = let res = ref (-1) in Array.iteri (fun i x -> if x=initial then res:=i) tab_v; !res in
	    let j = let res = ref (-1) in Array.iteri (fun i x -> if x=final then res:=i) tab_v; !res in
	    (* seach the indices in tab_v corresponding to [initial] and [final] *)
	    if (i= -1) || (j= -1)
	    then
	      (* if one of the two states was removed, do nothing *)
	      list_acc
	    else
	      try
	      begin
		match mat_v.(i).(j) with
		    Regexp.Empty -> list_acc
		  | Regexp.Exp exp -> exp::list_acc
	      end
	      with
		  Invalid_argument _ -> (Format.eprintf "%d and %d should be lesser than %d@." i j nb_v; failwith "ops" )
	  in let fold_initial initial list_acc =
	       SetteI.fold (fold_final initial) final_states list_acc
	     in
	     SetteI.fold fold_initial initial_states []
	in
	match list_res with
	    [] -> Regexp.Empty
	  | _ -> Regexp.Exp (Regexp.Union list_res)




      end

  (*  ==================================================================== *)
  (** {3 Language operations for one-dimensional automata} *)
  (*  ==================================================================== *)

  module Language = struct

    let transpose auto =
      let info = Graph.info auto in
      let dim = Array.length info.initial in
      Graph.transpose auto
	(fun vertex attrvertex ~pred ~succ -> dim-1-attrvertex)
	(fun edge attredge -> attredge)
	(fun info -> { info with
	  det = false;
	  min = false;
	  initial = Array.init dim (fun i -> info.final.(dim-1-i));
	  final = Array.init dim (fun i -> info.initial.(dim-1-i));
	})

    let check_onedim (auto:'a t) (msg:string) : unit
      =
      let info = Graph.info auto in
      let dim = Array.length info.initial in
      if dim<>1 then
	failwith (Format.sprintf "%s: requires one-dimensional automaton" msg)
      ;

    module Concat = struct
      let merge_trans (auto : 'a t) (edge:int*int) (letter:'a PL.t)
	:
	'a t
	=
	let oldletter =
	  try Graph.attredge auto edge
	  with Not_found -> A.Map.empty
	in
	let info = Graph.info auto in
	Graph.add_edge auto edge (PL.join info.lattice oldletter letter)

      let merge_final_initial
	  (auto:'a t)
	(final:SetteI.t) (initial:SetteI.t)
	:
	'a t
	=
	let info = Graph.info auto in
	(* Compute a map [successor of initial -> letter of (initial,succ) ] *)
	let mapV_A_L =
	  SetteI.fold
	    (begin fun initial map ->
	      let succ = Graph.succ auto initial in
	      SetteI.fold
		(begin fun succ map ->
		  let mapA_L = Graph.attredge auto (initial,succ) in
		  let mapV_A_L = MappeI.add succ mapA_L MappeI.empty in
		  MappeI.merge (PL.join info.lattice) map mapV_A_L
		end)
		succ map
	    end)
	    initial MappeI.empty
	in
	let nauto =
	  SetteI.fold
	    (begin fun final auto ->
	      MappeI.fold
	      (begin fun succ mapA_L auto ->
		merge_trans auto (final,succ) mapA_L
	      end)
	      mapV_A_L auto
	    end)
	    final auto
	in
	let info = Graph.info auto in
	let nauto = Graph.set_info nauto
	  { info with
	    det=info.det && Det.is_pvertex_deterministic nauto final;
	    min=false;
	  }
	in
	nauto
    end

    (* Assume one-dimensional non bottom automata and disjoint
       states *)
    let concat_one_dim (auto1:'a t) (auto2:'a t) : 'a t =
      let info1 = Graph.info auto1 in
      let info2 = Graph.info auto2 in
      let auto = Union.merge auto1 auto2 in
	let auto =
	  Concat.merge_final_initial
	    auto
	    info1.final.(0) info2.initial.(0)
	in
	let info = { (Graph.info auto) with
		       initial = [|info1.initial.(0)|];
		       final = [|
			 if (SetteI.inter info2.initial.(0) info2.final.(0))<>SetteI.empty
			 then
			   SetteI.union info1.final.(0) info2.final.(0)
			 else
			   info2.final.(0)
		       |];
		   }
	in
	let auto = Graph.set_info auto info in
	  (* remove former initial states in info2.initial that are not
	     reachable any more *)
      let auto =
	SetteI.fold
	  (begin fun initial2 auto ->
	    let pred = Graph.pred auto2 initial2 in
	    if pred=SetteI.empty then
	      del_state auto initial2
	    else
	      auto
	  end)
	  info2.initial.(0)
	  auto
      in
      auto


      let concat (auto1:'a t) (auto2:'a t) : 'a t =
	if is_bottom auto1|| is_empty auto2 0  then
	  auto1
	else if is_bottom auto2|| is_empty auto1 0  then
	  auto2
	else begin
	  let info1 = Graph.info auto1 in
	  let info2 = Graph.info auto2 in
	    if info1.counter > info2.counter (* auto1 probably bigger *)
	    then concat_one_dim auto1 (Union.auto_offset auto2 info1.counter)
	    else concat_one_dim (Union.auto_offset auto1 info2.counter) auto2

	end

    let concat_list x = listop_of_binop "LAutomaton.concat_list" concat x
    let concat_array x = arrayop_of_binop "LAutomaton.concat_array" concat x

    let plus (auto:'a t) : 'a t
      =
      check_onedim auto "LAutomaton.plus";
      if is_bottom auto || is_empty auto 0 then
	auto
      else begin
	let info = Graph.info auto in
	let auto = Concat.merge_final_initial auto info.final.(0) info.initial.(0) in
	auto
      end

    let star (auto:'a t) :'a t
      =
      let auto = plus auto in
      let info = Graph.info auto in
      Graph.set_info auto
	{ info with final = [|SetteI.union info.initial.(0) info.final.(0)|] }

    let cons_right (auto:'a t) (letter:'a PL.t)  : 'a t
      =
      check_onedim auto "LAutomaton.cons_right";
      cons_right auto 0 letter

    let deriv_right ?cond (auto:'a t) : 'a t
      =
      check_onedim auto "LAutomaton.deriv_right";
      deriv_right ?cond auto 0

    let look_right ?cond (auto : 'a t) : 'a PL.t =
      check_onedim auto "LAutomaton.look_right";
      look_right ?cond auto 0

    let cons_left (auto:'a t) (letter:'a PL.t) : 'a t
      =
      check_onedim auto "LAutomaton.cons_left";
      cons_left auto 0 letter

    let deriv_left ?cond (auto:'a t) : 'a t
      =
      check_onedim auto "LAutomaton.deriv_left";
      deriv_left ?cond auto 0

    let look_left ?cond (auto : 'a t) : 'a PL.t =
      check_onedim auto "LAutomaton.look_left";
      look_left ?cond auto 0

    let empty ~(lattice:'a PL.lattice) ~(partition:'a PL.t) = allempty ~lattice ~partition 1

    let letter ~(lattice:'a PL.lattice) ~(partition:'a PL.t) (letter:'a PL.t) : 'a t =
      cons_left (empty ~lattice ~partition) letter

    let word ~(lattice:'a PL.lattice) ~(partition:'a PL.t) (letters:'a PL.t list) : 'a t =
      List.fold_left
	(begin fun auto letter ->
	  cons_right auto letter
	end)
	(empty ~lattice ~partition) letters

    let rec of_exp ~(lattice:'a PL.lattice) ~(partition:'a PL.t) (exp:'a PL.t Regexp.exp) : 'a t
	=
      match exp with
	| Regexp.Epsilon ->
	    empty ~lattice ~partition
	| Regexp.Letter label ->
	    letter ~lattice ~partition label
	| Regexp.Union list ->
	    if list=[] then empty ~lattice ~partition
	    else
	      union_list
		(List.map (fun exp -> of_exp ~lattice ~partition exp) list)
	| Regexp.Concat list ->
	    if list=[] then empty ~lattice ~partition else
	      concat_list
		(List.map (fun exp -> of_exp ~lattice ~partition exp) list)
	| Regexp.Star exp ->
	    star (of_exp ~lattice ~partition exp)
	| Regexp.Plus exp ->
	    plus (of_exp ~lattice ~partition exp)

    let of_regexp ~(lattice:'a PL.lattice) ~(partition:'a PL.t) (regexp:'a PL.t Regexp.t) : 'a t
      =
      match regexp with
      | Regexp.Empty -> bottom ~lattice ~partition 1
      | Regexp.Exp(exp) -> of_exp ~lattice ~partition exp

  end
    
(** {3 Concatenation} *)

    module Concat = struct
      let merge_trans (auto : 'a t) (edge:int*int) (letter:'a PL.t)
	:
	'a t
	=
	let oldletter =
	  try Graph.attredge auto edge
	  with Not_found -> A.Map.empty
	in
	let info = Graph.info auto in
	Graph.add_edge auto edge (PL.join info.lattice oldletter letter)

      let merge_final_initial
	  (auto:'a t)
	(final:SetteI.t) (initial:SetteI.t)
	:
	'a t
	=
	let info = Graph.info auto in
	(* Compute a map [successor of initial -> letter of (initial,succ) ] *)
	let mapV_A_L =
	  SetteI.fold
	    (begin fun initial map ->
	      let succ = Graph.succ auto initial in
	      SetteI.fold
		(begin fun succ map ->
		  let mapA_L = Graph.attredge auto (initial,succ) in
		  let mapV_A_L = MappeI.add succ mapA_L MappeI.empty in
		  MappeI.merge (PL.join info.lattice) map mapV_A_L
		end)
		succ map
	    end)
	    initial MappeI.empty
	in
	let nauto =
	  SetteI.fold
	    (begin fun final auto ->
	      MappeI.fold
	      (begin fun succ mapA_L auto ->
		merge_trans auto (final,succ) mapA_L
	      end)
	      mapV_A_L auto
	    end)
	    final auto
	in
	let info = Graph.info auto in
	let nauto = Graph.set_info nauto
	  { info with
	    det=info.det && Det.is_pvertex_deterministic nauto final;
	    min=false;
	  }
	in
	nauto



      (* merge the two automata and change the dimension as a result
	 of the concatenation of auto1 followed by auto2.  Assume
	 different set of states.  *)


      let merge (auto1:'a t) (auto2:'a t) : 'a t = 
	let info1 = Graph.info auto1 in
	let info2 = Graph.info auto2 in
	let dim1 = Array.length info1.initial in
	let dim2 = Array.length info2.initial in
	let auto2bis = Graph.map_vertex auto2 (fun _ d ~pred ~succ -> d + dim1-1) in
	  (* change the attributes of the vertices of auto2r to add dim1-1 *)
	let auto1r = Graph.repr auto1 in
	let auto2r = Graph.repr auto2bis in

	let auto = {
	  FGraph.nodes = Mappe.Compare.addmap MappeI.Setkey.Ord.compare auto1r.FGraph.nodes auto2r.FGraph.nodes;
	  FGraph.arcs = Mappe.Compare.addmap MapII.Setkey.Ord.compare auto1r.FGraph.arcs auto2r.FGraph.arcs;
	  FGraph.info = { (auto1r.FGraph.info) with
			    det = false;
			    min = false;
			    counter = Pervasives.max
	      auto1r.FGraph.info.counter
	      auto2r.FGraph.info.counter;
			}
	}
	in
	let auto = Graph.obj auto in
	let auto =
	  merge_final_initial
	    auto
	    info1.final.(dim1-1) info2.initial.(0)
	in
	let new_initial = 
	  Array.init 
	    (dim1+dim2-1) 
	    (fun i -> 
	       if i<dim1 
	       then info1.initial.(i) 
	       else info2.initial.(i-dim1+1)
	    )
	and new_final = 
	  Array.init 
	    (dim1+dim2-1) 
	    (fun i -> 
	       if i<dim1-1 
	       then info1.final.(i)
	       else 
		 if i = (dim1-1) (* special case *)
		 then
		   begin
		     if (SetteI.inter info2.initial.(0) info2.final.(0))<>SetteI.empty
			 then
			   SetteI.union info1.final.(dim1-1) info2.final.(0)
			 else
			   info2.final.(0)

		   end
		 else 
		   info2.final.(i-dim1+1)
	    )
	in
	let info = { (Graph.info auto) with
		       det = false;
		       min = false;
		       initial = new_initial;
		       final = new_final;
		   }
	in
	let auto = Graph.set_info auto info in

	  (* remove former initial states in info2.initial that are not
	     reachable any more *)
	let auto =
	  SetteI.fold
	    (begin fun initial2 auto ->
	       let pred = Graph.pred auto2 initial2 in
		 if pred=SetteI.empty then
		   del_state auto initial2
		 else
		   auto
	     end)
	    info2.initial.(0)
	    auto
	in
	  auto

    end

    let concat (auto1:'a t) (auto2:'a t) : 'a t =
      if is_bottom auto1  then
	auto1
      else if is_bottom auto2 then
	auto2
      else begin
	let info1 = Graph.info auto1 in
	let info2 = Graph.info auto2 in	 
	  if info1.counter > info2.counter (* auto1 probably bigger *)
		then Concat.merge auto1 (Union.auto_offset auto2 info1.counter)
		else Concat.merge (Union.auto_offset auto1 info2.counter) auto2	  
	end

    let rec of_exp ~(lattice:'a PL.lattice) ~(partition:'a PL.t) (exp:'a PL.t Regexp.exp) : 'a t
	=
      match exp with
	| Regexp.Epsilon ->
	    Language.empty ~lattice ~partition
	| Regexp.Letter label ->
	    if PL.is_sep label
	    then
	      begin
		let auto = cons_right (Language.empty ~lattice ~partition) 0 label in
		let info = Graph.info auto in

		let new_initial = [|info.initial.(0); info.final.(0)|] in
		  (* sep generates a two dimensional automaton, with
		     both queues empty *)
		let ninfo = 
		  {
		    (info) with
		      initial = new_initial;
		      final = new_initial
		  }
		in
		let auto = Graph.set_info auto ninfo in
		  (* updates the attribute of the final state *)
		  assert (SetteI.cardinal info.final.(0) = 1);
		  let final_state = SetteI.choose info.final.(0) 
		  in
		    add_state auto final_state ~initial:true ~final:true 1
	      end

	    else
	      cons_right (Language.empty ~lattice ~partition) 0 label

	| Regexp.Union [] ->
	    failwith ""
	| Regexp.Union (exp::list) -> 
	    let auto = of_exp ~lattice ~partition exp in
	      List.fold_left
		(fun res exp -> 
		   let auto_exp = of_exp ~lattice ~partition exp in 
		     (* auto_check auto auto_exp "Regexp.union";*)
		     union res auto_exp
		)
		auto
		list


	| Regexp.Concat [] ->
	    failwith ""
	| Regexp.Concat (exp::list) -> 
	    let auto = of_exp ~lattice ~partition exp in
	      List.fold_left
		(fun res exp -> let auto_exp = of_exp ~lattice ~partition exp in concat res auto_exp)
		auto
		list
	| Regexp.Star exp ->
	    let auto = of_exp ~lattice ~partition exp in
	      begin
		Language.check_onedim auto "of_exp (multi dim)";
		Language.star (auto)
	      end
	| Regexp.Plus exp ->
	    let auto = of_exp ~lattice ~partition exp in
	      begin
		Language.check_onedim auto "of_exp (multi dim)";
		Language.plus (auto)
	      end


   let of_regexp ~(lattice:'a PL.lattice) ~(partition:'a PL.t) (dim:int) (regexp:'a PL.t Regexp.t) : 'a t
       =
     match regexp with
       | Regexp.Empty -> bottom ~lattice ~partition dim
       | Regexp.Exp(exp) -> 
	   let auto = of_exp ~lattice ~partition exp in
	     assert (
	       let dim_auto = Array.length (Graph.info auto).initial in
		 dim=dim_auto
	     ) ;
	     auto


(* ANCIENNE FONCTION BUGGEE
   let of_regexp ~(lattice:'a PL.lattice) ~(partition:'a PL.t) (dim:int) (regexp:'a PL.t Regexp.t) : 'a t
       =
     let rec concat_auto_regexp (prefix:'a t) (i:int) (exp:'a PL.t Regexp.exp)
	 :
	 'a t * int
	 = 
       match exp with
	 | Regexp.Epsilon ->
	     (prefix,i)
	 | Regexp.Letter label ->
	     if PL.is_sep label then begin
	       let info = Graph.info prefix in
	       let nprefix = cons_right prefix i label in
	       let ninfo = Graph.info nprefix in
	       let ninfo = { ninfo with
			       initial = Array.append info.initial [| ninfo.final.(i) |];
			       final = Array.append info.final [| ninfo.final.(i)|];
			   }
	       in
	       let nprefix = Graph.set_info nprefix ninfo in
		 assert (SetteI.cardinal ninfo.initial.(i+1) = 1);
		 let ninitial = SetteI.choose ninfo.initial.(i+1) in
		 let nprefix = Graph.add_vertex nprefix ninitial (i+1) in
		   (nprefix, i+1)
	     end
	     else
	       (cons_right prefix i label, i)
	 | Regexp.Union list ->
	     begin match list with
	       | [] -> failwith ""
	       | exp::list ->
		   let (nres,ni) = concat_auto_regexp prefix i exp in
		     List.fold_left
		       (begin fun (res,i) exp ->
			  let (nres,ni) = concat_auto_regexp prefix i exp in
			    assert (i==ni);
			    (union res nres, ni)
			end)
		       (nres,ni) list
	     end
	 | Regexp.Concat list ->
	     begin match list with
	       | [] -> failwith ""
	       | exp::list ->
		   let (res,i) = concat_auto_regexp prefix i exp in
		     List.fold_left
		       (begin fun (res,i) exp ->
			  concat_auto_regexp res i exp
			end)
		       (res,i) list
	     end
	 | Regexp.Plus _
	 | Regexp.Star _ ->
	     (* We assume that it contains no separation letter *)
	  let res = Language.of_exp ~lattice ~partition exp in
	  let res =
	    if i=0 then res
	    else begin
	      let res = Graph.map_vertex res
		(fun vertex attrvertex ~pred ~succ -> i)
	      in
	      let res = Graph.map_info res
		(begin fun info -> { info with
		  initial = Array.append (Array.create i SetteI.empty) info.initial;
		  final = Array.append (Array.create i SetteI.empty) info.final;
		}
		end)
	      in
	      res
	    end
	  in
	  let infoprefix = Graph.info prefix in
	  let res = Union.auto_offset res infoprefix.counter in
	  let infores = Graph.info res in
	  let auto = Union.merge prefix res in
	  (Graph.info auto).det <- infoprefix.det && infores.det;
	  let auto =
	    Language.Concat.merge_final_initial
	      auto
	      infoprefix.final.(i) infores.initial.(i)
	  in
	  let info = Graph.info auto in
	  info.initial.(i) <- infoprefix.initial.(i);
	  info.final.(i) <- begin
	    if (SetteI.inter infores.initial.(i) infores.final.(i))<>SetteI.empty
	    then
	      SetteI.union infoprefix.final.(i) infores.final.(i)
	    else
	      infores.final.(i)
	  end;
	  (reachability auto, i)
    in
    match regexp with
    | Regexp.Empty -> bottom ~lattice ~partition dim
    | Regexp.Exp(exp) ->
	let (res,i) =
	  concat_auto_regexp
	    (Language.empty ~lattice ~partition) 0 exp
	in
	if i<>dim-1 then failwith "";
	  check_sep res "LAutomaton.of_regexp-end";
	res

*)

  (*  ==================================================================== *)
  (** {3 is_prefix} *)
  (*  ==================================================================== *)

  let rec is_prefix_aux lattice auto set_init p_w =
    if SetteI.is_empty set_init
    then false
    else
      begin
	match p_w with
	    [] -> true
	  | p_elt::reste -> 
	      begin
		let map_succ = Det.succ_pvertex auto set_init in
		  A.Map.fold
		    (fun key abs1 res ->
		      try 
			begin
			  let (abs2,set_newinit) = A.Map.find key map_succ in
			    res||((lattice.PLattice.is_leq key abs1 abs2)&& (is_prefix_aux lattice auto set_newinit reste))
			end		      
		      with
		    Not_found -> res)
		    (* checks whether the transition is firable, and recursively calls is_prefix_aux *)
		    p_elt
		    false
	      end
      end


  let is_prefix ~projection (w : 'a list) (auto : 'b t) (i:int) : bool =
    if is_bottom auto
    then false
    else
      begin
	let info =  Graph.info auto in
	let tab_init = info.initial in
	  assert( (Array.length tab_init > i) && (i >=0));
	  let lattice = info.lattice 
	  and partition = info.partition in
	  let p_w = List.map (fun elt -> PL.project ~project:projection elt lattice partition) w in
	    (* p_w is the projection of w on the partition of auto *) 
	    is_prefix_aux lattice auto tab_init.(i) p_w 
      end

  (*  ==================================================================== *)
  (** {3 Widening} *)
  (*  ==================================================================== *)

  let widening_shape (abstract:'a t -> 'a t) (auto1:'a t) (auto2:'a t) : 'a t
      =
    auto_check auto1 auto2 "LAutomaton.widening_shape ";
    (*
      if auto1 == auto2
      then auto1
      else
      begin
    *)
    let info1 = Graph.info auto1 in
      if is_bottom auto1 || is_allempty auto1 then auto2
      else begin
	let auto1 = abstract auto1 in
	let auto2 = abstract auto2 in
	  if Graph.size auto1 <> Graph.size auto2 then
	    auto2
	  else begin
	    try
	      let shape1 = shape auto1 in
	      let shape2 = shape auto2 in
	      let tshape1 = Language.transpose shape1 in
	      let tshape2 = Language.transpose shape2 in
	      let simulation21 =
		Simulation.simulation_nondet
		  shape2 shape1
		  (Simulation.simulation_initial shape2 shape1)
	      in
	      let simulation21 =
		Simulation.simulation_nondet
		  (tshape2) (tshape1)
		  simulation21
	      in
	      let simulation =
		Simulation.simulation_nondet
		  shape1 shape2
		  (LAutomatonRep.simulation_inv simulation21)
	      in
	      let simulation =
		Simulation.simulation_nondet
		  (tshape1) (tshape2)
		  simulation
	      in
	      let corresp12 = HashheI.create 11 in
		MappeI.iter
		  (begin fun vertex1 pvertex2 ->
		     if (SetteI.cardinal pvertex2)<>1 then
		       begin
			 Format.printf 
			   "@. DEBUG: cardinal: %d @. auto1:@[%a@]@. auto2:@[%a@]@."
			   (SetteI.cardinal pvertex2)
			   dprints auto1
			   dprints auto2;
			 failwith ""
		       end;
		     assert ((SetteI.cardinal pvertex2)=1);
		     let vertex2 = SetteI.choose pvertex2 in
		       HashheI.add corresp12 vertex1 vertex2
		   end)
		  simulation
		;
		Graph.map_edge auto1
		  (begin fun (vertex1,succ1) letter1 ->
		     let vertex2 = HashheI.find corresp12 vertex1 in
		     let succ2 = HashheI.find corresp12 succ1 in
		     let letter2 = Graph.attredge auto2 (vertex2,succ2) in
		       PL.widening info1.lattice
			 ~partition:info1.partition letter1 letter2
		   end)
	    with Simulation.Abort ->
	      auto2
	  end
      end
(* end *)

 (*  ==================================================================== *)
  (** {3 Widening} *)
  (*  ==================================================================== *)

  let widening_shape2 (abstract:'a t -> 'a t) (auto1:'a t) (auto2:'a t) : 'a t
      =
    auto_check auto1 auto2 "LAutomaton.widening_shape2 ";
    assert (is_leq auto1 auto2);
    let auto1 = minimise auto1
    and auto2 = minimise auto2
    in
    let info1 = Graph.info auto1 in
      if is_bottom auto1 || is_allempty auto1 then auto2
      else 
	begin
	  if is_leq  (shape auto2) (shape auto1) 
	    (* since both shapes recognize the same language, they are the same up to an isomorphism *) 
	  then
	    (* application of the widening on each transition *)
	    begin
	      let simulation = Simulation.simulation_initial auto1 auto2 in
		(* checks that the simulation is a one-to-one relation*)
		dprints Format.std_formatter auto1;
		dprints Format.std_formatter auto2;
		assert
		  (begin
		     MappeI.is_empty (MappeI.filter (fun _ sx -> SetteI.cardinal sx <> 1) simulation)
		   end)
		  ;

		Graph.map_edge
		  auto1
		  (begin
		     fun (x1,y1) lbl1 ->
		       let x2 = 
			 SetteI.choose 
			   (try MappeI.find x1 simulation with Not_found -> failwith "widening_shape2, bug1 ")
		       and y2 = 
			 SetteI.choose 
			   (try MappeI.find y1 simulation with Not_found -> failwith "widening_shape2, bug2 ")
		       in 
		       let lbl2 = 
			 try Graph.attredge auto2 (x2,y2) with Not_found -> failwith "widening_shape2, bug3 "
		       in
			 PL.widening info1.lattice info1.partition lbl1 lbl2
		   end)		  
	    end
	  else
	    (* *)
	    abstract auto2
	end





  module Abstract = struct

    let partition_std ?(initial=true) ?(final=true) (auto:'a t) =
      let info = Graph.info auto in
      let map =
	let auto = Graph.repr auto in
	MappeI.obj
	  (Mappe.mapi
	    (begin fun vertex node ->
	      let i = node.FGraph.attrvertex in
		match (initial, final) with
		    (false,false) -> i
		  | (true,false) -> 2*i + (if SetteI.mem vertex info.initial.(i) then 1 else 0)
		  | (false,true) -> 2*i + (if SetteI.mem vertex info.final.(i) then 1 else 0)
		  | (true,true) -> 4*i +
		      begin
			match (SetteI.mem vertex info.initial.(i),SetteI.mem vertex info.final.(i)) with
			    (false,false) -> 0
			  | (true,false) -> 1
			  | (false,true) -> 2
			  | (true,true) -> 3
		      end
	    end)
	    auto.FGraph.nodes)
      in
      partition_of_map map

    let partition_refine
      ?(forward=true)
      ?(backward=true)
      (auto:'a t)
      (partition:partition)
      :
      partition
      =
      let letters_after_vertex (vertex:int) : A.Set.t
	=
	let succ = Graph.succ auto vertex in
	SetteI.fold
	  (begin fun succ res ->
	    let mapA_L = Graph.attredge auto (vertex,succ) in
	    let setA = A.Map.maptoset mapA_L in
	    A.Set.union res setA
	  end)
	  succ
	  A.Set.empty
      in

      let letters_before_vertex (vertex:int) : A.Set.t
	=
	let pred = Graph.pred auto vertex in
	SetteI.fold
	  (begin fun pred res ->
	    let mapA_L = Graph.attredge auto (pred,vertex) in
	    let setA = A.Map.maptoset mapA_L in
	    A.Set.union res setA
	  end)
	  pred
	  A.Set.empty
      in
      let newclasses = ref MappeI.empty in
      let counter = ref(-1) in
      let list = ref [] in

      MappeI.iter
	(begin fun id pvertex ->
	  list := [];
	  SetteI.iter
	    (begin fun vertex ->
	      let fletters =
		if forward then
		  A.Set.elements (letters_after_vertex vertex)
		else
		  []
	      in
	      let bletters =
		if backward then
		  A.Set.elements (letters_before_vertex vertex)
		else
		  []
	      in
	      try
		let rset = List.assoc (fletters,bletters) !list in
		rset := SetteI.add vertex !rset
	      with Not_found ->
		list := ((fletters,bletters), ref (SetteI.singleton vertex)) :: !list
	    end)
	    pvertex
	  ;
	  List.iter
	    (begin fun (letters,rset) ->
	      incr counter;
	      newclasses := MappeI.add !counter !rset !newclasses
	    end)
	    !list
	end)
	partition.classes
      ;
      partition_of_classes !newclasses

    let standard ?(initial=true) ?(final=true) ~(forward:int) ~(backward:int) (auto:'a t) =
      check_sep auto "LAutomaton.standard";
      if is_bottom auto 
      then auto 
      else 
	begin
	  let partition = partition_std ~initial:initial ~final:final auto in
	  let partition = bisimulation_shape ~depth:forward auto partition in
	    check_sep auto "LAutomaton.standard-partition";
	  let partition = 
	    if backward>0 
	    then bisimulation_shape ~depth:backward (Language.transpose auto) partition 
	    else partition 
	  in
	  let auto = quotient auto partition in
	    (*assert (is_deterministic ~check:true auto);*)
	    check_sep auto "LAutomaton.standard-end";
	    auto
	end

   

    let init_partition_bounded 
	?(initial=true) 
	?(final=true) 
	?(local=true)
	(auto:'a t) 
	: (int,(PL.A.t list Sette.t) * (PL.A.t list Sette.t) ) Mappe.t =
      (* constructs a map state -> (init language forward, init language backward) 
	 each language is either empty or contains the word epsilon, represented by an empty list:

	 - init forward language : empty unless [final]=true and the state is a final state
	 - init backward language : empty unless [initial]=true and the state is an initial state

	 the term "local" means that languages are computed for each
	 queue. If [local]=true, initial and final states are defined
	 for each queue, according to the info of [auto]. Otherwise
	 the only initial states are the ones of the first queue, and
	 the only initial states are the ones of the last queue.  
      *)
      let info = Graph.info auto in
      let dim = Array.length info.initial in

      let f_vertex x i ~pred ~succ res_map =
	(* computes both languages for state x, and add this information in res_map *)
	let init_lang_fwd =
	  if final 
	    && 
	    (
	      if local then
		SetteI.mem x info.final.(i)
	      else
		SetteI.mem x info.final.(dim-1)
	    )
	  then
	    Sette.singleton []
	  else
	    Sette.empty

	and init_lang_bwd =
	  if initial 
	    && 
	    (
	      if local then
		SetteI.mem x info.initial.(i)
	      else
		SetteI.mem x info.initial.(0)
	    )
	  then
	    Sette.singleton []
	  else
	    Sette.empty

	in
	  Mappe.add x (init_lang_fwd,init_lang_bwd) res_map

      (* applying f_vertex to each vertex of [auto] *)
      in
	Graph.fold_vertex auto Mappe.empty f_vertex



    let refine_partition_bounded
	?(local=true)
	~(forward:int) 
	~(backward:int) 
	(auto:'a t) 
	(mappe_init:  (int,(PL.A.t list Sette.t) * (PL.A.t list Sette.t) ) Mappe.t)
	: 
	(int,(PL.A.t list Sette.t) * (PL.A.t list Sette.t) ) Mappe.t
	=
      (* constructs a map state -> (bounded language forward, bounded language backward) 
	 each language is a set of words, a word being a list 
	 - forward language : words are stored in the proper order
	 - backward language : words are stored in the reversed order
	 
	 forward (resp. backward) language is bounded by the lengh
	 [forward] (resp. [backward]).

	 the term "local" means that languages are computed for each
	 queue if [local]=true, i.e. # transitions are ignored, and
	 globally otherwise.  *)

      let map_res = ref mappe_init in

      let f_iter_vertex old_map k_fwd k_bwd x i ~pred ~succ =
	let (old_set_forward,old_set_backward) = 
	  try Mappe.find x old_map
	  with Not_found -> failwith "bug refine_partition_bounded, bug 1"
	in

	  (* forward part *)
	  begin
	    let f_iter_pred_fwd px =
	      let lbl = Graph.attredge auto (px,x) in
		if local && (PL.is_sep lbl)
		then 
		  ()
		else 
		  (begin
		     let (new_set_forward_px,new_set_backward_px) = 
		       try Mappe.find px !map_res 
		       with Not_found -> failwith "bug refine_partition_bounded, bug2"
		     in 		     
		     let new_set_forward_px = 
		       PL.fold 
			 (begin 
			    fun letter _ res_set -> 
			      Sette.union 
				(Sette.fold 
				   (fun word set -> 
				      Sette.add (letter::word) set) 
				   old_set_forward 
				   Sette.empty
				)
				res_set
				(* adds all words l.w where l is in lbl and w is in old_set_fwd *)
			  end) 
			 lbl 
			 new_set_forward_px
		     in		     
		       map_res := Mappe.add px (new_set_forward_px,new_set_backward_px) !map_res 
		   end)
	    in
	      if k_fwd > 0
	      then
		SetteI.iter f_iter_pred_fwd pred
		  
	  end ;

	  (* backward part *)
	  begin
	    let f_iter_succ_bwd sx =
	      let lbl = Graph.attredge auto (x,sx) in
		if local && (PL.is_sep lbl)
		then 
		  ()
		else 
		  (begin
		     let (new_set_forward_sx,new_set_backward_sx) = 
		       try Mappe.find sx !map_res 
		       with Not_found -> failwith "bug refine_partition_bounded,bug3"
		     in 
		     let new_set_backward_sx = 
		       PL.fold 
			 (begin 
			    fun letter _ res_set -> 
			      Sette.union 
				(Sette.fold (fun word set -> Sette.add (letter::word) set) old_set_backward Sette.empty)
				res_set
				(* adds all words l.w where l is in lbl and w is in old_set_bwd*)
			  end) 
			 lbl 
			 new_set_backward_sx
		     in		     
		       map_res := Mappe.add sx (new_set_forward_sx,new_set_backward_sx) !map_res 
		   end)
	    in
	      if k_bwd > 0
	      then
		SetteI.iter f_iter_succ_bwd succ
	  end
	    
      in 

      (* refinement *)
      let rec f_refine map forward backward =
	if (forward >0) || (backward >0)
	then
	  begin
	    Graph.iter_vertex auto (f_iter_vertex map forward backward);
	    f_refine !map_res (max (forward-1) 0) (max (backward-1) 0)
	  end
	else
	  !map_res
      in
	f_refine mappe_init forward backward



    let construct_partition_bounded
	?(initial=true) 
	?(final=true) 
	~(forward:int) 
	~(backward:int) 
	(auto:'a t) 	
	=
      let mappe_init_local =  
	init_partition_bounded 
	~initial:initial 
	~final:final
	~local:true
	auto
      in
      let mappe_lang_local =
	refine_partition_bounded
	~local:true
	~forward 
	~backward 
	auto 
	mappe_init_local
      in 

      let mappe_init_global =  
	init_partition_bounded 
	~initial:initial 
	~final:final
	~local:false
	auto
      in
      let mappe_lang_global =
	refine_partition_bounded
	~local:false
	~forward 
	~backward 
	auto 
	mappe_init_global
      in 


	(* construction of the partition  *)
      let cpt_classes = ref (-1) in
      let map_classes = ref Mappe.empty in
	(* map classes : nbq,lang_fwd_loc,lang_bwd_loc,lang_fwd_glob,lang_bwd_glob -> nb_class *)
      let map_partition = ref MappeI.empty in
	(* map partition : vertex -> nb_class *)

      let f_iter_vertex x i ~pred ~succ =
	let (lang_fwd_loc,lang_bwd_loc) = 
	  try Mappe.find x mappe_lang_local
	  with Not_found -> failwith "bug construct_partition_bounded,bug1"
	and (lang_fwd_glob,lang_bwd_glob) = 
	  try Mappe.find x mappe_lang_global
	  with Not_found -> failwith "bug construct_partition_bounded,bug2"
	in
	let class_x =
	 try Mappe.find (i,lang_fwd_loc,lang_bwd_loc,lang_fwd_glob,lang_bwd_glob) !map_classes 
	 with
	     Not_found ->
	    (* if the class has not any number yet, then add one *)
	       begin
		 incr cpt_classes;
		 map_classes := 
		   Mappe.add 
		     (i,lang_fwd_loc,lang_bwd_loc,lang_fwd_glob,lang_bwd_glob) 
		     !cpt_classes 
		     !map_classes
		 ;
		 !cpt_classes
	       end
	in
	   map_partition := MappeI.add x class_x  !map_partition

      in
	Graph.iter_vertex auto f_iter_vertex;
	  
	partition_of_map !map_partition

	(*    construction of the partition - end *)



    let bounded_languages 
	?(initial=true) 
	?(final=true) 
	~(forward:int) 
	~(backward:int) 
	(auto:'a t) = 
      check_sep auto "LAutomaton.bounded_languages";
      if is_bottom auto 
      then auto 
      else 
	begin
	  let partition = construct_partition_bounded ~initial:initial ~final:final ~forward ~backward auto in
(*
	    Format.printf 
	      "****************@. auto:@[%a@] @.partition:@[%a@] @.****************@."
	      dprints
	      auto
	      partition_print
	      partition
	    ;
*)
	  let auto = quotient auto partition in
	   (* assert (is_deterministic ~check:true auto);*)
	    check_sep auto "LAutomaton.bounded_languages-end";
	    auto

	end

  end
    
end
