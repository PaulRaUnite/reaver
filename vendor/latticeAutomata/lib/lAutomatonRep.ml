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

open Format 
(* point of the input program *)
type point = {
  line : int;
  col : int;
  char : int;
}
let print_point fmt (point:point)
  =
  fprintf fmt "(L%i C%i)"
    point.line point.col


type ('a,'b) info = {
  lattice : 'a; (* underlying lattice *)
  partition : 'b; (* partitionned lattice *)
  initial : SetteI.t array; (* sets of initial states *)
  final : SetteI.t array; (* sets of final states *)
  mutable det : bool;(* if det, then the automaton is deterministic *)
  mutable min : bool; (* if min, then the automaton is minimal *)
  mutable counter : int; (* next fresh value to generate a new state *)
}

let print_info
  (print_letter: Format.formatter -> 'b -> unit)
  fmt (info:('a,'b) info)
  =
  Format.fprintf fmt
    "{ @[<v>initial = %a;@ final = %a;@ det=%b; min=%b; counter=%i;@ partition=%a;@] }"
    (Print.array (SetteI.print Format.pp_print_int)) info.initial
    (Print.array (SetteI.print Format.pp_print_int)) info.final
    info.det info.min info.counter
    print_letter info.partition

let copy_info info = { info with
  initial = Array.copy info.initial;
  final = Array.copy info.final
}

(* info on powerset automata *)
type pinfo = {
  pinitial : SetteI.t array; (* sets of initial states *)
  pfinal : SetteI.t array; (* sets of final states *)
  hashPI : (SetteI.t,int) Hashhe.t; (* has table associating a set of satet to a number *)
  mutable pcounter : int; (* next fresh value to generate a new state *)
}

(* Return an empty pinfo *)
let make_pinfo dim =  {
  pinitial = Array.create dim SetteI.empty;
  pfinal = Array.create dim SetteI.empty;
  hashPI = Hashhe.create 7;
  pcounter = 0
}

(*  ==================================================================== *)
(** {3 Partitions of states} *)
(*  ==================================================================== *)

(** A partition of states is represented either by a map [vertex ->
  classid(vertex)] associating states with an equivalence class identifier,
  or by a set of equivalence classes defined by a map [classid -> SetteI.t].

  WARNING: except its name, it has nothing common with the partitionned lattice 
*)

type partition = {
  mutable map : int MappeI.t; (*  map [vertex ->  classid(vertex)] *)
  mutable classes : SetteI.t MappeI.t; (* map [classid -> SetteI.t] *)
}

(** creates a partition from a map [vertex ->  classid(vertex)] *)
let partition_of_map (map:int MappeI.t) : partition
  =
  let mapIdSet =
    MappeI.fold
      (begin fun vertex id (mapIdSet:(SetteI.t ref) MappeI.t) ->
	try
	  let rset = MappeI.find id mapIdSet in
	  rset := SetteI.add vertex !rset;
	  mapIdSet
	with Not_found ->
	  let rset = ref (SetteI.singleton vertex) in
	  MappeI.add id rset mapIdSet
      end)
      map
      MappeI.empty
  in
  {
    map = map;
    classes = MappeI.map (fun x -> !x) mapIdSet;
  }

(** creates a partition from a map [classid -> SetteI.t] *)
let partition_of_classes (classes:SetteI.t MappeI.t) : partition
  =
  let map =
    MappeI.fold
      (begin fun id set map ->
	SetteI.fold
	(begin fun vertex map ->
	  MappeI.add vertex id map
	end)
	set
	map
      end)
      classes
      MappeI.empty
  in
  {
    map = map;
    classes = classes;
  }

let partition_print fmt partition =
  fprintf fmt "{ @[<v>classes = %a;@ map = %a@] }"
    (MappeI.print
      pp_print_int
      (SetteI.print pp_print_int))
    partition.classes
    (MappeI.print
      ~first:"@["
      pp_print_int
      pp_print_int)
    partition.map

(*  ==================================================================== *)
(** {3 Simulation relations} *)
(*  ==================================================================== *)

(** a relation xRy on the set of integers (states identifiers) is represented
    by a map [x -> setY] with setY = {y | xRy} *)

type simulation = SetteI.t MappeI.t

let simulation_print fmt simulation =
  MappeI.print
    pp_print_int
    (SetteI.print pp_print_int)
    fmt
    simulation

(** [simulate R x y] returns true if xRy *)
let simulate 
    (simulation:SetteI.t MappeI.t)
    (vertex1:int)
    (vertex2:int) 
    :
    bool
    =
  let pvertex2 = 
    try MappeI.find vertex1 simulation
    with Not_found -> SetteI.empty
  in
    SetteI.mem vertex2 pvertex2
      
(** [simulation_inv R] computes the relation R^-1 : x R y <=> y R^-1 x *)
let simulation_inv
    (simulation:SetteI.t MappeI.t)
    :
    SetteI.t MappeI.t
    =
  let res = ref MappeI.empty in
    MappeI.iter
      (begin fun vertex1 pvertex2 ->
	SetteI.iter
	  (begin fun vertex2 ->
	    try
	      let pvertex1 = MappeI.find vertex2 !res in
		pvertex1 := SetteI.add vertex1 !pvertex1
	    with Not_found ->
	      res := MappeI.add vertex2 (ref (SetteI.singleton vertex1)) !res
	    end)
	  pvertex2
	end)
      simulation
    ;
    MappeI.map (fun x -> !x) !res

(*  ********************************************************************** *)
(** {2 Modules} *)
(*  ********************************************************************** *)

module SetII = Sette.Make(struct
  type t=int*int
  let compare (x1,x2) (y1,y2) =
    if x1<y1 then -1
    else if x1>y1 then 1
    else if x2<y2 then -1
    else if x2>y2 then 1
    else 0
end)
module MapII = Mappe.Make(SetII)
module HashII = Hashhe.Make(struct
  type t = int*int
  let equal (x1,y1) (x2,y2) = x1==x2 && y1==y2
  let hash (x,y) = x + 11*y
end)
module Graph = FGraph.Make(struct
  module MapV = MappeI
  module MapE = MapII
end)

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
  val mapV_A_L_of_mapA_LxV_list : ('a * int) list A.Map.t -> 'a A.Map.t MappeI.t
    (** Change of representation of functions *)

  val check_sep :  'a t -> string -> unit
    (** [check_sep auto message] checks that:
      - x is in initial.(i) iff there is a transition y--sep-> x or i=0
      - x is in final.(i) iff there is a transition x--sep-> y or i=dim(auto)
      and raises [Failure message] if not. 
*)

  val auto_check : 'a t -> 'a t -> string -> unit
    (** [auto_check auto1 auto2 message] checks that:
      - check_sep the two automata returns true
      - the array of initial/final states are of same length
      - the two partitions are the same
      and raises [Failure message] if not. 
	
*)

  val add_state :
    'a t -> int -> ?initial:bool -> ?final:bool -> int -> 'a t
  val del_state : 'a t -> int -> 'a t

  val restore_sep_consistance : 'a t -> 'a t
    (* rebuild the information about initial and final states
       according to the transitions. Recomandation: do not use it *)

  val reachability : 'a t -> 'a t
  val coreachability : 'a t -> 'a t

  val is_trim : 'a t -> bool
    (** [is_trim auto] checks that all states of auto are reachable
	from the initial states and coreachable from the final states *)

  (*  ==================================================================== *)
  (** {3 Printing} *)
  (*  ==================================================================== *)

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

  (*  ==================================================================== *)
  (** {3 Constructor } *)
  (*  ==================================================================== *)

  val bottom : lattice:'a PL.lattice -> partition:'a PL.t -> int -> 'a t
  val top    : lattice:'a PL.lattice -> partition:'a PL.t -> int -> 'a t
  val allempty  : lattice:'a PL.lattice -> partition:'a PL.t -> int -> 'a t

  (*  ==================================================================== *)
  (** {3 Tests} *)
  (*  ==================================================================== *)

  val is_bottom : 'a t -> bool
  val is_top    : 'a t -> bool
  val is_empty  : 'a t -> int -> bool
  val is_allempty  : 'a t -> bool

  val is_deterministic : ?check:bool -> 'a t -> bool
  val is_minimal : 'a t -> bool

  val shape : 'a t -> shape

  val dimension : 'a t -> int

  val nb_states : 'a t -> int

  (*  ==================================================================== *)
  (** {3 Module Powerset: powerset automata} *)
  (*  ==================================================================== *)

  module Powerset : sig
    type 'a pauto = (int, 'a PL.t, pinfo) Graph.t
      (* Powerset automaton *)
    val add_pvertex : 'a t -> 'a pauto ref -> ?id:int -> SetteI.t -> int
      (* Add the pvertex to [pauto] and returns its index.
	 If id=None, use pinfo.counter as new id;
	 If id=Some(id), use id
	 Return the id.
      *)
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
      (* Return a map
	 [possible letter after a state of the set in auto ->
	 (merge of the associated lattice elements,
	 set of successor states)
      *)
    val determinise : 'a t -> 'a Powerset.pauto
    val determinise_partial : 'a t -> 'a Powerset.pauto
  end

  val determinise : 'a t -> 'a t
  val determinise_partial : 'a t -> 'a t

  (*  ==================================================================== *)
  (** {3 Bisimulation} *)
  (*  ==================================================================== *)

  (** Assumes deterministic automata. *)
  module Bisim : sig
    val letters_after_pvertex : 'a t -> SetteI.t -> A.Set.t
      (* Returns the set of outgoing letters of a pvertex *)
    val succ_pvertex_by_letter :
      'a t -> partition -> SetteI.t -> A.t ->
      SetteI.t ref MappeI.t * SetteI.t
      (* [succ_pvertex auto partition pvertex letter]
	 Returns a map
	 [identifier of a successor equivalence class by the letter ->
	 set of vertices in pvertex leading to classid with this letter]
	 and the set of vertices that cannot move by this letter.
      *)
  end
  val bisimulation_shape : ?depth:int -> 'a t -> partition -> partition

  (*  ==================================================================== *)
  (** {3 Simulation} *)
  (*  ==================================================================== *)

  (** Assumes deterministic automata *)
  module Simulation : sig
    val succ_vertex_det : 'a t -> int ->  ('a * int) A.Map.t
      (** Associates with a vertex a map
	[outgoing letter ->
	 associated lattice element and successor vertex]
      *)
    val simulate_initial :
      ((A.t -> 'a*int -> 'a*int -> bool) ->
       ('a*int) A.Map.t -> ('a*int) A.Map.t -> bool) ->
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
    (** Inclusion and equality checks.
      Assumes deterministic automata. *)

  (*  ==================================================================== *)
  (** {3 Quotient} *)
  (*  ==================================================================== *)

  module Quotient : sig
    val succ_pvertex : 'a t -> int MappeI.t -> SetteI.t -> 'a MapAV.t
      (* Return a map
	 [(possible letter after a state in pvertex in auto,
	   identifier of a successor equivalence class) ->
	  merge of the associated lattice elements]
      *)
    val quotient : 'a t -> partition -> 'a Powerset.pauto
  end
  val quotient : 'a t -> partition -> 'a t
    (** Compute the quotient of an automaton by a partition of states *)

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

module Make
  (PL : PLattice.S)
= struct

  module A = PL.A
  module PL = PL
  module SetV = SetteI
  module MapV = MappeI

  module SetAV = Sette.Make(struct
    type t = A.t * int
    let compare (x1,y1) (x2,y2) =
      let res =  SetteI.Ord.compare y1 y2 in
      if res<>0 then res
      else A.Set.Ord.compare x1 x2
  end)
  module MapAV = Mappe.Make(SetAV)

  type 'a t = (int, 'a PL.t, ('a PL.lattice, 'a PL.t) info) Graph.t
  type shape = unit t
    (** Invariant: states between operations
      are supposed to be reachable and coreachable *)

  let (lunit:'a PL.lattice) = {
    PLattice.sep = ();
    PLattice.is_bottom = begin fun letter () -> false end;
    PLattice.is_leq = begin fun letter () () -> true end;
    PLattice.is_eq = begin fun letter () () -> true end;
    PLattice.join = begin fun letter () () -> () end;
    PLattice.meet = begin fun letter () () -> () end;
    PLattice.widening = begin fun letter () () -> () end;
  }

  (*  ==================================================================== *)
  (** {3 Printing} *)
  (*  ==================================================================== *)

  let print
    (print_letter :  Format.formatter -> 'a PL.t -> unit)
    fmt
    (auto:'a t)
    =
    Graph.print
      Format.pp_print_int
      Format.pp_print_int
      print_letter
      (print_info print_letter)
      fmt auto

  let print_dot
    ?(margin=40)
    ?(titlestyle:string="shape=ellipse,style=bold,style=filled,fontsize=20")
    ?(vertexstyle:string="shape=ellipse,fontsize=12")
    ?(edgestyle:string="fontsize=12")
    ?(title:string="")
    (print_letter: Format.formatter -> 'a PL.t -> unit)
    fmt
    (auto:'a t)
    =
    let info = Graph.info auto in

    let print_vertex fmt vertex i =
      let s1 = if SetV.mem vertex info.initial.(i) then "I" else "" in
      let s2 = if SetV.mem vertex info.final.(i) then "F" else "" in
      let s = (string_of_int vertex)^"  "^(string_of_int i)^s1^s2 in
      Format.pp_print_string fmt s
    in
    let print_edge fmt edge attredge =
      ignore (Format.flush_str_formatter ());
      fprintf Format.str_formatter "@[<v>%a@]" print_letter attredge;
      Format.pp_print_string fmt (Print.escaped ~linebreak:'l' (Format.flush_str_formatter ()))
    in

    let oldmargin = Format.pp_get_margin  Format.str_formatter () in
    Format.pp_set_margin  Format.str_formatter margin;
    Graph.print_dot
      ~titlestyle
      ~vertexstyle
      ~edgestyle
      ~title:title
      Format.pp_print_int
      print_vertex
      print_edge
      fmt auto
    ;
    Format.pp_print_flush fmt ();
    Format.pp_set_margin  Format.str_formatter oldmargin;
    ()

  (*  ==================================================================== *)
  (** {3 Constructor } *)
  (*  ==================================================================== *)

  let bottom ~(lattice:'a PL.lattice) ~(partition:'a PL.t) (dim:int) : 'a t
    =
    let info = {
      lattice = lattice;
      partition = partition;
      initial = Array.create dim SetteI.empty ;
      final = Array.create dim SetteI.empty;
      det = true ;
      min = true ;
      counter = 0;
    }
    in
    Graph.empty info

  let top ~(lattice:'a PL.lattice) ~(partition:'a PL.t) (dim:int) : 'a t
    =
    let t = Array.init dim (fun i -> SetteI.singleton i) in
    let info = {
      lattice = lattice;
      partition = partition;
      initial = t;
      final = Array.copy t;
      det = true;
      min = true;
      counter = dim;
    }
    in
    let g = ref (Graph.empty info) in
    let sep = PL.sep lattice in
    let partition = PL.A.Map.remove PL.A.sep partition in
    for i=0 to dim-1 do
      g := Graph.add_vertex !g i i;
      g := Graph.add_edge !g (i,i) partition;
      if i>=1 then
	g := Graph.add_edge !g (i-1,i) sep;
    done;
    !g

  let allempty ~(lattice:'a PL.lattice) ~(partition:'a PL.t) (dim:int) : 'a t
    =
    let t = Array.init dim (fun i -> SetteI.singleton i) in
    let info = {
      lattice = lattice;
      partition = partition;
      initial = t;
      final = Array.copy t;
      det = true;
      min = true;
      counter = dim;
    }
    in
    let g = ref (Graph.empty info) in
    let sep = PL.sep lattice in
    for i=0 to dim-1 do
      g := Graph.add_vertex !g i i;
      if i>=1 then
	g := Graph.add_edge !g (i-1,i) sep;
    done;
    !g

  (*  ==================================================================== *)
  (** {3 Tests} *)
  (*  ==================================================================== *)

  let is_bottom (auto:'a t) : bool
    =
    let info = Graph.info auto in
    info.initial.(0)=SetteI.empty

  let is_empty (auto:'a t) (i:int) : bool
    =
    if is_bottom auto then
      false
    else
      let info = Graph.info auto in
      SetteI.equal info.initial.(i) info.final.(i) &&
      begin
	try
	  SetteI.iter
	    (begin fun initial ->
	      let succ = Graph.succ auto initial in
	      SetteI.iter
		(begin fun succ ->
		  let attredge = Graph.attredge auto (initial,succ) in
		  if not (PL.is_sep attredge) then raise Exit;
		end)
		succ
	    end)
	    info.initial.(i)
	  ;
	  true
	with Exit ->
	  false
      end

  let is_allempty auto =
    let info = Graph.info auto in
    let dim = Array.length info.initial in
    try
      for i=0 to dim - 1 do
	if not (is_empty auto i) then raise Exit
      done;
      true
    with Exit ->
      false

  let is_top auto =
    let info = Graph.info auto in
    if info.min then
      try
	if is_bottom auto then raise Exit;
	let dim = Array.length info.initial in
	if SetteI.cardinal (Graph.vertices auto) <> dim then raise Exit;
	for i=0 to dim-1 do
	  let vertex = SetteI.choose info.initial.(i) in
	  try
	    let letter = Graph.attredge auto (vertex,vertex) in
	    if not (PL.is_eq info.lattice letter info.partition) then raise Exit
	  with Not_found ->
	    raise Exit
	done;
	true
      with Exit ->
	false
    else
      failwith "LAutomaton.is_top: require normalized automaton"

  let is_minimal (auto:'a t) : bool =
    (Graph.info auto).min

  (*  ==================================================================== *)
  (** {3 Shape automata} *)
  (*  ==================================================================== *)

  let shape (auto:'a t) : shape  =
    let shape = Graph.map_edge auto
      (begin fun edge pelt ->
	A.Map.map (fun elt -> ()) pelt
      end)
    in
    let shape = Graph.map_info shape
      (begin fun info -> { info with
	lattice = lunit;
	partition = A.Map.map (fun _ -> ()) info.partition;
      }
      end)
    in
    shape

  let dimension (auto:'a t) : int =
    let info = Graph.info auto in
    Array.length info.initial

  let nb_states (auto:'a t) : int =
    Graph.fold_vertex auto 0 (fun _ _ ~pred ~succ n -> n+1)


  (*  ==================================================================== *)
  (** {3 Utility functions} *)
  (*  ==================================================================== *)

  (*  -------------------------------------------------------------------- *)
  (** {4 Transformation of maps} *)
  (*  -------------------------------------------------------------------- *)

  (* A name [mapX_Y_Z] denotes a map [X -> (Y -> Z)], and
     [XxY] denotes the cartesian product of types [X] and [Y].
     In such names:
     - [V] denotes vertices
     - [P] (for powerset) sets of vertices
     - [A] denotes letters (alphabet)
     - [L] denotes lattice elements
  *)

  let mapV_A_L_of_mapAV_L (mapAV_L:'a MapAV.t) : 'a A.Map.t MappeI.t
    =
    let (mapV_A_L:('a A.Map.t ref) MappeI.t) =
      MapAV.fold
	(begin fun (letter,vertex) elt mapV_A_L ->
	  try
	    let rmapA_L = MappeI.find vertex mapV_A_L in
	    rmapA_L := A.Map.add letter elt !rmapA_L;
	    mapV_A_L
	  with Not_found ->
	    let mapA_L = A.Map.add letter elt A.Map.empty in
	    MappeI.add vertex (ref mapA_L) mapV_A_L
	end)
	mapAV_L
	MappeI.empty
    in
    MappeI.map (fun x -> !x) mapV_A_L

  let mapV_A_L_of_mapA_LxV (mapA_LxV:('a*int) A.Map.t) : 'a A.Map.t MappeI.t
    =
    let (mapV_A_L:('a A.Map.t ref) MappeI.t) =
      A.Map.fold
	(begin fun letter (elt,vertex) mapV_A_L ->
	  try
	    let rmapA_L = MappeI.find vertex mapV_A_L in
	    rmapA_L := A.Map.add letter elt !rmapA_L;
	    mapV_A_L
	  with Not_found ->
	    let mapA_L = A.Map.add letter elt A.Map.empty in
	    MappeI.add vertex (ref mapA_L) mapV_A_L
	end)
	mapA_LxV
	MappeI.empty
    in
    MappeI.map (fun x -> !x) mapV_A_L

  let mapV_A_L_of_mapA_LxV_list (mapA_LxV_list:('a*int) list A.Map.t) : 'a A.Map.t MappeI.t
    =
    let (mapV_A_L:('a A.Map.t ref) MappeI.t) =
      A.Map.fold
	(begin fun letter leltvertex mapV_A_L ->
	  List.fold_left
	  (begin fun mapV_A_L (elt,vertex) ->
	    try
	      let rmapA_L = MappeI.find vertex mapV_A_L in
	      rmapA_L := A.Map.add letter elt !rmapA_L;
	      mapV_A_L
	    with Not_found ->
	      let mapA_L = A.Map.add letter elt A.Map.empty in
	      MappeI.add vertex (ref mapA_L) mapV_A_L
	  end)
	  mapV_A_L leltvertex 
	end)
	mapA_LxV_list
	MappeI.empty
    in
    MappeI.map (fun x -> !x) mapV_A_L
      
  (*  -------------------------------------------------------------------- *)
  (** {4 Check compatibility} *)
  (*  -------------------------------------------------------------------- *)


  let check_sep auto msg  =
(* EFFECTIVE VERSION *)
  let info =  Graph.info auto in
  let dim = Array.length info.initial in
    assert (Array.length info.initial=Array.length info.final);

    let f_fold_vertex x nx ~pred ~succ b =
      b &&
	begin
	  let nb_sep = ref 0 in
	  let initx = ref false and finalx = ref false in
	    SetteI.iter 
	      (fun sx -> 
		 if PL.is_sep (Graph.attredge auto (x,sx))
		 then 
		   begin
		     nb_sep := !nb_sep+1;
		     assert( (Graph.attrvertex auto x) +1 = Graph.attrvertex auto sx); 
		     finalx := true
		   end
	      )
	      succ;
	    SetteI.iter 
	      (fun px -> 
		 if PL.is_sep (Graph.attredge auto (px,x)) 
		 then 
		   begin 
		     assert( (Graph.attrvertex auto px) +1 = Graph.attrvertex auto x); 
		     initx := true
		   end
	      )
	      pred;
	    ((nx = 0) || (!initx = SetteI.mem x info.initial.(nx))) 
	    &&
	      ((nx = dim-1) || (!finalx = SetteI.mem x info.final.(nx)))
	   (* &&
	      (!nb_sep <2) *)
	end
    in
      if (is_bottom auto)||
	(Graph.fold_vertex
	   auto
	   true
	   f_fold_vertex)
      then
	() (* auto is OK *)
      else
	failwith (msg^" (sep error)")






 (*
 OLD VERSION
      let f_iter_edges (x,y) lbl =
	let nx = Graph.attrvertex auto x
	and ny = Graph.attrvertex auto y
	in
	if PL.is_sep lbl then
	  begin
	    assert (nx+1 = ny);
	    assert (SetteI.mem x info.final.(nx)) ;
	    assert (SetteI.mem y info.initial.(ny)) ;
	  end
	else
	  (* the two vertices must have the same attribut *)
	  assert (nx = ny)
      in
	Graph.iter_edge auto f_iter_edges ;

	let f_iter_sette nx x =
	  assert (nx = Graph.attrvertex auto x)
	in
	  for i = 0 to dim-1 do
	    SetteI.iter (f_iter_sette i) info.initial.(i) ;
	    SetteI.iter (f_iter_sette i) info.final.(i) ;
	  done;
 *)

	

  let auto_check auto1 auto2 msg =
    let info1 = Graph.info auto1 in
    let info2 = Graph.info auto2 in
    if not (info1.lattice==info2.lattice) then
      failwith (Format.sprintf "%s: the two decision automata have different lattice managers" msg)
    ;
    let dim1 = Array.length info1.initial in
    let dim2 = Array.length info2.initial in
    if dim1<>dim2 then
      failwith (Format.sprintf "%s: the two decision automata have different dimensions %i and %i" msg dim1 dim2)
    ;
    if not (PL.is_eq info1.lattice info1.partition info2.partition)
    then
      failwith (Format.sprintf "%s: the two lattice automata are defined on different partitions" msg)
    ;
    check_sep auto1 msg ;
    check_sep auto2 msg ;
    ()

  (*  -------------------------------------------------------------------- *)
  (** {4 Others} *)
  (*  -------------------------------------------------------------------- *)
  (* WARNING : side-effect on info *)

  let add_state
    (auto : 'a t)
    (i:int)
    ?(initial=false)
    ?(final=false)
    (vertex:int)
    :
    'a t
    =
    (*assert ((i>=0)&& (i< dimension auto));*)
    let auto = Graph.add_vertex auto vertex i in
    let info = Graph.info auto in
    if initial then
      info.initial.(i) <- SetteI.add vertex info.initial.(i)
    ;
    if final then
      info.final.(i) <- SetteI.add vertex info.final.(i)
    ;
    auto

  let del_state (auto:'a t) (vertex:int) : 'a t
    =
    let i = Graph.attrvertex auto vertex in
    let auto = Graph.remove_vertex auto vertex in
    let info = Graph.info auto in
    info.initial.(i) <- SetteI.remove vertex info.initial.(i);
    info.final.(i) <- SetteI.remove vertex info.final.(i);
    auto

  let restore_sep_consistance (auto:'a t) : 'a t =
    (* ensures that the information on initial and final states are accurate *)
    (* normally only used in reachability and coreachability functions *)
    let info = Graph.info auto in
    let dim = Array.length info.initial in
    let tab_init = Array.make dim SetteI.empty
    and tab_final = Array.make dim SetteI.empty 
    in
      if dim >0
      then
	begin
	  tab_init.(0) <- info.initial.(0);
	  tab_final.(dim-1) <- info.final.(dim-1);
	  let f_iter_edge (x,y) lbl =
	    if PL.is_sep lbl
	    then
	      begin
		let nx = Graph.attrvertex auto x 
		and ny = Graph.attrvertex auto y
		in
		  if (nx+1 <> ny)
		  then
		    begin
		      Format.printf "@.***************************************@.";
		      Format.printf "ERROR: (%d nbq %d) --sep -> (%d nbq %d)" x nx y ny;
		      Format.printf "@.***************************************@."
		    end ;
		 
		  tab_init.(ny) <- SetteI.add y tab_init.(ny);
		  tab_final.(nx) <- SetteI.add x tab_final.(nx)		
	      end	
	  in
	    Graph.iter_edge auto f_iter_edge ;
	    let auto = 
	      Graph.map_info 
		auto
		(fun info -> { info with
				 initial = tab_init ;
				 final = tab_final
			     }
		)
	    in
	      check_sep auto "restore_sep";
	      auto	      
	end
      else
	begin
	  assert (is_bottom auto);
	  auto
	end





  let reachability (auto:'a t)  : 'a t =
    let info = Graph.info auto in
    let (notreachable:SetteI.t) = Graph.reachable_multi (-1) auto info.initial.(0) in
    let auto = ref auto in
    SetteI.iter
      (begin fun vertex ->
	auto := del_state !auto vertex;
      end)
      notreachable
    ;
    let dim = Array.length info.final in
    if info.final.(dim-1)=SetteI.empty then
      bottom ~lattice:info.lattice ~partition:info.partition dim
    else
      begin
	auto := restore_sep_consistance !auto;
	check_sep !auto "LAutomaton.reachability-end";
      !auto
      end

  let coreachability (auto:'a t)  : 'a t =
    let info = Graph.info auto in
    let dim = Array.length info.initial in
    let (notcoreachable:SetteI.t) = Graph.coreachable_multi (-1) auto info.final.(dim-1) in
    let auto = ref auto in
    SetteI.iter
      (begin fun vertex ->
	auto := del_state !auto vertex
      end)
      notcoreachable
    ;
    let dim = Array.length info.final in
    if info.initial.(0)=SetteI.empty then
      bottom ~lattice:info.lattice ~partition:info.partition dim
    else
      begin
	auto := restore_sep_consistance !auto;
	check_sep !auto "LAutomaton.coreachability-end";
	!auto
      end


  let is_trim (auto: 'a t) : bool =
    let info = Graph.info auto in
    let dim = Array.length info.initial in
    let (notcoreachable:SetteI.t) = Graph.coreachable_multi (-1) auto info.final.(dim-1) 
    and (notreachable:SetteI.t) = Graph.reachable_multi (-1) auto info.initial.(0) 
    in
      (SetteI.is_empty notcoreachable) && (SetteI.is_empty notreachable)

  (*  ==================================================================== *)
  (** {3 Module Powerset: powerset automata} *)
  (*  ==================================================================== *)

  (* This is used for determinisation and quotient operations *)

  module Powerset = struct
    type 'a pauto = (int, 'a PL.t, pinfo) Graph.t
      (* Powerset automaton *)

    (* Add the pvertex to [pauto] and returns its index.
       If id=None, use pinfo.counter as new id;
       If id=Some(id), use id
       Return the id.

       Makes a pvertex initial or final as soon as one vertex in
       the set is initial or final.  

       This is actually not correct for initial states, when
       called by determinisation functions, that need to call at
       some point {!restore_sep_consistance}.
    *)
    let add_pvertex (auto:'a t) (pauto:'a pauto ref) ?id (pvertex:SetteI.t) : int
	=
      assert (pvertex<>SetteI.empty);
      let i =
	let vertex = SetteI.choose pvertex in
	Graph.attrvertex auto vertex
      in
      let pinfo = Graph.info !pauto in
      try Hashhe.find pinfo.hashPI pvertex
      with Not_found ->
	let info = Graph.info auto in
	let index = match id with
	  | None ->
	      pinfo.pcounter <- pinfo.pcounter + 1;
	      pinfo.pcounter - 1
	  | Some x ->
	      pinfo.pcounter <- Pervasives.max pinfo.pcounter (x+1);
	      x
	in
	pauto := Graph.add_vertex !pauto index i;
	Hashhe.add pinfo.hashPI pvertex index;

	let inter = SetteI.inter info.initial.(i) pvertex in
	if inter<>SetteI.empty then
	  pinfo.pinitial.(i) <- SetteI.add index pinfo.pinitial.(i)
	;
	let inter = SetteI.inter info.final.(i) pvertex in
	if inter<>SetteI.empty then
	  pinfo.pfinal.(i) <- SetteI.add index pinfo.pfinal.(i)
	;
	index

  end

  (*  ==================================================================== *)
  (** {3 Determinisation } *)
  (*  ==================================================================== *)

  module Det = struct

    let raise_exit_if_vertex_not_deterministic
      (auto:'a t) (vertex:int) ~(succ:SetteI.t)
      :
      unit
      =
      let outgoing = ref A.Set.empty in
      SetteI.iter
	(begin fun succ ->
	  let mapA_L = Graph.attredge auto (vertex,succ) in
	  let setA = A.Map.maptoset mapA_L in
	  let inter = A.Set.inter setA !outgoing in
	  if inter<>A.Set.empty then
	    raise Exit;
	  outgoing := A.Set.union !outgoing setA
	end)
	succ;
      ()

    let is_vertex_deterministic (auto:'a t) (vertex:int) : bool =
      let res =
	try
	  let succ = Graph.succ auto vertex in
	  raise_exit_if_vertex_not_deterministic auto vertex ~succ;
	  true
	with Exit ->
	  false
      in
      res

    let is_pvertex_deterministic (auto:'a t) (pvertex:SetteI.t) : bool =
      let res =
	try
	  SetteI.iter
	    (begin fun vertex ->
	      let succ = Graph.succ auto vertex in
	      raise_exit_if_vertex_not_deterministic auto vertex ~succ
	    end)
	    pvertex;
	  true
	with Exit ->
	  false
      in
      res

    (* Return a map
       [possible letter after a state of the set in auto ->
       (merge of the associated lattice elements,
       set of successor states)
    *)
    let succ_pvertex (auto:'a t) (pvertex:SetteI.t) : ('a * SetteI.t) A.Map.t
      =
      let info = Graph.info auto in
      SetteI.fold
	(begin fun vertex map ->
	  let succ = Graph.succ auto vertex in
	  SetteI.fold
	    (begin fun succ map ->
	      let mapA_L = Graph.attredge auto (vertex,succ) in
	      let mapA_LxP = 
		A.Map.map
		  (begin fun elt ->
		    (elt, SetteI.singleton succ)
		  end)
		  mapA_L
	      in
	      A.Map.mergei
		(begin fun letter (elt1,p1) (elt2,p2) ->
		  (info.lattice.PLattice.join letter elt1 elt2,
		  SetteI.union p1 p2)
		end)
		map
		mapA_LxP
	    end)
	    succ
	    map
	end)
	pvertex
	A.Map.empty

    (* determinisation, returns a pauto *)
    let determinise (auto:'a t) : 'a Powerset.pauto
      =
      check_sep auto "LAutomaton.determinise";
      let info = Graph.info auto in
      let dim = Array.length info.initial in
      let (pinfo:pinfo) = make_pinfo dim in
      let (pauto:'a Powerset.pauto ref) = (* powerset automaton *)
	ref (Graph.empty pinfo)
      in
      let (marked : unit HashheI.t) =
	(* set of marked pvertex *)
	HashheI.create 7
      in

      (* Explore recursively the successor(s) of a (not yet marked) pvertex
	 and build pgraph *)
      let rec explore_pvertex (id_pvertex:int) (pvertex:SetteI.t)
	:
	unit
	=
	HashheI.add marked id_pvertex ();
	let mapA_LxP = succ_pvertex auto pvertex in
	let mapA_LxV =
	  A.Map.mapi
	    (begin fun letter (elt,psucc) ->
	      let id_psucc = Powerset.add_pvertex auto pauto psucc in
	      if not (HashheI.mem marked id_psucc) then
		explore_pvertex id_psucc psucc
	      ;
	      (elt,id_psucc)
	    end)
	    mapA_LxP
	in
	let mapV_A_L = mapV_A_L_of_mapA_LxV mapA_LxV in
	MappeI.iter
	  (begin fun id_psucc mapA_L ->
	    pauto := Graph.add_edge !pauto
	      (id_pvertex, id_psucc)
	      mapA_L
	    ;
	  end)
	  mapV_A_L
	;
	()
      in

      (* main *)
      let id_initial = Powerset.add_pvertex auto pauto info.initial.(0) in
      explore_pvertex id_initial info.initial.(0);
      pinfo.pinitial.(0) <- SetteI.singleton id_initial;
      !pauto

    (* Return a map
       [possible letter after a state of the set in auto ->
        list of (associated lattice elements,
                 set of successor states)
    *)
    let succ_pvertex_partial (auto:'a t) (pvertex:SetteI.t) : ('a * SetteI.t) list A.Map.t
      =
      let info = Graph.info auto in

      let rec merge 
	(acc : ('a * SetteI.t) list) 
	(letter:A.t) (elt:'a) (succ:int) (leltpvertex:('a * SetteI.t) list) 
	: 
	('a * SetteI.t) list
	=
	match leltpvertex with
	| [] -> 
	    (elt,SetteI.singleton succ)::acc
	| ((elt1,pvertex1) as x)::rest ->
	    if info.lattice.PLattice.is_eq letter elt elt1 then begin
	      (elt1,
	      SetteI.add succ pvertex1)
	      :: (acc@rest)
	    end else begin
	      merge (x::acc) letter elt succ rest
	    end
      in

      SetteI.fold
	(begin fun vertex map ->
	  let succ = Graph.succ auto vertex in
	  SetteI.fold
	    (begin fun succ map ->
	      let mapA_L = Graph.attredge auto (vertex,succ) in
	      A.Map.combine
		(begin fun letter oelt oleltpvertex ->
		  match (oelt,oleltpvertex) with 
		  | None,None -> failwith ""
		  | None, _ -> 
		      oleltpvertex
		  | (Some elt), None ->
		      Some [(elt,SetteI.singleton succ)]
		  | (Some elt), (Some leltpvertex) ->
		      Some (merge [] letter elt succ leltpvertex)
		end)
		mapA_L
		map
	    end)
	    succ
	    map
	end)
	pvertex
	(A.Map.empty:('a * SetteI.t) list A.Map.t)

     (* determinisation, returns a pauto *)
    let determinise_partial (auto:'a t) : 'a Powerset.pauto
      =
      check_sep auto "determinise_partial";
      let info = Graph.info auto in
      let dim = Array.length info.initial in
      let (pinfo:pinfo) = make_pinfo dim in
      let (pauto:'a Powerset.pauto ref) = (* powerset automaton *)
	ref (Graph.empty pinfo)
      in
      let (marked : unit HashheI.t) =
	(* set of marked pvertex *)
	HashheI.create 7
      in

      (* Explore recursively the successor(s) of a (not yet marked) pvertex
	 and build pgraph *)
      let rec explore_pvertex (id_pvertex:int) (pvertex:SetteI.t)
	:
	unit
	=
	HashheI.add marked id_pvertex ();
	let mapA_LxP_list = succ_pvertex_partial auto pvertex in
	let mapA_LxV_list =
	  A.Map.mapi
	    (begin fun letter leltpsucc ->
	      List.map
	      (begin fun (elt,psucc) ->
		let id_psucc = Powerset.add_pvertex auto pauto psucc in
		if not (HashheI.mem marked id_psucc) then
		  explore_pvertex id_psucc psucc
		;
		(elt,id_psucc)
	      end)
	      leltpsucc
	    end)
	    mapA_LxP_list
	in
	let mapV_A_L = mapV_A_L_of_mapA_LxV_list mapA_LxV_list in
	MappeI.iter
	  (begin fun id_psucc mapA_L ->
	    pauto := Graph.add_edge !pauto
	    (id_pvertex, id_psucc)
	    mapA_L
	    ;
	  end)
	  mapV_A_L
	;
	()
      in

      (* main *)
      let id_initial = Powerset.add_pvertex auto pauto info.initial.(0) in
      explore_pvertex id_initial info.initial.(0);
      !pauto

  end

  let is_deterministic ?(check=false) (auto:'a t) : bool
    =
    check_sep auto "is_deterministic";
    let info = Graph.info auto in
    if info.det then true
    else if check then
      try
	if (SetteI.cardinal info.initial.(0))>1 then raise Exit;
	Graph.iter_vertex auto
	  (begin fun vertex _ ~pred ~succ ->
	    Det.raise_exit_if_vertex_not_deterministic auto vertex ~succ
	  end)
	;
	info.det <- true;
	true
      with Exit ->
	false
    else
      false

  let determinise (auto:'a t) : 'a t
    =
    check_sep auto "LAutomaton.determinise";
    let info = Graph.info auto in
    if info.det then
      auto
    else
      let pauto = Det.determinise auto in
      let auto = 
	Graph.map_info pauto
	(fun pinfo -> {
	  lattice = info.lattice;
	  partition = info.partition;
	  initial = pinfo.pinitial;
	  final = pinfo.pfinal;
	  det = true;
	  min = false;
	  counter = pinfo.pcounter;
	})
      in
      let auto = restore_sep_consistance auto in
	check_sep auto "LAutomaton.determinise-end";
	auto

  let determinise_partial (auto:'a t) : 'a t
    =
    let info = Graph.info auto in
    if info.det then
      auto
    else
      let pauto = Det.determinise_partial auto in
      let auto = Graph.map_info pauto
	(fun pinfo -> {
	  lattice = info.lattice;
	  partition = info.partition;
	  initial = pinfo.pinitial;
	  final = pinfo.pfinal;
	  det = false;
	  min = false;
	  counter = pinfo.pcounter;
	})
      in
      let info = Graph.info auto in
	info.det <- is_deterministic ~check:true auto;
	check_sep auto "LAutomaton.determinise_partial-end";
	auto

  (*  ==================================================================== *)
  (** {3 Bisimulation} *)
  (*  ==================================================================== *)

  (* Work for non-deterministic automaton ! *)
  module Bisim = struct

    (* Returns the set of outgoing letters of a pvertex *)
    let letters_after_pvertex (auto:'a t) (pvertex:SetteI.t) : A.Set.t
      =
      SetteI.fold
	(begin fun vertex res ->
	  let succ = Graph.succ auto vertex in
	  SetteI.fold
	    (begin fun succ res ->
	      let mapA_L = Graph.attredge auto (vertex,succ) in
	      let setA = A.Map.maptoset mapA_L in
	      A.Set.union res setA
	    end)
	    succ
	    res
	end)
	pvertex
	A.Set.empty

    (* Returns a map
       [identifier of a successor equivalence class by the letter ->
	set of vertices in pvertex leading to classid with this letter]
       and the vertices without successors by this letter
    *)
    let succ_pvertex_by_letter
      (auto:'a t) (partition:partition)
      (pvertex:SetteI.t) (letter:A.t)
      :
      SetteI.t ref MappeI.t * SetteI.t
      =
      let map = ref MappeI.empty
      and set = ref SetteI.empty
      and canmove = ref true
      in
      SetteI.iter
	(begin fun vertex ->
	  canmove := false;
	  let succ = Graph.succ auto vertex in
	  SetteI.iter
	    (begin fun succ ->
	      let mapA_L = Graph.attredge auto (vertex,succ) in
	      if A.Map.mem letter mapA_L then begin
		canmove := true;
		let id_psucc = MappeI.find succ partition.map in
		try
		  let rset = MappeI.find id_psucc !map in
		  rset := SetteI.add vertex !rset;
		with Not_found ->
		  let rset = ref (SetteI.singleton vertex) in
		  map := MappeI.add id_psucc rset !map
	      end
	    end)
	    succ
	  ;
	  if not !canmove then begin
	    set := SetteI.add vertex !set
	  end;
	  ()
	end)
	pvertex
      ;
      (!map,!set)

    let rec partition_lset (lset:SetteI.t list) (res:SetteI.t list) 
      : 
      SetteI.t list
      =
      match lset with
      | [] -> res
      | x::l ->
	  let (nx,lx,lxn) = 
	    List.fold_left
	      (begin fun (nx,lx,lxn) y ->
		let yx = SetteI.inter y x in
		if yx = SetteI.empty then
		  (nx, lx, y::lxn)
		else begin
		  let nx = SetteI.diff nx y in
		  let yxn = SetteI.diff y x in
		  if yxn=SetteI.empty then
		    (nx, y::lx, lxn)
		  else
		    (nx, yx::lx, yxn::lxn)
		end
	      end)
	      (x,[],[]) l
	  in
	  let nres = if nx<>SetteI.empty then nx::res else res in
	  let nres = partition_lset lx nres in
	  let nres = partition_lset lxn nres in
	  nres
	    
    let bisimulation_shape_refine 
      (auto:'a t)
      (partition:partition)
      (counter:int ref)
      :
      partition option
      =
     let info = Graph.info auto in
      let current = partition in
      let next = { (* next partition *)
	map = partition.map;
	classes = partition.classes
      }
      in
      let changed = ref false in (* has something changed ? *)
      let first = ref true in

      let rec split_class id_pvertex pvertex (letters:A.t list) =
	assert(pvertex<>SetteI.empty);
	match letters with
	| [] ->
	    next.classes <- MappeI.add id_pvertex pvertex next.classes;
	    SetteI.iter
	      (begin fun vertex ->
		next.map <- MappeI.add vertex id_pvertex next.map;
	      end)
	      pvertex
	| letter :: list ->
	    let (mapV_P,others) =
	      succ_pvertex_by_letter auto current pvertex letter
	    in
	    let lset = 
	      MappeI.fold
		(begin fun id_psucc rset lset ->
		  !rset :: lset
		end)
		mapV_P
		[]
	    in
	    let lset = if info.det then lset else (partition_lset lset []) in
	    let lset = if others=SetteI.empty then lset else others::lset in
	    first := true;
	    List.iter
	      (begin fun set ->
		if !first then begin
		  first := false;
		  split_class id_pvertex set list
		end
		else begin
		  changed := true;
		  incr counter;
		  split_class !counter set list
		end
	      end)
	      lset;
	    ()
      in
      
      MappeI.iter
	(begin fun id_pvertex pvertex ->
	  let letters = letters_after_pvertex auto pvertex in
	  let letters = A.Set.elements letters in
	  split_class id_pvertex pvertex letters
	end)
	current.classes
      ;
      if not !changed then
	None
      else
	Some next

    let bisimulation_shape
      ?(depth=max_int)
      (auto:'a t)
      (partition:partition)
      :
      partition
      =
      (* Compute maximum classid *)
      let counter =
	let set = MappeI.maptoset partition.classes in
	SetteI.max_elt set
      in
      let counter = ref counter in (* for creating new classes *)
      let current = {
	map = partition.map;
	classes = partition.classes
      }
      in
      begin try
	for idepth=0 to pred depth do
	  let onext = bisimulation_shape_refine auto current counter in
	  match onext with
	  | None -> raise Exit
	  | Some next ->
	      current.map <- next.map;
	      current.classes <- next.classes;
	done
      with Exit ->
	()
      end;
      current

  end

  let bisimulation_shape = Bisim.bisimulation_shape

  (*  ==================================================================== *)
  (** {3 Simulation} *)
  (*  ==================================================================== *)

  (** Assumes a deterministic automaton *)
  module Simulation = struct

    (** Associates with a vertex a map
      [outging letter ->
       associated lattice element and successor vertex]
    *)
    let succ_vertex_det (auto:'a t) (vertex:int) : ('a * int) A.Map.t
      =
      let succ = Graph.succ auto vertex in
      SetteI.fold
	(begin fun succ map ->
	  let mapA_L = Graph.attredge auto (vertex,succ) in
	  let mapA_LxV = A.Map.map (fun elt -> (elt,succ)) mapA_L in
	  A.Map.addmap map mapA_LxV
	end)
	succ
	A.Map.empty

    let simulate_initial
      (mapA_comparei : (A.t -> 'a*int -> 'a*int -> bool) -> ('a*int) A.Map.t -> ('a*int) A.Map.t -> bool)
	(* either A.Map.subseti or A.Map.equali *)
      (lattice_is_Xeq : A.t -> 'a -> 'a -> bool)
	(* either is_leq or is_eq *)
      (auto1:'a t)
      (auto2:'a t)
      :
      bool
      =
      auto_check auto1 auto2 "LatticeAutomaton.simulate";
      let info1 = Graph.info auto1 in
      let info2 = Graph.info auto2 in
      if not info1.det || not info2.det then
	failwith "LatticeAutomatonRep.simulate_initial: at least one automaton is not deterministic"
      ;

      let rec simulate_aux 
	(assumption:SetII.t) 
	(vertex1:int) (vertex2:int) 
	: 
	bool
	=
	if SetII.mem (vertex1,vertex2) assumption then
	  true
	else begin
	  let i = Graph.attrvertex auto1 vertex1 in
	  if SetteI.mem vertex1 info1.final.(i) && not (SetteI.mem vertex2 info2.final.(i)) then
	    false
	  else begin
	    let nassumption = SetII.add (vertex1,vertex2) assumption in
	    let mapA_LxV_1 = succ_vertex_det auto1 vertex1 in
	    let mapA_LxV_2 = succ_vertex_det auto2 vertex2 in
	    mapA_comparei
	      (begin fun letter (elt1,succ1) (elt2,succ2) ->
		lattice_is_Xeq letter elt1 elt2 &&
		simulate_aux nassumption succ1 succ2
	      end)
	      mapA_LxV_1 mapA_LxV_2
	  end
	end
      in
      let res =
	simulate_aux 
	  SetII.empty
	  (SetteI.choose info1.initial.(0)) (SetteI.choose info2.initial.(0))
      in
      res

    (** Associates with a vertex a map
      [outgoing letter ->
      list of associated lattice element and successor vertex]
    *)
    let succ_vertex_nondet (auto:'a t) (vertex:int) : ('a * int) list A.Map.t
      =
      let succ = Graph.succ auto vertex in
      let res = 
	SetteI.fold
	  (begin fun succ map ->
	    let mapA_L = Graph.attredge auto (vertex,succ) in
	    let mapA_LxV_list = A.Map.map (fun elt -> [(elt,succ)]) mapA_L in
	    A.Map.merge List.append map mapA_LxV_list 
	  end)
	  succ
	  A.Map.empty
      in
      res

    exception Abort

    let simulation_refine
      (lattice:'a PL.lattice)
      (mapV_A_LxV_list_1: ('a * int) list A.Map.t MappeI.t)
      (mapV_A_LxV_list_2: ('a * int) list A.Map.t MappeI.t)
      (simulation:SetteI.t MappeI.t)
      (cache: (A.t*int*int*int*int, bool) Hashhe.t)
      :
      SetteI.t MappeI.t option
      =
      let change = ref false in

      let is_leq letter
	vertex1 elt1 succ1
	vertex2 elt2 succ2
	=
	try Hashhe.find cache (letter,vertex1,succ1,vertex2,succ2)
	with Not_found ->
	  let res = lattice.PLattice.is_leq letter elt1 elt2 in
	  Hashhe.add cache (letter,vertex1,succ1,vertex2,succ2) res;
	  res
      in

      let nsimulation = 
	MappeI.mapi
	  (begin fun vertex1 pvertex2 ->
	    let mapA_LxV_list_1 = MappeI.find vertex1 mapV_A_LxV_list_1 in
	    let npvertex2 = 
	      SetteI.filter
		(begin fun vertex2 ->
		  let mapA_LxV_list_2 = MappeI.find vertex2 mapV_A_LxV_list_2 in
		  let simulate = 
		    A.Map.subseti
		      (begin fun letter l1_eltsucc l2_eltsucc ->
			List.for_all
			(begin fun (elt1,succ1) ->
			  let res = 
			    List.exists
			      (begin fun (elt2,succ2) ->
				simulate simulation succ1 succ2 &&
				(is_leq letter 
				  vertex1 elt1 succ1
				  vertex2 elt2 succ2)
			      end)
			      l2_eltsucc
			  in
			  res
			end)
			l1_eltsucc
		      end)
		      mapA_LxV_list_1 mapA_LxV_list_2
		  in
		  if not simulate then change := true;
		  simulate
		end)
		pvertex2
	    in
	    if npvertex2 = SetteI.empty then 
	      raise Abort
	    else
	      npvertex2
	  end)
	  simulation
      in
      if !change then Some nsimulation else None
    
    let simulation_initial
      (auto1:'a t)
      (auto2:'a t)
      :
      SetteI.t MappeI.t
      =
      let info1 = Graph.info auto1 in
      let info2 = Graph.info auto2 in
      let array_pvertex2 = Array.create (Array.length info1.initial) SetteI.empty in
      Graph.iter_vertex auto2
	(begin fun vertex2 i2 ~pred ~succ ->
	  array_pvertex2.(i2) <- SetteI.add vertex2 array_pvertex2.(i2)
	end)
      ;
      let current = ref MappeI.empty in
      Graph.iter_vertex auto1
	(begin fun vertex1 i ~pred ~succ ->
	  let set2 = 
	    if SetteI.mem vertex1 info1.final.(i) then
	      info2.final.(i)
	    else
	      array_pvertex2.(i)
	  in
	  if set2 <> SetteI.empty then
	    current := MappeI.add vertex1 set2 !current
	  else
	    raise Abort
	end)
      ;
      !current

    let simulation_nondet
      ?(depth=max_int)
      (auto1:'a t)
      (auto2:'a t)
      (simulation : SetteI.t MappeI.t)
      :
      SetteI.t MappeI.t
      =
      auto_check auto1 auto2 "LatticeAutomaton.simulate_nondet";
      let info1 = Graph.info auto1 in
      let current = ref simulation in
      let vertices1 = Graph.vertices auto1 in
      let vertices2 = Graph.vertices auto2 in
      let mapV_A_LxV_list_1 = 
	MappeI.mapofset (succ_vertex_nondet auto1) vertices1
      in
      let mapV_A_LxV_list_2 = 
	MappeI.mapofset (succ_vertex_nondet auto2) vertices2
      in
      let cache = Hashhe.create 11 in
      begin try
	for idepth = 0 to pred depth do
	  let onext = 
	    simulation_refine 
	      info1.lattice
	      mapV_A_LxV_list_1
	      mapV_A_LxV_list_2
	      !current
	      cache
	  in
	  match onext with
	  | None -> raise Exit
	  | Some next ->
	      current := next;
	done
      with Exit ->
	()
      end;
      !current
	
  end

  let is_leq auto1 auto2 =
    auto1==auto2 ||
    begin
      auto_check auto1 auto2 "LAutomatonRep.is_leq";
      if is_bottom auto1 then true
      else if is_bottom auto2 then false
      else if is_deterministic auto1 && is_deterministic auto2 then begin
	let info1 = Graph.info auto1 in
	Simulation.simulate_initial
	  A.Map.subseti info1.lattice.PLattice.is_leq
	  auto1 auto2
      end
      else begin
	try 
	  ignore 
	    (Simulation.simulation_nondet 
	      auto1 auto2
	      (Simulation.simulation_initial auto1 auto2));
	  true
	with Simulation.Abort ->
	  false
      end
    end

  let is_top auto =
    let info = Graph.info auto in
      is_leq (top info.lattice info.partition (dimension auto)) auto
     

  let is_eq auto1 auto2 =
    auto1==auto2 ||
    begin
      auto_check auto1 auto2 "LAutomatonRep.is_eq";
      if is_bottom auto1 then is_bottom auto2
      else if is_bottom auto2 then false
      else if is_deterministic auto1 && is_deterministic auto2 then begin
	let info1 = Graph.info auto1 in
	let info2 = Graph.info auto2 in
	(if info1.min && info2.min then 
	  (Graph.size auto1 = Graph.size auto2)
	else
	  true
	)
	&&
	(Simulation.simulate_initial
	  A.Map.equali info1.lattice.PLattice.is_eq
	  auto1 auto2)
      end
      else begin
	try 
 	  ignore 
	    (Simulation.simulation_nondet
	      auto1 auto2
	      (Simulation.simulation_initial auto1 auto2));
	  ignore 
	    (Simulation.simulation_nondet 
	      auto2 auto1
	      (Simulation.simulation_initial auto2 auto1));
	  true
	with Simulation.Abort ->
	  false
      end
    end

  (*  ==================================================================== *)
  (** {3 Quotient} *)
  (*  ==================================================================== *)

  module Quotient = struct
    (* Return a map
       [(possible letter after a state in pvertex in auto,
	 identifier of a successor equivalence class) ->
	 merge of the associated lattice elements]
    *)
    let succ_pvertex
      (auto:'a t)
      (partition: int MappeI.t) (pvertex:SetteI.t)
      :
      'a MapAV.t
      =
      let info = Graph.info auto in
      let (mapAxV_L : 'a ref MapAV.t ref) = ref MapAV.empty in
      SetteI.iter
	(begin fun vertex ->
	  let succ = Graph.succ auto vertex in
	  SetteI.iter
	    (begin fun succ ->
	      let mapA_L = Graph.attredge auto (vertex,succ) in
	      let id_psucc = MappeI.find succ partition in
	      A.Map.iter
		(begin fun letter elt ->
		  try
		    let refelt = MapAV.find (letter,id_psucc) !mapAxV_L in
		    refelt := info.lattice.PLattice.join letter elt !refelt
		  with Not_found ->
		    let refelt = ref elt in
		    mapAxV_L := MapAV.add (letter,id_psucc) refelt !mapAxV_L
		end)
		mapA_L
	      ;
	    end)
	    succ
	  ;
	end)
	pvertex
      ;
      MapAV.map (fun x -> !x) !mapAxV_L

    let quotient (auto:'a t) (partition:partition) : 'a Powerset.pauto
      =
      let info = Graph.info auto in
      let dim = Array.length info.initial in
      let pinfo = make_pinfo dim in
      let (pauto:'a Powerset.pauto ref) = (* powerset automaton *)
	ref (Graph.empty pinfo)
      in
      (* Build the vertices *)
      MappeI.iter
	(begin fun id_pvertex pvertex ->
	  let id = Powerset.add_pvertex auto pauto ~id:id_pvertex pvertex in
	  assert(id=id_pvertex);
	end)
	partition.classes
      ;
      (* Now build quotient transitions *)
       MappeI.iter
	 (begin fun id_pvertex pvertex ->
	   let mapAV_L =
	     succ_pvertex auto partition.map pvertex
	   in
	   let mapV_A_L = mapV_A_L_of_mapAV_L mapAV_L in
	   MappeI.iter
	     (begin fun id_psucc mapA_L ->
	       pauto := Graph.add_edge !pauto
	       (id_pvertex, id_psucc)
		 mapA_L
	     end)
	     mapV_A_L
	 end)
	 partition.classes
       ;
       !pauto
  end

  let quotient (auto:'a t) (partition:partition) : 'a t =
    if
      (MappeI.cardinal partition.classes) =
      (SetteI.cardinal (Graph.vertices auto))
    then auto
    else begin
      let info = Graph.info auto in
      let pauto = Quotient.quotient auto partition in
      Graph.map_info pauto
	(fun pinfo -> {
	  lattice = info.lattice;
	  partition = info.partition;
	  initial = pinfo.pinitial;
	  final = pinfo.pfinal;
	  det = false;
	  min = false;
	  counter = pinfo.pcounter;
	})
      end

  (*  ==================================================================== *)
  (** {3 Map and iteration on the trantisitions} *)
  (*  ==================================================================== *)

  let map_trans (f_apply : (int * int) -> 'a PL.t -> 'a PL.t) (auto : 'a t) : 'a t =
    Graph.map_edge auto f_apply

  (** [map_trans f auto] applies f to all transitions of auto and
      returns a new automaton *)

  let iter_trans (f_iter : (int * int) -> 'a PL.t -> unit) (auto : 'a t) : unit =
    Graph.iter_edge auto f_iter
      (** [iter_trans f auto] applies f to all transitions of auto  *)

end
