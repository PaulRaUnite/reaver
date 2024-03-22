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

type ('letter,'abstract) glattice = {
  mutable sep : 'abstract;
  mutable is_bottom : 'letter -> 'abstract -> bool;
  mutable is_leq: 'letter -> 'abstract -> 'abstract -> bool;
  mutable is_eq: 'letter -> 'abstract -> 'abstract -> bool;
  mutable join: 'letter -> 'abstract -> 'abstract -> 'abstract;
  mutable meet: 'letter -> 'abstract -> 'abstract -> 'abstract;
  mutable widening: 'letter -> 'abstract -> 'abstract -> 'abstract;
}

module type Alphabet = sig
  type t
  val sep : t
  module Set : (Sette.S with type elt=t)
  module Map : (Mappe.S with type key=t and module Setkey=Set)
end

module MakeAlphabet(Ord:sig include Set.OrderedType val sep:t end) = struct
  type t = Ord.t
  let sep = Ord.sep
  module Set=Sette.Make(Ord)
  module Map=Mappe.Make(Set)
end

module type S = sig
  module A:Alphabet

  module SetAA : (Sette.S with type elt=A.t*A.t)
  module MapAA : (Mappe.S with type key=A.t*A.t
			  and module Setkey=SetAA)

  type 'abstract lattice = (A.t,'abstract) glattice
  type 'abstract t = 'abstract A.Map.t

  val sep : 'a lattice -> 'a t
  val is_sep : 'a t -> bool

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

  val bottom : 'a t
  val singleton : A.t -> 'a -> 'a t

  val normalize : 'a lattice -> 'a t -> 'a t

  val is_bottom : 'a lattice -> 'a t -> bool
  val is_leq : 'a lattice -> 'a t -> 'a t -> bool
  val is_eq : 'a lattice -> 'a t -> 'a t -> bool

  val meet : 'a lattice -> 'a t -> 'a t -> 'a t
  val join : 'a lattice -> 'a t -> 'a t -> 'a t
  val widening : 'a lattice -> partition:'a t -> 'a t -> 'a t -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (A.t -> 'a -> 'b) -> 'a t -> 'b t
  val iter : (A.t -> 'a -> unit) -> 'a t -> unit
  val fold : (A.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b


  val project :
    project:('a -> A.t * 'b -> 'b) -> 
    'a -> 'b lattice -> 'b t -> 
    'b t

  val merge :
    empty:'b -> 
    merge:(A.t * 'a -> 'b -> 'b) ->
    'a t -> 'b

  val lcm :
    ?coherent:bool ->
    meet:(A.t * 'a -> A.t * 'a -> 'a option) ->
    'a t -> 'a t -> 'a MapAA.t
     
  val transfer :
    ?coherent:bool ->
    meet:(A.t * 'a -> A.t * 'a -> 'a option) ->
    'a lattice -> 'a t -> 'a t -> 'a t

end


module Make(A:Alphabet) = struct
  module A=A
  module SetAA = Sette.Make(struct
    type t = A.t*A.t
    let compare (x1,y1) (x2,y2) =
      let res = A.Set.Ord.compare x1 x2 in
      if res=0 then
	A.Set.Ord.compare y1 y2
      else
	res
  end)
  module MapAA = Mappe.Make(SetAA)

  type 'a lattice = (A.t,'a) glattice
  type 'a t = 'a A.Map.t

  let sep lattice = A.Map.add A.sep lattice.sep A.Map.empty
  let is_sep pletter = 
    let map = A.Map.repr pletter in
    match map with
    | Mappe.Node(Mappe.Empty, x, data, Mappe.Empty, _) ->
	(A.Set.Ord.compare x A.sep) = 0
    | _ -> false

  let print   
    ?(first = ("{@[" : (unit, Format.formatter, unit) format))
    ?(sep = (";@ ":(unit, Format.formatter, unit) format))
    ?(last = ("@]}":(unit, Format.formatter, unit) format))
    ?(firstbind = ("(" : (unit, Format.formatter, unit) format))
    ?(sepbind = (",":(unit, Format.formatter, unit) format))
    ?(lastbind = (")":(unit, Format.formatter, unit) format))
    print_letter
    print_abstract
    fmt p 
    =
    A.Map.print
      ~first ~sep ~last
      ~firstbind ~sepbind ~lastbind
      print_letter print_abstract fmt p

  let bottom = A.Map.empty
  let singleton letter elt = A.Map.add letter elt A.Map.empty
  let is_bottom lattice p = (p=A.Map.empty)
  let is_leq lattice p1 p2 = p1==p2 || A.Map.subseti lattice.is_leq p1 p2
  let is_eq lattice p1 p2 = p1==p2 || A.Map.equali lattice.is_eq p1 p2

  let normalize lattice p =
    A.Map.fold
      (begin fun letter elt res ->
	if lattice.is_bottom letter elt then
	  A.Map.remove letter res
	else
	  res
      end)
      p p

  let meet lattice pelt1 pelt2 = 
    if pelt1==pelt2 then 
      pelt1 
    else 
      normalize lattice
	(A.Map.commoni lattice.meet pelt1 pelt2)

  let join lattice pelt1 pelt2 = 
    if pelt1==pelt2 then 
      pelt1 
    else 
      A.Map.mergei lattice.join pelt1 pelt2

  let widening lattice ~partition pelt1 pelt2 = 
    if pelt1==pelt2 then pelt1 
    else 
      A.Map.mergei
	(begin fun l elt1 elt2 -> 
	  lattice.meet l 
	  (lattice.widening l elt1 elt2) 
	  (A.Map.find l partition)
	end)
	pelt1 pelt2

  let map = A.Map.map
  let mapi = A.Map.mapi
  let iter = A.Map.iter
  let fold = A.Map.fold

	
  let project 
    ~(project:'a -> A.t * 'b -> 'b)
    (elt:'a) 
    (lattice:'b lattice)
    (p:'b t) 
    : 
    'b t
    =
    A.Map.fold
      (begin fun (letter:A.t) (abs:'b) (res:'b t) ->
	let (inter:'b) = project elt (letter,abs) in
	if not (lattice.is_bottom letter inter) then
	  A.Map.add letter inter res
	else
	  res
      end)
      p 
      A.Map.empty
      
  let merge 
    ~(empty : 'b) 
    ~(merge: A.t * 'a -> 'b -> 'b)
    (p:'a t) 
    : 
    'b
    =
    A.Map.fold
      (begin fun letter elt res ->
	merge (letter,elt) res
      end)
      p
      empty

  let rec lcm 
    ?(coherent=false) 
    ~(meet:A.t * 'a -> A.t * 'a -> 'a option)
    (p1:'a t) (p2:'a t) 
    : 
    'a MapAA.t
    =
    if coherent then begin
      let common = A.Map.common (fun x y -> x) p1 p2 in
      let set = A.Map.maptoset common in
      let res = lcm ~coherent:false ~meet 
	(A.Map.diffset p1 set) (A.Map.diffset p2 set)
      in
      A.Map.fold
	(begin fun x elt res -> MapAA.add (x,x) elt res end)
	common res
    end
    else begin
      let res = ref MapAA.empty in
      A.Map.iter
	(begin fun l1 elt1 ->
	  A.Map.iter
	  (begin fun l2 elt2 ->
	    let elt = meet (l1,elt1) (l2,elt2) in
	    match elt with
	    | None -> ()
	    | Some elt ->
		res := MapAA.add (l1,l2) elt !res
	  end)
	  p2
	end)
	p1
      ;
      !res
    end

  let transfer 
    ?(coherent=false) 
    ~(meet:A.t * 'a -> A.t * 'a -> 'a option)
    (lattice:'a lattice)
    (elt1:'a t) 
    (p2:'a t) 
    =
    let lcm = lcm ~coherent ~meet elt1 p2 in
    let map =
      MapAA.fold
	(begin fun (l1,l2) elt map ->
	  try
	    let relt = A.Map.find l2 map in
	    relt := lattice.join l2 elt !relt;
	    map
	  with Not_found ->
	    A.Map.add l2 (ref elt) map
	end)
	lcm 
	A.Map.empty
    in
    A.Map.map (fun x -> !x) map

end
