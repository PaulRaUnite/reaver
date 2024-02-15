(** Manipulation of lists of guards and leafs (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

type ('a,'b) elt = {
  guard : 'a Cudd.Bdd.t;
  leaf : 'b;
}

type ('a,'b) t = ('a,'b) elt list

(*  ********************************************************************** *)
(** {3 Utilities} *)
(*  ********************************************************************** *)

let bdd_diff a b = Cudd.Bdd.dand a (Cudd.Bdd.dnot b)

(** Applies f to all pairs [(elt1,elt2)] with [elt1] in [list1]
    and [elt2] in [list2]. Iterates first of the first list, then
    on the second. *)

let iter2 f list1 list2 =
  List.iter
    (begin fun elt1 ->
      List.iter
	(begin fun elt2 ->
	  f elt1 elt2
	end)
	list2
    end)
    list1

let fold2 f res list1 list2 =
  List.fold_left
    (begin fun res elt1 ->
      List.fold_left
	(begin fun res elt2 ->
	  f res elt1 elt2
	end)
	res
	list2
    end)
    res
    list1

(*  ********************************************************************** *)
(** {3 Normalisation} *)
(*  ********************************************************************** *)

(** Checking function: raises [Failure] if problem, returns [true]
    otherwise.

    Checks that
    - no guard is false
    - no abstract value is bottom
    - no duplicatas of abstract values
*)

let check_unicity ~is_equal list =
  let rec check = function
    | [] -> true
    | elt::list ->
	if Cudd.Bdd.is_false elt.guard then
	  failwith "Bddleaf.check_unicity: false guard in a list"
	;
	List.iter
	  (begin fun elt2 ->
	    if is_equal elt.leaf elt2.leaf then
	      failwith "Bddapron.Bddleaf.check_unicity: two equal APRON values in a canonicalized abstract value."
	    ;
	  end)
	  list
	;
	check list
  in
  check list

(** Checking function: raises [Failure] if problem, returns [true]
    otherwise. Checks that the guards are exclusive. *)
let check_disjointness list =
  let rec check = function
    | [] -> true
    | elt::list ->
	List.iter
	  (begin fun elt2 ->
	    if not (Cudd.Bdd.is_inter_empty elt.guard elt2.guard) then
	      failwith "Bddleaf.check_disjointness: two non disjoint guards in the list";
	  end)
	  list
	;
	check list
  in
  check list

(** Performs the join of a list with an element.

    Assuming that the list argument satisfies the unicity property,
    ensures it in the result *)
let cons_unique ~is_equal elt1 list2 =
  let rec join_unique res list2 = match list2 with
    | [] -> elt1::res
    | elt2::tail2 ->
	if is_equal elt1.leaf elt2.leaf then begin
	  let nelt2 = { elt2 with
	    guard = Cudd.Bdd.dor elt1.guard elt2.guard
	  } in
	  nelt2 :: (List.rev_append res tail2)
	end
	else
	  join_unique (elt2::res) tail2
  in
  join_unique [] list2

(** Append the two lists.

    Assuming that the first list argument satisfies the unicity property,
    ensures it in the result *)
let append_unique ~is_equal list1 list2 =
  List.fold_left
    (fun res elt2 -> cons_unique ~is_equal elt2 res)
    list1
    list2

(** Performs the join of a list with an element.

    Assuming that the list argument satisfies the disjointness
    (and unicity) property, ensures it in the result *)

let cons_disjoint ~is_equal ~merge elt1 list2 =
  let (nlist2,nlist12,nguard1) =
    List.fold_left
      (begin fun (list2,list12,guard1) elt2 ->
	if Cudd.Bdd.is_inter_empty guard1 elt2.guard then
	  (elt2::list2, list12, guard1)
	else begin
	  let nguard1 = bdd_diff guard1 elt2.guard in
	  let nguard2 = bdd_diff elt2.guard guard1 in
	  let nguard12 =
	    if Cudd.Bdd.is_false nguard1 then guard1
	    else if Cudd.Bdd.is_false nguard2 then elt2.guard
	    else Cudd.Bdd.dand guard1 elt2.guard
	  in
	  let nlist2 =
	    if Cudd.Bdd.is_false nguard2 then
	      list2
	    else
	      { elt2 with guard = nguard2 } :: list2
	  in
	  let nlist12 =
	    if Cudd.Bdd.is_false nguard12 then
	      list12
	    else
	      cons_unique ~is_equal
		{
		  guard = nguard12;
		  leaf = merge elt1.leaf elt2.leaf
		}
		list12
	  in
	  (nlist2,nlist12,nguard1)
	end
      end)
      ([],[],elt1.guard)
      list2
  in
  let res = append_unique ~is_equal nlist2 nlist12 in
  if Cudd.Bdd.is_false nguard1 then
    res
  else
    cons_unique ~is_equal { elt1 with guard = nguard1 } res

(** Appends the two lists.

    Assuming that the first list argument satisfies the disjointness
    (and unicity) property, ensures it in the result *)
let append_disjoint ~is_equal ~merge list1 list2 =
  List.fold_left
    (fun res elt2 -> cons_disjoint ~is_equal ~merge elt2 res)
    list1
    list2

(** Calls the right cons function depending on the options. *)
let cons ~is_equal ~merge ~unique ~disjoint elt1 list2 =
  assert(if disjoint then unique else true);
  if disjoint then
    cons_disjoint ~is_equal ~merge elt1 list2
  else if unique then
    cons_unique ~is_equal elt1 list2
  else
    elt1::list2

(** Calls the right append function depending on the options. *)
let append ~is_equal ~merge ~unique ~disjoint list1 list2 =
  if disjoint then
    append_disjoint ~is_equal ~merge list1 list2
  else if unique then
    append_unique ~is_equal list1 list2
  else
    List.rev_append list2 list1

(** Remove duplicatas (by reconstructing the list) *)
let make_unique  ~is_equal ~merge ~disjoint list =
  let nlist =
    (if disjoint then append_disjoint ~merge else append_unique)
      ~is_equal
      [] list
  in
  nlist

(*  ********************************************************************** *)
(** {3 Others} *)
(*  ********************************************************************** *)

(** Return the union of guards in the list *)
let guard ~cudd list =
  List.fold_left
    (begin fun res elt -> Cudd.Bdd.dor res elt.guard end)
    (Cudd.Bdd.dfalse cudd)
    list
