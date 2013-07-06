(******************************************************************************)
(* Util *)
(* utilities *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

(********************** data structure conversions  ***************************)

(* gets the list of keys in a mappe *)
let mappe2keylist (m:('a, 'b) Mappe.t) : ('a list) =
  Mappe.fold (fun a b c -> a::c) m []
let pmappe2keylist (m:('a, 'b) PMappe.t) : ('a list) =
  PMappe.fold (fun a b c -> a::c) m []

(* gets the list of values in a mappe *)
let pmappe2valuelist (m:('a, 'b) PMappe.t) : ('b list) =
  PMappe.fold (fun a b c -> b::c) m []

(* converts a list to a pmappe *)
let list2pmappe compare (abl: ('a * 'b) list) = 
  List.fold_left 
    (fun (abm: ('a, 'b) PMappe.t) ((a,b): 'a * 'b) -> PMappe.add a b abm) 
    (PMappe.empty (compare))
    abl

(* converts a map into a list of pairs *)
let mappe2list (m: ('a, 'b) Mappe.t) = 
  Mappe.fold (fun a b c -> (a,b)::c) m []

(* converts a map to a pair of lists *)
let mappe2lists (m:('a, 'b) Mappe.t) : ('a list * 'b list) =
  Mappe.fold (fun a b c -> (a::(fst c),b::(snd c))) m ([],[])

(* converts a list to a pmappe *)
let list2mappe (abl: ('a * 'b) list) = 
  List.fold_left 
    (fun (abm: ('a, 'b) Mappe.t) ((a,b): 'a * 'b) -> Mappe.add a b abm) 
    (Mappe.empty)
    abl

(* converts an array to a set *)
let array2sette (bb: 'b array) = 
  Array.fold_left 
    (fun (aa: 'b Sette.t) (b: 'b) -> Sette.add b aa) 
    Sette.empty
    bb
(* computes the set where each element is mapped to its image by f *)
let psette_map (f: 'a -> 'b) (compare: 'b ->'b -> int) (a: 'a PSette.t) =
  PSette.fold 
    (fun (e:'a) (mset: 'b PSette.t) -> PSette.add (f e) mset) 
    a
    (PSette.empty (compare))

(* filters two lists in parallel *)
let list_filter2 filter a b = 
  List.fold_left2 (fun (aa,bb) a b  -> 
    if filter a b then (a::aa,b::bb) else (aa,bb))
    ([],[]) a b 

(* concats two arrays (removes duplicates) *)
let array_union a b = Array.of_list
  (Sette.elements (Sette.union (array2sette a) (array2sette b)))

(* concats two arrays, keeping only the values present in both (removes duplicates) *)
let array_inter a b = Array.of_list
  (Sette.elements (Sette.inter (array2sette a) (array2sette b)))


(* converts a list to a set *)
let list2sette (bb: 'b list) = 
  List.fold_left 
    (fun (aa: 'b Sette.t) (b: 'b) -> Sette.add b aa) 
    Sette.empty
    bb

(* converts an array to a set *)
let array2psette compare (bb: 'b array) = 
  Array.fold_left 
    (fun (aa: 'b PSette.t) (b: 'b) -> PSette.add b aa) 
    (PSette.empty compare)
    bb

(* checks whether a list is empty *)
let list_is_empty l = match l with | [] -> true | _ -> false

(* returns the key of an element in an assoc-list *)
let assoclist_find_key l e = fst (List.find (fun (k,ee) -> ee=e) l)

(* converts a set into a list *)
let psette2list (aa: 'a PSette.t) = 
  PSette.fold 
    (fun (a: 'a) (bb: 'a list) -> (a::bb)) 
    aa
    []

(* converts a list to a set *)
let list2psette compare (bb: 'b list) = 
  List.fold_left 
    (fun (aa: 'b PSette.t) (b: 'b) -> PSette.add b aa) 
    (PSette.empty (compare))
    bb

(* returns the set of equivalence classes according to eqrel *)
let sette2eqclasses (s : 'a Sette.t) (eqrel : 'a -> 'a -> bool) =
  Sette.fold
    (fun e cls -> 
      let samecls = Sette.filter (fun cl -> eqrel e (Sette.choose cl)) cls in
      if Sette.is_empty samecls then
        Sette.add (Sette.add e Sette.empty) cls
      else
        let cl = Sette.choose samecls in
        Sette.add (Sette.add e cl) (Sette.remove cl cls))
    s Sette.empty

(* returns the set of equivalence classes according to eqrel *)
let psette2eqclasses (s : 'a PSette.t) (compare : 'a -> 'a -> int) 
  (eqrel : 'a -> 'a -> bool) =
  PSette.fold
    (fun e cls -> 
      let samecls = PSette.filter (fun cl -> eqrel e (PSette.choose cl)) cls in
      if PSette.is_empty samecls then
        PSette.add (PSette.add e (PSette.empty (compare))) cls
      else
        let cl = PSette.choose samecls in
        PSette.add (PSette.add e cl) (PSette.remove cl cls))
    s (PSette.empty (PSette.compare))
 
(************************** list functions  ******************************)

(* removes duplicated elements from a list *)
let list_compress l = 
  Sette.elements (List.fold_left (fun s e -> Sette.add e s) Sette.empty l)

(* creates a list that contains the elements in l1 that are not in l2 *)
let list_diff l1 l2 = 
  List.filter (fun e -> not(List.mem e l2)) l1

(* creates a list that contains the elements that are both in l1 and l2 *)
let list_inter l1 l2 = 
  List.filter (fun e -> List.mem e l2) l1

(* returns the minimum and the maximum value in a list *)
let list_getminmax compare l =
  assert ((List.length l)>0);
  List.fold_left
    (fun (min,max) e -> 
      if (compare e max)>0 then (min,e) 
      else 
        if (compare e min)<0 then (e,max) 
        else (min,max))
    (List.hd l, List.hd l) l

(* tests two lists for equality *)
let list_equal equal l1 l2 =
  let rec comp l1 l2 =
    match (l1,l2) with
      |(hd1::tl1,hd2::tl2) -> 
        if equal hd1 hd2 then comp tl1 tl2
	else false
      |([],[]) -> true
      |_ -> assert(false)
  in
  if (List.length l1)<>(List.length l2) then false
  else comp l1 l2

(* computes the product of two lists *) 
let list_product combine l1 l2 =
  List.fold_right 
    (fun e1 res -> 
      List.append res (List.map (fun e2 -> combine e1 e2) l2))
    l1 []

(************************** array utilities  ******************************)

(* splits an array of pairs (like List.split) *)
let array_split l =
  let len = Array.length l in
  if len=0 then ([||],[||])
  else
    let (e1,e2) = l.(0) in
    let l1 = Array.make len e1 in
    let l2 = Array.make len e2 in
    for i=0 to len-1 do begin
      let (e1,e2) = l.(i) in
      l1.(i) <- e1;
      l2.(i) <- e2;
    end done;
    (l1,l2)

(* checks whether e is an element of l *)
let array_mem e l = 
  let len = Array.length l in
  let rec check i =
    if i>=len then false 
    else 
      if l.(i)=e then true
      else check (i+1)
  in
  check 0

(* creates an array that contains the elements in l1 that are not in l2 *)
let array_diff l1 l2 = 
  let len = Array.length l1 in
  let rec build i l =
    if i<0 then l
    else
      let e = l1.(i) in
      if not(array_mem e l2) then build (i-1) (e::l)
      else build (i-1) l
  in
  Array.of_list (build (len-1) [])

(* maps two arrays to another array using f (like List.map2) *)
let array_map2 f l1 l2 =
  let len = Array.length l1 in
  assert((Array.length l2)=len);
  if len=0 then [||]
  else
  begin
    let e0 = f l1.(0) l2.(0) in
    let res = Array.make len e0 in
    for i=0 to len-1 do
      res.(i) <- f l1.(i) l2.(i)
    done;
    res
  end

(* checks whether the predicate p evaluates true over at least one element in l *)
let array_exists p l =
  let len = Array.length l in
  let rec check i =  
    if i>=len then false
    else 
      if p l.(i) then true else check (i+1)
  in
  check 0

(* checks whether the predicate p evaluates true over at least one 
   pair of elements in l1,l2 *)
let array_exists2 p l1 l2 =
  let len = Array.length l1 in
  assert((Array.length l2)=len);
  let rec check i =  
    if i>=len then false
    else 
      if p l1.(i) l2.(i) then true else check (i+1)
  in
  check 0

(* prints an array *)
let array_print ?(csep=";") ?(copen="[") ?(cclose="]") pp fmt l =
  Format.pp_print_string fmt copen;
  let start = ref(true) in
  Array.iter
    (fun e-> 
      if not(!start) then Format.pp_print_string fmt csep
      else start := false; 
      pp fmt e) 
    l;
  Format.pp_print_string fmt cclose

(************************** string manipulation  ******************************)

(* removes r from string s *)
let string_remove s r = Str.global_replace (Str.regexp r) "" s

(* removes whitespaces from string s *)
let string_remove_whitespaces s = 
  Str.global_replace (Str.regexp "\\( \\)\\|\n\\|\t\\|\r") "" s
  
(* removes double spaces and newlines from s *)
let string_compact s = 
  Str.global_replace (Str.regexp "\\( \\)+") " "
    (Str.global_replace (Str.regexp "\n") "" s)

(******************************** printing  ***********************************)

(* prints a list *)
let list_print ?(csep=";") ?(copen="[") ?(cclose="]") pp fmt l =
  Format.pp_print_string fmt copen;
  let start = ref(true) in
  List.iter
    (fun e-> 
      if not(!start) then Format.pp_print_string fmt csep
      else start := false; 
      pp fmt e) 
    l;
  Format.pp_print_string fmt cclose

(* prints a string such that linebreaks happen at whitespaces *)
let print_breakable fmt s =
  let ss = Str.split (Str.regexp_string " ") s in
  List.iter (fun s -> Format.fprintf fmt "%s@ " s) ss

(* prints a string such of a fixed length (truncates or fills up with whitespaces *)
let print_fixed fmt len s =
  let slen = String.length s in
  if slen>len then 
    Format.pp_print_string fmt (String.sub s 0 len)
  else
  begin
    Format.pp_print_string fmt s;
    for i=slen to len-1 do Format.pp_print_string fmt " "; done
  end

