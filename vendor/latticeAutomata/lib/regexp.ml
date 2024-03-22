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

(** Regular expressions *)

type 'a exp =
| Epsilon
| Letter of 'a
| Union  of 'a exp list
| Concat of 'a exp list
| Star   of 'a exp
| Plus   of 'a exp

type 'a t =
  | Empty
  | Exp of 'a exp

let rec exp_map (f:'a -> 'b) (exp:'a exp) : 'b exp
  =
  match exp with
  | Epsilon -> Epsilon
  | Letter l -> Letter (f l)
  | Union list -> Union (List.map (exp_map f) list)
  | Concat list -> Concat (List.map (exp_map f) list)
  | Star e -> Star(exp_map f e)
  | Plus e -> Plus(exp_map f e)

let map (f:'a -> 'b) (exp:'a t) : 'b t 
  =
  match exp with
  | Empty -> Empty
  | Exp(exp) -> Exp(exp_map f exp)


let rec exp_iter (f:'a -> unit) (exp:'a exp) : unit
  =
  match exp with
  | Epsilon -> ()
  | Letter l -> f l
  | Union list -> List.iter (exp_iter f) list
  | Concat list -> List.iter (exp_iter f) list
  | Star e -> exp_iter f e
  | Plus e -> exp_iter f e

let iter (f:'a -> unit) (exp:'a t) : unit 
  =
  match exp with
  | Empty -> ()
  | Exp(exp) -> exp_iter f exp



let rec exp_equal
  (letter_equal:'a -> 'a -> bool)
  (e1:'a exp)
  (e2:'a exp)
  :
  bool
  =
  match (e1,e2) with
  | Epsilon,Epsilon -> true
  | (Letter l1), (Letter l2) -> letter_equal l1 l2
  | (Union e1), (Union e2) -> lexp_equal letter_equal e1 e2
  | (Concat e1), (Concat e2) -> lexp_equal letter_equal e1 e2
  | (Star e1), (Star e2) -> exp_equal letter_equal e1 e2
  | (Plus e1), (Plus e2) -> exp_equal letter_equal e1 e2
  | _ -> false
and lexp_equal
  (letter_equal:'a -> 'a -> bool)
  (l1:'a exp list)
  (l2:'a exp list)
  :
  bool
  =
  match (l1,l2) with
  | [],[] -> true
  | e1::l1, e2::l2 ->
      exp_equal letter_equal e1 e2 && lexp_equal letter_equal l1 l2
  | _ -> false

let equal 
  (letter_equal:'a -> 'a -> bool)
  (e1:'a t)
  (e2:'a t)
  :
  bool
  =
  match (e1,e2) with
  | Empty,Empty -> true
  | Exp e1, Exp e2 -> exp_equal letter_equal e1 e2
  | _ -> false

let rec simplify_concat
  (letter_equal:'a -> 'a -> bool)
  (lexp : 'a exp list)
  : 
  'a exp list
  =
  (* apply the rules:
     x.x* | x*.x | x+.x* | x*.x+ -> x+
     x*.x* -> x*
     (e+x).x* -> x*
   *)
  match lexp with
  | [] -> []
  | [e] as l -> l
  | e1::(e2::rest2 as rest1) ->
      begin match (e1,e2) with
      | (Star x, Star y) when (exp_equal letter_equal x y) -> 
	  simplify_concat letter_equal ((Star x)::rest2)
      | (Plus x,Star y) when (exp_equal letter_equal x y) -> 
	  simplify_concat letter_equal ((Plus x)::rest2)
      | (Star y, Plus x) when (exp_equal letter_equal x y) -> 
	  simplify_concat letter_equal ((Plus x)::rest2)
      | (x,Star y) when (exp_equal letter_equal x y) -> 
	  simplify_concat letter_equal ((Plus x)::rest2)
      | (Star y, x) when (exp_equal letter_equal x y) -> 
	  simplify_concat letter_equal ((Plus x)::rest2)
      | (Union([Epsilon;y]), Star x) when (exp_equal letter_equal x y) -> 
	  simplify_concat letter_equal ((Star x)::rest2)
      | _ ->
	  e1::(simplify_concat letter_equal rest1)
      end
      
(* Remove duplicatas from a sorted list *)
let rec remove_doublons (letter_equal:'a -> 'a -> bool) (l:'a list) 
  : 
  'a list 
  =
  match l with
  | [] -> []
  | [x] -> l
  | x::(y::rest2 as rest1) ->
      if letter_equal x y then
	remove_doublons letter_equal rest1
      else
	x::(remove_doublons letter_equal rest1)

let simplify_union  
  (letter_equal:'a -> 'a -> bool)
  (lexp:'a exp list) 
  : 
  'a exp list 
  =
  (* apply the rules:
     x + x+ + ... -> x+ + ...
     x + x* + ... -> x* + ...
     (we do not track x+ + x* + ... -> x* + ... )
   *)
  let is_subsumed x exp = match exp with
  | Star y when exp_equal letter_equal x y -> true
  | Plus y when exp_equal letter_equal x y -> true
  | y when exp_equal letter_equal x y -> true
  | _ -> false
  in
  let rec subsume (res:'a exp list) (lexp:'a exp list) : 'a exp list =
    match lexp with
    | [] -> res
    | x::rest ->
	if
	  List.exists (is_subsumed x) res ||
	  List.exists (is_subsumed x) rest
	then
	  subsume res rest
	else
	  subsume (x::res) rest
  in
  subsume [] lexp

let simplify 
  (letter_equal:'a -> 'a -> bool)
  (expr:'a t) 
  : 
  'a t
  =
  let rec simp (exp:'a exp) : 'a exp =
    match exp with
      Epsilon -> exp
    | Letter _ -> exp
    | Concat(lexp) ->
	let lexp = List.map simp lexp in
	(* we remove epsilon *)
	let lexp = List.filter (fun exp -> exp <> Epsilon) lexp in
	(* we simplify *)
	let lexp = simplify_concat letter_equal lexp in
	begin match lexp with
	| [] -> Epsilon
	| [e] -> e
	| _ -> Concat(lexp)
	end
    | Union(lexp) ->
	let lexp = List.map simp lexp in
	(* we split into non-epsilon and epsilon *)
	let (lexp,leps) = List.partition (fun exp -> exp<>Epsilon) lexp in
	let lexp = simplify_union letter_equal (List.sort Pervasives.compare lexp) in
	let lexp =
	  if leps=[] then
	    lexp
	  else begin
	    match lexp with
	    | [Star x] | [Plus x] ->
		lexp
	    | _ ->
		Epsilon::lexp
	  end
	in
	begin match lexp with
	| [] -> Epsilon
	| [x] -> x
	| _ -> Union lexp
	end
    | Star(exp) ->
	let exp = simp exp in
	begin match exp with
	| Epsilon -> exp
	| Star(_) -> exp
	| Plus(x) -> Star(x)
	| Union(Epsilon::lx) -> Star(Union lx)
	| _ -> Star(exp)
	end
    | Plus(exp) ->
	let exp = simp exp in
	begin match exp with
	| Epsilon -> exp
	| Star(_) -> exp
	| Plus(_) -> exp
	| Union(Epsilon::lx) -> Star(Union lx)
	| _ -> Plus(exp)
	end
  in begin
  match expr with
  | Empty -> Empty
  | Exp exp -> Exp(simp exp)
  end


open Format

let print
  (print : Format.formatter -> 'a -> unit)
  (fmt:Format.formatter)
  (expr : 'a t)
  :
  unit
  =
  let rec print_expression_aux (priority : int) (fmt:Format.formatter) (expr : 'a exp) : unit =
    match expr with
    | Epsilon -> Format.pp_print_string fmt "_"
    | Letter e -> print fmt e
    | Union(lexp) ->
	if priority > 1
	then Print.list ~first:"(@[<hv>" ~sep:"@,|@," ~last:"@])" (print_expression_aux 1) fmt lexp
	else Print.list ~first:"@[<hv>" ~sep:"@,|@," ~last:"@]" (print_expression_aux 1) fmt lexp
    | Concat(lexp) ->
	if priority > 2
	then Print.list ~first:"(@[<hov>" ~sep:".@," ~last:"@])" (print_expression_aux 2) fmt lexp
	else Print.list ~first:"@[<hov>" ~sep:".@," ~last:"@]" (print_expression_aux 2) fmt lexp
    | Star exp ->
	begin
	  print_expression_aux 3 fmt  exp;
	  Format.pp_print_string fmt "^*"
	end
    | Plus exp ->
	begin
	  print_expression_aux 3 fmt exp;
	  Format.pp_print_string fmt "^+"
	end

  in (* print_expression_aux 0 (simp_expression expr) *)
  match expr with
  | Empty -> Format.pp_print_string fmt "Ø"
  | Exp exp -> print_expression_aux 0 fmt exp
