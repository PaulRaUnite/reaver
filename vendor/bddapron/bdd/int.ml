(** Bounded integer expressions with BDDs *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(**

This module is intended to encode operations based on bounded integers (or
more generally, enumerated types), using BDDs. It represents a layer on top of
[Bddreg], which provides sign management and type checking.

Let us give an example: suppose we have a variable [i: int[0..7]]; this
variable will be encoded by 3 Boolean variables [i0,i1,i2], starting from the
least significant bit. Now, how to translate the expression [i<=5] ? Here the
result will be [i2 and not i1 or not i2]

This module requires the mlcuddidl library and the [Bddreg] module.
*)

type 'a t = {
    signed: bool;
    reg: 'a Cudd.Bdd.t array;
  }
  (** Type of an enumerated variable *)

type dt = Cudd.Man.d t
type vt = Cudd.Man.v t

exception Typing of string
  (** Raised when operands are of incompatible type (sign and size) *)

let error str = raise (Typing str)


(*  ********************************************************************** *)
(** {3 Operations} *)
(*  ********************************************************************** *)

(** Test whether the register holds a constant value *)

let is_cst a = Reg.is_cst a.reg

(** This function extends the size of a signed or unsigned integer. *)

let extend man (n:int) (a:'a t) :'a t
  =
  { signed = a.signed;
    reg = Reg.extend man ~signed:a.signed n a.reg
  }

(*
(** [normalize] sets to the same size a pair of integers. [adjust1] does the
  same, but add one more bit. *)

let adjust1 (a:'a t) (b:'a t) : 'a t * 'a t
  =
  let la = Array.length a.reg and
      lb = Array.length b.reg
  in
  let (ea,eb) =
    if la > lb then (1, 1+la-lb)
    else if la < lb then (1+lb-la,1)
    else (1,1)
  in
  (extend ea a, extend eb b)

let normalize (a:'a t) (b:'a t) : 'a t * 'a t
  =
  if a.signed = b.signed && (Array.length a.reg)=(Array.length b.reg) then
    (a,b)
  else
    let acst = is_cst a and bcst = is_cst b in
    if acst && bcst then
      adjust1 a b
    else if acst then
      (extend ((Array.length b.reg) - (Array.length a.reg)) a, b)
    else if bcst then
      (a, extend ((Array.length a.reg) - (Array.length b.reg)) b)
    else
      error "Bddint.normalize: enumerated variables of different types"
*)

let check (a:'a t) (b:'a t) : unit
  =
  if not (a.signed = b.signed && (Array.length a.reg)=(Array.length b.reg)) then
    error "bounded integer of different types"

let rec neg (a:'a t) : 'a t =
  if a.signed then
    { signed = true;
      reg = Reg.neg a.reg }
  else
    error "Bddint.neg: negation of an unsigned integer"

let succ a =
  if a.reg=[||] then a
  else
    let man = Cudd.Bdd.manager a.reg.(0) in
    { signed = a.signed; reg = fst (Reg.succ man a.reg) }
let pred a =
  if a.reg=[||] then a
  else
    let man = Cudd.Bdd.manager a.reg.(0) in
    { signed = a.signed; reg = fst (Reg.pred man a.reg) }

let add a b =
  check a b;
  if a.reg=[||] then a
  else
    let man = Cudd.Bdd.manager a.reg.(0) in
    let (z,_,_) = Reg.add man a.reg b.reg in
    { signed = a.signed; reg = z }
let sub a b =
  check a b;
  if a.reg=[||] then a
  else
    let man = Cudd.Bdd.manager a.reg.(0) in
    let (z,_,_) = Reg.sub man a.reg b.reg in
    { signed = a.signed; reg = z }

let mul a b =
  check a b;
  if a.reg=[||] then
    { signed = a.signed || b.signed; reg=[||] }
  else begin
    let man = Cudd.Bdd.manager a.reg.(0) in
    let size = Array.length a.reg in
    let dfalse = Cudd.Bdd.dfalse man in
    let (signa,rega) =
      if a.signed then
	let reg = Array.copy a.reg in
	reg.(size-1) <- dfalse;
	(a.reg.(size-1),reg)
      else
	(dfalse,a.reg)
    in
    let (signb,regb) =
      if b.signed then
	let reg = Array.copy b.reg in
	reg.(size-1) <- dfalse;
	(b.reg.(size-1),reg)
      else
	(dfalse,b.reg)
    in
    let regc = Reg.mul a.reg b.reg in
    if a.signed || b.signed then begin
      regc.(size-1) <- Cudd.Bdd.xor signa signb;
      { signed = true; reg = regc }
    end
    else begin
      { signed = false; reg = regc }
    end
  end

let shift_left n a =
  if a.reg=[||] then a
  else
    let man = Cudd.Bdd.manager a.reg.(0) in
    {
      signed = a.signed;
      reg = fst (Reg.shift_left man n a.reg)
    }
let shift_right n a =
  if a.reg=[||] then a
  else
    let man = Cudd.Bdd.manager a.reg.(0) in
    {
      signed = a.signed;
      reg = fst ((if a.signed then Reg.shift_right else Reg.shift_right_logical) man n a.reg)
    }

let scale n a =
  { signed = a.signed || (n<0);
    reg = Reg.scale n a.reg }

let ite bdd a b =
  check a b;
  { signed = a.signed;
    reg = Reg.ite bdd a.reg b.reg }

(*  ********************************************************************** *)
(** {3 Predicates} *)
(*  ********************************************************************** *)

let zero man a = Reg.zero man a.reg

let equal man a b =
  check a b;
  Reg.equal man a.reg b.reg

let greatereq man a b =
  check a b;
  (if a.signed then Reg.greatereq else Reg.highereq) man a.reg b.reg

let greater man a b =
  check a b;
  (if a.signed then Reg.greater else Reg.higher) man a.reg b.reg

let of_int man sign size cst =
  { signed = sign;
    reg = Reg.of_int man size cst }

let to_int a =
  Reg.to_int a.signed a.reg

let equal_int man a cst =
  let b = of_int man a.signed (Array.length a.reg) cst in
  equal man a b

let greatereq_int man a cst =
  let b = of_int man a.signed (Array.length a.reg) cst in
  greatereq man a b

let greater_int man a cst =
  let b = of_int man a.signed (Array.length a.reg) cst in
  greater man a b

(*  *********************************************************************** *)
(** {3 Decomposition in guarded form} *)
(*  *********************************************************************** *)

module Minterm = struct
  (** Iters the function [f] on the (signed) value of all completely
    determinated minterms generated from the given non determinated minterm
    [minterm]. *)
  let iter ~(signed:bool) (f:int -> unit) (minterm:Reg.Minterm.t) : unit
    =
    let rec parcours minterm i =
      if i = (Array.length minterm) then
	f (Reg.Minterm.to_int ~signed minterm)
      else
	match minterm.(i) with
	| Cudd.Man.False | Cudd.Man.True -> parcours minterm (i+1)
	| Cudd.Man.Top ->
	    minterm.(i) <- Cudd.Man.False;
	    parcours minterm (i+1);
	    minterm.(i) <- Cudd.Man.True;
	    parcours minterm (i+1);
	    minterm.(i) <- Cudd.Man.Top
    in
    parcours (Array.copy minterm) 0

  let map ~(signed:bool) (f:int -> 'a) (minterm:Reg.Minterm.t) : 'a list
    =
    let res = ref [] in
    let nf minterm = begin res := (f minterm) :: !res end in
    iter ~signed nf minterm;
    List.rev !res

end

let guard_of_int man (x:'a t) (code:int) : 'a Cudd.Bdd.t
  =
  Reg.guard_of_int man x.reg code

let guardints man (x:'a t) : ('a Cudd.Bdd.t * int) list
  =
  Reg.guardints man ~signed:x.signed x.reg

(*  ********************************************************************** *)
(** {3 Evaluation} *)
(*  ********************************************************************** *)

let cofactor x bdd = { x with reg = Reg.cofactor x.reg bdd }
let restrict x bdd = { x with reg = Reg.restrict x.reg bdd }
let tdrestrict x bdd = { x with reg = Reg.tdrestrict x.reg bdd }

(*  ********************************************************************** *)
(** {3 Printing} *)
(*  ********************************************************************** *)

open Format
let print f fmt t =
  fprintf fmt "{ @[<hv>signed=%b;@ reg=%a@] }"
    t.signed
    (Reg.print f) t.reg

let print_minterm print_bdd fmt t =
  Reg.print_minterm ~signed:t.signed print_bdd fmt t.reg

let permute ?memo x tab = { x with reg = Reg.permute ?memo x.reg tab }
let varmap x = { x with reg = Reg.varmap x.reg }
let vectorcompose ?memo tab x =
  { x with reg = Reg.vectorcompose ?memo tab x.reg }
