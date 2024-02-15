(** Bounded unsigned integer expressions with BDDs *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(**

This module encode the classical arithmetic and logical operations on arrays of
bits, each bit being defined by a BDD.  It doesn't need any initialization, and
there is no in-place modification.

The type handled by the module is an array of BDDs, which represent a
processor-like register with the Least Significant Bit in first position.

This module requires the mlcuddidl library.

*)

type 'a t = 'a Cudd.Bdd.t array
  (** type of arrays of bits *)

type dt = Cudd.Man.d t
type vt = Cudd.Man.v t

(*  *********************************************************************** *)
(** {3 Logical operations} *)
(*  *********************************************************************** *)

let lnot x =
  Array.map (fun bdd -> Cudd.Bdd.dnot bdd) x

let shift_left man n x =
  let size = Array.length x in
  if size=0 then
    (x,Cudd.Bdd.dfalse man)
  else if n=0 then
    (Array.copy x, Cudd.Bdd.dfalse man)
  else if n>=1 then begin
    let nx = Array.make size (Cudd.Bdd.dfalse man) in
    if n>=1 && n<=size then begin
      Array.blit x 0 nx n (size-n) ;
      (nx, x.(size-n))
    end
    else
      (nx, Cudd.Bdd.dfalse man)
  end
  else
    failwith ("Bdd.Reg.shift_left: negative range for the shift "^(string_of_int n))


let shift_right man n x =
  let size = Array.length x in
  if size=0 then
    (x,Cudd.Bdd.dfalse man)
  else if n=0 then
    (Array.copy x, Cudd.Bdd.dfalse man)
  else if n>=1 then begin
    let nx = Array.make size x.(size-1) in
    if n<=size then begin
      Array.blit x n nx 0 (size-n);
      (nx, x.(n-1))
    end
    else
      (nx, x.(size-1))
  end
  else
    failwith ("Bdd.Reg.shift_right: negative range for the shift "^(string_of_int n))

let shift_right_logical man n x =
  let size = Array.length x in
  if size=0 then
    (x,Cudd.Bdd.dfalse man)
  else if n=0 then
    (Array.copy x, Cudd.Bdd.dfalse man)
  else if n>=1 then begin
    let nx = Array.make size (Cudd.Bdd.dfalse man) in
    if n<=size then begin
      Array.blit x n nx 0 (size-n);
      (nx, x.(n-1))
    end
    else
      (nx, Cudd.Bdd.dfalse man)
  end
  else
    failwith ("Bdd.Reg.shift_right_logical: negative range for the shift "^(string_of_int n))

(** This function extends the size of a signed or unsigned integer. *)

let extend man ~(signed:bool) (n:int) (x:'a t) =
  let size = Array.length x in
  if n > 0 then
    let bit =
      if signed && size>0 then
	x.(size-1)
      else
	(Cudd.Bdd.dfalse man)
    in
    let nx = Array.make (size + n) bit in
    for i = 0 to size - 1 do
      nx.(i) <- x.(i)
    done;
    nx
  else if n < 0 then
    Array.init (size + n) (fun i -> x.(i))
  else
    Array.copy x

(*  *********************************************************************** *)
(** {3 Arithmetic operations} *)
(*  *********************************************************************** *)

(** These are the successor and predecessor operations. They returns the new
arrays, and the carry. *)

let succ man x =
  let size = (Array.length x) in
  if size=0 then
    (x, Cudd.Bdd.dtrue man)
  else begin
    let carry = Array.make (size+1) (Cudd.Bdd.dtrue man) in
    for i=1 to size do
      carry.(i) <- Cudd.Bdd.dand carry.(i-1) x.(i-1)
    done;
    (Array.init size (fun i -> Cudd.Bdd.xor x.(i) carry.(i)),
    carry.(size))
  end

let pred man x =
  let size = (Array.length x) in
  if size=0 then
    (x, Cudd.Bdd.dtrue man)
  else begin
    let carry = Array.make (size+1) (Cudd.Bdd.dtrue man) in
    for i=1 to size do
      carry.(i) <- Cudd.Bdd.dand carry.(i-1) (Cudd.Bdd.dnot x.(i-1))
    done;
    (Array.init size (fun i -> Cudd.Bdd.xor x.(i) carry.(i)),
    carry.(size))
  end

(** These are the arithmetic operations. We assume operands have same
  size. The result, carry and overflow are returned. *)

let add man x y =
  let size = (Array.length x) in
  if size=0 then
    let dfalse = Cudd.Bdd.dfalse man in
    (x, dfalse, dfalse)
  else begin
    (* carry *)
    let carry = Array.make (size+1) (Cudd.Bdd.dfalse man) in
    (* computation *)
    for i=1 to size do
      carry.(i) <- (Cudd.Bdd.ite carry.(i-1)
	(Cudd.Bdd.dor x.(i-1) y.(i-1))
	(Cudd.Bdd.dand x.(i-1) y.(i-1)))
    done;
    let z = Array.init size (fun i -> Cudd.Bdd.xor carry.(i) (Cudd.Bdd.xor x.(i) y.(i))) in
    (z,
    carry.(size),
    Cudd.Bdd.dand (Cudd.Bdd.eq x.(size-1) y.(size-1)) (Cudd.Bdd.xor x.(size-1) z.(size-1)))
  end

let sub man x y =
  let size = (Array.length x) in
  if size=0 then
    let dfalse = Cudd.Bdd.dfalse man in
    (x, dfalse, dfalse)
  else begin
    (* carry *)
    let carry = Array.make (size+1) (Cudd.Bdd.dfalse man) in
    (* computation *)
    for i=1 to size do
      carry.(i) <- (Cudd.Bdd.ite carry.(i-1)
	(Cudd.Bdd.dor (Cudd.Bdd.dnot x.(i-1)) y.(i-1))
	(Cudd.Bdd.dand (Cudd.Bdd.dnot x.(i-1)) y.(i-1)))
    done;
    let z = Array.init size (fun i -> Cudd.Bdd.xor carry.(i) (Cudd.Bdd.xor x.(i) y.(i))) in
    (z,
    carry.(size),
    Cudd.Bdd.dand (Cudd.Bdd.xor x.(size-1) y.(size-1)) (Cudd.Bdd.xor x.(size-1) z.(size-1)))
  end

let neg x =
  if x=[||] then x else fst (succ (Cudd.Bdd.manager x.(0)) (lnot x))

let scale cst x =
  if x=[||] then
    x
  else begin
    let man = Cudd.Bdd.manager x.(0) in
    let size = Array.length x and
      dfalse = Cudd.Bdd.dfalse man
    in
    let cst = ref cst in
    let z = ref(Array.make size dfalse) and l = ref 0 in
    while !cst <> 0 do
      if !cst land 1 = 1 then begin
	let (r,_,_) = add man !z (if !l=0 then x else (fst (shift_left man !l x))) in
	z := r
      end;
      cst := !cst lsr 1; incr l;
    done;
    !z
  end

let ite bdd x y =
  let res = Array.copy x in
  for i=0 to (Array.length x) - 1 do
    res.(i) <- Cudd.Bdd.ite bdd x.(i) y.(i)
  done;
  res

let mul x y =
  if x=[||] then
    x
  else begin
    let man = Cudd.Bdd.manager x.(0) in
    let size = Array.length x and
      dfalse = Cudd.Bdd.dfalse man
    in
    let z = ref(Array.make size dfalse) in
    for i=0 to size-1 do
      let (r,_,_) =
	add man !z (if i=0 then y else (fst (shift_left man i y)))
      in
      z := ite x.(i) r !z
    done;
    !z
  end

(** {3 Predicates} *)

let is_cst (x:'a t) : bool =
  try
    Array.iter
      (begin fun bdd -> if not (Cudd.Bdd.is_cst bdd) then raise Exit end)
      x
    ;
    true
  with Exit ->
    false

let zero man x =
  let res = ref (Cudd.Bdd.dtrue man) in
  for i=0 to (Array.length x) - 1 do
    res := Cudd.Bdd.dand !res (Cudd.Bdd.dnot x.(i))
  done;
  !res

let equal man x y =
  let res = ref (Cudd.Bdd.dtrue man) in
  for i=0 to (Array.length x) - 1 do
    res := Cudd.Bdd.dand !res (Cudd.Bdd.eq x.(i) y.(i));
  done;
  !res

let greatereq man x y =
  if x=[||] then
    Cudd.Bdd.dtrue man
  else
    let (z,c,v) = sub man x y in
    (Cudd.Bdd.eq v z.((Array.length z) - 1))

let greater man x y =
  if x=[||] then
    Cudd.Bdd.dfalse man
  else
    let (z,c,v) = sub man x y in
    Cudd.Bdd.dand (Cudd.Bdd.eq v z.((Array.length z) - 1)) (Cudd.Bdd.dnot (zero man z))

let highereq man x y =
  if x=[||] then
    Cudd.Bdd.dtrue man
  else
    let (z,c,v) = sub man x y in
    Cudd.Bdd.dnot c

let higher man x y =
  if x=[||] then
    Cudd.Bdd.dfalse man
  else
    let (z,c,v) = sub man x y in
    Cudd.Bdd.dand (Cudd.Bdd.dnot c) (Cudd.Bdd.dnot (zero man z))

(*  *********************************************************************** *)
(** {3 Constants} *)
(*  *********************************************************************** *)

let min_size n =
  if n=0 then
    0
  else begin
    let sign = (n<0) in
    let size = ref 0 and reg = ref 1 in
    if not sign then
      while n >= !reg do reg := !reg lsl 1; incr size done
    else begin
      incr size;
      while -n > !reg do reg := !reg lsl 1; incr size done
    end;
    !size
  end

let of_int man size n =
  if size=0 && n=0 then
    [||]
  else begin
    (* range checking *)
    if size < min_size n then
      failwith (Format.sprintf "Bddreg.of_int size=%d n=%d: size is too small !" size n);
    let bddtrue = Cudd.Bdd.dtrue man and bddfalse = Cudd.Bdd.dfalse man in
    (* read the bits of n *)
    let bit = ref 1 in
    let x = Array.init size
      (begin fun i ->
	let res =
	  if (n land !bit) = !bit then bddtrue else bddfalse
	in
	bit := !bit lsl 1;
	res
      end)
    in
    x
  end

let to_int ~(signed:bool) (x:'a t) =
  if x=[||] then
    0
  else begin
    let acc =
      Array.fold_right
	(begin fun bdd acc ->
	  if Cudd.Bdd.is_true bdd then
	    2 * acc + 1
	  else if Cudd.Bdd.is_false bdd then
	    2 * acc
	  else
	    raise (Invalid_argument ("Bddreg.to_int: argument does not correspond to a constant value"))
	end)
	x
	0
    in
    let seuil = 1 lsl ((Array.length x)-1) in
    if signed && acc >= seuil
    then acc - (seuil lsl 1)
    else acc
  end

let equal_int man x n =
  let y = of_int man (Array.length x) n in
  equal man x y
let greatereq_int man x n =
  let y = of_int man (Array.length x) n in
  greatereq man x y
let greater_int man x n =
  let y = of_int man (Array.length x) n in
  greater man x y
let highereq_int man x n =
  let y = of_int man (Array.length x) n in
  highereq man x y
let higher_int man x n =
  let y = of_int man (Array.length x) n in
  higher man x y

(*  *********************************************************************** *)
(** {3 Decomposition in guarded form} *)
(*  *********************************************************************** *)

(*  ====================================================================== *)
(** {4 Operations on minterms} *)
(*  ====================================================================== *)

module Minterm = struct
  type t = Cudd.Man.tbool array

  (** Tests if the minterm is completely non determinated *)
  let is_indet (minterm:t) : bool
    =
    Array.fold_left
      (fun res elt -> res && (elt=Cudd.Man.Top))
      true minterm

  (** Converts an possibly negative integer into a minterm of size [size] *)
  let of_int (size:int) (n:int) : t
    =
    if size=0 && n=0 then
      [||]
    else begin
      (* range checking *)
      if size < min_size n then
	failwith (Format.sprintf "Bddreg.Minterm.of_int size=%d n=%d: size is too small !" size n);
      (* read the bits of n *)
      let bit = ref 1 in
      let x = Array.init size
	(begin fun i ->
	  let res =
	    if (n land !bit) = !bit then Cudd.Man.True else Cudd.Man.False
	  in
	  bit := !bit lsl 1;
	  res
	end)
      in
      x
    end

  (** Converts the *determinated* minterm [minterm] to an integer, with the
    Least Significant Bit in the first position. *)
  let to_int ~(signed:bool) (minterm:t) : int
    =
    if minterm=[||] then
      0
    else begin
      let acc =
	Array.fold_right
	  (begin fun (tbool:Cudd.Man.tbool) acc ->
	    begin match tbool with
	    | Cudd.Man.True -> 2 * acc + 1;
	    | Cudd.Man.Top ->
		raise (Invalid_argument ("Bddreg.Minterm.to_int: argument does not correspond to a constant value"))
	    | Cudd.Man.False -> 2 * acc
	    end;
	  end)
	  minterm
	  0
      in
      let seuil = 1 lsl ((Array.length minterm)-1) in
      if signed && acc >= seuil
      then acc - (seuil lsl 1)
      else acc
    end

  (** Iters the function [f] on all the completely determinated minterms
    generated from the given non determinated minterm [minterm]. *)
  let iter (f:t -> unit) (minterm:t) : unit
    =
    let rec parcours minterm i =
      if i = (Array.length minterm) then
	f minterm
      else
	match minterm.(i) with
	| Cudd.Man.False | Cudd.Man.True -> parcours minterm (i+1)
	| Cudd.Man.Top ->
	    let nminterm = Array.copy minterm in
	    nminterm.(i) <- Cudd.Man.False;
	    parcours nminterm (i+1);
	    let nminterm = Array.copy minterm in
	    nminterm.(i) <- Cudd.Man.True;
	    parcours nminterm (i+1)
    in
    parcours minterm 0

  let map (f:t -> 'a) (minterm:t) : 'a list
    =
    let res = ref [] in
    let nf minterm = begin res := (f minterm) :: !res end in
    iter nf minterm;
    List.rev !res

end

let guard_of_minterm man (x:'a t) (minterm:Minterm.t) : 'a Cudd.Bdd.t
  =
  let guard = ref (Cudd.Bdd.dtrue man) in
  begin try
    Array.iteri
      (begin fun i (tbool:Cudd.Man.tbool) ->
	let nguard = match tbool with
	  | Cudd.Man.True -> x.(i)
	  | Cudd.Man.False -> Cudd.Bdd.dnot x.(i)
	  | Cudd.Man.Top -> raise (Invalid_argument ("Bddreg.guard_of_minterm: argument does not correspond to a constant value"))
	in
	guard := Cudd.Bdd.dand !guard nguard;
	if Cudd.Bdd.is_false !guard then raise Exit;
      end)
      minterm;
    ()
  with Exit ->
    ()
  end;
  !guard

let guard_of_int man (x:'a t) (code:int) : 'a Cudd.Bdd.t
  =
  let minterm = Minterm.of_int (Array.length x) code in
  guard_of_minterm man x minterm

let guardints man ~(signed:bool) (x:'a t) : ('a Cudd.Bdd.t * int) list
  =
  let lguardint = ref [] in
  let minterm =
    Array.map
      (begin fun bdd ->
	begin match Cudd.Bdd.inspect bdd with
	| Cudd.Bdd.Bool(b) -> if b then Cudd.Man.True else Cudd.Man.False
	| _ -> Cudd.Man.Top
	end
      end)
      x
  in
  Minterm.iter
    (begin fun valminterm ->
      let guard = guard_of_minterm man x valminterm in
      if not (Cudd.Bdd.is_false guard) then begin
	let code = Minterm.to_int ~signed valminterm in
	lguardint := (guard,code) :: !lguardint
      end
    end)
    minterm
  ;
  !lguardint

(*  *********************************************************************** *)
(** {3 Evaluation} *)
(*  *********************************************************************** *)

let cofactor x bdd = Array.map (fun x -> Cudd.Bdd.cofactor x bdd) x
let restrict x bdd = Array.map (fun x -> Cudd.Bdd.restrict x bdd) x
let tdrestrict x bdd = Array.map (fun x -> Cudd.Bdd.tdrestrict x bdd) x

(*  *********************************************************************** *)
(** {3 Printing} *)
(*  *********************************************************************** *)

open Format

let print f fmt x =
  Print.array ~first:"[|@[<hov>" ~sep:";@ " ~last:"]@]|]"
    (Cudd.Bdd.print f)
    fmt x

let print_minterm
  ~(signed:bool)
  (print_bdd: Format.formatter -> 'a Cudd.Bdd.t -> unit)
  fmt
  (x:'a t)
  =
  if x=[||] then
    pp_print_char fmt '0'
  else begin
    if is_cst x then begin
      let n = to_int ~signed:signed x in
      fprintf fmt "{ %i }" n
    end
    else begin
      let lguardints = guardints (Cudd.Bdd.manager x.(0)) ~signed x in
      Print.list ~first:"{ @[<v>" ~sep:"@; " ~last:"@] }"
	(fun fmt (guard,code) ->
	  fprintf fmt "%i IF %a" code print_bdd guard)
	fmt
	lguardints
    end
  end

let permute_memo memo reg tab =
  Array.map (fun x -> Cudd.Bdd.permute ~memo x tab) reg

let permute ?memo reg tab =
  match memo with
  | Some memo -> permute_memo memo reg tab
  | None ->
      let hash = Cudd.Hash.create 1 in
      let memo = Cudd.Memo.Hash hash in
      let res = permute_memo memo reg tab in
      Cudd.Hash.clear hash;
      res

let varmap reg =
  Array.map Cudd.Bdd.varmap reg

let vectorcompose_memo memo tab reg =
  Array.map (Cudd.Bdd.vectorcompose ~memo tab) reg

let vectorcompose ?memo tab reg =
  match memo with
  | Some memo -> vectorcompose_memo memo tab reg
  | None ->
      let hash = Cudd.Hash.create 1 in
      let memo = Cudd.Memo.Hash hash in
      let res = vectorcompose_memo memo tab reg in
      Cudd.Hash.clear hash;
      res

(*i \section{Tests} %====================================================== i*)

(*
open Bddreg;;

let manager = Cudd.Man.make 0 0 0 0 0;;
#install_printer Cudd.Bdd.print__minterm;;


(* logical operations *)
let xx = [| Cudd.Bdd.ithvar manager 0; Cudd.Bdd.ithvar manager 1; Cudd.Bdd.ithvar manager 2; Cudd.Bdd.ithvar manager 1 |];;
let yy = [| Cudd.Bdd.ithvar manager 6; Cudd.Bdd.ithvar manager 5; Cudd.Bdd.ithvar manager 4; Cudd.Bdd.ithvar manager 3 |];;
lnot xx;;
lnot yy;;
shift_left 0 xx;;
shift_left 1 xx;;
shift_left 2 xx;;
shift_left 3 xx;;
shift_left 4 xx;;
shift_left 5 xx;;
shift_right 0 xx;;
shift_right 1 xx;;
shift_right 2 xx;;
shift_right 3 xx;;
shift_right 4 xx;;
shift_right 5 xx;;
shift_right_logical 0 xx;;
shift_right_logical 1 xx;;
shift_right_logical 2 xx;;
shift_right_logical 3 xx;;
shift_right_logical 4 xx;;
shift_right_logical 5 xx;;

(* Arithmetical operations *)
let xx = [| Cudd.Bdd.ithvar manager 0; Cudd.Bdd.ithvar manager 1; Cudd.Bdd.ithvar manager 2; Cudd.Bdd.ithvar manager 1 |];;
let yy = [| Cudd.Bdd.ithvar manager 6; Cudd.Bdd.ithvar manager 5; Cudd.Bdd.ithvar manager 4; Cudd.Bdd.ithvar manager 3 |];;
succ xx;;
pred (fst (succ xx));;
succ (fst (pred xx));;

succ yy;;
pred (fst (succ yy));;
succ (fst (pred yy));;

let dtrue = Cudd.Bdd.dtrue() and dfalse = Cudd.Bdd.dfalse();;
let x = [| dtrue;dfalse;dtrue |] and y = [| dtrue; dtrue; dfalse |];;
(* x=-3 ou 5, y=3 *)
add x y;;             (* 0 *)
add (fst (succ x)) y;;      (* 1 *)
add x (fst (succ y));;      (* 1 *)
add (fst (succ x)) (fst (succ y));; (* 2 *)
add (fst (pred y)) (fst (pred (fst (pred y))));; (* 3 *)

sub x y;;             (* 2 *)
sub (fst (succ x)) y;;      (* 3 *)
sub x (fst (succ y));;      (* 1 *)
sub (fst (succ x)) (fst (succ y));; (* 2 *)
sub (fst (pred y)) (fst (pred (fst (pred y))));; (* 1 *)

neg x;;
neg y;;
scale 3 (Array.append x [| dfalse;dfalse;dfalse |]);;
scale 4 (Array.append x [| dfalse;dfalse;dfalse |]);;
ite (Cudd.Bdd.ithvar manager 0) xx yy;;

(* Predicates *)
let xx = [| Cudd.Bdd.ithvar manager 0; Cudd.Bdd.ithvar manager 1; Cudd.Bdd.ithvar manager 2; Cudd.Bdd.ithvar manager 1 |];;
let yy = [| Cudd.Bdd.ithvar manager 6; Cudd.Bdd.ithvar manager 5; Cudd.Bdd.ithvar manager 4; Cudd.Bdd.ithvar manager 3 |];;
let dtrue = Cudd.Bdd.dtrue() and dfalse = Cudd.Bdd.dfalse();;
let x = [| dtrue;dfalse;dtrue |] and y = [| dtrue; dtrue; dfalse |];;
(* x=-3 ou 5, y=3 *)
zero xx;;
zero x;;
zero y;;
zero (Array.make 5 dfalse);;
equal xx xx;;
equal [| dtrue;dfalse;dtrue |] [| Cudd.Bdd.ithvar manager 0; Cudd.Bdd.ithvar manager 1; Cudd.Bdd.ithvar manager 2 |];;

greatereq_int yy 0;;
greatereq_int yy 1;;
highereq_int yy 0;;
highereq_int yy 1;;

greatereq x y;;
greatereq y x;;
greatereq yy [| dtrue;dfalse;dtrue;dfalse |];;
greatereq [| dtrue;dfalse;dtrue;dfalse |] yy;;
greatereq yy yy;;
greater x y;;
greater y x;;
greater yy [| dtrue;dfalse;dtrue;dfalse |];;
greater [| dtrue;dfalse;dtrue;dfalse |] yy;;
greater yy yy;;


highereq x y;;
highereq y x;;
highereq yy [| dtrue;dfalse;dtrue;dfalse |];;
highereq [| dtrue;dfalse;dtrue;dfalse |] yy;;
highereq yy yy;;
higher x y;;
higher y x;;
higher yy [| dtrue;dfalse;dtrue;dfalse |];;
higher [| dtrue;dfalse;dtrue;dfalse |] yy;;
higher yy yy;;

of_int manager 3 0;;
of_int manager 3 1;;
of_int manager 3 2;;
of_int manager 3 3;;
of_int manager 3 4;;
of_int manager 3 5;;
of_int manager 3 (-1);;
of_int manager 3 (-2);;
of_int manager 3 (-3);;
of_int manager 3 (-4);;
Minterm.of_int 3 0;;
Minterm.of_int 3 1;;
Minterm.of_int 3 2;;
Minterm.of_int 3 3;;
Minterm.of_int 3 4;;
Minterm.of_int 3 5;;
Minterm.of_int 3 (-1);;
Minterm.of_int 3 (-2);;
Minterm.of_int 3 (-3);;
Minterm.of_int 3 (-4);;

*)
