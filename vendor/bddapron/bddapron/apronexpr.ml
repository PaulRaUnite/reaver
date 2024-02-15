(** Purely arithmetic expressions (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

type 'a symbol = 'a Bdd.Env.symbol = {
  compare : 'a -> 'a -> int;
  marshal : 'a -> string;
  unmarshal : string -> 'a;
  mutable print : Format.formatter -> 'a -> unit;
}

type typ = [
  | `Int
  | `Real
]

type ('a,'b) typ_of_var = 'a -> 'b constraint 'b = [> typ]

exception Constant of int

let mpqf_of_coeff coeff =
  match coeff with
  | Apron.Coeff.Scalar (Apron.Scalar.Float x) -> Mpqf.of_float x
  | Apron.Coeff.Scalar (Apron.Scalar.Mpfrf x) -> Mpfrf.to_mpqf x
  | Apron.Coeff.Scalar (Apron.Scalar.Mpqf x) -> x
  | Apron.Coeff.Interval _ -> raise (Invalid_argument ("unable to convert an APRON interval coefficient into a Bddapron.Apronexpr scalar coefficient"))


(*  ********************************************************************** *)
(** {3 Expressions} *)
(*  ********************************************************************** *)

(* Scale the list such that the minimum rational it contains is with gcd equal
   to 1.  The factor applied is returned together with the new list.

   Assume non-empty list
*)
let reduce_list (list:(Mpqf.t * 'a) list) : (Mpqf.t * (Mpqf.t * 'a) list)
  =
  if list=[] then raise (Invalid_argument "Arith.reduce_list");
  (* compute lcm of denominators *)
  let tmp = Mpz.init () in
  let lcm = Mpz.init_set_si 1 in
  List.iter
    (begin fun (c,_) ->
      Mpq.get_den tmp (Mpqf.to_mpq c);
      Mpz.lcm lcm lcm tmp
    end)
    list
  ;
  (* multiplying by lcm -> getting a list of integers *)
  let (ncoeffs : Mpz.t list) =
    List.map
      (begin fun (c,_) ->
	let mpz = Mpz.init () in
	Mpq.get_den tmp (Mpqf.to_mpq c);
	Mpz.divexact mpz lcm tmp;
	Mpq.get_num tmp (Mpqf.to_mpq c);
	Mpz.mul mpz mpz tmp;
	mpz
      end)
      list
  in
  (* compute gcd of ncoeff *)
  let gcd = Mpz.init () in
  Mpz.set gcd (List.hd ncoeffs);
  Mpz.abs gcd gcd;
  List.iter
    (begin fun coeff ->
      Mpz.gcd gcd gcd coeff
    end)
    (List.tl ncoeffs)
  ;
  let factor = Mpqf.of_mpz2 lcm gcd in
  let nlist =
    List.map2
      (begin fun coeff (_,something) ->
	Mpz.divexact coeff coeff gcd;
	(Mpqf.of_mpz coeff,something)
      end)
      ncoeffs list
  in
  assert(Mpqf.sgn factor > 0);
  (factor,nlist)

(*  ==================================================================== *)
(** {4 Linear expressions} *)
(*  ==================================================================== *)

module Lin = struct
  type 'a term = Mpqf.t * 'a
  type 'a t = {
    cst : Mpqf.t;
    lterm : 'a term list;
  }

  let is_dependent_on_integer_only typ_of_var (e:'a t) =
    List.for_all
      (begin fun (coeff,var) ->
	(typ_of_var var) = `Int
      end)
      e.lterm

  let normalize man e = {
    cst = e.cst;
    lterm = List.stable_sort (fun (c1,v1) (c2,v2) -> man.compare v1 v2) e.lterm
  }

  let support man (e:'a t) : 'a PSette.t =
    List.fold_left
      (begin fun res (_,var) -> PSette.add var res end)
      (PSette.empty man.compare)
      e.lterm

  let substitute_by_var man e (substitution:('a,'a) PMappe.t)
    =
    let ne = {
      cst = e.cst;
      lterm = begin
	List.map
	  (begin fun ((c,v) as x) ->
	    try
	      (c, PMappe.find v substitution)
	    with Not_found ->
	      x
	  end)
	  e.lterm
      end
    }
    in
    normalize man ne

   let print man fmt expr =
    let first = ref true in
    fprintf fmt "@[";
    if expr.lterm<>[] then begin
      List.iter
	(begin fun (coeff,var) ->
	  let sgn = Mpqf.sgn coeff in
	  assert(sgn<>0);
	  if sgn>0 then begin
	    if not !first then Format.pp_print_string fmt "+";
	  end;
	  if not !first then Format.fprintf fmt "@,";
	  if Mpqf.cmp_int coeff (-1) = 0 then
	    Format.pp_print_string fmt "-"
	  else if Mpqf.cmp_int coeff 1 <> 0 then
	    Mpqf.print fmt coeff
	  ;
	  man.print fmt var;
	  first := false;
	end)
	expr.lterm;
    end;
    let sgn = Mpqf.sgn expr.cst in
    if !first || sgn < 0 then
      Mpqf.print fmt expr.cst
    else if sgn > 0 then
      fprintf fmt "+%a" Mpqf.print expr.cst
    ;
    fprintf fmt "@]"

  let rec compare_lterm man l1 l2 =
    match (l1,l2) with
    | ((c1,v1)::r1,(c2,v2)::r2) ->
	let cmp = man.compare v1 v2 in
	if cmp < 0 then
	  Mpqf.sgn c1
	else if cmp>0 then
	  -(Mpqf.sgn c2)
	else
	  let s = Mpqf.cmp c1 c2 in
	  if s=0 then compare_lterm man r1 r2 else s
    | ((c1,v1)::r1,[]) ->
	let s = Mpqf.sgn c1 in
	if s=0 then failwith "";
	s
    | ([],(c1,v1)::r1) ->
	let s = Mpqf.sgn c1 in
	if s=0 then failwith "";
	-s
    | ([],[]) -> 0

  let compare man e1 e2 =
    let res = compare_lterm man e1.lterm e2.lterm in
    if res=0 then
      let res = Mpqf.cmp e1.cst e2.cst in
      if res>0 then 1 else if res < 0 then -1 else 0
    else
      if res>0 then 2 else if res < 0 then -2 else 0

  let var (x:'a) = {
    cst = Mpqf.of_int 0;
    lterm = [ (Mpqf.of_int 1, x) ]
  }

  let cst (x:Mpqf.t) = {
    cst = x;
    lterm = [];
  }

  let zero = cst (Mpqf.of_int 0)
  let one = cst (Mpqf.of_int 1)

  let rec add_lterm man l1 l2 = match (l1,l2) with
    | (l,[])
    | ([],l)
      -> l
    | ((c1,v1)::r1,(c2,v2)::r2) ->
	let cmp = man.compare v1 v2 in
	if cmp<0 then
	  (c1,v1)::(add_lterm man r1 l2)
	else if cmp>0 then
	  (c2,v2)::(add_lterm man l1 r2)
	else
	  let c = Mpqf.add c1 c2 in
	  if (Mpqf.sgn c)<> 0
	  then (c,v1)::(add_lterm man r1 r2)
	  else add_lterm man r1 r2

  let scale f e =
    if (Mpqf.sgn f) = 0 then {
      cst = Mpqf.of_int 0;
      lterm = [];
    }
    else {
      cst = Mpqf.mul f e.cst;
      lterm = List.map (fun (c,v) -> (Mpqf.mul f c,v)) e.lterm
    }

  let negate_lterm l =
    List.map (fun (c,v) -> (Mpqf.neg c, v)) l

  let negate e = {
    cst = Mpqf.neg e.cst;
    lterm = negate_lterm e.lterm;
  }

  let add man e1 e2 = {
    cst = Mpqf.add e1.cst e2.cst;
    lterm = add_lterm man e1.lterm e2.lterm
  }

  let rec sub_lterm man l1 l2 = match (l1,l2) with
    | (l,[]) -> l
    | ([],l) -> negate_lterm l
    | ((c1,v1)::r1,(c2,v2)::r2) ->
	let cmp = man.compare v1 v2 in
	if cmp<0 then
	  (c1,v1)::(sub_lterm man r1 l2)
	else if cmp>0 then
	  (Mpqf.neg c2,v2)::(sub_lterm man l1 r2)
	else
	  let c = Mpqf.sub c1 c2 in
	  if (Mpqf.sgn c)<> 0
	  then (c,v1)::(sub_lterm man r1 r2)
	  else sub_lterm man r1 r2

  let sub man e1 e2 = {
    cst = Mpqf.sub e1.cst e2.cst;
    lterm = sub_lterm man e1.lterm e2.lterm
  }

  let normalize_as_constraint e =
    if e.lterm=[] then
      raise (Constant (Mpqf.sgn e.cst))
    else
      let (factor,lterm) = reduce_list e.lterm in
	{
	  cst = Mpqf.mul factor e.cst;
	  lterm = lterm;
	}

  let to_linexpr1 man (env:Apron.Environment.t) e : Apron.Linexpr1.t =
    let res = Apron.Linexpr1.make ~sparse:true env in
    let list = List.map
      (fun (mpqf,name) ->
	(Apron.Coeff.s_of_mpqf mpqf,
	 Apron.Var.of_string (man.marshal name))
      )
      e.lterm
    in
    Apron.Linexpr1.set_list res list
      (if (Mpqf.sgn e.cst)=0
      then None else
	Some(Apron.Coeff.s_of_mpqf e.cst))
    ;
    res
  let to_linexpr0 man env e =
    let linexpr1 = to_linexpr1 man env e in
    linexpr1.Apron.Linexpr1.linexpr0

  let of_linexpr1 man (e:Apron.Linexpr1.t) =
    let cst =
      let coeff = Apron.Linexpr1.get_cst e in
      mpqf_of_coeff coeff
    in
    let lterm = ref [] in
    Apron.Linexpr1.iter
      (begin fun coeff avar ->
	let mpqf = mpqf_of_coeff coeff in
	if (Mpqf.sgn mpqf) <> 0 then begin
	  let var = man.unmarshal (Apron.Var.to_string avar) in
	  lterm := (mpqf,var)::(!lterm)
	end;
      end)
      e
    ;
    lterm := List.sort (fun (_,v1) (_,v2) -> man.compare v1 v2) !lterm;
    { cst=cst; lterm = !lterm }

  let of_linexpr0 man env e =
    of_linexpr1 man { Apron.Linexpr1.linexpr0 = e; Apron.Linexpr1.env = env }

end

(*  ==================================================================== *)
(** {4 Polynomial expressions} *)
(*  ==================================================================== *)

module Poly = struct

  type 'a varexp = 'a * int
  type 'a monomial = 'a varexp list
  type 'a term = Mpqf.t * 'a monomial
  type 'a t = 'a term list

  let is_dependent_on_integer_only typ_of_var (e:'a t) =
    List.for_all
      (begin fun (c,mon) ->
	List.for_all
	(begin fun (var,exp) ->
	  (typ_of_var var) = `Int
	end)
	mon
      end)
      e

  let print_varexp man fmt (var,exp) =
    man.print fmt var;
    if exp<>1 then
      fprintf fmt "^%i" exp;
    ()

  let print_monomial man fmt lvarexp =
    Print.list ~first:"" ~sep:"." ~last:""
      (print_varexp man)
      fmt
      lvarexp

  let print man fmt expr =
    let first = ref true in
    fprintf fmt "@[";
    List.iter
      (begin fun (coeff,mon) ->
	let sgn = Mpqf.sgn coeff in
	if sgn <> 0 then begin
	  if sgn>0 then begin
	    if not !first then Format.pp_print_string fmt "+";
	  end;
	  if not !first then Format.fprintf fmt "@,";
	  if Mpqf.cmp_int coeff (-1) = 0 then
	    Format.pp_print_string fmt "-"
	  else if Mpqf.cmp_int coeff 1  <> 0 then
	    Mpqf.print fmt coeff
	  ;
	  print_monomial man fmt mon;
	  first := false;
	end
       end)
      expr
    ;
    if !first then pp_print_string fmt "0";
    fprintf fmt "@]";
    ()

  let compare_varexp man (v1,n1) (v2,n2) =
    let res = man.compare v1 v2 in
    if res<>0
    then res
    else n1-n2

  let rec compare_monomial man m1 m2 = match (m1,m2) with
    | (v1::r1,v2::r2) ->
	let res = compare_varexp man v1 v2 in
	if res<>0
	then res
	else compare_monomial man r1 r2
    | (v1::r1,[]) -> 1
    | ([],v1::r1) -> -1
    | ([],[]) -> 0

  let normalize_monomial man m =
    let nm = List.filter
      (begin fun (v,n) ->
	if n<0
	then raise Exit
	else n>0
      end)
      m
    in
    List.stable_sort (compare_varexp man) nm

  let normalize man e =
    let ne = List.filter (fun (c,m) -> Mpqf.sgn c <> 0) e in
    List.stable_sort (fun (c1,m1) (c2,m2) -> compare_monomial man m1 m2) ne

  let normalize_full man e =
    let ne = List.filter (fun (c,m) -> Mpqf.sgn c <> 0) e in
    let ne2 = List.map (fun (c,m)  -> (c,normalize_monomial man m)) ne in
    List.stable_sort (fun (c1,m1) (c2,m2) -> compare_monomial man m1 m2) ne2

  let substitute_by_var man (e:'a t) (substitution:('a,'a) PMappe.t) : 'b t
    =
    let rename_varexp ((var,exp) as varexp) : 'a varexp =
      try (PMappe.find var substitution, exp)
      with Not_found -> varexp
    in
    let rename_monomial (monomial:'a monomial) : 'a monomial =
      let res = List.map rename_varexp monomial in
      normalize_monomial man res
    in
    let rename_term (c,mon) : 'a term = (c,rename_monomial mon)
    in
    let res = List.map rename_term e in
    List.stable_sort (fun (c1,m1) (c2,m2) -> compare_monomial man m1 m2) res

  let support man (e:'a t) : 'a PSette.t =
    List.fold_left
      (begin fun res (_,monomial) ->
	List.fold_left
	  (begin fun res (var,_) ->
	    PSette.add var res
	  end)
	  res
	  monomial
      end)
      (PSette.empty man.compare)
      e

  let compare man l1 l2 =
    let rec compare l1 l2 = match (l1,l2) with
      | ((c1,m1)::r1,(c2,m2)::r2) ->
	  let s = compare_monomial man m1 m2 in
	  if s<0 then
	    Mpqf.sgn c1
	  else if s>0 then
	    -(Mpqf.sgn c2)
	  else
	    let s = Mpqf.cmp c1 c2 in
	    if s=0 then compare r1 r2 else s
      | ((c1,m1)::r1,[]) ->
	  let s = Mpqf.sgn c1 in
	  if s=0 then failwith "";
	  s
      | ([],(c1,m1)::r1) ->
	  let s = Mpqf.sgn c1 in
	  if s=0 then failwith "";
	  -s
      | ([],[]) -> 0
    in
    let (cst1,l1) = match l1 with
      | (c,[])::r -> (c,r)
      | _ as l -> (Mpqf.of_int 0, l)
    in
    let (cst2,l2) = match l2 with
      | (c,[])::r -> (c,r)
      | _ as l -> (Mpqf.of_int 0, l)
    in
    let res = compare l1 l2 in
    if res=0 then
      let res = Mpqf.cmp cst1 cst2 in
      if res>0 then 1 else if res < 0 then -1 else 0
    else
      if res>0 then 2 else if res < 0 then -2 else 0

  let cst x : 'a t = [(x,[])]
  let var x : 'a t = [(Mpqf.of_int 1),[(x,1)]]

  let negate e =
    List.map (fun (c,m) -> (Mpqf.neg c, m)) e

  let rec add man l1 l2 = match (l1,l2) with
    | (l,[])
    | ([],l)
      -> l
    | ((c1,m1)::r1,(c2,m2)::r2) ->
	let cmp = compare_monomial man m1 m2 in
	if cmp<0 then
	  (c1,m1)::(add man r1 l2)
	else if cmp>0 then
	  (c2,m2)::(add man l1 r2)
	else
	  let c = Mpqf.add c1 c2 in
	  if (Mpqf.sgn c)<> 0
	  then (c,m1)::(add man r1 r2)
	  else add man r1 r2

  let rec sub man l1 l2 = match (l1,l2) with
    | (l,[]) -> l
    | ([],l) -> negate l
    | ((c1,m1)::r1,(c2,m2)::r2) ->
	let cmp = compare_monomial man m1 m2 in
	if cmp<0 then
	  (c1,m1)::(sub man r1 l2)
	else if cmp>0 then
	  (Mpqf.neg c2,m2)::(sub man l1 r2)
	else
	  let c = Mpqf.sub c1 c2 in
	  if (Mpqf.sgn c)<> 0
	  then (c,m1)::(sub man r1 r2)
	  else sub man r1 r2

  let rec mul_monomial man m1 m2 = match (m1,m2) with
    | ([],l)
    | (l,[]) ->
	l
    | ((v1,n1)::r1,(v2,n2)::r2) ->
	let cmp = man.compare v1 v2 in
	if cmp<0 then (v1,n1)::(mul_monomial man r1 m2)
	else if cmp>0 then (v2,n2)::(mul_monomial man m1 r2)
	else (v1,n1+n2)::(mul_monomial man r1 r2)

  let scale man ((coeff,mon):Mpqf.t*'a monomial) (l:'a t) =
    if (Mpqf.sgn coeff) = 0 then []
    else begin
      let res =
	List.map
	  (fun (c,m) -> (Mpqf.mul coeff c, mul_monomial man mon m))
	  l
      in
      normalize man res
    end

  let mul man (l1:'a t) (l2:'a t) =
    List.fold_left
      (begin fun res cm ->
	add man res (scale man cm l2)
      end)
      [] l1

  let div man (l1:'a t) (l2:'a t) =
    match l2 with
    | [] -> failwith "Num.Poly.div: division by zero"
    | [(c,m)] ->
	let res = scale man (c,List.map (fun (v,n) -> (v,-n)) m) l1 in
	normalize_full man res
    | _ -> raise Exit

  let normalize_as_constraint l =
    let (cst,l) = match l with
      | [] -> raise (Constant 0)
      | (c,[])::r ->
	  if r=[] then
	    raise (Constant (Mpqf.sgn c))
	  else
	    (c,r)
      | _ as l -> (Mpqf.of_int 0, l)
    in
    let (factor,nl) = reduce_list l in
    if Mpqf.sgn cst=0 then
      nl
    else
      (Mpqf.mul factor cst,[])::nl

end

(*  ==================================================================== *)
(** {4 Tree expressions} *)
(*  ==================================================================== *)

module Tree = struct
  type unop = Apron.Texpr1.unop =
	      | Neg
	      | Cast
	      | Sqrt
  (** Binary operators *)
  type binop = Apron.Texpr1.binop =
	       | Add
	       | Sub
	       | Mul
	       | Div
	       | Mod
	       | Pow
  (** Destination type for rounding *)
  type typ = Apron.Texpr1.typ =
	     | Real
	     | Int
	     | Single
	     | Double
	     | Extended
	     | Quad
  (** Rounding direction *)
  type round = Apron.Texpr1.round =
	       | Near
	       | Zero
	       | Up
	       | Down
	       | Rnd

  type 'a t =
    | Cst of Apron.Coeff.t
    | Var of 'a
    | Unop of unop * 'a t * typ * round
    | Binop of binop * 'a t * 'a t * typ * round

  let is_zero = function
    | Cst(coeff) when Apron.Coeff.is_zero coeff -> true
    | _ -> false

  let equal_int t b = match t with
    | Cst(coeff) when Apron.Coeff.equal_int coeff b -> true
    | _ -> false

  let is_exact = function
    | Cst _ | Var _ -> true
    | Unop(Neg,_,_,_) -> true
    | Unop(_,_,Real,_) -> true
    | Binop(_,_,_,Real,_) -> true
    | Binop(Add,_,_,Int,_) -> true
    | Binop(Sub,_,_,Int,_) -> true
    | Binop(Mul,_,_,Int,_) -> true
    | _ -> false

  let negate = function
    | Unop(Neg,e,_,_) -> e
    | _ as e -> Unop(Neg,e,Real,Rnd)

  let add ?(typ=Real) ?(round=Rnd) e1 e2 =
    if is_zero e1 then e2
    else if is_zero e2 then e1
    else Binop(Add,e1,e2,typ,round)

  let sub ?(typ=Real) ?(round=Rnd) e1 e2 =
    if is_zero e1 then negate e2
    else if is_zero e2 then e1
    else Binop(Sub,e1,e2,typ,round)

  let mul ?(typ=Real) ?(round=Rnd) e1 e2 =
    if is_zero e1 then e1
    else if is_zero e2 then e2
    else if equal_int e1 1 then e2
    else if equal_int e2 1 then e1
    else if equal_int e1 (-1) then negate e2
    else if equal_int e2 (-1) then negate e1
    else Binop(Mul,e1,e2,typ,round)

  let div ?(typ=Real) ?(round=Rnd) e1 e2 =
    if equal_int e2 1 then e1
    else if equal_int e2 (-1) then negate e1
    else Binop(Div,e1,e2,typ,round)

  let rec support man = function
    | Cst _ -> (PSette.empty man.compare)
    | Var(var) -> PSette.singleton man.compare var
    | Unop(op,e,_,_) -> support man e
    | Binop(op,e1,e2,_,_) -> PSette.union (support man e1) (support man e2)

  let substitute_by_var e (substitution:('a,'a) PMappe.t) =
    let rec parcours = function
    | Cst _ as x -> x
    | Var(var) as x ->
	begin
	  try Var(PMappe.find var substitution)
	  with Not_found -> x
	end
    | Unop(op,e,t,r) -> Unop(op,parcours e, t,r)
    | Binop(op,e1,e2,t,r) -> Binop(op,parcours e1, parcours e2, t, r)
    in
    parcours e

  let rec of_expr man = function
    | Apron.Texpr1.Cst x -> Cst x
    | Apron.Texpr1.Var(var) ->
	Var(man.unmarshal (Apron.Var.to_string var))
    | Apron.Texpr1.Unop(op,e,t,r) ->
	Unop(op,(of_expr man e), t,r)
    | Apron.Texpr1.Binop(op,e1,e2,t,r) ->
	Binop(op,(of_expr man e1),(of_expr man e2), t,r)

  let rec to_expr man = function
    | Cst x -> Apron.Texpr1.Cst x
    | Var(var) ->
	Apron.Texpr1.Var(Apron.Var.of_string (man.marshal var))
    | Unop(op,e,t,r) ->
	Apron.Texpr1.Unop(op,(to_expr man e), t,r)
    | Binop(op,e1,e2,t,r) ->
	Apron.Texpr1.Binop(op,(to_expr man e1),(to_expr man e2), t,r)

  let rec print man fmt expr =
    let precedence_of_expr = function
      | Cst _
      | Var _ -> 5
      | Unop(op,_,_,_) -> Apron.Texpr0.print_precedence_of_unop op
      | Binop(op,_,_,_,_) -> Apron.Texpr0.print_precedence_of_binop op
    in
    match expr with
    | Cst x -> Apron.Coeff.print fmt x
    | Var x -> man.print fmt x
    | Unop(op,e,typ,round) ->
	let prec = Apron.Texpr0.print_precedence_of_unop op in
	let prec1 = precedence_of_expr e in
	let par = prec1<=prec in
	Format.fprintf fmt "%s%s%a%s"
	  (Apron.Texpr0.print_sprint_unop op typ round)
	  (if par then "(" else "")
	  (print man) e
	  (if par then ")" else "")
    | Binop(op,e1,e2,typ,round) ->
	let prec = Apron.Texpr0.print_precedence_of_binop op in
	let prec1 = precedence_of_expr e1 in
	let prec2 = precedence_of_expr e2 in
	let par1 = prec1<prec in
	let par2 = prec2<=prec in
	Format.fprintf fmt "%s%a%s %s %s%a%s"
	  (if par1 then "(" else "")
	  (print man) e1
	  (if par1 then ")" else "")
	  (Apron.Texpr0.print_sprint_binop op typ round)
	  (if par2 then "(" else "")
	  (print man) e2
	  (if par2 then ")" else "")

  let rec compare man x y =
    let rec compare x y =
      match (x,y) with
      | (Cst x), (Cst y) -> Apron.Coeff.cmp x y
      | (Cst _), _ -> -1
      | (Var _), (Cst _) -> 1
      | (Var x), (Var y) ->
	let cmp = man.compare x y in
	if cmp<0 then -2 else if cmp>0 then 2 else 0
      | (Var _), _ -> -1
      | (Unop _), (Cst _)
      | (Unop _), (Var _) -> 1
      | (Unop(op1,e1,t1,r1),Unop(op2,e2,t2,r2)) ->
	  let res = Pervasives.compare (op1,t1,r1) (op2,t2,r2) in
	  if res<>0 then
	    res
	  else
	    compare e1 e2
      | (Unop _), (Binop _) -> -1
      | (Binop(op1,ea1,eb1,t1,r1),Binop(op2,ea2,eb2,t2,r2)) ->
	  let res = Pervasives.compare (op1,t1,r1) (op2,t2,r2) in
	  if res<>0 then res
	  else
	    let res = compare ea1 ea2 in
	    if res<>0 then res
	    else
	      compare eb1 eb2
      | (Binop _), _ -> 1
    in
    let res = compare x y in
    if res>0 then 2 else if res<0 then -2 else 0

end

(*  ==================================================================== *)
(** {4 Conversions} *)
(*  ==================================================================== *)

let rec lin_of_poly man (p:'a Poly.t) : 'a Lin.t = match p with
  | (c,m)::r ->
      let lexpr = begin match m with
	| [] -> Lin.cst c
	| [(v,1)] -> Lin.var v
	| _ -> raise Exit
      end
      in
      Lin.add man lexpr (lin_of_poly man r)
  | [] -> Lin.cst (Mpqf.of_int 0)

let rec lin_of_tree man (x:'a Tree.t) : 'a Lin.t =
  if not (Tree.is_exact x) then raise Exit;
  match x with
  | Tree.Cst x -> Lin.cst (mpqf_of_coeff x)
  | Tree.Var x -> Lin.var x
  | Tree.Unop(Tree.Neg,e,t,r) ->
      let l = lin_of_tree man e in
      Lin.negate l
  | Tree.Binop(op,e1,e2,t,r) ->
      let l1 = lin_of_tree man e1 in
      let l2 = lin_of_tree man e2 in
      begin match op with
      | Tree.Add -> Lin.add man l1 l2
      | Tree.Sub -> Lin.sub man l1 l2
      | Tree.Mul ->
	  if l1.Lin.lterm=[] then
	    Lin.scale l1.Lin.cst l2
	  else if l2.Lin.lterm=[] then
	    Lin.scale l2.Lin.cst l1
	  else
	    raise Exit
      | Tree.Div ->
	  if l2.Lin.lterm=[] then
	    Lin.scale (Mpqf.inv l2.Lin.cst) l1
	  else
	    raise Exit
      | Tree.Mod ->
	  if l1.Lin.lterm=[] && l2.Lin.lterm=[] &&
	    (Mpzf.cmp_int (Mpqf.get_den l1.Lin.cst) 1) = 0 &&
	    (Mpzf.cmp_int (Mpqf.get_den l2.Lin.cst) 1) = 0 then
	      Lin.cst
		(Mpqf.of_mpz
		  (Mpzf.gmod
		    (Mpqf.get_num l1.Lin.cst)
		    (Mpqf.get_num l1.Lin.cst)))
	  else
	    raise Exit
      | Tree.Pow -> raise Exit
      end
  | _ -> raise Exit

let rec poly_of_tree man (x:'a Tree.t) : 'a Poly.t =
  if not (Tree.is_exact x) then raise Exit;
  match x with
  | Tree.Cst x -> Poly.cst (mpqf_of_coeff x)
  | Tree.Var x -> Poly.var x
  | Tree.Unop(Tree.Neg,e,t,r) ->
      let l = poly_of_tree man e in
      Poly.negate l
  | Tree.Binop(op,e1,e2,t,r) ->
      let l1 = poly_of_tree man e1 in
      let l2 = poly_of_tree man e2 in
      begin match op with
      | Tree.Add -> Poly.add man l1 l2
      | Tree.Sub -> Poly.sub man l1 l2
      | Tree.Mul -> Poly.mul man l1 l2
      | Tree.Div -> Poly.div man l1 l2
      | _ -> raise Exit
      end
  | _ -> raise Exit

let tree_of_lin (lin:'a Lin.t) : 'a Tree.t =
  List.fold_left
    (begin fun res (c,v) ->
      Tree.add
	res
	(Tree.mul
	  (Tree.Cst (Apron.Coeff.s_of_mpqf c))
	  (Tree.Var v))
    end)
    (Tree.Cst (Apron.Coeff.s_of_mpqf lin.Lin.cst))
    lin.Lin.lterm

let tree_of_poly (poly:'a Poly.t) : 'a Tree.t =
  let tree_of_monomial (mon:'a Poly.monomial) : 'a Tree.t =
    List.fold_left
      (begin fun res (var,exp) ->
	let t = Tree.Var var in
	let res1 = ref res in
	for i=1 to exp do
	  res1 := Tree.mul t !res1;
	done;
	!res1
      end)
      (Tree.Cst (Apron.Coeff.s_of_int 1))
      mon
  in
  List.fold_left
    (begin fun res (c,mon) ->
      Tree.add res
	(Tree.mul
	  (Tree.Cst(Apron.Coeff.s_of_mpqf c))
	  (tree_of_monomial mon))
    end)
    (Tree.Cst (Apron.Coeff.s_of_int 0))
    poly

(*  ********************************************************************** *)
(** {3 General expressions and operations} *)
(*  ********************************************************************** *)

type 'a t =
  | Lin of 'a Lin.t
  | Poly of 'a Poly.t
  | Tree of 'a Tree.t
type 'a expr = 'a t

let is_dependent_on_integer_only typ_of_var expr = match expr with
  | Lin e -> Lin.is_dependent_on_integer_only typ_of_var e
  | Poly e -> Poly.is_dependent_on_integer_only typ_of_var e
  | _ -> false

let print man fmt expr = match expr with
  | Lin x -> Lin.print man fmt x
  | Poly x -> Poly.print man fmt x
  | Tree x -> Tree.print man fmt x

let print_typ fmt (typ:[> typ]) =
  pp_print_string fmt
    begin match typ with
    | `Int -> "int"
    | `Real -> "real"
    | _ -> "unknown type"
    end

let normalize man expr =
  match expr with
  | Lin _ -> expr
  | Poly p ->
      begin try
	Lin(lin_of_poly man p)
      with Exit ->
	expr
      end
  | Tree t ->
      begin try
	let p = poly_of_tree man t in
	begin try
	  let l = lin_of_poly man p in
	  Lin l
	with Exit ->
	  Poly p
	end
      with Exit ->
	expr
      end

let var man typ_of_var x =
  match typ_of_var x with
  | #typ -> Lin(Lin.var x)
  | _ ->
      failwith
      (Print.sprintf "Arith.var: reference %a undeclared or of wrong type in environment"
	man.print x)

let zero = Lin(Lin.zero)
let one = Lin(Lin.one)

let cst (x:Apron.Coeff.t) =
  Lin(Lin.cst (mpqf_of_coeff x))

let to_tree = function
  | Lin l -> tree_of_lin l
  | Poly p -> tree_of_poly p
  | Tree t -> t

let add man ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  try
    begin
      if typ<>Apron.Texpr1.Real then raise Exit;
      match (e1,e2) with
      | (Lin e1, Lin e2) -> Lin(Lin.add man e1 e2)
      | (Poly e1, Poly e2) -> normalize man (Poly(Poly.add man e1 e2))
      | _ -> raise Exit
    end
  with Exit ->
    normalize man (Tree(Tree.Binop(Tree.Add, to_tree e1, to_tree e2,typ,round)))

let sub man ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  try
    begin
      if typ<>Apron.Texpr1.Real then raise Exit;
      match (e1,e2) with
      | (Lin e1, Lin e2) -> Lin(Lin.sub man e1 e2)
      | (Poly e1, Poly e2) -> normalize man (Poly(Poly.sub man e1 e2))
      | _ -> raise Exit
    end
  with Exit ->
    normalize man (Tree(Tree.Binop(Tree.Sub, to_tree e1, to_tree e2,typ,round)))

let mul man ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  try
    begin
      if typ<>Apron.Texpr1.Real then raise Exit;
      begin match typ with
      | Apron.Texpr1.Real -> ()
      | _ -> raise Exit
      end;
      match (e1,e2) with
      | (Lin e1, Lin e2) when e1.Lin.lterm=[] -> Lin(Lin.scale e1.Lin.cst e2)
      | (Lin e1, Lin e2) when e2.Lin.lterm=[] -> Lin(Lin.scale e2.Lin.cst e1)
      | (Poly e1, Poly e2) -> Poly(Poly.mul man e1 e2)
      | _ -> raise Exit
    end
  with Exit ->
    normalize man (Tree(Tree.Binop(Tree.Mul, to_tree e1, to_tree e2,typ,round)))

let div man ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  try begin
    if typ<>Apron.Texpr1.Real then raise Exit;
    match (e1,e2) with
    | (Lin e1, Lin e2) when e2.Lin.lterm=[] -> Lin(Lin.scale (Mpqf.inv e2.Lin.cst) e1)
    | (Poly e1, Poly e2) -> normalize man (Poly(Poly.div man e1 e2))
    | _ -> raise Exit
  end
  with Exit ->
    normalize man (Tree(Tree.Binop(Tree.Div, to_tree e1, to_tree e2,typ,round)))

let gmod man ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  normalize man (Tree(Tree.Binop(Tree.Mod, to_tree e1, to_tree e2,typ,round)))

let negate e = match e with
  | Lin l -> Lin(Lin.negate l)
  | Poly p -> Poly(Poly.negate p)
  | Tree t -> Tree(Tree.Binop(Tree.Sub,(Tree.Cst (Apron.Coeff.s_of_int 0)),t,Apron.Texpr1.Real, Apron.Texpr1.Rnd))

let cast ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
  if typ=Apron.Texpr1.Real then
    e
  else
    Tree(Tree.Unop(Tree.Cast, to_tree e,typ,round))

let sqrt ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
  Tree(Tree.Unop(Tree.Sqrt, to_tree e,typ,round))

let substitute_by_var man e (substitution:('a,'a) PMappe.t) =
  match e with
  | Lin l -> Lin(Lin.substitute_by_var man l substitution)
  | Poly p -> normalize man (Poly(Poly.substitute_by_var man p substitution))
  | Tree t -> normalize man (Tree(Tree.substitute_by_var t substitution))

let support man = function
  | Lin l -> Lin.support man l
  | Poly p -> Poly.support man p
  | Tree t -> Tree.support man t

(* Assume normalized expressions *)
let compare man e1 e2 =  match (e1,e2) with
  | (Lin e1, Lin e2) -> Lin.compare man e1 e2
  | (Lin _,_) -> -2
  | (Poly _, Lin _) -> 2
  | (Poly e1, Poly e2) -> Poly.compare man e1 e2
  | (Poly _, Tree _) -> -2
  | (Tree e1, Tree e2) -> Tree.compare man e1 e2
  | (Tree _, _) -> 2

(* Assume normalized expressions *)
let equal man e1 e2 = (compare man e1 e2)=0

(* Better with normalized expressions *)
let hash man = Hashtbl.hash

(* Assume normalized expressions *)
let normalize_as_constraint expr = match expr with
  | Lin e -> Lin (Lin.normalize_as_constraint e)
  | Poly e -> Poly (Poly.normalize_as_constraint e)
  | _ -> expr

let typ_of_expr typ_of_var expr =
  if is_dependent_on_integer_only typ_of_var expr then `Int else `Real

let of_linexpr1 man linexpr1 =
  Lin(Lin.of_linexpr1 man linexpr1)
let of_linexpr0 man env linexpr0 =
  of_linexpr1 man { Apron.Linexpr1.linexpr0 = linexpr0; Apron.Linexpr1.env=env }
let to_linexpr1 man env expr =
  match expr with
  | Lin e -> Lin.to_linexpr1 man env e
  | _ -> raise (Invalid_argument "Linear expression expected")
let to_linexpr0 man env expr =
  let linexpr1 = to_linexpr1 man env expr in
  linexpr1.Apron.Linexpr1.linexpr0

let of_texpr1 man texpr1 =
  normalize man (Tree(Tree.of_expr man (Apron.Texpr1.to_expr texpr1)))
let of_texpr0 man env texpr0 =
  of_texpr1 man { Apron.Texpr1.texpr0 = texpr0; Apron.Texpr1.env=env }

let to_texpr1 man (env:Apron.Environment.t) expr =
  Apron.Texpr1.of_expr env (Tree.to_expr man (to_tree expr))

let to_texpr0 man (env:Apron.Environment.t) expr =
  let texpr1 = to_texpr1 man env expr in
  texpr1.Apron.Texpr1.texpr0

let to_apron1 man (env:Apron.Environment.t) (expr:'a t) = match expr with
  | Lin e -> `Lin (Lin.to_linexpr1 man env e)
  | _ -> `Tree (to_texpr1 man env expr)
let to_apron0 man (env:Apron.Environment.t) expr = match expr with
  | Lin e -> `Lin (Lin.to_linexpr0 man env e)
  | _ -> `Tree (to_texpr0 man env expr)

let extract_cst expr =
  match expr with
  | Lin e -> e.Lin.cst
  | Poly e ->
      begin match e with
      | (c,[])::_ -> c
      | _ -> Mpqf.of_int 0
      end
  | _ -> raise (Invalid_argument "")

let extract_fstcoeff expr =
  match expr with
  | Lin e ->
      if e.Lin.lterm <> [] then
	fst (List.hd e.Lin.lterm)
      else if Mpqf.sgn e.Lin.cst<>0 then
	e.Lin.cst
      else
	Mpqf.of_int 0
  | Poly lterm ->
      begin match lterm with
      | (_,[])::term::_
      | term::_ ->
	  fst term
      | [] ->
	  Mpqf.of_int 0
      end
  | Tree expr ->
      let rec parcours = function
	| Tree.Cst coeff ->
	    if Apron.Coeff.is_zero coeff then None
	    else begin
	      let sgn =
		match coeff with
		| Apron.Coeff.Scalar scalar -> Apron.Scalar.sgn scalar
		| Apron.Coeff.Interval itv ->
		    let sgn = Apron.Scalar.sgn itv.Apron.Interval.inf in
		    if sgn<>0
		    then sgn
		    else Apron.Scalar.sgn itv.Apron.Interval.sup
	      in
	      assert(sgn<>0);
	      Some(Mpqf.of_int sgn)
	    end
	| Tree.Var _ -> None
	| Tree.Unop(_,e,_,_) -> parcours e
	| Tree.Binop(_,e1,e2,_,_) ->
	    match parcours e1 with
	    | None -> parcours e2
	    | _ as res -> res
      in
      match parcours expr with
      | Some coeff -> coeff
      | None -> Mpqf.of_int 0

let modify_cst expr cst = match expr with
  | Lin e ->
      Lin { Lin.cst = cst; Lin.lterm = e.Lin.lterm }
  | Poly e ->
      let s = Mpqf.sgn cst in
      Poly
	begin match e with
	| (_,[])::r ->
	    if s<>0 then (cst,[])::r else r
	| _ as r ->
	    if s<>0 then (cst,[])::r else r
	end
  | _ -> raise (Invalid_argument "")

module Condition = struct
  type typ = Apron.Tcons1.typ =
    EQ | SUPEQ | SUP | DISEQ | EQMOD of Apron.Scalar.t

  type 'a t = typ * 'a expr

  let print_typ fmt typ =
    pp_print_string fmt (Apron.Lincons0.string_of_typ typ)

  let print man fmt =
    let revcmp = function
      | SUPEQ -> "<=" | SUP -> "<" | (EQ | EQMOD _) -> "=" | DISEQ -> "<>"
    and print_coeff fmt coeff =
      if Mpqf.cmp_int coeff (-1) = 0 then
        Format.pp_print_string fmt "-"
      else if Mpqf.cmp_int coeff 1 <> 0 then
        Mpqf.print fmt coeff
    in function
      (* XXX A very basic filter for readability of basic inequalities: *)
      | typ, Lin { Lin.lterm = [ (coeff, var) ]; Lin.cst = cst } ->
          let c, op, cst =
            if Mpqf.sgn coeff > 0 then
              coeff, Apron.Lincons0.string_of_typ typ, Mpqf.neg cst
            else
              Mpqf.neg coeff, revcmp typ, cst
          in
          fprintf fmt "%a%a%s%a" print_coeff c man.print var op Mpqf.print cst
      | typ, expr ->
          fprintf fmt "%a%a0" (print man) expr print_typ typ

  (** Assume a normalized-as-constraint expression *)
  let normalize typ_of_var (cons:'a t) : [ `Cond of 'a t | `Bool of bool ]
    =
    let (typ,expr) = cons in
    let cons = match typ with
      | EQ | DISEQ ->
	  let coeff = extract_fstcoeff expr in
	  let sgn = Mpqf.sgn coeff in
	  if sgn<0 then
	    (typ,negate expr)
	  else
	    cons
      | _ -> cons
    in
    match expr with
    | Tree _ -> `Cond(cons)
    | _ ->
	if is_dependent_on_integer_only typ_of_var expr then begin
	  let cst = extract_cst expr in
	  let is_integer = (Mpzf.cmp_int (Mpqf.get_den cst) 1)=0 in
	  begin match (typ,is_integer) with
	  | (EQ,false) -> `Bool false
	  | (DISEQ,false) -> `Bool true
	  | (SUPEQ,false)
	  | (SUP,false) ->
	      let
		  ncst = Mpzf.fdiv_q (Mpqf.get_num cst) (Mpqf.get_den cst)
	      in
	      let ncst = Mpqf.of_mpz ncst in
	      `Cond(SUPEQ,modify_cst expr ncst)
	  | (SUP,true) ->
	      let ncst = Mpqf.sub cst (Mpqf.of_int 1) in
	      `Cond(SUPEQ,modify_cst expr ncst)
	  | _ ->
	      `Cond(cons)
	  end
	end
	else
	  `Cond(cons)

  let make typ_of_var typ expr =
    begin try
      let nexpr = normalize_as_constraint expr in
      normalize typ_of_var (typ,nexpr)
    with Constant sgn ->
      begin match typ with
      | EQ -> `Bool(sgn=0)
      | DISEQ -> `Bool(sgn<>0)
      | SUP -> `Bool(sgn>0)
      | SUPEQ -> `Bool(sgn>=0)
      | EQMOD m -> failwith ""
      end
    end

  let negate typ_of_var (cons:'a t)
    =
    let ncons =
      match cons with
      | (EQ,expr) -> `Cond (DISEQ,expr)
      | (DISEQ,expr) -> `Cond (EQ,expr)
      | (SUPEQ,expr) ->
	  normalize typ_of_var (SUP,negate expr)
      | (SUP,expr) ->
	  normalize typ_of_var (SUPEQ,negate expr)
      | (EQMOD _, _) ->
	  failwith ""
    in
    match ncons with
    | `Bool _ -> failwith ""
    | `Cond x -> x

  let support man (t,e) = support man e

  let compare man (t1,e1) (t2,e2) : int
    =
    let sgn = compare man e1 e2 in
    let asgn = abs sgn in
    match (t1,t2) with
    | (EQMOD m1, EQMOD m2) ->
	if asgn=0 then
	  let cmp = Apron.Scalar.cmp m1 m2 in
	  if cmp > 0 then 2 else (-2)
	else
	  (if sgn>0 then 3 else -3)
    | (EQMOD _, _) ->
	3
    | (_, EQMOD _) ->
	-3
    | (_,_) ->
	if asgn >= 2 then
	  (if sgn>0 then 3 else -3)
	else if asgn = 1 then begin
	  match (t1,t2) with
	  | (EQMOD _, _)
	  | (_, EQMOD _) -> failwith ""
	  | (DISEQ,_) -> 2*sgn
	  | (EQ,DISEQ) -> sgn
	  | (_,DISEQ) -> 2*sgn
	  | (_,EQ) -> 2*sgn
	  | _ -> sgn
	end
	else begin
	  if t1=t2 then 0
	  else
	    (* order SUP,EQ,DISEQ,SUPEQ *)
	    begin match (t1,t2) with
	    | (EQMOD _, _)
	    | (_, EQMOD _) -> failwith ""

	    | (SUP,SUPEQ) -> -1
	    | (SUPEQ,SUP) -> 1
	    | (SUP,DISEQ) -> -1
	    | (DISEQ,SUP) -> 1
	    | (SUP,_) -> -2
	    | (_,SUP) -> 2

	    | (EQ,SUPEQ) -> -1
	    | (SUPEQ,EQ) -> 1
	    | (EQ,_) -> -2
	    | (_,EQ) -> 2

	    | (DISEQ,_) -> -2
	    | (_,DISEQ) -> 2
	    | _ -> assert(t1=t2); 0
	    end
	end

  let of_lincons1 man typ_of_var lincons1 =
    let typ = Apron.Lincons1.get_typ lincons1 in
    let linexpr1 = Apron.Lincons1.get_linexpr1 lincons1 in
    let expr = of_linexpr1 man linexpr1 in
    make typ_of_var typ expr
  let of_lincons0 man typ_of_var env lincons0 =
    of_lincons1 man typ_of_var
      { Apron.Lincons1.lincons0=lincons0; Apron.Lincons1.env=env }
  let of_tcons1 man typ_of_var tcons1 =
    let typ = Apron.Tcons1.get_typ tcons1 in
    let texpr1 = Apron.Tcons1.get_texpr1 tcons1 in
    let expr = of_texpr1 man texpr1 in
    make typ_of_var typ expr
  let of_tcons0 man typ_of_var env tcons0 =
    of_tcons1 man typ_of_var
      { Apron.Tcons1.tcons0=tcons0; Apron.Tcons1.env=env }

  let to_lincons1 man env (typ,expr) =
    Apron.Lincons1.make (to_linexpr1 man env expr) typ
  let to_lincons0 man env (typ,expr) =
    Apron.Lincons0.make (to_linexpr0 man env expr) typ
  let to_tcons1 man env (typ,expr) =
    Apron.Tcons1.make (to_texpr1 man env expr) typ
  let to_tcons0 man env (typ,expr) =
    Apron.Tcons0.make (to_texpr0 man env expr) typ

  let to_apron1 man env (typ,expr) =
    match expr with
    | Lin e -> `Lin (to_lincons1 man env (typ,expr))
    | _ -> `Tree (to_tcons1 man env (typ,expr))
  let to_apron0 man env (typ,expr) =
    match expr with
    | Lin e -> `Lin (to_lincons0 man env (typ,expr))
    | _ -> `Tree (to_tcons0 man env (typ,expr))

end
