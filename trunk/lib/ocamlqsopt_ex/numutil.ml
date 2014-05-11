module type Num = sig
	type num
	val ( +. ) : num -> num -> num
	val ( *. ) : num -> num -> num
	val from_int : int -> num
	val to_string : num -> string
	val from_string : string -> num
	val ( =. ) : num -> num -> bool
	val ( <=. ) : num -> num -> bool
	val abs : num -> num
	val zero : num
	val one : num
	val minus_one : num
end

module type NumLat = sig
	include Num
	val bot : num
	val top : num
end

module type FNum = sig
	include Num
	val min : num -> num -> num
	val max : num -> num -> num
	val neg : num -> num	
end

module type FNumLat = sig
	include NumLat
	val min : num -> num -> num
	val max : num -> num -> num	
	val neg : num -> num
end

module NumFunctions(N:Num) = struct
	include N
	let min a b = if a <=. b then a else b
	let max a b = if b <=. a then a else b
	let neg b = minus_one *. b
end

module NumLatFunctions(N:NumLat) = struct
	include N 
	let min a b = if a <=. b then a else b
	let max a b = if b <=. a then a else b
	let neg b = minus_one *. b
        let ( <. ) x y = x <=. y && (not (x =. y))
end

module BigInt : Num = struct
	open Big_int

	type num = big_int
	let ( +. ) = add_big_int
	let ( *. ) = mult_big_int
	let from_int = big_int_of_int
	let to_string = string_of_big_int
	let from_string = big_int_of_string
	let ( =. ) = eq_big_int
	let ( <=. ) = le_big_int
	let abs = abs_big_int
	let zero = from_int 0
	let one = from_int 1
	let minus_one = from_int (-1)
end

module Float = struct
  type num = float 
  let ( +. ) = ( +. ) 
  let ( *. ) = ( *. )
  let from_int = float_of_int
  let to_string = string_of_float
  let from_string = float_of_string
  let ( =. ) = ( = ) 
  let ( <=. ) = ( <= )
  let abs = abs_float 
  let zero = 0.
  let one = 1.
  let minus_one = -1.
end

module Ratio : sig
	include Num
	val ( >. ) : num -> num -> bool
	val ( <. ) : num -> num -> bool
	val ( /. ) : num -> num -> num 
	val ( -. ) : num -> num -> num
	val neg : num -> num 
end = struct
	open Ratio 

	type num = ratio 
	let ( +. ) = add_ratio
	let ( -. ) = sub_ratio
	let ( *. ) = mult_ratio
	let ( /. ) = div_ratio
	let from_int = ratio_of_int
	let from_string = ratio_of_string
	let ( =. ) = eq_ratio
	let ( <=. ) = le_ratio
	let ( >. ) = gt_ratio
	let ( <. ) = lt_ratio
	let abs = abs_ratio
	let zero = from_int 0
	let one = from_int 1
	let minus_one = from_int (-1)
	let neg x = minus_one *. x
	let to_string = string_of_ratio 
end

module GmpRatio : sig
	include Num
	val ( >. ) : num -> num -> bool
	val ( <. ) : num -> num -> bool
	val ( /. ) : num -> num -> num 
	val ( -. ) : num -> num -> num
	val neg : num -> num 
end = struct
	open Mpqf 

	type num = t 
	let ( +. ) = add
	let ( -. ) = sub
	let ( *. ) = mul
	let ( /. ) = div
	let from_int = of_int
	let from_string = of_string
	let ( =. ) = ( = )
	let ( <=. ) = ( <= )
	let ( >. ) = ( > )
	let ( <. ) = ( < )
	let abs = abs
	let zero = from_int 0
	let one = from_int 1
	let minus_one = from_int (-1)
	let neg x = minus_one *. x
	let to_string = to_string 
end 

module InfNum (N:Num) = struct
	open N
	
	type num = Neginfty | Infty | Num of N.num

	let is_zero = function
		Neginfty | Infty -> false
	  | Num x -> x =. zero

	let is_negative = function
		Neginfty -> true
	  | Infty -> false
	  | Num x -> not (zero <=. x)
	
	let ( +. ) a b =
		match (a,b) with
			(Neginfty,_) | (_,Neginfty) -> Neginfty
		  |	(Infty,_) | (_,Infty) -> Infty
		  | (Num a, Num b) -> Num(a +. b)
	
	let ( *. ) a b =
		match (a,b) with
			(Neginfty,x) | (x,Neginfty) ->
				(* let _ = assert (not (is_zero x)) in *)
				if is_negative x then
					Infty
				else if is_zero x then
					Num zero
				else
					Neginfty 
		  |	(Infty,x) | (x,Infty) ->
				(* let _ = assert (not (is_zero x)) in *)
				if is_negative x then
					Neginfty
				else if is_zero x then
					Num zero
				else
					Infty
		  | (Num a, Num b) -> Num(a *. b)

	let from_int x = Num(from_int x) 

	let to_string = function
		Neginfty -> "--"
	  | Infty -> "++"
	  | Num a -> to_string a

	let from_string s =
		match s with
			"--" -> Neginfty
		  | "++" -> Infty
		  | s -> Num(from_string s)

	let ( =. ) a b =
		match (a,b) with
			(Neginfty,Neginfty) | (Infty,Infty) -> true
		  | (Num a, Num b) -> (a =. b)
		  | _ -> false

	let ( <=. ) a b =
		match (a,b) with
			(Neginfty,_) | (_,Infty) -> true
		  | (Num a, Num b) -> a <=. b
		  | _ -> false

	let abs = function
		Neginfty -> Neginfty
	  | Infty -> Infty
	  | Num a -> Num(abs a)

	let bot = Neginfty
	let top = Infty
	let zero = Num(zero)
	let one = Num(one)
	let minus_one = Num(minus_one)
end

module InfRatio = InfNum(Ratio)
module InfRatioFunctions = NumLatFunctions(InfRatio)

module InfGmpRatio = InfNum(GmpRatio)
module InfGmpRatioFunctions = NumLatFunctions(InfGmpRatio)

module InfFloat = InfNum(Float)
module InfFloatFunctions = NumLatFunctions(InfFloat)

module InfBigInt = InfNum(BigInt)
module InfBigIntFunctions = NumLatFunctions(InfBigInt)


module type X = sig
  type x
  val string_of_x : x -> string
end

module StringX = struct
  type x = string
  let string_of_x x = x
end

module ExprProp(R:Num)(X:X) = struct
  include X
  include R

  open List

  type op = Add | Mul

  let string_of_op = function
    Add -> "+"
  | Mul -> "*"

  let function_of_op = function 
    Add -> ( +. )
  | Mul -> ( *. )

  let neutral_element_of_op = function
    Add -> zero
  | Mul -> one

  type expr = 
    Const of num 
  | Var of x  
  | Expr of op * expr list

  let expr_const num = Const num
  let expr_var x     = Var x
  let expr_add es    = Expr(Add,es)
  let expr_mul es    = Expr(Mul,es)

  let is_op_expr op = function 
    Expr(op',_) when op = op' -> true
  | _                         -> false

  let rec contains_vars = function
    Var _      -> true
  | Expr(_,es) -> exists contains_vars es
  | _          -> false

  let rec expr_partial_eval = function
    Const _ as c -> c
  | Var _ as x   -> x
  | Expr(op,es) ->
      let es = map expr_partial_eval es in
      let (cs,es) = partition (function Const _ -> true | _ -> false) es in
      let sum_cs = fold_left (function_of_op op) (neutral_element_of_op op) (map (function Const n -> n) cs) in 
      match es with
        [] -> Const sum_cs
      | _  -> Expr(op, Const(sum_cs) :: es)

  let rec substitute subst = function 
    Const(_) as c -> c
  | Var x         -> subst x
  | Expr(op,es)   -> Expr(op, map (substitute subst) es)

  let add_monom lin x coeff = 
    if mem_assoc x lin then
      let coeff' = assoc x lin in
      (x,coeff +. coeff') :: remove_assoc x lin
    else
      (x,coeff) :: lin

  type lin_expr = num * (x * num) list  

  let rec lin_expr_of_expr coeff (const,lin) = function
    Const n                -> (const +. (coeff *. n), lin)
  | Var x                  -> (const, add_monom lin x coeff)
  | Expr(Add,es)           -> List.fold_left (lin_expr_of_expr coeff) (const,lin) es
  | Expr(Mul,[Const c; es]) -> lin_expr_of_expr (c *. coeff) (const,lin) es
      

  let lin_expr_of_expr = lin_expr_of_expr one (zero,[])

  let expr_of_lin_expr (c,x_c_list) = 
    let lin = map (fun (x,c) -> Expr(Mul,[Const c; Var x])) x_c_list in
    Expr(Add, (Const c) :: lin)

  let expr_linearize e = expr_of_lin_expr (lin_expr_of_expr (expr_partial_eval e))

  let rec string_of_expr = function
    Const n       -> to_string n
  | Var x         -> string_of_x x
  | Expr(op,args) -> "(" ^ String.concat (" " ^ string_of_op op ^ " ") (map string_of_expr args) ^ ")"

  type rel_sym = Eq | Leq 

  let string_of_rel_sym = function
    Eq  -> "="
  | Leq -> "<="

  type atom = expr * rel_sym * expr

  let atom_partial_eval (e1, rel_sym, e2) = 
    (expr_partial_eval e1, rel_sym, expr_partial_eval e2)

  let atom_linearize (e1,rel_sym,e2) = 
    let e1 = expr_linearize e1 in
    let e2 = expr_linearize e2 in
    let Expr(Add, c1 :: e1) = e1 in
    let Expr(Add, c2 :: e2) = e2 in
    let e1 = Expr(Add, Expr(Mul, [Const minus_one; Expr(Add, e2)]) :: e1) 
    and e2 = Expr(Add, [Expr(Mul, [Const minus_one; c1]); c2]) in
    let e1 = expr_linearize e1 in
    let e2 = expr_linearize e2 in
    (e1, rel_sym, e2)

  let atom_split_into_ineqs = function 
    (e1, Eq, e2) -> [(e1, Leq, e2); (Expr(Mul,[Const minus_one; e1]), Leq, Expr(Mul,[Const minus_one; e2]))]
  | ineq         -> [ineq]

  let atom_eq  e1 e2 = (e1, Eq, e2)
  let atom_leq e1 e2 = (e1, Leq, e2)
  let atom_geq e1 e2 = atom_leq e2 e1

  let atom_rel = function
    "="  -> atom_eq 
  | "<=" -> atom_leq
  | ">=" -> atom_geq

  let atom_true  = atom_eq (expr_const zero) (expr_const zero)
  let atom_false = atom_eq (expr_const zero) (expr_const one)

  let string_of_atom (e1,r,e2) = 
    string_of_expr e1 ^ " " ^ 
    string_of_rel_sym r ^ " " ^ 
    string_of_expr e2

  type conj = atom list

  let conj_partial_eval conj = map atom_partial_eval conj
  
  let conj_linearize conj = map atom_linearize conj

  let conj_split_into_ineqs conj = flatten (map atom_split_into_ineqs conj)

  let string_of_conj conj = 
    "{" ^ String.concat ", " (map string_of_atom conj) ^ "}"
  
  let conj_true = []

  type pform = 
    Atom of atom
  | And  of pform list
  | Or   of pform list

  let rec pform_simplify_atoms simplify pform =
    let f = pform_simplify_atoms simplify in
    match pform with
      Atom a -> Atom(simplify a)
    | And fs -> And(map f fs)
    | Or  fs -> Or(map f fs)

  let rec pform_partial_eval = pform_simplify_atoms atom_partial_eval
  
  let rec pform_linearize = pform_simplify_atoms atom_linearize

  let pform_atom a = Atom a
  let pform_true = pform_atom atom_true
  let pform_false = pform_atom atom_false
  let pform_and pforms = And pforms
  let pform_or pforms = Or pforms

  let rec string_of_pform = function
    Atom a -> string_of_atom a
  | And fs -> "(" ^ String.concat " /\\ " (map string_of_pform fs) ^ ")"
  | Or  fs -> "(" ^ String.concat " \\/ " (map string_of_pform fs) ^ ")"

  type assignment = (x * expr) list

  let rec fill_assignment_aux ass vars acc =
    match vars with
      [] -> acc
    | v::vs -> try (v, assoc v ass)::(fill_assignment_aux ass vs acc) with Not_found -> fill_assignment_aux ass vs acc

  let fill_assignment ass vars = (* fill_assignment_aux ass vars [] *)
    map 
      (
        fun var -> 
          try 
            (var, assoc var ass)
          with Not_found -> (var, Var(var) )
      ) 
      vars

  let assignment_partial_eval (x,e) = (x, expr_partial_eval e)

  let assignment_linearize ass = 
    map (fun (x,e) -> (x, expr_linearize e)) ass

  let assignment_id = ([],[])

  let string_of_assignment (xs,es) = 
    "<" ^ String.concat ", " (map string_of_x xs) ^ ">" ^ 
    " := " ^ 
    "<" ^ String.concat ", " (map string_of_expr es) ^ ">"

  let string_of_assignment ass = 
    string_of_assignment (split ass)
end
