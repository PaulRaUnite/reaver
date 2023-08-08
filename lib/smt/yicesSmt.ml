(******************************************************************************)
(* YicesSmt *)
(* translation of BDD APRON formulas to SMT formulas *)
(* author: Thomas Gawlitza, Peter Schrammel *)
(* version: 0.9.3 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

open List
open Yices

exception AssumptionDoesNotHold of string

type ctx = { context : context; 
             real_var_decls : (string,var_decl) Hashtbl.t;
             mutable real_type : typ }

type assertion_id = Yices.assertion_id
type ymodel = Yices.model
type yexpr = Yices.expr

type ratio = {num : Big_int.big_int; den : Big_int.big_int}

type value = VBool of bool | VRatio of ratio

type interpretation = (string,value) Hashtbl.t

type expr =
  Const of ratio
| Real of string
| Sum of expr list
| Mul of ratio * expr

let string_of_ratio x = 
  (Big_int.string_of_big_int x.num)^"/"^(Big_int.string_of_big_int x.den)

let rec string_of_expr = function
  Const c  -> string_of_ratio c
| Real x   -> x
| Sum es   -> "(" ^ String.concat " + " (map string_of_expr es) ^ ")"
| Mul(c,e) -> (string_of_ratio c) ^ " * " ^ string_of_expr e

type formula = 
  T 
| F
| Bool of string
| Let of string * formula
| And of formula * formula
| Not of formula
| Or of formula * formula
| Eq of expr * expr
| Gt of expr * expr
| Le of expr * expr

let rec string_of_formula = function
  T          -> "T"
| F          -> "F"
| Bool x     -> x
| Let(x,f)   -> "(" ^ x ^ " = " ^ string_of_formula f ^ ")"
| And(f,g)   -> "(" ^ string_of_formula f ^ " /\\ " ^ string_of_formula g ^ ")"
| Not f      -> "( not " ^ string_of_formula f ^ ")"
| Or (f,g)   -> "(" ^ string_of_formula f ^ " \\/ " ^ string_of_formula g ^ ")"
| Eq (e1,e2) -> "(" ^ string_of_expr e1 ^ " = " ^ string_of_expr e2 ^ ")"
| Gt (e1,e2) -> "(" ^ string_of_expr e1 ^ " > " ^ string_of_expr e2 ^ ")"
| Le (e1,e2) -> "(" ^ string_of_expr e1 ^ " <= " ^ string_of_expr e2 ^ ")"

let rec rename_real_vars_expr mapping = function
  Const _ as c     -> c
| Real n as v      -> if mem_assoc n mapping then Real (assoc n mapping) else v
| Sum es           -> Sum (map (rename_real_vars_expr mapping) es)
| Mul(c,e)         -> Mul(c,rename_real_vars_expr mapping e)

let rec rename_real_vars mapping = function
  T           -> T
| F           -> F
| Bool _ as x -> x
| Let(x,f)  -> Let(x, rename_real_vars mapping f)
| And(f1,f2)  -> And(rename_real_vars mapping f1, rename_real_vars mapping f2)
| Not(f)      -> Not(rename_real_vars mapping f)
| Or(f1,f2)   -> Or(rename_real_vars mapping f1, rename_real_vars mapping f2)
| Eq(f1,f2)   -> Eq(rename_real_vars_expr mapping f1, rename_real_vars_expr mapping f2)
| Le(f1,f2)   -> Le(rename_real_vars_expr mapping f1, rename_real_vars_expr mapping f2)


let cases bool_expr fl fr = Or(And(Not(bool_expr),fl), And(bool_expr,fr))

let bool_let x f = Let (x,f)
(*Or(And(fl,fr), And(Not fl,Not fr))*)

let create_and = fold_left (fun a f -> if a = T then f else And(a,f)) T

let create_or = fold_left (fun a f -> if a = T then f else Or(a,f)) F

let create_sum es = Sum(es)

let mk_real_var ctx x = 
  let dv = 
  match Hashtbl.find_all ctx.real_var_decls x with
    [] -> 
      let dv = mk_var_decl ctx.context x ctx.real_type in
      Hashtbl.replace ctx.real_var_decls x dv;
      dv
  | [dv] -> dv
  in
  mk_var_from_decl ctx.context dv


let bool_of_lbool = function 
  False -> false 
| True -> true 
| Undef -> raise (AssumptionDoesNotHold "Undef")


let ratio_of_yratio x =  {num=Ratio.numerator_ratio x; 
                       den=Ratio.denominator_ratio x}
let yratio_of_ratio x = Ratio.create_ratio x.num x.den
let ratio_of_string x =  
  Format.pp_print_newline Format.std_formatter ();
  match Str.split (Str.regexp_string "/") x with
  |[a;b] -> {num=Big_int.big_int_of_string a;den=Big_int.big_int_of_string b}
  |[x] -> {num=Big_int.big_int_of_string x; den=Big_int.big_int_of_int 1}
  |[] -> assert(false)
  

let string_of_value = function
  |VBool b -> string_of_bool b
  |VRatio r -> string_of_ratio r

let big_int_one = Big_int.big_int_of_int 1
let big_int_minusone = Big_int.big_int_of_int (-1)

let mk_const ctx x = (*mk_num_from_string ctx (string_of_ratio x) *)
  if (Big_int.eq_big_int x.den big_int_one) && 
     (Big_int.is_int_big_int x.num) then 
    mk_num ctx (Big_int.int_of_big_int x.num)
  else if (Big_int.eq_big_int x.den big_int_minusone) && 
          (Big_int.is_int_big_int x.num) then 
    mk_num ctx (-(Big_int.int_of_big_int x.num))
  else mk_num_from_string ctx (string_of_ratio x)

let rec make_yarith_expr ctx = function
  Const c       -> mk_const ctx.context c
| Sum es        -> mk_sum ctx.context (Array.of_list (map (make_yarith_expr ctx) es))
| Mul(c,e)      -> mk_mul ctx.context [| mk_const ctx.context c; make_yarith_expr ctx e |]
| Real v        -> mk_real_var ctx v

let rec make_yexpr ctx = function
  T             -> mk_true ctx.context
| F             -> mk_false ctx.context
| Bool v        -> mk_bool_var ctx.context v
| Let(v,f)     -> mk_eq ctx.context (mk_bool_var ctx.context v) (make_yexpr ctx f)
| And(f1,f2)    -> mk_and ctx.context [| make_yexpr ctx f1; make_yexpr ctx f2 |]
| Not f         -> mk_not ctx.context (make_yexpr ctx f)
| Or(f1,f2)     -> mk_or ctx.context [| make_yexpr ctx f1; make_yexpr ctx f2 |]
| Eq(a1,a2)     -> mk_eq ctx.context (make_yarith_expr ctx a1) (make_yarith_expr ctx a2)
| Gt(a1,a2)     -> mk_gt ctx.context (make_yarith_expr ctx a1) (make_yarith_expr ctx a2)
| Le(a1,a2)     -> mk_le ctx.context (make_yarith_expr ctx a1) (make_yarith_expr ctx a2)

let iter_real_var_decl f ctx =
  Hashtbl.iter f ctx.real_var_decls

let big_int_lcm m n = 
  if m=Big_int.zero_big_int || n=Big_int.zero_big_int then
    Big_int.zero_big_int
  else Big_int.div_big_int 
       (Big_int.abs_big_int (Big_int.mult_big_int m n)) 
       (Big_int.gcd_big_int m n)


let ratio_zero = {num=Big_int.zero_big_int; den=big_int_one}
let ratio_one = {num=big_int_one; den=big_int_one}
let ratio_minusone = {num=big_int_minusone; den=big_int_one}

(* numerical evaluation helpers *)
let ratio_add a b =
  let lcd = big_int_lcm a.den b.den in
  let res = {num=Big_int.add_big_int 
    (Big_int.div_big_int (Big_int.mult_big_int a.num lcd) a.den)
    (Big_int.div_big_int (Big_int.mult_big_int b.num lcd) b.den); 
             den=lcd} in
  res

let ratio_mul a b =
  let n = Big_int.mult_big_int a.num b.num in
  let d = Big_int.mult_big_int a.den b.den in
  let gcd = Big_int.gcd_big_int n d in
  let res = {num=Big_int.div_big_int n gcd; 
             den=Big_int.div_big_int d gcd} in
  res

let ratio_eq a b = 
  Big_int.eq_big_int (Big_int.mult_big_int a.num b.den) 
                     (Big_int.mult_big_int b.num a.den)

let ratio_gt a b = 
  Big_int.gt_big_int (Big_int.mult_big_int a.num b.den) 
                     (Big_int.mult_big_int b.num a.den)

let ratio_le a b = 
  Big_int.le_big_int (Big_int.mult_big_int a.num b.den) 
                     (Big_int.mult_big_int b.num a.den)

let ymodel_to_model ctx m = 
  let ret_val = Hashtbl.create 42 in
  iter_bool_var_decl 
    (fun x -> Hashtbl.add ret_val (get_var_decl_name x) 
                    (VBool (bool_of_lbool (get_value m x))))
    ctx.context;
  iter_real_var_decl 
    (fun v vy -> Hashtbl.add ret_val v
                    (VRatio (ratio_of_yratio (get_ratio_value m vy))))
    ctx;
  ret_val

let compute_model_ctx ctx =
  match check ctx.context with
  | True ->
      let m = get_model ctx.context in
      Some (ymodel_to_model ctx m)
  | False -> None
  | Undef -> raise (AssumptionDoesNotHold "")

let bool_variables i = 
  Hashtbl.fold (fun v _ res -> v::res) i []

let bool_value i var = 
  match Hashtbl.find i var with
    |VBool x -> x
    |_ -> raise Not_found

let create_ctx () = 
  let context = mk_context () in
  { 
    context = context; 
    real_var_decls = Hashtbl.create 10; 
    real_type = mk_type context real_type_name
  }

let reset ctx =
  reset ctx.context;
  Hashtbl.clear ctx.real_var_decls;
  ctx.real_type <- mk_type ctx.context real_type_name;
  ()

let compute_model phi =
  let ctx = create_ctx () in 
  let e = make_yexpr ctx phi in
  assert_simple ctx.context e;
  let m = compute_model_ctx ctx in
  del_context ctx.context;
  m

let compute_model_assumption ctx phi =
  let e = make_yexpr ctx phi in
  let id = assert_retractable ctx.context e in
  let m = compute_model_ctx ctx in
  retract ctx.context id;
  m

let create_ctx phi =
  let ctx = create_ctx () in
  let e = make_yexpr ctx phi in
  assert_simple ctx.context e;
  ctx

let assert_ctx ctx phi =
  let e = make_yexpr ctx phi in
  assert_simple ctx.context e;
  ctx

let assert_retractable_ctx ctx phi =
  let e = make_yexpr ctx phi in
  let id = assert_retractable ctx.context e in
  id

let retract_ctx ctx id =
  retract ctx.context id;
  ctx

let del_ctx ctx = 
  del_context ctx.context

(* instantiates the formula
   (and evaluates constraints)  *)
let rec instantiate_formula model f =
  match f with
  | T -> f  
  | F -> f
  | Bool v -> instantiate_bool_var model v
  | Let (v,f) -> 
  begin
    match instantiate_bool_var model v with
    |Bool v -> Let (v,instantiate_formula model f)
    |T |F -> T (* equality is obsolete *)
  end
  | And (f1,f2) -> 
  begin
    match (instantiate_formula model f1,instantiate_formula model f2) with
      |(T,f) |(f,T) -> f
      |(F,_) |(_,F) -> F
      |(f1,f2) -> And (f1,f2)
  end
  | Not f -> 
  begin
    match instantiate_formula model f with
      |T -> F
      |F -> T
      |Not f -> f
      |f -> Not f
  end
  | Or (f1,f2) ->
  begin
    match (instantiate_formula model f1,instantiate_formula model f2) with
      |(F,f) |(f,F) -> f
      |(T,_) |(_,T) -> T
      |(f1,f2) -> Or (f1,f2)
  end
  | Eq (e1,e2) -> 
  begin
    let e1 = instantiate_expr model e1 in 
    let e2 = instantiate_expr model e2 in
    match (e1,e2) with
      |(Const c1,Const c2) -> if ratio_eq c1 c2 then T else F
      |_ -> Eq (e1,e2)
  end
  | Gt (e1,e2) -> 
  begin
    let e1 = instantiate_expr model e1 in 
    let e2 = instantiate_expr model e2 in
    match (e1,e2) with
      |(Const c1,Const c2) -> if ratio_gt c1 c2 then T else F
      |_ -> Gt (e1,e2)
  end
  | Le (e1,e2) ->
  begin
    let e1 = instantiate_expr model e1 in 
    let e2 = instantiate_expr model e2 in
    match (e1,e2) with
      |(Const c1,Const c2) -> if ratio_le c1 c2 then T else F
      |_ -> Le (e1,e2)
  end
and instantiate_expr model e = 
  match e with
  | Const _ -> e
  | Real v -> instantiate_real_var model v
  | Sum ee -> 
  begin
    let (sum,ee) = List.fold_left
      (fun (sum,ee) e -> 
        let e = instantiate_expr model e in 
        match e with
        | Const c -> (ratio_add sum c,ee)
        |_ -> (sum,e::ee))
      (ratio_zero,[]) ee
    in
    match ee with
      |[] -> Const sum
      |_ -> Sum ((Const sum)::ee)
  end
  | Mul (c1,e) -> 
  begin
    let e = instantiate_expr model e in 
    match e with
      | Const c2 -> Const (ratio_mul c1 c2)
      |_ -> Mul (c1,e)
  end
and instantiate_bool_var model v =
  try
    match Hashtbl.find model v with
      |VBool x -> if x then T else F
      |_ -> assert(false)
  with Not_found -> Bool v
and instantiate_real_var model v =
  try 
    match Hashtbl.find model v with
      |VRatio x -> Const x
      |_ -> assert(false)
  with Not_found -> Real v

let evaluate_in_ymodel ymodel yexpr =
  match evaluate_in_model ymodel yexpr with
  | True -> true
  | False -> false
  | Undef -> raise (AssumptionDoesNotHold "")

let assert_retractable_ctx_yexpr ctx yexpr =
  assert_retractable ctx.context yexpr

let formula_to_yexpr = make_yexpr

let get_ymodel ctx = get_model ctx.context
