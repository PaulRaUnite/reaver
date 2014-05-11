module type Num =
  sig
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
module type NumLat =
  sig
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
    val bot : num
    val top : num
  end
module type FNum =
  sig
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
    val min : num -> num -> num
    val max : num -> num -> num
    val neg : num -> num
  end
module type FNumLat =
  sig
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
    val bot : num
    val top : num
    val min : num -> num -> num
    val max : num -> num -> num
    val neg : num -> num
  end
module NumFunctions :
  functor (N : Num) ->
    sig
      type num = N.num
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
      val min : num -> num -> num
      val max : num -> num -> num
      val neg : num -> num
    end
module NumLatFunctions :
  functor (N : NumLat) ->
    sig
      type num = N.num
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
      val bot : num
      val top : num
      val min : num -> num -> num
      val max : num -> num -> num
      val neg : num -> num
      val ( <. ) : num -> num -> bool
    end
module BigInt : Num
module Float :
  sig
    type num = float
    val ( +. ) : float -> float -> float
    val ( *. ) : float -> float -> float
    val from_int : int -> float
    val to_string : float -> string
    val from_string : string -> float
    val ( =. ) : 'a -> 'a -> bool
    val ( <=. ) : 'a -> 'a -> bool
    val abs : float -> float
    val zero : float
    val one : float
    val minus_one : float
  end
module Ratio :
  sig
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
    val ( >. ) : num -> num -> bool
    val ( <. ) : num -> num -> bool
    val ( /. ) : num -> num -> num
    val ( -. ) : num -> num -> num
    val neg : num -> num
  end
module GmpRatio :
  sig
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
    val ( >. ) : num -> num -> bool
    val ( <. ) : num -> num -> bool
    val ( /. ) : num -> num -> num
    val ( -. ) : num -> num -> num
    val neg : num -> num
  end
module InfNum :
  functor (N : Num) ->
    sig
      type num = Neginfty | Infty | Num of N.num
      val is_zero : num -> bool
      val is_negative : num -> bool
      val ( +. ) : num -> num -> num
      val ( *. ) : num -> num -> num
      val from_int : int -> num
      val to_string : num -> string
      val from_string : string -> num
      val ( =. ) : num -> num -> bool
      val ( <=. ) : num -> num -> bool
      val abs : num -> num
      val bot : num
      val top : num
      val zero : num
      val one : num
      val minus_one : num
    end
module InfRatio :
  sig
    type num = InfNum(Ratio).num = Neginfty | Infty | Num of Ratio.num
    val is_zero : num -> bool
    val is_negative : num -> bool
    val ( +. ) : num -> num -> num
    val ( *. ) : num -> num -> num
    val from_int : int -> num
    val to_string : num -> string
    val from_string : string -> num
    val ( =. ) : num -> num -> bool
    val ( <=. ) : num -> num -> bool
    val abs : num -> num
    val bot : num
    val top : num
    val zero : num
    val one : num
    val minus_one : num
  end
module InfRatioFunctions :
  sig
    type num = InfRatio.num
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
    val bot : num
    val top : num
    val min : num -> num -> num
    val max : num -> num -> num
    val neg : num -> num
    val ( <. ) : num -> num -> bool
  end
module InfGmpRatio :
  sig
    type num = InfNum(GmpRatio).num = Neginfty | Infty | Num of GmpRatio.num
    val is_zero : num -> bool
    val is_negative : num -> bool
    val ( +. ) : num -> num -> num
    val ( *. ) : num -> num -> num
    val from_int : int -> num
    val to_string : num -> string
    val from_string : string -> num
    val ( =. ) : num -> num -> bool
    val ( <=. ) : num -> num -> bool
    val abs : num -> num
    val bot : num
    val top : num
    val zero : num
    val one : num
    val minus_one : num
  end
module InfGmpRatioFunctions :
  sig
    type num = InfGmpRatio.num
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
    val bot : num
    val top : num
    val min : num -> num -> num
    val max : num -> num -> num
    val neg : num -> num
    val ( <. ) : num -> num -> bool
  end
module InfFloat :
  sig
    type num = InfNum(Float).num = Neginfty | Infty | Num of Float.num
    val is_zero : num -> bool
    val is_negative : num -> bool
    val ( +. ) : num -> num -> num
    val ( *. ) : num -> num -> num
    val from_int : int -> num
    val to_string : num -> string
    val from_string : string -> num
    val ( =. ) : num -> num -> bool
    val ( <=. ) : num -> num -> bool
    val abs : num -> num
    val bot : num
    val top : num
    val zero : num
    val one : num
    val minus_one : num
  end
module InfFloatFunctions :
  sig
    type num = InfFloat.num
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
    val bot : num
    val top : num
    val min : num -> num -> num
    val max : num -> num -> num
    val neg : num -> num
    val ( <. ) : num -> num -> bool
  end
module InfBigInt :
  sig
    type num = InfNum(BigInt).num = Neginfty | Infty | Num of BigInt.num
    val is_zero : num -> bool
    val is_negative : num -> bool
    val ( +. ) : num -> num -> num
    val ( *. ) : num -> num -> num
    val from_int : int -> num
    val to_string : num -> string
    val from_string : string -> num
    val ( =. ) : num -> num -> bool
    val ( <=. ) : num -> num -> bool
    val abs : num -> num
    val bot : num
    val top : num
    val zero : num
    val one : num
    val minus_one : num
  end
module InfBigIntFunctions :
  sig
    type num = InfBigInt.num
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
    val bot : num
    val top : num
    val min : num -> num -> num
    val max : num -> num -> num
    val neg : num -> num
    val ( <. ) : num -> num -> bool
  end
module type X = sig type x val string_of_x : x -> string end
module StringX : sig type x = string val string_of_x : 'a -> 'a end
module ExprProp :
  functor (R : Num) ->
    functor (X : X) ->
      sig
        type x = X.x
        val string_of_x : x -> string
        type num = R.num
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
        type op = Add | Mul
        val string_of_op : op -> string
        val function_of_op : op -> num -> num -> num
        val neutral_element_of_op : op -> num
        type expr = Const of num | Var of x | Expr of op * expr list
        val expr_const : num -> expr
        val expr_var : x -> expr
        val expr_add : expr list -> expr
        val expr_mul : expr list -> expr
        val is_op_expr : op -> expr -> bool
        val contains_vars : expr -> bool
        val expr_partial_eval : expr -> expr
        val substitute : (x -> expr) -> expr -> expr
        val add_monom : ('a * num) list -> 'a -> num -> ('a * num) list
        type lin_expr = num * (x * num) list
        val lin_expr_of_expr : expr -> num * (x * num) list
        val expr_of_lin_expr : num * (x * num) list -> expr
        val expr_linearize : expr -> expr
        val string_of_expr : expr -> string
        type rel_sym = Eq | Leq
        val string_of_rel_sym : rel_sym -> string
        type atom = expr * rel_sym * expr
        val atom_partial_eval : expr * 'a * expr -> expr * 'a * expr
        val atom_linearize : expr * 'a * expr -> expr * 'a * expr
        val atom_split_into_ineqs :
          expr * rel_sym * expr -> (expr * rel_sym * expr) list
        val atom_eq : 'a -> 'b -> 'a * rel_sym * 'b
        val atom_leq : 'a -> 'b -> 'a * rel_sym * 'b
        val atom_geq : 'a -> 'b -> 'b * rel_sym * 'a
        val atom_rel : string -> 'a -> 'a -> 'a * rel_sym * 'a
        val atom_true : expr * rel_sym * expr
        val atom_false : expr * rel_sym * expr
        val string_of_atom : expr * rel_sym * expr -> string
        type conj = atom list
        val conj_partial_eval :
          (expr * 'a * expr) list -> (expr * 'a * expr) list
        val conj_linearize :
          (expr * 'a * expr) list -> (expr * 'a * expr) list
        val conj_split_into_ineqs :
          (expr * rel_sym * expr) list -> (expr * rel_sym * expr) list
        val string_of_conj : (expr * rel_sym * expr) list -> string
        val conj_true : 'a list
        type pform = Atom of atom | And of pform list | Or of pform list
        val pform_simplify_atoms : (atom -> atom) -> pform -> pform
        val pform_partial_eval : pform -> pform
        val pform_linearize : pform -> pform
        val pform_atom : atom -> pform
        val pform_true : pform
        val pform_false : pform
        val pform_and : pform list -> pform
        val pform_or : pform list -> pform
        val string_of_pform : pform -> string
        type assignment = (x * expr) list
        val fill_assignment_aux :
          ('a * 'b) list -> 'a list -> ('a * 'b) list -> ('a * 'b) list
        val fill_assignment : (x * expr) list -> x list -> (x * expr) list
        val assignment_partial_eval : 'a * expr -> 'a * expr
        val assignment_linearize : ('a * expr) list -> ('a * expr) list
        val assignment_id : 'a list * 'b list
        val string_of_assignment : (x * expr) list -> string
      end
