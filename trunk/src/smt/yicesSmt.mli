type ctx = { context : Yices.context; 
             real_var_decls : (string,Yices.var_decl) Hashtbl.t;
             mutable real_type : Yices.typ }

type assertion_id = Yices.assertion_id
type ymodel = Yices.model
type yexpr = Yices.expr

type ratio = {num : Big_int.big_int; den : Big_int.big_int}

val  ratio_zero : ratio
val  ratio_one : ratio
val  ratio_minusone : ratio
val  ratio_of_string : string -> ratio
val  string_of_ratio : ratio -> string

type expr =
    Const of ratio
  | Real of string
  | Sum of expr list
  | Mul of ratio * expr

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

val cases : formula -> formula -> formula -> formula

val bool_let : string -> formula -> formula

val create_and : formula list -> formula

val create_or : formula list -> formula

val create_sum : expr list -> expr

type value = VBool of bool | VRatio of ratio

type interpretation = (string,value) Hashtbl.t

val bool_variables : interpretation -> string list

val bool_value : interpretation -> string -> bool

(** 
 Computes a model of the formula phi
 @param phi the formula
 *)
val compute_model : formula -> interpretation option

val compute_model_ctx : ctx -> interpretation option

val compute_model_assumption : ctx -> formula -> interpretation option

val rename_real_vars_expr : (string * string) list -> expr -> expr

val rename_real_vars : (string * string) list -> formula -> formula

val string_of_formula : formula -> string

val string_of_expr : expr -> string

val string_of_value : value -> string

val assert_ctx : ctx -> formula -> ctx

val assert_retractable_ctx : ctx -> formula -> assertion_id

val retract_ctx : ctx -> assertion_id -> ctx

val create_ctx : formula -> ctx

val del_ctx : ctx -> unit

val instantiate_formula : interpretation -> formula -> formula

val evaluate_in_ymodel : ymodel -> yexpr -> bool

val assert_retractable_ctx_yexpr : ctx -> yexpr -> assertion_id

val formula_to_yexpr : ctx -> formula -> yexpr

val ymodel_to_model : ctx -> ymodel -> interpretation

val get_ymodel :  ctx -> ymodel
