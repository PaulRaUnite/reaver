val split : ('a * 'b) list -> 'a list * 'b list
exception Todo
exception Unsup
module type X = sig type x val string_of_x : x -> string end
module type LP =
  sig
    type x
    type expr =
        Const of Numutil.GmpRatio.num
      | Var of x
      | Mul of Numutil.GmpRatio.num * expr
      | Sum of expr list
    type obj = Min of expr | Max of expr
    type constr = Eq of expr * expr | Le of expr * expr
    type constrs = constr list
    type normal_lin_expr = (Numutil.GmpRatio.num * x) list
    type normal_expr = Numutil.GmpRatio.num * normal_lin_expr
    type normal_constr =
        NEq of normal_lin_expr * Numutil.GmpRatio.num
      | NLe of normal_lin_expr * Numutil.GmpRatio.num
    type lp = { prob_name : string; obj : obj; constrs : constr list; }
    type sol =
        Numutil.InfGmpRatio.num *
        (x -> Numutil.GmpRatio.num)
    val transform_into_max_obj : obj -> obj
    val contains_var : expr -> bool
    val new_prob_name : unit -> string
    val ( ++. ) : expr -> expr -> expr
    val ( **. ) : Numutil.GmpRatio.num -> expr -> expr
    val expr_neg : expr -> expr
    val ( --. ) : expr -> expr -> expr
    val ( ==. ) : expr -> expr -> constr
    val ( <=. ) : expr -> expr -> constr
    val ( >=. ) : expr -> expr -> constr
    val normal_expr : expr -> normal_expr
    val normal_constr : constr -> normal_constr
    val string_of_x : x -> string
    val string_of_normal_constr : normal_constr -> string
    val string_of_expr : expr -> string
    val string_of_obj : obj -> string
    val string_of_constr : constr -> string
    val string_of_constrs : constr list -> string
    val string_of_lp : lp -> string
    val solve : lp -> sol
    val solve' : string -> constrs -> obj -> sol
  end
module Make :
  functor (X : X) ->
    sig
      type x = X.x
      val string_of_x : x -> string
      type expr =
          Const of Numutil.GmpRatio.num
        | Var of x
        | Mul of Numutil.GmpRatio.num * expr
        | Sum of expr list
      type obj = Min of expr | Max of expr
      type constr = Eq of expr * expr | Le of expr * expr
      type constrs = constr list
      type lp = { prob_name : string; obj : obj; constrs : constr list; }
      type sol =
          Numutil.InfGmpRatio.num *
          (x -> Numutil.GmpRatio.num)
      exception Var_not_found of x
      val prob_id : int ref
      val new_prob_name : unit -> string
      val ( ++. ) : expr -> expr -> expr
      val ( **. ) : Numutil.GmpRatio.num -> expr -> expr
      val expr_neg : expr -> expr
      val ( --. ) : expr -> expr -> expr
      val ( ==. ) : expr -> expr -> constr
      val ( <=. ) : expr -> expr -> constr
      val ( >=. ) : expr -> expr -> constr
      val vars_of_expr : expr -> x list
      val contains_var : expr -> bool
      val string_of_expr : expr -> string
      val string_of_obj : obj -> string
      val string_of_constr : constr -> string
      val string_of_constrs : constr list -> string
      val string_of_lp : lp -> string
      val transform_into_max_obj : obj -> obj
      val transform_into_max : lp -> lp
      val normal_expr :
        expr ->
        Numutil.GmpRatio.num *
        (Numutil.GmpRatio.num * x) list
      type normal_lin_expr = (Numutil.GmpRatio.num * x) list
      type normal_expr = Numutil.GmpRatio.num * normal_lin_expr
      type normal_constr =
          NEq of normal_lin_expr * Numutil.GmpRatio.num
        | NLe of normal_lin_expr * Numutil.GmpRatio.num
      val string_of_normal_constr : normal_constr -> string
      val normal_constr : constr -> normal_constr
      val is_bound : normal_constr -> bool
      val is_non_trivial : normal_constr -> bool
      val is_trivially_unsat : normal_constr -> bool
      val ( ++ ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
      val is_min_lp : lp -> bool
      val is_max_lp : lp -> bool
      val solve :
        lp ->
        Numutil.InfGmpRatio.num *
        (x -> Numutil.GmpRatio.num)
      val solve' :
        string ->
        constr list ->
        obj ->
        Numutil.InfGmpRatio.num *
        (x -> Numutil.GmpRatio.num)
    end
module StringX : sig type x = string val string_of_x : 'a -> 'a end
module Dualizer :
  functor (PrimalLP : LP) ->
    sig
      module DualX :
        sig
          type x = PrimalLP.constr
          val string_of_x : PrimalLP.constr -> string
        end
      module DualLP :
        sig
          type x = DualX.x
          val string_of_x : x -> string
          type expr =
            Make(DualX).expr =
              Const of Numutil.GmpRatio.num
            | Var of x
            | Mul of Numutil.GmpRatio.num * expr
            | Sum of expr list
          type obj = Make(DualX).obj = Min of expr | Max of expr
          type constr =
            Make(DualX).constr =
              Eq of expr * expr
            | Le of expr * expr
          type constrs = constr list
          type lp =
            Make(DualX).lp = {
            prob_name : string;
            obj : obj;
            constrs : constr list;
          }
          type sol =
              Numutil.InfGmpRatio.num *
              (x -> Numutil.GmpRatio.num)
          exception Var_not_found of x
          val prob_id : int ref
          val new_prob_name : unit -> string
          val ( ++. ) : expr -> expr -> expr
          val ( **. ) : Numutil.GmpRatio.num -> expr -> expr
          val expr_neg : expr -> expr
          val ( --. ) : expr -> expr -> expr
          val ( ==. ) : expr -> expr -> constr
          val ( <=. ) : expr -> expr -> constr
          val ( >=. ) : expr -> expr -> constr
          val vars_of_expr : expr -> x list
          val contains_var : expr -> bool
          val string_of_expr : expr -> string
          val string_of_obj : obj -> string
          val string_of_constr : constr -> string
          val string_of_constrs : constr list -> string
          val string_of_lp : lp -> string
          val transform_into_max_obj : obj -> obj
          val transform_into_max : lp -> lp
          val normal_expr :
            expr ->
            Numutil.GmpRatio.num *
            (Numutil.GmpRatio.num * x) list
          type normal_lin_expr = (Numutil.GmpRatio.num * x) list
          type normal_expr = Numutil.GmpRatio.num * normal_lin_expr
          type normal_constr =
            Make(DualX).normal_constr =
              NEq of normal_lin_expr * Numutil.GmpRatio.num
            | NLe of normal_lin_expr * Numutil.GmpRatio.num
          val string_of_normal_constr : normal_constr -> string
          val normal_constr : constr -> normal_constr
          val is_bound : normal_constr -> bool
          val is_non_trivial : normal_constr -> bool
          val is_trivially_unsat : normal_constr -> bool
          val ( ++ ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
          val is_min_lp : lp -> bool
          val is_max_lp : lp -> bool
          val solve :
            lp ->
            Numutil.InfGmpRatio.num *
            (x -> Numutil.GmpRatio.num)
          val solve' :
            string ->
            constr list ->
            obj ->
            Numutil.InfGmpRatio.num *
            (x -> Numutil.GmpRatio.num)
        end
      val dualize_for_constr :
        DualLP.x * PrimalLP.normal_constr -> DualLP.expr
      val dualize_obj_expr :
        (DualLP.x * PrimalLP.normal_constr) list -> DualLP.expr
      val create_dual_obj :
        (DualLP.x * PrimalLP.normal_constr) list ->
        PrimalLP.obj -> DualLP.obj
      val partition : ('a -> 'a -> int) -> 'a list -> 'a list list
      val create_dual_constrs :
        (DualLP.x * PrimalLP.normal_constr) list ->
        'a * (Numutil.GmpRatio.num * PrimalLP.x) list ->
        DualLP.constr list
      val dualize : PrimalLP.lp -> DualLP.lp
      val dual_solve_no_test :
        PrimalLP.lp ->
        Numutil.InfGmpRatio.num *
        (DualLP.x -> Numutil.GmpRatio.num)
      val dual_solve :
        PrimalLP.lp ->
        Numutil.InfGmpRatio.num *
        (DualLP.x -> Numutil.GmpRatio.num)
      val dual_solve_no_test' :
        string ->
        PrimalLP.constr list ->
        PrimalLP.obj ->
        Numutil.InfGmpRatio.num *
        (DualLP.x -> Numutil.GmpRatio.num)
      val dual_solve_without_feasibility_check :
        PrimalLP.lp ->
        Numutil.InfGmpRatio.num *
        (DualLP.x -> Numutil.GmpRatio.num)
      val dual_solve' :
        string ->
        PrimalLP.constr list ->
        PrimalLP.obj ->
        Numutil.InfGmpRatio.num *
        (DualLP.x -> Numutil.GmpRatio.num)
    end
