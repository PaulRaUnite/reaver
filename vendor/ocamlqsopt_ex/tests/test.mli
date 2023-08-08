module StringLP :
  sig
    type x = Ocamlqsopt_ex.Lp.StringX.x
    val string_of_x : x -> string
    type expr =
      Ocamlqsopt_ex.Lp.Make(Ocamlqsopt_ex.Lp.StringX).expr =
        Const of Ocamlqsopt_ex.Numutil.GmpRatio.num
      | Var of x
      | Mul of Ocamlqsopt_ex.Numutil.GmpRatio.num * expr
      | Sum of expr list
    type obj =
      Ocamlqsopt_ex.Lp.Make(Ocamlqsopt_ex.Lp.StringX).obj =
        Min of expr
      | Max of expr
    type constr =
      Ocamlqsopt_ex.Lp.Make(Ocamlqsopt_ex.Lp.StringX).constr =
        Eq of expr * expr
      | Le of expr * expr
    type constrs = constr list
    type lp =
      Ocamlqsopt_ex.Lp.Make(Ocamlqsopt_ex.Lp.StringX).lp = {
      prob_name : string;
      obj : obj;
      constrs : constr list;
    }
    type sol =
        Ocamlqsopt_ex.Numutil.InfGmpRatio.num *
        (x -> Ocamlqsopt_ex.Numutil.GmpRatio.num)
    exception Var_not_found of x
    val prob_id : int ref
    val new_prob_name : unit -> string
    val ( ++. ) : expr -> expr -> expr
    val ( **. ) : Ocamlqsopt_ex.Numutil.GmpRatio.num -> expr -> expr
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
      Ocamlqsopt_ex.Numutil.GmpRatio.num *
      (Ocamlqsopt_ex.Numutil.GmpRatio.num * x) list
    type normal_lin_expr = (Ocamlqsopt_ex.Numutil.GmpRatio.num * x) list
    type normal_expr = Ocamlqsopt_ex.Numutil.GmpRatio.num * normal_lin_expr
    type normal_constr =
      Ocamlqsopt_ex.Lp.Make(Ocamlqsopt_ex.Lp.StringX).normal_constr =
        NEq of normal_lin_expr * Ocamlqsopt_ex.Numutil.GmpRatio.num
      | NLe of normal_lin_expr * Ocamlqsopt_ex.Numutil.GmpRatio.num
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
      Ocamlqsopt_ex.Numutil.InfGmpRatio.num * (x -> Ocamlqsopt_ex.Numutil.GmpRatio.num)
    val solve' :
      string ->
      constr list ->
      obj ->
      Ocamlqsopt_ex.Numutil.InfGmpRatio.num * (x -> Ocamlqsopt_ex.Numutil.GmpRatio.num)
  end
module MyDualizer :
  sig
    module DualX :
      sig
        type x = StringLP.constr
        val string_of_x : StringLP.constr -> string
      end
    module DualLP :
      sig
        type x = DualX.x
        val string_of_x : x -> string
        type expr =
          Ocamlqsopt_ex.Lp.Make(DualX).expr =
            Const of Ocamlqsopt_ex.Numutil.GmpRatio.num
          | Var of x
          | Mul of Ocamlqsopt_ex.Numutil.GmpRatio.num * expr
          | Sum of expr list
        type obj =
          Ocamlqsopt_ex.Lp.Make(DualX).obj =
            Min of expr
          | Max of expr
        type constr =
          Ocamlqsopt_ex.Lp.Make(DualX).constr =
            Eq of expr * expr
          | Le of expr * expr
        type constrs = constr list
        type lp =
          Ocamlqsopt_ex.Lp.Make(DualX).lp = {
          prob_name : string;
          obj : obj;
          constrs : constr list;
        }
        type sol =
            Ocamlqsopt_ex.Numutil.InfGmpRatio.num *
            (x -> Ocamlqsopt_ex.Numutil.GmpRatio.num)
        exception Var_not_found of x
        val prob_id : int ref
        val new_prob_name : unit -> string
        val ( ++. ) : expr -> expr -> expr
        val ( **. ) : Ocamlqsopt_ex.Numutil.GmpRatio.num -> expr -> expr
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
          Ocamlqsopt_ex.Numutil.GmpRatio.num *
          (Ocamlqsopt_ex.Numutil.GmpRatio.num * x) list
        type normal_lin_expr = (Ocamlqsopt_ex.Numutil.GmpRatio.num * x) list
        type normal_expr = Ocamlqsopt_ex.Numutil.GmpRatio.num * normal_lin_expr
        type normal_constr =
          Ocamlqsopt_ex.Lp.Make(DualX).normal_constr =
            NEq of normal_lin_expr * Ocamlqsopt_ex.Numutil.GmpRatio.num
          | NLe of normal_lin_expr * Ocamlqsopt_ex.Numutil.GmpRatio.num
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
          Ocamlqsopt_ex.Numutil.InfGmpRatio.num *
          (x -> Ocamlqsopt_ex.Numutil.GmpRatio.num)
        val solve' :
          string ->
          constr list ->
          obj ->
          Ocamlqsopt_ex.Numutil.InfGmpRatio.num *
          (x -> Ocamlqsopt_ex.Numutil.GmpRatio.num)
      end
    val dualize_for_constr : DualLP.x * StringLP.normal_constr -> DualLP.expr
    val dualize_obj_expr :
      (DualLP.x * StringLP.normal_constr) list -> DualLP.expr
    val create_dual_obj :
      (DualLP.x * StringLP.normal_constr) list -> StringLP.obj -> DualLP.obj
    val partition : ('a -> 'a -> int) -> 'a list -> 'a list list
    val create_dual_constrs :
      (DualLP.x * StringLP.normal_constr) list ->
      'a * (Ocamlqsopt_ex.Numutil.GmpRatio.num * StringLP.x) list ->
      DualLP.constr list
    val dualize : StringLP.lp -> DualLP.lp
    val dual_solve_no_test :
      StringLP.lp ->
      Ocamlqsopt_ex.Numutil.InfGmpRatio.num *
      (DualLP.x -> Ocamlqsopt_ex.Numutil.GmpRatio.num)
    val dual_solve :
      StringLP.lp ->
      Ocamlqsopt_ex.Numutil.InfGmpRatio.num *
      (DualLP.x -> Ocamlqsopt_ex.Numutil.GmpRatio.num)
    val dual_solve_no_test' :
      string ->
      StringLP.constr list ->
      StringLP.obj ->
      Ocamlqsopt_ex.Numutil.InfGmpRatio.num *
      (DualLP.x -> Ocamlqsopt_ex.Numutil.GmpRatio.num)
    val dual_solve_without_feasibility_check :
      StringLP.lp ->
      Ocamlqsopt_ex.Numutil.InfGmpRatio.num *
      (DualLP.x -> Ocamlqsopt_ex.Numutil.GmpRatio.num)
    val dual_solve' :
      string ->
      StringLP.constr list ->
      StringLP.obj ->
      Ocamlqsopt_ex.Numutil.InfGmpRatio.num *
      (DualLP.x -> Ocamlqsopt_ex.Numutil.GmpRatio.num)
  end
val lp : StringLP.lp
val dual_lp : MyDualizer.DualLP.lp
val objective_value : Ocamlqsopt_ex.Numutil.InfGmpRatio.num
val optimal_solution : MyDualizer.DualLP.x -> Ocamlqsopt_ex.Numutil.GmpRatio.num
