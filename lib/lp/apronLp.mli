
module TheLp : Ocamlqsopt_ex.Lp.LP


type lp_expr_t = TheLp.expr
type lp_cons_t = TheLp.constr
type lp_conss_t = TheLp.constrs
type lp_obj_fct_t = TheLp.obj

type obj_val_t = Apron.Coeff.t
type solution_t = (ApronUtil.var_t -> Apron.Coeff.t)


val make_conss_conss : ?rename:(string -> string) -> ApronUtil.linconss_t -> lp_conss_t
val make_conss_equs : ?rename_lhs:(string -> string) -> ?rename_rhs:(string -> string) -> ApronUtil.equs_t -> lp_conss_t
val make_conss_lequs : ?rename_lhs:(string -> string) -> ?rename_rhs:(string -> string) -> ApronUtil.equs_t -> lp_conss_t
val make_conss_gequs : ?rename_lhs:(string -> string) -> ?rename_rhs:(string -> string) -> ApronUtil.equs_t -> lp_conss_t
val make_conss_equ : ?rename_lhs:(string -> string) -> ?rename_rhs:(string -> string) -> ApronUtil.equ_t -> lp_cons_t
val make_conss_lequ : ?rename_lhs:(string -> string) -> ?rename_rhs:(string -> string) -> ApronUtil.equ_t -> lp_cons_t
val make_conss_gequ : ?rename_lhs:(string -> string) -> ?rename_rhs:(string -> string) -> ApronUtil.equ_t -> lp_cons_t
val make_conss_vars_bounded_one : ApronUtil.vars_t -> lp_conss_t
val make_conss_vars_bounded_minusone : ApronUtil.vars_t -> lp_conss_t
val concat_conss : lp_conss_t -> lp_conss_t -> lp_conss_t

val print_lp_conss : Format.formatter -> lp_conss_t -> unit
val print_lp_sol : ApronUtil.vars_t -> Format.formatter -> solution_t -> unit

val make_obj_fct_max : ?rename:(string -> string) -> ApronUtil.linexpr_t -> lp_obj_fct_t
val make_obj_fct_max_sum_vars : ApronUtil.vars_t -> lp_obj_fct_t
val make_obj_fct_max_negsum_vars : ApronUtil.vars_t -> lp_obj_fct_t

val solve : lp_conss_t -> lp_obj_fct_t -> (obj_val_t * solution_t)
val get_bounded_vars : lp_conss_t -> ApronUtil.vars_t -> ApronUtil.vars_t
