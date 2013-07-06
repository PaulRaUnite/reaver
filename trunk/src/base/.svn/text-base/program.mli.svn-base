(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** framework base: data-flow and control-flow program types *)

(** {2 Data-flow program } *)

(** variable declarations *)
type declaration_t = {
  typdef: (Env.var_t,Env.typdef) PMappe.t;
  state: (Env.var_t,Env.typ) Mappe.t;
  input: (Env.var_t,Env.typ) Mappe.t;
  local: (Env.var_t,Env.typ) Mappe.t;
}

(** dataflow program (with additional infos): 
      the representation produced by the frontend *)
type dfprog_t = 
{
  d_disc_equs : Env.equs_t;
  d_cont_equs : Env.equs_t;
  d_zero_defs : Env.zerodefs_t;
  d_init : Env.boolexpr_t;
  d_final : Env.boolexpr_t;
  d_ass : Env.boolexpr_t;
}

(** creates a DF program *)
val make_dfprog : Env.equs_t -> Env.equs_t -> Env.zerodefs_t -> Env.boolexpr_t -> Env.boolexpr_t -> Env.boolexpr_t -> dfprog_t

(** creates the empty DF program *)
val make_empty_dfprog : Env.t -> dfprog_t

(** applies the given BDD variable permutation to 
     the given equations (after an environment change) *)
val permute_equations : int array option -> Env.equs_t -> Env.equs_t

(** applies the given BDD variable permutation to 
     the given Boolean expression (after an environment change) *)
val permute_boolexpr : int array option -> Env.boolexpr_t -> Env.boolexpr_t

(** applies the given BDD variable permutation to 
     the given DF program (after an environment change) *)
val apply_env_permutation : Env.t -> dfprog_t -> int array option -> dfprog_t


(** {2 Control-flow program } *)

(** CFG program - the representation for analysis *)
type cfprog_t = 
{
  c_cfg : Cfg.t;
  c_disc_equs : Env.equs_t;
  c_cont_equs : Env.equs_t;
  c_init : Env.boolexpr_t;
  c_final : Env.boolexpr_t;
  c_ass : Env.boolexpr_t;
}

(* creates a CF program *)
val make_cfprog : Cfg.t -> Env.equs_t -> Env.equs_t -> Env.boolexpr_t -> Env.boolexpr_t -> Env.boolexpr_t -> cfprog_t

(** duplicates a CFG program structure *)
val copy_cfprog : cfprog_t -> cfprog_t

(** {2 Front-End Interface } *)

(** produces a data-flow program (callback returned by parser_t) *)
type translate_t = Env.t -> dfprog_t

(** parses the given input file and returns 
      the variable declarations, a data-flow program translator, and
      the default DF2CF and verification strategies *)
type parser_t = string -> (declaration_t * translate_t * string * string)


(** {2 Data-Flow to Control-Flow Interface } *)

(** converts a DF program into a CF program *)
type df2cf_t = Env.t -> dfprog_t -> Env.t * cfprog_t

