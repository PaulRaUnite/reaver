(******************************************************************************)
(* Program *)
(* program structures, interface to frontends, 
   interface for data-to-control-flow *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

(******************************************************************************)
(** {2 Program structures } *)
(******************************************************************************)

(** type and variable declarations *)
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
(*  aut : PartitionUtil.aut_t option; *)
}

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

(** default constructor for data-flow program structure *)
let make_dfprog d_disc_equs d_cont_equs d_zero_defs d_init d_final d_ass  =
  {d_disc_equs; d_cont_equs; d_zero_defs; d_init; d_final; d_ass}
(* add new make_* functions here, if you add fields to df_prog_t *)

let make_empty_dfprog env =
  let t = Cudd.Bdd.dtrue env.Env.cuddman in
  {d_disc_equs=[]; d_cont_equs=[]; d_zero_defs=[]; 
   d_init=t; d_final=t; d_ass=t}

(** default constructor for CFG program structure *)
let make_cfprog c_cfg c_disc_equs c_cont_equs c_init c_final c_ass =
  {c_cfg; c_disc_equs; c_cont_equs; c_init; c_final; c_ass}
(* add new make_* functions here, if you add fields to cf_prog_t *)

(** duplicates a CFG program structure *)
let copy_cfprog cfprog =
  make_cfprog (Cfg.copy cfprog.c_cfg)
    cfprog.c_disc_equs cfprog.c_cont_equs 
    cfprog.c_init cfprog.c_final cfprog.c_ass

(******************************************************************************)
(** {2 Frontend interface } *)
(******************************************************************************)

(** function protoypes for the frontend *)
type translate_t = Env.t -> dfprog_t
type parser_t = string -> (declaration_t * translate_t * string * string)

(******************************************************************************)
(** {2 Data-to-control-flow interface } *)
(******************************************************************************)

(** function prototype for transforming a data-flow program into a CFG *)
type df2cf_t = Env.t -> dfprog_t -> Env.t * cfprog_t

(******************************************************************************)
(** {2 Miscellaneous } *)
(******************************************************************************)

(******************************************************************************)
let permute_equations perm eqs = 
  match perm with
    |None -> eqs
    |Some perm -> List.map (fun (v,e) -> (v,Bddapron.Expr0.permute e perm)) eqs

let permute_boolexpr perm e = 
  match perm with
    |None -> e
    |Some perm -> Bddapron.Expr0.Bool.permute e perm

(** apply an environment permutation to the params *)
let apply_env_permutation env dfprog perm = 
      {
        d_disc_equs = permute_equations perm dfprog.d_disc_equs;
        d_cont_equs = permute_equations perm dfprog.d_cont_equs;
        d_zero_defs = dfprog.d_zero_defs;
        d_init = permute_boolexpr perm dfprog.d_init;
        d_final = permute_boolexpr perm dfprog.d_final;
        d_ass = permute_boolexpr perm dfprog.d_ass
      }
