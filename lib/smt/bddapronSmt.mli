(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** utilities for translating BddApron formulas to SMT formulas *)

exception NotSupported of string

type model_t = string BddapronUtil.equs_t

type 'a bddmap_t = {mapb : ('a BddapronUtil.boolexpr_t,
                            ('a * YicesSmt.formula)) Hashhe.t;
                    mutable lastidb : int; 
                    prefixb : 'a}
type ('a, 'b) mtbddmap_t = {mapm : ('b Cudd.Mtbdd.t,('a * YicesSmt.formula))Hashhe.t;
                            mutable lastidm : int;
                            prefixm:'a}

val bddmap_create : string -> string bddmap_t
val mtbddmap_create : string -> (string, 'b) mtbddmap_t

val bddmap_to_smt : string bddmap_t -> YicesSmt.formula
val mtbddmap_to_smt : (string, 'b) mtbddmap_t -> YicesSmt.formula

val mtbdd_to_smt : ?prime_bool:string Sette.t -> (string, 'b) mtbddmap_t ->
  ('b -> YicesSmt.formula) ->  
  string BddapronUtil.env_t -> string BddapronUtil.cond_t -> 
  'b Cudd.Mtbdd.t -> YicesSmt.formula

(** translates a boolexpr to an SMT formula *)
val boolexpr_to_smt : string bddmap_t -> string BddapronUtil.env_t -> string BddapronUtil.cond_t -> string BddapronUtil.boolexpr_t -> YicesSmt.formula

(** translates an SMT formula to a boolexpr *)
val smt_to_boolexpr : ?unprime:bool -> string BddapronUtil.env_t -> string BddapronUtil.cond_t -> YicesSmt.formula -> string BddapronUtil.boolexpr_t

(** translates an MTBDD to an SMT formula *)
(*
val mtbdd_to_smt : (string -> YicesSmt.formula) -> string BddapronUtil.env_t -> string BddapronUtil.cond_t -> string Cudd.Mtbdd.t -> Smt.formula
*)

(** translates an equation to an SMT formula *)
val equs_to_smt : string bddmap_t -> string BddapronUtil.env_t -> string BddapronUtil.cond_t -> string BddapronUtil.equs_t -> YicesSmt.formula

(** translates an APRON linear constraint to an SMT formula *)
val apron_lincons_to_smt : string BddapronUtil.env_t -> ApronUtil.lincons_t -> YicesSmt.formula

(** translates an APRON conjunction of linear constraints to an SMT formula *)
val apron_linconss_to_smt : string BddapronUtil.env_t -> ApronUtil.linconss_t -> YicesSmt.formula

(** translates an APRON equation to an SMT formula *)
val apron_equs_to_smt : string BddapronUtil.env_t -> ApronUtil.equs_t -> YicesSmt.formula

(** translates an equation to an SMT formula *)
val smt_and : YicesSmt.formula list -> YicesSmt.formula

(** computes a model for the SMT formula *)
val smt_convert_model : string BddapronUtil.env_t -> string BddapronUtil.cond_t -> YicesSmt.interpretation -> model_t

(** computes a model for the SMT formula *)
val smt_compute_model : string BddapronUtil.env_t -> string BddapronUtil.cond_t -> YicesSmt.formula -> YicesSmt.interpretation option

(** adds a formula to the given context *)
val smt_assert_ctx : YicesSmt.ctx option -> YicesSmt.formula -> YicesSmt.ctx

(** adds a retractable formula to the given context *)
val smt_assert_retractable_ctx : YicesSmt.ctx -> YicesSmt.formula -> YicesSmt.assertion_id

(** retracts a formula from the given context *)
val smt_retract_ctx : YicesSmt.ctx -> YicesSmt.assertion_id -> YicesSmt.ctx

(** computes a model for the SMT formula in the given context*)
val smt_compute_model_ctx : string BddapronUtil.env_t -> string BddapronUtil.cond_t -> YicesSmt.ctx -> YicesSmt.interpretation option

val smt_print_model : string BddapronUtil.env_t -> string BddapronUtil.cond_t -> Format.formatter -> model_t -> unit

val smt_print_result : Format.formatter -> 'a option -> unit

val model_to_boolexpr : ?unprime:bool -> string BddapronUtil.env_t -> string BddapronUtil.cond_t -> model_t -> string BddapronUtil.boolexpr_t

val vars_to_smtvars : string BddapronUtil.env_t -> string BddapronUtil.cond_t ->string list  -> string list

val smt_check_model : YicesSmt.ctx -> YicesSmt.interpretation -> YicesSmt.formula -> YicesSmt.interpretation option

val smt_check_model2 : YicesSmt.ctx -> YicesSmt.ymodel -> YicesSmt.yexpr -> YicesSmt.interpretation option
