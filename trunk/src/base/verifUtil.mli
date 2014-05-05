(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** framework base: utilities and types for the verification loop *)

(** CFG Transformation Interface *)
type trans_t = Env.t -> Program.cfprog_t -> Analysis.result_to_bddapron_t -> 
  Program.cfprog_t

(** Analysis Interface *)
type analysis_t = Analysis.analyze_t

(** verification strategy *)
type stratitem_t =
  | Trans of trans_t * string
  | Analysis of analysis_t * string

(** verification strategies *)
type strategy_t = stratitem_t list

(** refinement of CFG location definitions by the analysis result *)
val refine : ?refine_bool:bool -> Env.t -> Program.cfprog_t -> 
  Analysis.refine_loc_t -> Program.cfprog_t

val print_overall_reach : Env.t -> Program.cfprog_t -> Analysis.result_to_bddapron_t -> unit
