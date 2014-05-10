(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** ReaVer verification main loop *)

(** invalid verification strategy *)
exception InvalidStrategy of string 

(** invalid verification strategy option *)
exception InvalidStrategyOption of string 

(** invalid verification strategy option value *)
exception InvalidStrategyOptionValue of string

val print_overall : bool ref (** print overall invariant *)
val check_property : bool ref (** check property or invariant *)

(** parse verification strategies *) 
val str_to_strategy : Env.t -> string -> VerifUtil.strategy_t

(** run the given verification strategies *)
val run : Env.t ->  VerifUtil.strategy_t ->  Program.cfprog_t -> 
  bool * Analysis.result_to_bddapron_t * Program.cfprog_t

(** print available verification strategies *)
val print_strategies : Format.formatter -> unit -> unit

(** print available abstract domains *)
val print_domains : Format.formatter -> unit -> unit

(** used for comparisons *)
val global_template : Template.template_t ref 
