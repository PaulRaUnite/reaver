(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** template analysis comparison utilities *)

(** comparison statistics *)
type stats_t = 
{
  mutable b1: int;
  mutable b2: int;
  mutable lt: int;
  mutable eq: int;
  mutable eqinf: int;
  mutable gt: int;
  mutable gtinf1: int;
  mutable gtinf2: int;
  mutable gtdev : float;
}
 
(** compare and compute statistics *)
val template_comparison_stat : Env.t -> Program.cfprog_t -> Analysis.bddapron_res_t -> Program.cfprog_t -> Analysis.bddapron_res_t -> Template.template_t -> 
(Cfg.locid_t * Template.template_t) list -> stats_t

(** print statistics *)
val print_template_comparison_stat : Format.formatter -> stats_t -> unit
