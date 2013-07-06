(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** frontend implementation: Hybrid NBAC *)


val translate_prog : NbacExpr.prog -> Program.translate_t

val parse : ?is_file:bool -> Program.parser_t
