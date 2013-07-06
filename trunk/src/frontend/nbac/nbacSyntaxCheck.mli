(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** frontend implementation: Hybrid NBAC - syntax check *)

val guesstyp : Program.declaration_t -> NbacExpr.symtype NbacExpr.expr -> NbacExpr.typ
val checktyp : Program.declaration_t -> NbacExpr.typ -> NbacExpr.symtype NbacExpr.expr -> unit

val check_declaration : Program.declaration_t -> unit
val check_definition_transition : NbacExpr.prog -> unit
val check_formulas : NbacExpr.prog -> unit

val sort_definitions : NbacExpr.prog -> unit

val check_prog : NbacExpr.prog -> unit
