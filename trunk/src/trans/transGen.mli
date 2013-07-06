(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** transformation implementation: general transformations *)

(** partitions by initial, final and other states *)
val initfinal : VerifUtil.trans_t

(** partitions by the given set of predicates *)
val manual : Env.boolexpr_t list -> VerifUtil.trans_t

(** partitions by enumerating the values of the given Booean variables *)
val enumerate : Env.var_t list -> VerifUtil.trans_t

(** refines the transition functions by the destination location 
    (computes the arc assertions) *)
val refine_by_destloc : VerifUtil.trans_t

(** boolean backward bisimulation refinement *) 
val boolbacksim : VerifUtil.trans_t

(** removes boolean inputs and splits arcs *)
val remove_bool_inputs : VerifUtil.trans_t

(** splits non-convex guards and transitions *)
val split_arcs : VerifUtil.trans_t
