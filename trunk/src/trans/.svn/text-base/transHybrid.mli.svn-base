(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** transformation implementation: hybrid partitioning *)

(** partitions by the Boolean-defined continuous modes (for the given
    set of numerical variables) with common staying conditions *)
val modes_bool1 : Env.vars_t -> VerifUtil.trans_t

(** partitions by the Boolean-defined continuous modes (for the given
    set of numerical variables) *)
val modes_bool2 : Env.vars_t -> VerifUtil.trans_t

(** partitions by convex numerical staying conditions *)
val convex_staycond : VerifUtil.trans_t

(** partitions by numerically defined continuous modes *)
val modes_num : Analysis.analyze_t -> VerifUtil.trans_t
