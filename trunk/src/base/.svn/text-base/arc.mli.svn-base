(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** framework base: arcs of control flow graphs *)

type t = 
    |Normal of Env.boolexpr_t * Env.equs_t 
    |Loop of Env.boolexpr_t * Env.equs_t
    |Accel of Env.boolexpr_t * Env.equs_t
    |BoolAccel of Env.boolexpr_t * Env.equs_t
    |BoolNaccAccel of Env.boolexpr_t * Env.equs_t
    |Bool of Env.boolexpr_t * Env.equs_t
    |BoolNacc of Env.boolexpr_t * Env.equs_t
    |Nonacc of Env.boolexpr_t * Env.equs_t
    |Id
    |Flow of Env.boolexpr_t * Env.equs_t
    |Apron of Env.boolexpr_t * (ApronUtil.equs_t * Env.equs_t)


(** {2 Printing} *)

(** print arc type *)
val print_type : Env.t -> Format.formatter -> t -> unit

(** print arc *)
val print : Env.t -> Format.formatter -> t -> unit

(** {2 Operations} *)

(** return (arc assertion, equations) *)
val get_ass_equs : Env.t -> t -> Env.boolexpr_t * Env.equs_t

(** simplify arc by the given expression *)
val simplify : Env.t -> t -> Env.boolexpr_t -> t

(** refine arc assertion by the given expression *)
val refine_ass : Env.t -> t -> Env.boolexpr_t -> t

(** check whether the arc is the identity w.r.t. the given invariant *)
val is_id : Env.t -> t -> Env.boolexpr_t -> bool

(** replace the arc asssertion and equations *)
val replace_ass_equs : t -> Env.boolexpr_t * Env.equs_t -> t
