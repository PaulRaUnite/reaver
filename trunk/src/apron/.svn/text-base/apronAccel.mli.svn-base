(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** APRON-based functions for acceleration *)

(** direction of analysis *)
type direction_t = [ `Forward | `Backward ]

(** type of transition *)
type transtype_t = 
    |Identity
    |Reset
    |Translation
    |TransReset
    |NonAcc

(** {2 Printing} *)

(** prints the direction *)
val print_direction : Format.formatter -> direction_t -> unit

(** prints the transition type *)
val print_transtype : Format.formatter -> transtype_t -> unit

(** {2 Operators} *)

(** compute accelerated transition *)
val acc : ?dir:direction_t ->   (** forward or backward computation,
                                    default: Forward *)
  trans_man:'a ApronUtil.man_t -> (** manager of the domain for 
                                       computing translation relation*)
  ApronUtil.linconss_t ->       (** guard (including assertion) *)
  ApronUtil.equs_t ->           (** transition function *)
  ApronUtil.vars_t ->           (** input variables *)
  'b ApronUtil.abstract_t ->    (** start abstract value *)
  'b ApronUtil.abstract_t       (* result abstract value *)

(** compute image *)
val image : ?dir:direction_t -> (** forward or backward image, 
                                    default: Forward *)
  ApronUtil.linconss_t ->       (** guard (including assertion) *)
  ApronUtil.equs_t ->           (** transition function *)
  ApronUtil.vars_t ->           (** input variables *)
  'a ApronUtil.abstract_t ->    (** start abstract value *)
  'a ApronUtil.abstract_t       (* result abstract value *)

(** {2 Checking for accelerability} *)

(** determines the transition type of a single equation *)
val get_transition_type_equ : ApronUtil.equ_t -> ApronUtil.vars_t -> transtype_t

(** determines the transition type of the given equations *)
val get_transition_type : ApronUtil.equs_t -> ApronUtil.vars_t -> transtype_t

(** checks whether the transition function is non-trivially accelerable *)
val is_acc : ApronUtil.equs_t -> ApronUtil.vars_t -> bool

(** checks whether the transition function is trivial *)
val is_trivial : ApronUtil.equs_t -> ApronUtil.vars_t -> bool

(** checks whether the transition function is not accelerable *)
val is_nonacc : ApronUtil.equs_t -> ApronUtil.vars_t -> bool
