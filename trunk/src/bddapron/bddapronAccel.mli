(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** BddApron-extended functions for acceleration *)

type direction_t = ApronAccel.direction_t

(** {2 Operators} *)

(** computes the accelerated transition *)
val acc : ?dir:direction_t ->   (** forward or backward computation,
                                    default: Forward *)
  trans_apronman:'e ApronUtil.man_t -> (** manager of the APRON domain for 
                                       computing translation relation*)
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t ->
  ('a, 'b, 'c, 'd) BddapronUtil.doman_t ->
  'a BddapronUtil.boolexpr_t ->    (** guard (including assertion) *)
  'a BddapronUtil.equs_t ->        (** transition function *)
  'a BddapronUtil.vars_t ->        (** numerical input variables *)
  'd BddapronUtil.abstract_t ->    (** start abstract value *)
  'd BddapronUtil.abstract_t       (* result abstract value *)

(** {2 Checking for accelerability} *)

(** determines the transition type of the given action *)
val get_transition_type_action : 
  'a BddapronUtil.env_t -> 'a BddapronUtil.cond_t ->
  'a BddapronUtil.vars_t ->    (** numerical input variables *)
  'a BddapronUtil.var_t ->     (** left-hand side variable *)
  'a BddapronUtil.action_t ->  (** numerical action *)
  ApronAccel.transtype_t

(** determines the transition type of the given actions *)
val get_transition_type_actions : 
  'a BddapronUtil.env_t -> 'a BddapronUtil.cond_t ->
  'a BddapronUtil.vars_t ->    (** numerical input variables *)
  'a BddapronUtil.vararr_t ->   (** left-hand side variables *)
  'a BddapronUtil.actions_t ->  (** numerical actions *)
  ApronAccel.transtype_t

(** determines the transition type of the given numerical transition function *)
val get_transition_type_numequs : 
  'a BddapronUtil.env_t -> 'a BddapronUtil.cond_t ->
  'a BddapronUtil.vars_t ->    (** numerical input variables *)
  'a BddapronUtil.equs_t ->   (** numerical transition function *)
  ApronAccel.transtype_t

val is_acc_action : 'a BddapronUtil.env_t -> 'a BddapronUtil.cond_t ->
  'a BddapronUtil.vars_t ->
  'a BddapronUtil.var_t -> 'a BddapronUtil.action_t -> bool
val is_trivial_action : 'a BddapronUtil.env_t -> 'a BddapronUtil.cond_t ->
  'a BddapronUtil.vars_t ->
  'a BddapronUtil.var_t -> 'a BddapronUtil.action_t -> bool
val is_nonacc_action : 'a BddapronUtil.env_t -> 'a BddapronUtil.cond_t ->
  'a BddapronUtil.vars_t ->
  'a BddapronUtil.var_t -> 'a BddapronUtil.action_t -> bool

val is_acc_actions : 'a BddapronUtil.env_t -> 'a BddapronUtil.cond_t ->
  'a BddapronUtil.vars_t ->
  'a BddapronUtil.vararr_t -> 'a BddapronUtil.actions_t -> bool
val is_trivial_actions : 'a BddapronUtil.env_t -> 'a BddapronUtil.cond_t ->
  'a BddapronUtil.vars_t ->
  'a BddapronUtil.vararr_t -> 'a BddapronUtil.actions_t -> bool
val is_nonacc_actions : 'a BddapronUtil.env_t -> 'a BddapronUtil.cond_t ->
  'a BddapronUtil.vars_t ->
  'a BddapronUtil.vararr_t -> 'a BddapronUtil.actions_t -> bool
val exists_acc_action : 'a BddapronUtil.env_t -> 'a BddapronUtil.cond_t ->
  'a BddapronUtil.vars_t ->
  'a BddapronUtil.vararr_t -> 'a BddapronUtil.actions_t -> bool

val is_acc_numequs : 'a BddapronUtil.env_t -> 'a BddapronUtil.cond_t ->
  'a BddapronUtil.vars_t -> 'a BddapronUtil.equs_t -> bool
val is_trivial_numequs : 'a BddapronUtil.env_t -> 'a BddapronUtil.cond_t ->
  'a BddapronUtil.vars_t -> 'a BddapronUtil.equs_t -> bool
val is_nonacc_numequs : 'a BddapronUtil.env_t -> 'a BddapronUtil.cond_t ->
  'a BddapronUtil.vars_t -> 'a BddapronUtil.equs_t -> bool
