(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** APRON-based functions for the continuous time elapse in hybrid systems *)

(** signature of a function for computing the continuous time elapse *)
type 'a cont_elapse_t = 
  'a ApronUtil.man_t ->   (** the manager of the domain for computing 
                              the time elapse *)
  ApronUtil.linconss_t -> (** staying condition (including assertion) *)
  ApronUtil.equs_t ->     (** differential equations *)
  ApronUtil.vars_t ->     (** input variables *)
  'a ApronUtil.abstract_t -> (** start abstract value *)
  'a ApronUtil.abstract_t    (* result abstract value *)

(** {2 Flow operators} *)

(** computes the time elapse of s by eqs up to staycond *)
val flow : 
  trans_man:'a ApronUtil.man_t ->  (** trans_man the manager of the domain 
                                       for computing the time elapse *)
  ?cont_elapse:'a cont_elapse_t ->  (** the function for computing the time
                                        elapse *)
  ApronUtil.linconss_t -> (** staying condition (including assertion) *)
  ApronUtil.equs_t ->     (** differential equations *)
  ApronUtil.vars_t ->     (** input variables *)
  'b ApronUtil.abstract_t -> (** start abstract value *)
  'b ApronUtil.abstract_t    (* result abstract value *)

(** {2 Continuous time-elapse operators} *)

(** simplest time-elapse operator 
     (forget everything about continuous variables) *)
val cont_elapse0 : 'a cont_elapse_t

(** polyhedral time-elapse operator for constant dynamics (forget otherwise) *)
val cont_elapse1 : 'a cont_elapse_t

(** polyhedral time-elapse operator for constant dynamics (forget otherwise),
    which also takes into account expressions dependent on inputs and 
    discrete resp. dot v = 0 variables  *)
val cont_elapse2 : (ApronUtil.var_t -> ApronUtil.var_t) -> 'a cont_elapse_t

