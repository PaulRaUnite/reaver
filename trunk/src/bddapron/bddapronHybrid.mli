(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** BddApron-extended functions for the continuous time elapse 
    in hybrid systems *)

(** signature of a function for computing the continuous time elapse *)
type ('a, 'b, 'c, 'd) cont_elapse_t = 
  'a BddapronUtil.env_t ->         (** BddApron environment *)
  'a BddapronUtil.cond_t ->        (** BddApron conditions *)
  ('a, 'b, 'c, 'd) BddapronUtil.doman_t -> (** BddApron domain manager *)
  'a BddapronUtil.boolexpr_t ->    (** guard (including assertion) *)
  'a BddapronUtil.equs_t ->        (** differential equations *)
  'a BddapronUtil.vars_t ->        (** input variables *)
  'd BddapronUtil.abstract_t ->    (** start abstract value *)
  'd BddapronUtil.abstract_t       (* result abstract value *)

(** {2 Flow operators} *)

(** computes the time elapse of s by eqs up to staycond *)
val flow : 
  trans_doman:('a, 'f, 'g, 'e) BddapronUtil.doman_t  ->
                               (** trans_man the manager of the domain 
                                       for computing the time elapse *)
  'a BddapronUtil.env_t ->         (** BddApron environment *)
  'a BddapronUtil.cond_t ->        (** BddApron conditions *)
  ('a, 'b, 'c, 'd) BddapronUtil.doman_t -> (** BddApron domain manager *)
  ?cont_elapse:('a, 'f, 'g, 'e) cont_elapse_t ->  
                     (** the function for computing the time elapse *)
  'a BddapronUtil.boolexpr_t ->    (** guard (including assertion) *)
  'a BddapronUtil.equs_t ->        (** differential equations *)
  'a BddapronUtil.vars_t ->        (** input variables *)
  'd BddapronUtil.abstract_t ->    (** start abstract value *)
  'd BddapronUtil.abstract_t       (* result abstract value *)

(** {2 Continuous time-elapse operators} *)

(** simplest time-elapse operator 
     (forget everything about continuous variables) *)
val cont_elapse0 : ('a, 'b, 'c, 'd) cont_elapse_t

(** polyhedral time-elapse operator for constant dynamics (forget otherwise) *)
val cont_elapse1 : ('a, 'b, 'c, 'd) cont_elapse_t

(** polyhedral time-elapse operator for constant dynamics (forget otherwise),
    which also takes into account expressions dependent on inputs and 
    discrete resp. dot v = 0 variables  *)
val cont_elapse2 : ?convexify:bool -> 
  ('a BddapronUtil.var_t -> 'a BddapronUtil.var_t) -> 
  ('a, 'b, 'c, 'd) cont_elapse_t
