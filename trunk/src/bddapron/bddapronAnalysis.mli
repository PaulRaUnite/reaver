(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** BddApron-based Boolean and logico-numerical analysis *)


type direction_t = ApronAccel.direction_t

(** iterates the given image operator until convergence (without widening) *)
val fixedpoint : 'a BddapronUtil.env_t -> 
  ('a, 'b, 'c, 'd) BddapronUtil.doman_t ->
  ('d BddapronUtil.abstract_t -> 'd BddapronUtil.abstract_t) -> (** image *)
  'd BddapronUtil.abstract_t ->   (** start abstract value *)
  'd BddapronUtil.abstract_t      (* fixed point *)

(** iterates the given image operator (on predicates) 
     until convergence (without widening) *)
val fixedpoint2 : 'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  'a BddapronUtil.boolexpr_t ->    (** start predicate *)
  ('a BddapronUtil.boolexpr_t -> 'a BddapronUtil.boolexpr_t) -> (** image *)
  'a BddapronUtil.boolexpr_t       (* fixed point predicate *)

(** {2 Logico-numerical analysis} *)

(** compute image *)
val image : ?dir:direction_t -> 
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  ('a, 'b, 'c, 'd) BddapronUtil.doman_t ->
  'a BddapronUtil.boolexpr_t ->    (** guard (including assertion) *)
  'a BddapronUtil.equs_t ->        (** equations *)
  'a BddapronUtil.vars_t ->        (** input variables *)
  'd BddapronUtil.abstract_t ->    (** start abstract value *)
  'd BddapronUtil.abstract_t       (* result abstract value *)

(** checks reachability of phi2 from phi1 by equs *)
val check_reach : ?dir:direction_t -> 
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  ('a, 'b, 'c, 'd) BddapronUtil.doman_t ->
  'a BddapronUtil.boolexpr_t ->    (** guard (including assertion) *)
  'a BddapronUtil.equs_t ->        (** equations *)
  'a BddapronUtil.vars_t ->        (** input variables *)
  'a BddapronUtil.boolexpr_t ->    (** start predicate *)
  'a BddapronUtil.boolexpr_t ->    (** goal predicate *)
  bool                             

(** computes the precondition for arriving by equs in phi2 *)
val pre_assertion : 
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  ('a, 'b, 'c, 'd) BddapronUtil.doman_t ->
  'a BddapronUtil.boolexpr_t ->    (** guard (including assertion) *)
  'a BddapronUtil.equs_t ->        (** equations *)
  'a BddapronUtil.boolexpr_t ->    (** start predicate *)
  'a BddapronUtil.boolexpr_t ->    (** goal predicate *)
  'a BddapronUtil.boolexpr_t       


(** {2 Boolean analysis on abstract domain values} *)

(** computes the image by boolean equations *)
val bool_image : ?dir:direction_t -> 
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  ('a, 'b, 'c, 'd) BddapronUtil.doman_t ->
  'a BddapronUtil.boolexpr_t ->    (** guard (including assertion) *)
  'a BddapronUtil.equs_t ->        (** equations *)
  'a BddapronUtil.vars_t ->        (** input and numerical variables *)
  'd BddapronUtil.boolexpr_t ->    (** start predicate *)
  'd BddapronUtil.boolexpr_t       (* result predicate *)

(** checks whether the given boolean invariant is reachable 
    by the given equations *)
val check_bool_reach : ?dir:direction_t -> 
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  ('a, 'b, 'c, 'd) BddapronUtil.doman_t ->
  'a BddapronUtil.boolexpr_t ->    (** guard (including assertion) *)
  'a BddapronUtil.equs_t ->        (** equations *)
  'a BddapronUtil.vars_t ->        (** input and numerical variables *)
  'a BddapronUtil.boolexpr_t ->    (** start predicate *)
  'a BddapronUtil.boolexpr_t ->    (** goal predicate *)
  bool                             

(** returns the boolean-reachable states as boolean expression *)
val bool_reach : ?dir:direction_t ->
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  ('a, 'b, 'c, 'd) BddapronUtil.doman_t ->
  'a BddapronUtil.boolexpr_t ->    (** guard (including assertion) *)
  'a BddapronUtil.equs_t ->        (** equations *)
  'a BddapronUtil.vars_t ->        (** input and numerical variables *)
  'a BddapronUtil.boolexpr_t ->    (** start predicate *)
  'a BddapronUtil.boolexpr_t       (* fixed point *)

(** {2 Boolean analysis using predicates } *)

(** checks whether the given boolean invariant is reachable 
    by the given equations *)
val check_bool_reach2 : ('a BddapronUtil.var_t  -> 'a BddapronUtil.var_t) -> 
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  'a BddapronUtil.boolexpr_t -> 
  'a BddapronUtil.equs_t ->
  'a BddapronUtil.boolexpr_t -> 
  'a BddapronUtil.boolexpr_t -> bool

(** returns the boolean-reachable states as boolean expression *)
val bool_reach2 : ?dir:direction_t ->
  get_primed_var:('a BddapronUtil.var_t  -> 'a BddapronUtil.var_t) ->
  get_unprimed_var:('a BddapronUtil.var_t  -> 'a BddapronUtil.var_t) ->
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  'a BddapronUtil.equs_t ->        (** equations *)
  'a BddapronUtil.vars_t ->        (** input and numerical variables *)
  'a BddapronUtil.boolexpr_t ->    (** guard (including assertion) *)
  'a BddapronUtil.boolexpr_t ->    (** start predicate *)
  'a BddapronUtil.boolexpr_t       (* fixed point *)

(** computes the assertion that must be satisfied in order to reach
    the given boolean invariant by the given equations *)
val bool_pre_assertion2 : ('a BddapronUtil.var_t  -> 'a BddapronUtil.var_t) -> 
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  'a BddapronUtil.boolexpr_t -> 
  'a BddapronUtil.boolexpr_t -> 
  'a BddapronUtil.equs_t -> 
  'a BddapronUtil.boolexpr_t -> 
  'a BddapronUtil.vars_t ->        (** input and numerical variables *)
  'a BddapronUtil.boolexpr_t

(** computes the Boolean postcondition *)
val bool_image2 : ('a BddapronUtil.var_t  -> 'a BddapronUtil.var_t) -> 
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  'a BddapronUtil.boolexpr_t list -> 
  'a BddapronUtil.boolexpr_t -> 
  'a BddapronUtil.boolexpr_t -> 
  'a BddapronUtil.boolexpr_t -> 
  'a BddapronUtil.boolexpr_t

(** {2 Boolean analysis with precomputed relations } *)

(** computes the boolean transition relation specialized to the 
    given start predicate *)
val get_freach : 
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  'a BddapronUtil.boolexpr_t -> 
  'a BddapronUtil.boolexpr_t ->
  'a BddapronUtil.boolexpr_t list ->
  'a BddapronUtil.boolexpr_t

(** checks whether the given boolean invariant is reachable 
    by the given precomputed transition relation *)
val check_bool_reach3 : 
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  'a BddapronUtil.boolexpr_t -> 
  'a BddapronUtil.boolexpr_t -> bool

(** {2 Checking transition functions} *)

(** checks whether the boolean equs represent the identity function w.r.t to 
    the given boolean invariant *)
val is_id_bequs2 : ('a BddapronUtil.var_t  -> 'a BddapronUtil.var_t) -> 
  ('a BddapronUtil.var_t  -> 'a BddapronUtil.var_t) -> 
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  'a BddapronUtil.boolexpr_t -> (** guard (including assertion) *)  
  'a BddapronUtil.equs_t ->     (** boolean equations *)
  'a BddapronUtil.boolexpr_t -> (** predicate (invariant) *)
  bool

(** checks whether equs represents the identity function w.r.t to the
    given boolean invariant; numerical identity is checked syntactically *)
val is_id_equs2 : ('a BddapronUtil.var_t -> 'a BddapronUtil.var_t) -> 
  ('a BddapronUtil.var_t  -> 'a BddapronUtil.var_t) -> 
  'a BddapronUtil.env_t ->
  'a BddapronUtil.cond_t -> 
  'a BddapronUtil.boolexpr_t -> (** guard (including assertion) *)  
  'a BddapronUtil.equs_t ->     (** boolean equations *)
  'a BddapronUtil.boolexpr_t -> (** predicate (invariant) *)
  bool

