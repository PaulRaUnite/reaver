(** Functor to transform an abstract domain interface from level 0 to level 1 (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

module type Level0 = sig
  type ('a,'b) man
    (** BDDAPRON Manager. The type parameter ['b] indicates the
	underlying APRON abstract domain, as in type {!'b
	Apron.Abstract0.t}, and ['a] the type of symbols. *)
  type 'b t
    (** BDDAPRON Abstract value. The type parameter ['b] indicates
	the underlying APRON abstract domain, as in type {!'b
	Apron.Abstract0.t}, and ['a] the type of symbols. *)

  val size : ('a,'b) man -> 'b t -> int
  val print :
    ?print_apron:(
      (int -> string) ->
	Format.formatter -> 'b Apron.Abstract0.t -> unit
    ) ->
    'a Env.t -> Format.formatter -> 'b t -> unit

  val bottom : ('a,'b) man -> 'a Env.t -> 'b t
  val top : ('a,'b) man -> 'a Env.t -> 'b t
  val of_apron : ('a,'b) man -> 'a Env.t -> 'b Apron.Abstract0.t -> 'b t
  val of_bddapron : ('a,'b) man -> 'a Env.t -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list -> 'b t

  val is_bottom : ('a,'b) man -> 'b t -> bool
  val is_top : ('a,'b) man -> 'b t -> bool
  val is_leq : ('a,'b) man -> 'b t -> 'b t -> bool
  val is_eq : ('a,'b) man -> 'b t -> 'b t -> bool
  val to_bddapron :
    ('a,'b) man -> 'b t -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list
  val meet : ('a,'b) man -> 'b t -> 'b t -> 'b t
  val join : ('a,'b) man -> 'b t -> 'b t -> 'b t
  val meet_condition :
    ('a,'b) man -> 'a Env.t -> 'a Cond.t -> 'b t -> 'a Expr0.Bool.t -> 'b t
  val assign_lexpr :
    ?relational:bool -> ?nodependency:bool ->
    ('a,'b) man -> 'a Env.t -> 'a Cond.t ->
    'b t -> 'a list -> 'a Expr0.t list -> 'b t option -> 'b t
  val substitute_lexpr :
    ('a,'b) man -> 'a Env.t -> 'a Cond.t ->
    'b t -> 'a list -> 'a Expr0.t list -> 'b t option -> 'b t
  val forget_list : ('a,'b) man -> 'a Env.t -> 'b t -> 'a list -> 'b t
  val widening : ('a,'b) man -> 'b t -> 'b t -> 'b t
  val widening_threshold : ('a,'b) man -> 'b t -> 'b t -> Apron.Lincons0.t array -> 'b t
  val apply_change :
    bottom:'b t -> ('a,'b) man -> 'b t -> Env.change -> 'b t
  val apply_permutation :
    ('a,'b) man -> 'b t -> int array option * Apron.Dim.perm option -> 'b t
end

(*  ********************************************************************** *)
(** {3 Abstract domain of level 1} *)
(*  ********************************************************************** *)

module type Level1 = sig
  type ('a,'b) man
    (** BDDAPRON Manager. The type parameter ['b] indicates the
	underlying APRON abstract domain, as in type {!'b
	Apron.Abstract0.t}, and ['a] the type of symbols. *)
  type 'b t0
    (** Level 0 abstract value. *)
  type ('a,'b) t = ('a Env.t, 'b t0) Env.value
     (** Level 1 abstract value *)

  val get_env : ('a,'b) t -> 'a Env.t
  val to_level0 : ('a,'b) t -> 'b t0
  val of_level0 : 'a Env.t -> 'b t0 -> ('a,'b) t

  val size : ('a,'b) man -> ('a,'b) t -> int
    (** Size of an abstract value. *)
  val print :
    ?print_apron:(
      (int -> string) ->
	Format.formatter -> 'b Apron.Abstract0.t -> unit
    ) ->
    Format.formatter -> ('a,'b) t -> unit
    (** Printing function *)

  (** {4 Basic constructor} *)
  val bottom : ('a,'b) man -> 'a Env.t -> ('a,'b) t
  val top : ('a,'b) man -> 'a Env.t -> ('a,'b) t
  val of_apron : ('a,'b) man -> 'a Env.t -> 'b Apron.Abstract1.t -> ('a,'b) t
  val of_bddapron : ('a,'b) man -> 'a Env.t -> ('a Expr1.Bool.t * 'b Apron.Abstract1.t) list -> ('a,'b) t

  (** {4 Tests} *)

  val is_bottom : ('a,'b) man -> ('a,'b) t -> bool
  val is_top : ('a,'b) man -> ('a,'b) t -> bool
     (** Emtpiness and Universality tests *)

  val is_leq : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> bool
  val is_eq : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> bool
    (** Inclusion and equality tests *)

  (** {4 Extraction of properties} *)
  val to_bddapron : ('a,'b) man -> ('a,'b) t -> ('a Expr1.Bool.t * 'b Apron.Abstract1.t) list
    (** Conversion to a disjunction of a conjunction of pair of a
	purely Boolean formula (without numerical constraints) and
	an APRON abstract value *)

  (** {4 Operations} *)

  val meet : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val join : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    (** Meet and join *)

  val meet_condition : ('a,'b) man -> 'a Cond.t -> ('a,'b) t -> 'a Expr1.Bool.t -> ('a,'b) t
  val meet_condition2 : ('a,'b) man -> ('a,'b) t -> 'a Expr2.Bool.t -> ('a,'b) t
     (** Intersection with a Boolean expression (that may involve
	numerical constraints) *)

  val assign_lexpr :
    ?relational:bool -> ?nodependency:bool ->
    ('a,'b) man -> 'a Cond.t ->
    ('a,'b) t -> 'a list -> 'a Expr1.t list -> ('a,'b) t option -> ('a,'b) t
  val assign_listexpr2 :
    ?relational:bool -> ?nodependency:bool ->
    ('a,'b) man ->
    ('a,'b) t -> 'a list -> 'a Expr2.List.t -> ('a,'b) t option ->
    ('a,'b) t
  val substitute_lexpr :
    ('a,'b) man -> 'a Cond.t ->
    ('a,'b) t -> 'a list -> 'a Expr1.t list -> ('a,'b) t option -> ('a,'b) t
  val substitute_listexpr2 :
    ('a,'b) man ->
    ('a,'b) t -> 'a list -> 'a Expr2.List.t -> ('a,'b) t option -> ('a,'b) t
    (** Parallel assignement/substitution of a list of variables
	by a list of expressions *)

  val forget_list :
    ('a,'b) man -> ('a,'b) t -> 'a list -> ('a,'b) t
    (** Forget (existential quantification) a list of variables *)
  val widening : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val widening_threshold : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> Apron.Lincons1.earray -> ('a,'b) t
    (** Widening *)

  (** {4 Change of environments and renaming} *)
  val change_environment : ('a,'b) man -> ('a,'b) t -> 'a Env.t -> ('a,'b) t
    (** Change the environment (eliminate (forget) variables not
	belonging to the new environment, and introduce new
	variables) *)
  val rename : ('a,'b) man -> ('a,'b) t -> ('a*'a) list -> ('a,'b) t
    (** Rename a list of variables (thus changing the
	environment). *)
  val unify : ('a,'b) man -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
    (** Unify two abstract values on their least common
	environment (lce, that should exist, which implies that no
	variable is defined with different types in the two
	initial environments).

	This is equivalent to change the environment to the lce, and
	to perform meet.  *)
end

module Make(Level0:Level0) :
  (Level1 with type ('a,'b) man = ('a,'b) Level0.man
	  and type 'b t0 = 'b Level0.t)
