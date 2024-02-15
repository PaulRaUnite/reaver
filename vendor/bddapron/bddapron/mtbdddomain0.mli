(** Boolean/Numerical domain, with MTBDDs over APRON values *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type ('a,'b) man = 'b ApronDD.man
  (** BDDAPRON Manager. The type parameter ['b] indicates the
      underlying APRON abstract domain, as in type {!'b
      Apron.Abstract1.t} *)

type 'b t = 'b ApronDD.t
  (** BDDAPRON Abstract value. *)

val make_man : ?global:bool -> 'b Apron.Manager.t -> ('a,'b) man
  (** Makes a BDDAPRON manager from an APRON manager.
      If [global=true] (default: [false]), uses a global (persistent)
      BDD cache for the operations [is_leq], [join], [meet]
      and [exist] (internal).
  *)

val size : ('a,'b) man -> 'b t -> int
  (** Size of an abstract value in terms of number of nodes of the MTBDD. *)
val print :
  ?print_apron:(
    (int -> string) ->
      Format.formatter -> 'b Apron.Abstract0.t -> unit
  ) ->
  'a Env.t -> Format.formatter -> 'b t -> unit
  (** Printing function *)

(*  ********************************************************************** *)
(** {3 Constructors, accessors, tests and property extraction} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {4 Basic constructor} *)
(*  ====================================================================== *)

val bottom : ('a,'b) man -> 'a Env.t -> 'b t
val top : ('a,'b) man -> 'a Env.t -> 'b t
val of_apron : ('a,'b) man -> 'a Env.t -> 'b Apron.Abstract0.t -> 'b t
val of_bddapron : ('a,'b) man -> 'a Env.t -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list -> 'b t

(*  ====================================================================== *)
(** {4 Tests} *)
(*  ====================================================================== *)

val is_bottom : ('a,'b) man -> 'b t -> bool
val is_top : ('a,'b) man -> 'b t -> bool
  (** Emtpiness and Universality tests *)

val is_leq : ('a,'b) man -> 'b t -> 'b t -> bool
val is_eq : ('a,'b) man -> 'b t -> 'b t -> bool
  (** Inclusion and equality tests *)

(*  ====================================================================== *)
(** {4 Extraction of properties} *)
(*  ====================================================================== *)

val to_bddapron : ('a,'b) man -> 'b t -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list
  (** Conversion to a disjunction of a conjunction of pair of a
      purely Boolean formula (without numerical constraints) and an
      APRON abstract value *)

(*  ********************************************************************** *)
(** {3 Operations} *)
(*  ********************************************************************** *)

val meet : ('a,'b) man -> 'b t -> 'b t -> 'b t
val join : ('a,'b) man -> 'b t -> 'b t -> 'b t
  (** Meet and join *)

val meet_condition : ('a,'b) man -> 'a Env.t -> 'a Cond.t -> 'b t -> 'a Expr0.Bool.t -> 'b t
  (** Intersection with a Boolean expression (that may involve
      numerical constraints) *)

val assign_lexpr :
  ?relational:bool -> ?nodependency:bool ->
  ('a,'b) man -> 'a Env.t -> 'a Cond.t ->
  'b t -> 'a list -> 'a Expr0.t list -> 'b t option -> 'b t
val substitute_lexpr :
  ('a,'b) man -> 'a Env.t -> 'a Cond.t ->
  'b t -> 'a list -> 'a Expr0.t list -> 'b t option -> 'b t
  (** Parallel assignement/substitution of a list of variables by
      a list of expressions *)

val forget_list :
  ('a,'b) man -> 'a Env.t -> 'b t -> 'a list -> 'b t
  (** Forget (existential quantification) a list of variables *)

val forall_bool_list :
  ('a,'b) man -> 'a Env.t -> 'b t -> 'a list -> 'b t
(** Universal quantification over a list of Boolean variables *)

val widening : ('a,'b) man -> 'b t -> 'b t -> 'b t
val widening_threshold : ('a,'b) man -> 'b t -> 'b t -> Apron.Lincons0.t array -> 'b t
  (** Widening *)

val apply_change :
  bottom:'b t -> ('a,'b) man -> 'b t -> Env.change -> 'b t
val apply_permutation :
  ('a,'b) man -> 'b t -> int array option * Apron.Dim.perm option -> 'b t

(*  ********************************************************************** *)
(** {3 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows extensions). The functions above are actually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig
  val meet_idcondb :
    ('a,'b) man -> 'c -> ('a,'c) Cond.O.t -> 'b t -> int * bool -> 'b t
  val size : ('a,'b) man -> 'b t -> int
  val print :
    ?print_apron:(
      (int -> string) ->
	Format.formatter -> 'b Apron.Abstract0.t -> unit
    ) ->
    ('a,'c,'d,'e) Env.O.t -> Format.formatter -> 'b t -> unit
  val bottom : ('a,'b) man -> ('a,'c,'d,'e) Env.O.t -> 'b t
  val top : ('a,'b) man -> ('a,'c,'d,'e) Env.O.t -> 'b t
  val of_apron : ('a,'b) man -> ('a,'c,'d,'e) Env.O.t -> 'b Apron.Abstract0.t -> 'b t
  val of_bddapron : ('a,'b) man -> ('a,'c,'d,'e) Env.O.t -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list -> 'b t
  val is_bottom : ('a,'b) man -> 'b t -> bool
  val is_top : ('a,'b) man -> 'b t -> bool
  val is_leq : ('a,'b) man -> 'b t -> 'b t -> bool
  val is_eq : ('a,'b) man -> 'b t -> 'b t -> bool
  val to_bddapron : ('a,'b) man -> 'b t -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list
  val meet : ('a,'b) man -> 'b t -> 'b t -> 'b t
  val join : ('a,'b) man -> 'b t -> 'b t -> 'b t
  val meet_cube : ('a,'b) man -> 'c -> ('a,'c) Cond.O.t -> 'b t -> 'b t -> 'a Expr0.Bool.t -> 'b t
  val meet_condition : ('a,'b) man -> 'c -> ('a,'c) Cond.O.t -> 'b t -> 'a Expr0.Bool.t -> 'b t
  val assign_lexpr :
    ?relational:bool -> ?nodependency:bool ->
    ('a,'b) man -> 'c -> ('a,'c) Cond.O.t ->
    'b t -> 'a list -> 'a Expr0.t list -> 'b t option -> 'b t
  val substitute_lexpr :
    ('a,'b) man -> 'c -> ('a,'c) Cond.O.t ->
    'b t -> 'a list -> 'a Expr0.t list -> 'b t option -> 'b t
  val forget_list :
    ('a,'b) man -> ('a,'c,'d,'e) Env.O.t -> 'b t -> 'a list -> 'b t
  val forall_bool_list :
    ('a,'b) man -> ('a,'c,'d,'e) Env.O.t -> 'b t -> 'a list -> 'b t
  val widening : ('a,'b) man -> 'b t -> 'b t -> 'b t
  val widening_threshold : ('a,'b) man -> 'b t -> 'b t -> Apron.Lincons0.t array -> 'b t
  val apply_change :
    bottom:'b t -> ('a,'b) man -> 'b t -> Env.change -> 'b t
  val apply_permutation :
    ('a,'b) man -> 'b t -> int array option * Apron.Dim.perm option -> 'b t

end
