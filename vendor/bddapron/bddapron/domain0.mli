(** Boolean/Numerical domain: generic interface *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 Generic interface} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {4 Types} *)
(*  ====================================================================== *)

type ('a,'b,'c,'d) man = {
  typ : string;
  man : 'c;
  canonicalize : ?apron:bool -> 'c -> 'd -> unit;
  size : 'c -> 'd -> int;
  print :
    ?print_apron:(
      (int -> string) ->
	Format.formatter -> 'b Apron.Abstract0.t -> unit
    ) ->
    'a Env.t -> Format.formatter -> 'd -> unit;
  bottom : 'c -> 'a Env.t -> 'd;
  top : 'c -> 'a Env.t -> 'd;
  of_apron : 'c -> 'a Env.t -> 'b Apron.Abstract0.t -> 'd;
  of_bddapron : 'c -> 'a Env.t -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list -> 'd;
  is_bottom : 'c -> 'd -> bool;
  is_top : 'c -> 'd -> bool;
  is_leq : 'c -> 'd -> 'd -> bool;
  is_eq : 'c -> 'd -> 'd -> bool;
  to_bddapron : 'c -> 'd -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list;
  meet : 'c -> 'd -> 'd -> 'd;
  join : 'c -> 'd -> 'd -> 'd;
  meet_condition : 'c -> 'a Env.t -> 'a Cond.t -> 'd -> 'a Expr0.Bool.t -> 'd;
  assign_lexpr : ?relational:bool -> ?nodependency:bool -> 'c ->  'a Env.t -> 'a Cond.t -> 'd -> 'a list -> 'a Expr0.t list -> 'd option -> 'd;
  substitute_lexpr : 'c -> 'a Env.t -> 'a Cond.t -> 'd -> 'a list -> 'a Expr0.t list -> 'd option -> 'd;
  forget_list : 'c -> 'a Env.t -> 'd -> 'a list -> 'd;
  forall_bool_list : 'c -> 'a Env.t -> 'd -> 'a list -> 'd;
  widening : 'c -> 'd -> 'd -> 'd;
  widening_threshold : 'c -> 'd -> 'd -> Apron.Lincons0.t array -> 'd;
  apply_change :  bottom:'d -> 'c -> 'd -> Env.change -> 'd;
  apply_permutation : 'c -> 'd -> int array option * Apron.Dim.perm option -> 'd;
}
(** Type of generic managers.

    - ['a]: type of symbols
    - ['b]: as in ['b Apron.Manager.t]
	    ([Box.t], [Polka.strict Polka.t], etc);
    - ['c]: type of the underlying manager;
    - ['d]: type of the underlying abstract values of level 0.
*)

type 'd t = 'd
(** Type of generic abstract values *)

(*  ====================================================================== *)
(** {4 Functions} *)
(*  ====================================================================== *)

val canonicalize : ?apron:bool -> ('a,'b,'c,'d) man -> 'd t -> unit
val size : ('a,'b,'c,'d) man -> 'd t -> int
val print :
  ?print_apron:(
    (int -> string) ->
      Format.formatter -> 'b Apron.Abstract0.t -> unit
  ) ->
  ('a,'b,'c,'d) man -> 'a Env.t -> Format.formatter -> 'd t -> unit
val bottom : ('a,'b,'c,'d) man -> 'a Env.t -> 'd t
val top : ('a,'b,'c,'d) man -> 'a Env.t -> 'd t
val of_apron : ('a,'b,'c,'d) man -> 'a Env.t -> 'b Apron.Abstract0.t -> 'd t
val of_bddapron : ('a,'b,'c,'d) man -> 'a Env.t -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list -> 'd t
val is_bottom : ('a,'b,'c,'d) man -> 'd t -> bool
val is_top : ('a,'b,'c,'d) man -> 'd t -> bool
val is_leq : ('a,'b,'c,'d) man -> 'd t -> 'd t -> bool
val is_eq : ('a,'b,'c,'d) man -> 'd t -> 'd t -> bool
val to_bddapron :
  ('a,'b,'c,'d) man -> 'd t -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list
val meet : ('a,'b,'c,'d) man -> 'd t -> 'd t -> 'd t
val join : ('a,'b,'c,'d) man -> 'd t -> 'd t -> 'd t
val meet_condition :
  ('a,'b,'c,'d) man -> 'a Env.t -> 'a Cond.t -> 'd t -> 'a Expr0.Bool.t -> 'd t
val assign_lexpr :
  ?relational:bool -> ?nodependency:bool ->
  ('a,'b,'c,'d) man ->
  'a Env.t -> 'a Cond.t -> 'd t -> 'a list -> 'a Expr0.t list -> 'd t option -> 'd t
val substitute_lexpr :
  ('a,'b,'c,'d) man ->
  'a Env.t -> 'a Cond.t -> 'd t -> 'a list -> 'a Expr0.t list -> 'd t option -> 'd t
val forget_list : ('a,'b,'c,'d) man -> 'a Env.t -> 'd t -> 'a list -> 'd t
val forall_bool_list : ('a,'b,'c,'d) man -> 'a Env.t -> 'd t -> 'a list -> 'd t
val widening : ('a,'b,'c,'d) man -> 'd t -> 'd t -> 'd t
val widening_threshold : ('a,'b,'c,'d) man -> 'd t -> 'd t -> Apron.Lincons0.t array -> 'd t
val apply_change : bottom:'d t -> ('a,'b,'c,'d) man -> 'd t -> Env.change -> 'd t
val apply_permutation : ('a,'b,'c,'d) man -> 'd t -> int array option * Apron.Dim.perm option -> 'd t

val man_get_apron : ('a,'b,'c,'d) man -> 'b Apron.Manager.t

(*  ********************************************************************** *)
(** {3 Implementation based on {!Mtbdddomain0}} *)
(*  ********************************************************************** *)

type ('a,'b) mtbdd =
  (
    'a,
    'b,
    ('a,'b) Mtbdddomain0.man,
    'b Mtbdddomain0.t
  ) man

val mtbdd_of_mtbdddomain : ('a,'b) Mtbdddomain0.man -> ('a,'b) mtbdd
  (** Make a mtbdd manager from an underlying BDDAPRON manager *)
val make_mtbdd : ?global:bool -> 'b Apron.Manager.t -> ('a,'b) mtbdd
  (** Make a mtbdd manager from an APRON manager *)

(*  ====================================================================== *)
(** {4 Type conversion functions} *)
(*  ====================================================================== *)

val man_is_mtbdd : ('a,'b,'c,'d) man -> bool
  (** Return [true] iff the argument manager is a mtbdd manager *)
val man_of_mtbdd : ('a,'b) mtbdd -> ('a,'b,'c,'d) man
  (** Makes a mtbdd manager generic *)
val man_to_mtbdd : ('a,'b,'c,'d) man -> ('a,'b) mtbdd
  (** Instanciate the type of a mtbdd manager.
      Raises [Failure] if the argument manager is not a mtbdd manager *)

val of_mtbdd : ('a,'b) mtbdd * 'b Mtbdddomain0.t t -> ('a,'b,'c,'d) man * 'd t
  (** Makes a pair (mtbdd manager,mtbdd abstract value) generic *)
val to_mtbdd : ('a,'b,'c,'d) man * 'd t -> ('a,'b) mtbdd * 'b Mtbdddomain0.t t
  (** Instanciate the type of a pair (mtbdd manager,mtbdd abstract value).
      Raises [Failure] if the argument manager is not a mtbdd manager *)

(*  ********************************************************************** *)
(** {3 Implementation based on {!Bdddomain0}} *)
(*  ********************************************************************** *)

type ('a,'b) bdd =
  (
    'a,
    'b,
    ('a,'b) Bdddomain0.man,
    'b Bdddomain0.t
  ) man

val bdd_of_bdddomain : ('a,'b) Bdddomain0.man -> ('a,'b) bdd
  (** Make a bdd manager from an underlying BDDAPRON manager *)
val make_bdd : 'b Apron.Manager.t -> ('a,'b) bdd
  (** Make a bdd manager from an APRON manager *)

(*  ====================================================================== *)
(** {4 Type conversion functions} *)
(*  ====================================================================== *)

val man_is_bdd : ('a,'b,'c,'d) man -> bool
  (** Return [true] iff the argument manager is a bdd manager *)
val man_of_bdd : ('a,'b) bdd -> ('a,'b,'c,'d) man
  (** Makes a bdd manager generic *)
val man_to_bdd : ('a,'b,'c,'d) man -> ('a,'b) bdd
  (** Instanciate the type of a bdd manager.
      Raises [Failure] if the argument manager is not a bdd manager *)

val of_bdd : ('a,'b) bdd * 'b Bdddomain0.t t -> ('a,'b,'c,'d) man * 'd t
  (** Makes a pair (bdd manager,bdd abstract value) generic *)
val to_bdd : ('a,'b,'c,'d) man * 'd t -> ('a,'b) bdd * 'b Bdddomain0.t t
  (** Instanciate the type of a pair (bdd manager,bdd abstract value).
      Raises [Failure] if the argument manager is not a bdd manager *)
