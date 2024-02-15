(** Finite-type expressions with BDDs *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(** This module allows to manipulate structured BDDs, where variables involved
  in the Boolean formula are not only Boolean variables, but also of bounded
  integer or enumerated type (such types are encoded with several Boolean
  variables).
*)

(*  ********************************************************************** *)
(** {3 Expressions} *)
(*  ********************************************************************** *)

type 'a t = [
  | `Bool of 'a Cudd.Bdd.t
      (** Boolean *)
  | `Bint of 'a Int.t
      (** Bounded integer *)
  | `Benum of 'a Enum.t
      (** Enumerated *)
]
type 'a expr = 'a t
  (** Type of general expressions *)

type dt = Cudd.Man.d t
type vt = Cudd.Man.v t
  (** *)

(** General expressions are described below, after Boolean, bounded
    integer and enumerated types expressions *)

(*  ====================================================================== *)
(** {4 Boolean expressions} *)
(*  ====================================================================== *)

module Bool : sig
  type 'a t = 'a Cudd.Bdd.t
  type dt = Cudd.Man.d t
  type vt = Cudd.Man.v t
  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr

  val dtrue : ('a,'b) Env.t -> 'b t
  val dfalse : ('a,'b) Env.t -> 'b t
  val of_bool : ('a,'b) Env.t -> bool -> 'b t
  val var : ('a,'b) Env.t -> 'a -> 'b t

  val dnot : ('a,'b) Env.t -> 'b t -> 'b t
  val dand : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t
  val dor : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t
    (** [not], [and] and [or] (use of 'd' prefix because of conflict with OCaml
	keywords) *)

  val xor : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t
  val nand : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t
  val nor : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t
  val nxor : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t
    (** Exclusive or, not and, nor or and not xor *)

  val leq : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t
    (** Implication *)
  val eq : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t
    (** Same as [nxor] *)

  val ite : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t -> 'b t
    (** If-then-else *)


  val is_true : ('a,'b) Env.t -> 'b t -> bool
  val is_false : ('a,'b) Env.t -> 'b t -> bool
  val is_cst : ('a,'b) Env.t -> 'b t -> bool
  val is_eq : ('a,'b) Env.t -> 'b t -> 'b t -> bool
  val is_leq : ('a,'b) Env.t -> 'b t -> 'b t -> bool
  val is_and_false : ('a,'b) Env.t -> 'b t -> 'b t -> bool

  val exist : ('a,'b) Env.t -> 'a list -> 'b t -> 'b t
  val forall : ('a,'b) Env.t -> 'a list -> 'b t -> 'b t

  val cofactor : 'a t -> 'a t -> 'a t
  val restrict : 'a t -> 'a t -> 'a t
  val tdrestrict : 'a t -> 'a t -> 'a t
  val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
  val varmap : 'a t -> 'a t

  val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b) Env.t -> 'b t -> ('a * 'a) list -> 'b t
  val substitute : ?memo:Cudd.Memo.t -> ('a,'b) Env.t -> 'b t -> ('a * 'b expr) list -> 'b t

  val print :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b) Env.t -> Format.formatter -> 'b t -> unit
end

(*  ====================================================================== *)
(** {4 Bounded integer expressions} *)
(*  ====================================================================== *)

module Bint : sig
  type 'a t = 'a Int.t
  type dt = Cudd.Man.d t
  type vt = Cudd.Man.v t
  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr

  val of_int : ('a,'b) Env.t -> [`Bint of bool * int] -> int -> 'b t
  val var : ('a,'b) Env.t -> 'a -> 'b t
  val ite : ('a,'b) Env.t -> 'b Bool.t -> 'b t -> 'b t -> 'b t

  val neg : ('a,'b) Env.t -> 'b t -> 'b t
  val succ : ('a,'b) Env.t -> 'b t -> 'b t
  val pred : ('a,'b) Env.t -> 'b t -> 'b t
  val add : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t
  val sub : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t
  val mul : ('a,'b) Env.t -> 'b t -> 'b t -> 'b t
  val shift_left : ('a,'b) Env.t -> int -> 'b t -> 'b t
  val shift_right : ('a,'b) Env.t -> int -> 'b t -> 'b t
  val scale : ('a,'b) Env.t -> int -> 'b t -> 'b t
  val zero : ('a,'b) Env.t -> 'b t -> 'b Bool.t
  val eq : ('a,'b) Env.t -> 'b t -> 'b t -> 'b Bool.t
  val eq_int : ('a,'b) Env.t -> 'b t -> int -> 'b Bool.t
  val supeq : ('a,'b) Env.t -> 'b t -> 'b t -> 'b Bool.t
  val supeq_int : ('a,'b) Env.t -> 'b t -> int -> 'b Bool.t
  val sup : ('a,'b) Env.t -> 'b t -> 'b t -> 'b Bool.t
  val sup_int : ('a,'b) Env.t -> 'b t -> int -> 'b Bool.t

  val cofactor : 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t
  val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
  val varmap : 'a t -> 'a t

  val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b) Env.t -> 'b t -> ('a * 'a) list -> 'b t
  val substitute : ?memo:Cudd.Memo.t -> ('a,'b) Env.t -> 'b t -> ('a * 'b expr) list -> 'b t

  val guard_of_int: ('a,'b) Env.t -> 'b t -> int -> 'b Bool.t
    (** Return the guard of the integer value. *)
  val guardints: ('a,'b) Env.t -> 'b t -> ('b Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

  val print :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b) Env.t -> Format.formatter -> 'b t -> unit
end

(*  ====================================================================== *)
(** {4 Enumerated expressions} *)
(*  ====================================================================== *)

module Benum : sig
  type 'a t = 'a Enum.t
  type dt = Cudd.Man.d t
  type vt = Cudd.Man.v t
  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr
  val var : ('a,'b) Env.t -> 'a -> 'b t
  val ite : ('a,'b) Env.t -> 'b Bool.t -> 'b t -> 'b t -> 'b t
  val eq : ('a,'b) Env.t -> 'b t -> 'b t -> 'b Bool.t
  val eq_label : ('a,'b) Env.t -> 'b t -> 'a -> 'b Bool.t
  val cofactor : 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t
  val permute : ?memo:Cudd.Memo.t -> 'a t -> int array -> 'a t
  val varmap : 'a t -> 'a t
  val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b) Env.t -> 'b t -> ('a * 'a) list -> 'b t
  val substitute : ?memo:Cudd.Memo.t -> ('a,'b) Env.t -> 'b t -> ('a * 'b expr) list -> 'b t
  val guard_of_label : ('a,'b) Env.t -> 'b t -> 'a -> 'b Bool.t
    (** Return the guard of the label. *)
  val guardlabels : ('a,'b) Env.t -> 'b t -> ('b Bool.t * 'a) list
    (** Return the list [g -> label] of guarded values. *)
  val print :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b) Env.t -> Format.formatter -> 'b t -> unit
end

(*  ====================================================================== *)
(** {4 General (typed) expressions} *)
(*  ====================================================================== *)

(** The following operations raise a [Failure] exception in case
    of a typing error. *)

val typ_of_expr : ('a,'b) Env.t -> 'b t -> 'a Env.typ
    (** Type of an expression *)

val var : ('a,'b) Env.t -> 'a -> 'b t
    (** Expression representing the litteral var *)

val ite : 'a Bool.t -> 'a t -> 'a t -> 'a t
    (** If-then-else operation *)

val eq : ('a,'b) Env.t -> 'b t -> 'b t -> 'b Bool.t
    (** Equality operation *)

val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b) Env.t -> 'b t -> ('a * 'a) list -> 'b t
val substitute_by_var_list : ?memo:Cudd.Memo.t -> ('a,'b) Env.t -> 'b t list -> ('a * 'a) list -> 'b t list
    (** Variable renaming.

	The new variables should already have been declared *)

val substitute : ?memo:Cudd.Memo.t -> ('a,'b) Env.t -> 'b t -> ('a * 'b t) list -> 'b t
val substitute_list : ?memo:Cudd.Memo.t -> ('a,'b) Env.t -> 'b t list -> ('a * 'b t) list -> 'b t list
    (** Parallel substitution of variables by expressions *)

val cofactor : 'a t -> 'a Cudd.Bdd.t -> 'a t
    (** Evaluate the expression. The BDD is assumed to be a cube *)

val restrict : 'a t -> 'a Cudd.Bdd.t -> 'a t
val tdrestrict : 'a t -> 'a Cudd.Bdd.t -> 'a t
    (** Simplify the expression knowing that the BDD is true.  Generalizes
      [cofactor]. *)

val support : ('a,'b) Env.t -> 'b t -> 'a PSette.t
    (** Support of the expression *)

val support_cond : 'a Cudd.Man.t -> 'a t -> 'a Cudd.Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
	identifiers involved in the expression *)

(*  ---------------------------------------------------------------------- *)
(** {5 Miscellaneous} *)
(*  ---------------------------------------------------------------------- *)

val cube_of_bdd : ('a,'b) Env.t -> 'b Cudd.Bdd.t -> 'b Cudd.Bdd.t
      (** Same as [Cudd.Bdd.cube_of_bdd], but keep only the
	the values of variables having a determinated value.

	Example: the classical [Cudd.Bdd.cube_of_bdd] could return
	[b and (x=1 or x=3)], whereas [cube_of_bdd] will return only [b] in
	such a case. *)

(*  ---------------------------------------------------------------------- *)
(** {5 Printing} *)
(*  ---------------------------------------------------------------------- *)

val print :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b) Env.t -> Format.formatter -> 'b t -> unit
  (** Print an expression *)

val print_minterm :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b) Env.t -> Format.formatter -> Cudd.Man.tbool array -> unit
  (** Print a minterm *)
val print_bdd :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b) Env.t -> Format.formatter -> 'b Cudd.Bdd.t -> unit
  (** Print a BDD *)
val print_idcondb :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b) Env.t -> Format.formatter -> int*bool -> unit
  (** Print the condition represented by the signed BDD index. *)
val print_idcond :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b) Env.t -> Format.formatter -> int -> unit
  (** Print the condition *)

(*  ********************************************************************** *)
(** {3 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows etxensions). The functions above are axtually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig

  val tid_of_var : ('a,'b,'c,'d,'e) Env.O.t -> 'a -> int array
  val reg_of_expr : 'd expr -> 'd Cudd.Bdd.t array

  (*  ==================================================================== *)
  (** {4 Expressions} *)
  (*  ==================================================================== *)

  (*  -------------------------------------------------------------------- *)
  (** {5 Boolean expressions} *)
  (*  -------------------------------------------------------------------- *)

  module Bool : sig
    type 'd t = 'd Cudd.Bdd.t
    type dt = Cudd.Man.d t
    type vt = Cudd.Man.v t
    val of_expr : [>`Bool of 'd t] -> 'd t
    val to_expr : 'd t -> [>`Bool of 'd t]

    val dtrue : ('a,'b,'c,'d,'e) Env.O.t -> 'd t
    val dfalse : ('a,'b,'c,'d,'e) Env.O.t -> 'd t
    val of_bool : ('a,'b,'c,'d,'e) Env.O.t -> bool -> 'd t
    val var : ('a,'b,'c,'d,'e) Env.O.t -> 'a -> 'd t

    val dnot : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t
    val dand : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t
    val dor : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t
    (** [not], [and] and [or] (use of 'd' prefix because of conflict with OCaml
	keywords) *)

    val xor : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t
    val nand : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t
    val nor : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t
    val nxor : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t
    (** Exclusive or, not and, nor or and not xor *)

    val leq : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t
    (** Implication *)
    val eq : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t
    (** Same as [nxor] *)

    val ite : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t -> 'd t
    (** If-then-else *)

    val is_true : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> bool
    val is_false : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> bool
    val is_cst : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> bool
    val is_eq : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> bool
    val is_leq : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> bool
    val is_and_false : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> bool

    val exist : ('a,'b,'c,'d,'e) Env.O.t -> 'a list -> 'd t -> 'd t
    val forall : ('a,'b,'c,'d,'e) Env.O.t -> 'a list -> 'd t -> 'd t

    val cofactor : 'd t -> 'd t -> 'd t
    val restrict : 'd t -> 'd t -> 'd t
    val tdrestrict : 'd t -> 'd t -> 'd t
    val permute : ?memo:Cudd.Memo.t -> 'd t -> int array -> 'd t
    val varmap : 'a t -> 'a t

    val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> ('a * 'a) list -> 'd t
    val substitute : ?memo:Cudd.Memo.t -> ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> ('a * 'd expr) list -> 'd t

    val print :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b,'c,'d,'e) Env.O.t -> Format.formatter -> 'd t -> unit
  end

  (*  -------------------------------------------------------------------- *)
  (** {5 Bounded integer expressions} *)
  (*  -------------------------------------------------------------------- *)

  module Bint : sig
    type 'd t = 'd Int.t
    type dt = Cudd.Man.d t
    type vt = Cudd.Man.v t
    val of_expr : [> `Bint of 'd t] -> 'd t
    val to_expr : 'd t -> [> `Bint of 'd t]

    val of_int : ('a,'b,'c,'d,'e) Env.O.t -> [> `Bint of bool * int ] -> int -> 'd t
    val var : ('a,'b,'c,'d,'e) Env.O.t -> 'a -> 'd t
    val ite : ('a,'b,'c,'d,'e) Env.O.t -> 'd Bool.t -> 'd t -> 'd t -> 'd t

    val neg : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t
    val succ : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t
    val pred : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t
    val add : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t
    val sub : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t
    val mul : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd t
    val shift_left : ('a,'b,'c,'d,'e) Env.O.t -> int -> 'd t -> 'd t
    val shift_right : ('a,'b,'c,'d,'e) Env.O.t -> int -> 'd t -> 'd t
    val scale : ('a,'b,'c,'d,'e) Env.O.t -> int -> 'd t -> 'd t
    val zero : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd Bool.t
    val eq : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd Bool.t
    val eq_int : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> int -> 'd Bool.t
    val supeq : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd Bool.t
    val supeq_int : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> int -> 'd Bool.t
    val sup : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd Bool.t
    val sup_int : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> int -> 'd Bool.t

    val cofactor : 'd t -> 'd Bool.t -> 'd t
    val restrict : 'd t -> 'd Bool.t -> 'd t
    val tdrestrict : 'd t -> 'd Bool.t -> 'd t
    val permute : ?memo:Cudd.Memo.t -> 'd t -> int array -> 'd t
    val varmap : 'a t -> 'a t

    val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> ('a * 'a) list -> 'd t
    val substitute : ?memo:Cudd.Memo.t -> ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> ('a * 'd expr) list -> 'd t

    val guard_of_int: ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> int -> 'd Bool.t
    (** Return the guard of the integer value. *)
    val guardints: ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> ('d Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

    val print :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b,'c,'d,'e) Env.O.t -> Format.formatter -> 'd t -> unit
  end

  (*  -------------------------------------------------------------------- *)
  (** {5 Enumerated expressions} *)
  (*  -------------------------------------------------------------------- *)

  module Benum : sig
    type 'd t = 'd Enum.t
    type dt = Cudd.Man.d t
    type vt = Cudd.Man.v t
    val of_expr : [> `Benum of 'd t] -> 'd t
    val to_expr : 'd t -> [> `Benum of 'd t]
    val var : ('a,'b,'c,'d,'e) Env.O.t -> 'a -> 'd t
    val ite : ('a,'b,'c,'d,'e) Env.O.t -> 'd Bool.t -> 'd t -> 'd t -> 'd t
    val eq : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd Bool.t
    val eq_label : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'a -> 'd Bool.t
    val cofactor : 'd t -> 'd Bool.t -> 'd t
    val restrict : 'd t -> 'd Bool.t -> 'd t
    val tdrestrict : 'd t -> 'd Bool.t -> 'd t
    val permute : ?memo:Cudd.Memo.t -> 'd t -> int array -> 'd t
    val varmap : 'a t -> 'a t
    val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> ('a * 'a) list -> 'd t
    val substitute : ?memo:Cudd.Memo.t -> ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> ('a * 'd expr) list -> 'd t
    val guard_of_label : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'a -> 'd Bool.t
    (** Return the guard of the label. *)
    val guardlabels : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> ('d Bool.t * 'a) list
    (** Return the list [g -> label] of guarded values. *)
    val print :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b,'c,'d,'e) Env.O.t -> Format.formatter -> 'd t -> unit
  end

  (*  -------------------------------------------------------------------- *)
  (** {5 General (typed) expressions} *)
  (*  -------------------------------------------------------------------- *)

(** The following operations raise a [Failure] exception in case of a typing
  error. *)

  val typ_of_expr : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> [>'a Env.typ]
  (** Type of an expression *)

  val var : ('a,'b,'c,'d,'e) Env.O.t -> 'a -> 'd t
  (** Expression representing the litteral var *)

  val ite : 'd Bool.t -> 'd t -> 'd t -> 'd t
  (** If-then-else operation *)

  val eq : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd Bool.t
  (** Equality operation *)

  val substitute_by_var : ?memo:Cudd.Memo.t -> ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> ('a * 'a) list -> 'd t
  val substitute_by_var_list : ?memo:Cudd.Memo.t -> ('a,'b,'c,'d,'e) Env.O.t -> 'd t list -> ('a * 'a) list -> 'd t list
    (** Variable renaming.
	The new variables should already have been declared *)

  val substitute : ?memo:Cudd.Memo.t -> ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> ('a * 'd t) list -> 'd t
  val substitute_list : ?memo:Cudd.Memo.t -> ('a,'b,'c,'d,'e) Env.O.t -> 'd t list -> ('a * 'd t) list -> 'd t list
    (** Parallel substitution of variables by expressions *)

  val support : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'a PSette.t
    (** Support of the expression *)

  val support_cond : 'd Cudd.Man.t -> 'd t -> 'd Cudd.Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
	identifiers involved in the expression *)

  (*  -------------------------------------------------------------------- *)
  (** {5 Miscellaneous} *)
  (*  -------------------------------------------------------------------- *)

  val cube_of_bdd : ('a,'b,'c,'d,'e) Env.O.t -> 'd Cudd.Bdd.t -> 'd Cudd.Bdd.t
      (** Same as [Cudd.Bdd.cube_of_bdd], but keep only the
	  the values of variables having a determinated value.

	  Example: the classical [Cudd.Bdd.cube_of_bdd] could return
	  [b and (x=1 or x=3)], whereas [cube_of_bdd] will return only [b] in
	  such a case. *)

  val tbdd_of_texpr : 'd t array -> 'd Cudd.Bdd.t array
  (** Concatenates in an array the BDDs involved in the expressions *)
  val texpr_of_tbdd : 'd t array -> 'd Cudd.Bdd.t array -> 'd t array
  (** Inverse operation: rebuild an array of expressions from the old array of
      expressions (for the types) and the array of BDDs. *)


  (*  ==================================================================== *)
  (** {4 Printing} *)
  (*  ==================================================================== *)

  val print :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b,'c,'d,'e) Env.O.t -> Format.formatter -> [<'d t] -> unit
  (** Print an expression *)

  val print_minterm :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b,'c,'d,'e) Env.O.t -> Format.formatter -> Cudd.Man.tbool array -> unit
  (** Print a minterm *)
  val print_bdd :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b,'c,'d,'e) Env.O.t -> Format.formatter -> 'd Cudd.Bdd.t -> unit
  (** Print a BDD *)

  val print_idcondb :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b,'c,'d,'e) Env.O.t -> Format.formatter -> int*bool -> unit
  (** Print the condition represented by the signed BDD index. *)
  val print_idcond :
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b,'c,'d,'e) Env.O.t -> Format.formatter -> int -> unit
  (** Print the condition *)

  (*  ==================================================================== *)
  (** {4 Internal functions} *)
  (*  ==================================================================== *)

  (*  -------------------------------------------------------------------- *)
  (** {5 Permutation and composition} *)
  (*  -------------------------------------------------------------------- *)

  val permutation_of_rename :
    ('a,'b,'c,'d,'e) Env.O.t -> ('a * 'a) list -> int array
  val composition_of_lvarexpr :
    ('a,'b,'c,'d,'e) Env.O.t -> ('a * 'd t) list -> 'd Cudd.Bdd.t array
  val composition_of_lvarlexpr :
    ('a,'b,'c,'d,'e) Env.O.t -> 'a list -> 'd t list -> 'd Cudd.Bdd.t array
  val bddsupport : ('a,'b,'c,'d,'e) Env.O.t -> 'a list -> 'd Cudd.Bdd.t

  val varmap : 'a t -> 'a t
  val permute : ?memo:Cudd.Memo.t -> 'd t -> int array -> 'd t
  val compose : ?memo:Cudd.Memo.t -> 'd t -> 'd Cudd.Bdd.t array -> 'd t
  val permute_list : ?memo:Cudd.Memo.t -> 'd t list -> int array -> 'd t list
  val compose_list : ?memo:Cudd.Memo.t -> 'd t list -> 'd Cudd.Bdd.t array -> 'd t list

  (*  ==================================================================== *)
  (** {4 Conversion to expressions} *)
  (*  ==================================================================== *)

  module Expr : sig
    (** Syntax tree for printing *)

    (** Atom *)
    type 'a atom =
      | Tbool of 'a * bool
	(** variable name and sign *)
      | Tint of 'a * int list
	(** variable name and list of possible values *)
      | Tenum of 'a * 'a list
	(** variable name, possibly primed, and list of possible labels *)

    (** Basic term *)
    type 'a term =
      | Tatom of 'a atom
	(** *)
      | Texternal of (int * bool)
	(** Unregistered BDD identifier and a Boolean for possible negation *)
      | Tcst of bool
	(** Boolean constant *)

    val map_atom : ('a -> 'b) -> 'a atom -> 'b atom
    val map_term : ('a -> 'b) -> 'a term -> 'b term

    val term_of_vint : 'a -> 'd Int.t -> Reg.Minterm.t -> 'a term

    val term_of_venum :
      ('a,'b,'c,'d,'e) Env.O.t ->
      'a -> 'd Enum.t -> Reg.Minterm.t -> 'a term
    val term_of_idcondb :
      ('a,'b,'c,'d,'e) Env.O.t -> int * bool -> 'a term
    val bool_of_tbool : Cudd.Man.tbool -> bool
    val mand : 'a term list ref -> 'a term -> unit
    val conjunction_of_minterm :
      ('a,'b,'c,'d,'e) Env.O.t -> Cudd.Man.tbool array -> 'a term Normalform.conjunction
    val dnf_of_bdd :
      ('a,'b,'c,'d,'e) Env.O.t -> 'd Cudd.Bdd.t -> 'a term Normalform.dnf
    val print_term :
      ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
      ('a,'b,'c,'d,'e) Env.O.t -> Format.formatter -> 'a term -> unit
    val print_conjunction :
      ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
      ('a,'b,'c,'d,'e) Env.O.t -> Format.formatter -> 'a term Normalform.conjunction -> unit
    val print_dnf :
      ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
      ('a,'b,'c,'d,'e) Env.O.t -> Format.formatter -> 'a term Normalform.dnf -> unit

  end
end
