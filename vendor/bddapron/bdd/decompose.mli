(** Separation of Boolean formula in purely Boolean/conditional parts *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type vdd = bool Cudd.Vdd.t
val vdd_of_bdd : Cudd.Bdd.vt -> bool Cudd.Vdd.t
val bdd_of_vdd : bool Cudd.Vdd.t -> Cudd.Bdd.vt

type typ = Bool | Cond | Other
type info = {
  mutable minlevelbool : int;
  mutable maxlevelbool : int;
  mutable minlevelcond : int;
  mutable maxlevelcond : int;
  varlevel : int array;
  levelvar : int array;
  vartyp : typ array;
  leveltyp : typ array;
}

val make_info :
  ('a, 'b, 'c, 'd, 'e) Env.t0 -> ('f, 'g, 'h, 'i) Cond.t -> info
  (** Builds a temporary record of type [info] which gathers
      various informations on environment and condition. *)

val split_level : Cudd.Bdd.vt -> int -> (Cudd.Bdd.vt * Cudd.Bdd.vt) list
  (** Decompose a BDD [f] into a disjunction [[(f1,g1);...;(fN,gN)]] such that
      - all decisions in f1...fN have levels strictly less than [level];
      - all decisions in g1...gN have levels greater than or equal to [level];
      - f1...fN are pairwise disjoint.
  *)

val splitpermutation_of_envcond :
  ('a, 'b, 'c, 'd, 'e) Env.t0 -> ('f, 'g, 'h, 'i) Cond.t ->
  [`BoolCond | `CondBool ] ->
  int * (int array * int array) option
  (** Two cases in [(level,operm)=splitpermutation_of_envcond ...]:
      - [operm=None]: a BDD should be split according to [level];
      - [operm=Some(perm1,perm2)]: a BDD should be split by
	applying permutation [perm1], splitting the result
	according to [level], and applying to the resulting BDDs
	the inverse permutation [perm2].
  *)

val split_bdd :
  ?memo1:Cudd.Memo.t -> ?memo2:Cudd.Memo.t ->
  int * (int array * int array) option ->
  Cudd.Bdd.vt -> (Cudd.Bdd.vt * Cudd.Bdd.vt) list

val cube_split :
  ('a, 'b, 'c, 'd) Cond.t -> 'd Cudd.Bdd.t -> 'd Cudd.Bdd.t * 'd Cudd.Bdd.t
    (** Split a cube into a cube of Booleans and a cube of conditions *)

val decompose_bdd_boolcond :
  ('a, 'b, 'c, 'd, 'e) Env.t0 -> ('f, 'g, 'h, 'i) Cond.t ->
  Cudd.Bdd.vt -> (Cudd.Bdd.vt * Cudd.Bdd.vt) list
  (** Decompose a BDD [f] into a disjunction [[(f1,g1);...;(fN,gN)]] such that
      - all decisions in f1...fN are Boolean variables;
      - all decisions in g1...gN are (non-Boolean) conditions;
      - f1...fN are pairwise disjoint.
  *)
val decompose_bdd_condbool :
  ('a, 'b, 'c, 'd, 'e) Env.t0 -> ('f, 'g, 'h, 'i) Cond.t ->
  Cudd.Bdd.vt -> (Cudd.Bdd.vt * Cudd.Bdd.vt) list
  (** Dual version *)

val decompose_dd_treecondbool :
  ?careset:'a Cudd.Bdd.t ->
  topvar:('b -> int) ->
  support:('b -> 'c Cudd.Bdd.t) ->
  cofactor:('b -> 'a Cudd.Bdd.t -> 'b) ->
  ('d, 'e, 'f, 'g, 'h) Env.t0 ->
  ('i, 'j, 'k, 'a) Cond.t -> 'b -> (int, 'b) Normalform.tree
  (** Internal use, look at the code.
      Be cautious: support is supposed to return a support intersected with conditions *)

val decompose_bdd_treecondbool :
   ('a, 'b, 'c, 'd, 'e) Env.t0 -> ('f, 'g, 'h, 'i) Cond.t ->
  'i Cudd.Bdd.t -> (int,'i Cudd.Bdd.t) Normalform.tree
val decompose_vdd_treecondbool :
  ?careset:Cudd.Bdd.vt ->
  ('a, 'b, 'c, 'd, 'e) Env.t0 -> ('f, 'g, 'h, Cudd.Man.v) Cond.t ->
  'i Cudd.Vdd.t -> (int,'i Cudd.Vdd.t) Normalform.tree
  (** Decompose a BDD/MTBDD into a tree with decisions on conditions,
      and purely Boolean BDDs on leaves *)

val decompose_tbdd_tvdd_treecondbool :
  ?careset:Cudd.Bdd.vt ->
  ('a, 'b, 'c, 'd, 'e) Env.t0 ->
  ('f, 'g, 'h, Cudd.Man.v) Cond.t ->
  Cudd.Bdd.vt array * 'i Cudd.Vdd.t array ->
  (int, Cudd.Bdd.vt array * 'i Cudd.Vdd.t array) Normalform.tree
    (** Same for an array of BDDs/VDDs *)

val conjunction_of_minterm :
  ?first:int -> ?last:int ->
  (int * bool -> 'a) -> Cudd.Man.tbool array -> 'a Normalform.conjunction
  (** [conjunction_of_minterm of_idb minterm] translates a
      minterm into an explicit conjunction of type ['a
      Normalform.conjunction], using the function [of_idb] to
      produce elements of type ['a].

      The optional arguments [first] and [last] allows focusing only
      on a part of the minterm.
  *)

val dnf_of_bdd :
  ?first:int -> ?last:int ->
  (int * bool -> 'a) ->
  'b Cudd.Bdd.t -> 'a Normalform.dnf
  (** Converts a BDD into a disjunctive normal form.  The
      arguments are the same as in {!conjunction_of_minterm}. *)

val descend :
  cudd:'c Cudd.Man.t ->
  maxdepth:int ->
  nocare:('a -> bool) ->
  cube_of_down:('a -> 'c Cudd.Bdd.t) ->
  cofactor:('a -> 'c Cudd.Bdd.t -> 'a) ->
  select:('a -> int) ->
  terminal:(depth:int ->
	    newcube:'c Cudd.Bdd.t -> cube:'c Cudd.Bdd.t -> down:'a -> 'b option) ->
  ite:(depth:int ->
       newcube:'c Cudd.Bdd.t ->
       cond:int -> dthen:'b option -> delse:'b option -> 'b option) ->
  down:'a -> 'b option

val select_cond: 'd Cudd.Bdd.t -> int
val select_cond_bdd : ('a,'b,'c,'d) Cond.t -> 'd Cudd.Bdd.t -> int
val bdd_support_cond :
  ('a, 'b, 'c, 'd) Cond.t -> 'd Cudd.Bdd.t -> 'd Cudd.Bdd.t
val vdd_support_cond :
  ('a, 'b, 'c, Cudd.Man.v) Cond.t -> 'd Cudd.Vdd.t -> Cudd.Bdd.vt
val tbdd_tvdd_support_cond :
  ('a, 'b, 'c, Cudd.Man.v) Cond.t -> Cudd.Bdd.vt array * 'd Cudd.Vdd.t array ->
  Cudd.Bdd.vt
val tbdd_tvdd_cofactor :
  Cudd.Bdd.vt array * 'a Cudd.Vdd.t array -> Cudd.Bdd.vt ->
  Cudd.Bdd.vt array * 'a Cudd.Vdd.t array
