(** Functions common to the two implementations of Combined Boolean/Numerical domain *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

val tcons0_array_of_cubecond :
  (('a, 'b, 'c, 'd) Env.O.t as 'e) ->
  ('a,'e) Cond.O.t ->
  Cudd.Bdd.vt -> Apron.Tcons0.t array
    (** Converts a cube of conditions into an array of APRON constraints *)

val lvar_split :
    ('a,'c,'d,'e) Env.O.t -> 'a list -> Cudd.Man.v Cudd.Bdd.t * Apron.Dim.t array
    (** Split the list of variable into a positive cube (support)
    of Boolean variables and an array of APRON dimensions *)

val condition_of_tcons0 :
  ('a, 'b, 'c, 'd) Env.O.t ->
  Apron.Tcons0.t -> [ `Bool of bool | `Cond of 'a Apronexpr.Condition.t ]

val bdd_of_tcons0 :
  (('a, 'b, 'c, 'd) Env.O.t as 'e) ->
  ('a,'e) Cond.O.t ->
  Apron.Tcons0.t -> Cudd.Bdd.vt
val bdd_of_tcons0_array :
  (('a, 'b, 'c, 'd) Env.O.t as 'e) ->
  ('a,'e) Cond.O.t ->
  Apron.Tcons0.t array -> Cudd.Bdd.vt
