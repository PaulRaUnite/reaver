(** Recursive descend on sets of diagrams (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

val texpr_cofactor :
  ('a Expr0.t -> 'b -> 'a Expr0.t) ->
  'a Expr0.t array -> 'b -> 'a Expr0.t array
val texpr_support :
  ('a,'b) Cond.O.t -> 'a Expr0.t array -> Cudd.Man.v Cudd.Bdd.t
val texpr_cofactors :
  ('a,'b,'c,'d) Env.O.t ->
  'a Expr0.t array -> int -> 'a Expr0.t array * 'a Expr0.t array
val split_lvar :
  'a Bdd.Env.symbol ->
  'a list -> 'a Expr0.t list -> 'a list * Apron.Var.t array
val split_texpr :
  'a Expr0.t array -> Cudd.Man.v Bdd.Expr0.t list * 'a ApronexprDD.t array
val split_lvarlexpr :
  'a Bdd.Env.symbol -> 'a list -> 'a Expr0.t list ->
  'a list * Cudd.Man.v Bdd.Expr0.t list * Apron.Var.t array *
  'a ApronexprDD.t array
val cofactors :
  'a ApronDD.man -> 'c -> ('b,'c) Cond.O.t ->
  'a ApronDD.t -> int -> 'a ApronDD.t * 'a ApronDD.t
val descend_mtbdd :
  'a ApronDD.man -> 'c -> ('b,'c) Cond.O.t ->
  ('a ApronDD.t -> 'b Expr0.t array -> 'a ApronDD.t) ->
  'a ApronDD.t -> 'b Expr0.t array -> 'a ApronDD.t

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
  (** Obsolete, moved in {!Bdd.Decompose} *)
