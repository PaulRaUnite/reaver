(** DDs on top of Apron abstract values (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type 'a leaf = 'a Apron.Abstract0.t
type 'a t = 'a leaf Cudd.Mtbddc.t
type 'a table = 'a leaf Cudd.Mtbddc.table

type 'a leaf_u = 'a leaf Cudd.Mtbddc.unique

type 'a global = {
  op_is_leq : ('a leaf_u, 'a leaf_u) Cudd.User.test2;
  op_join : ('a leaf_u, 'a leaf_u, 'a leaf_u) Cudd.User.op2;
  op_meet : ('a leaf_u, 'a leaf_u, 'a leaf_u) Cudd.User.op2;
  op_exist : 'a leaf_u Cudd.User.exist;
  op_forall : 'a leaf_u Cudd.User.exist;
}

type 'a man = {
  apron : 'a Apron.Manager.t;
  table : 'a table;
  oglobal : 'a global option;
}

val make_table : 'a Apron.Manager.t -> 'a table
val neutral_join : 'a Apron.Abstract0.t -> bool
val special_is_leq :
  'a Apron.Manager.t -> 'a t -> 'a t -> bool option
val special_join :
  'a Apron.Manager.t -> 'a t -> 'a t -> 'a t option
val special_meet :
  'a Apron.Manager.t -> 'a t -> 'a t -> 'a t option
val make_global :
  'a Apron.Manager.t -> 'a table -> 'a global
val make_man : ?global:bool -> 'a Apron.Manager.t -> 'a man
val make_op_join : 'a man -> ('a leaf_u,'a leaf_u,'a leaf_u) Cudd.User.op2
val print :
  ?print_apron:(
    (int -> string) ->
      Format.formatter -> 'a Apron.Abstract0.t -> unit
  ) ->
  (Format.formatter -> Cudd.Bdd.vt -> unit) ->
  (int -> string) ->
  Format.formatter -> 'a t -> unit
val cst : cudd:Cudd.Man.vt -> 'a man -> 'a Apron.Abstract0.t -> 'a t
val bottom : cudd:Cudd.Man.vt -> 'a man -> Apron.Dim.dimension -> 'a t
val top : cudd:Cudd.Man.vt -> 'a man -> Apron.Dim.dimension -> 'a t
val is_bottom : 'a man -> 'a t -> bool
val is_top : 'a man -> 'a t -> bool
val is_eq : 'a man -> 'a t -> 'a t -> bool
val is_leq : 'a man -> 'a t -> 'a t -> bool
val join : 'a man -> 'a t -> 'a t -> 'a t
val meet : 'a man -> 'a t -> 'a t -> 'a t
val widening : 'a man -> 'a t -> 'a t -> 'a t
val widening_threshold : 'a man -> 'a t -> 'a t -> Apron.Lincons0.t array -> 'a t
val meet_tcons_array : 'a man -> 'a t -> Apron.Tcons0.t array -> 'a t
val forget_array : 'a man -> 'a t -> Apron.Dim.t array -> 'a t
val permute_dimensions : 'a man -> 'a t -> Apron.Dim.perm -> 'a t
val add_dimensions : 'a man -> 'a t -> Apron.Dim.change -> bool -> 'a t
val remove_dimensions : 'a man -> 'a t -> Apron.Dim.change -> 'a t
val apply_dimchange2 : 'a man -> 'a t -> Apron.Dim.change2 -> bool -> 'a t

type asssub = Assign | Substitute

val asssub_texpr_array :
  ?asssub_bdd:(Cudd.Bdd.vt -> Cudd.Bdd.vt) ->
  asssub ->
  'b Bdd.Env.symbol ->
  'a man ->
  Apron.Environment.t ->
  'a t ->
  Apron.Dim.t array -> 'b ApronexprDD.t array -> 'a t option -> 'a t

val assign_texpr_array :
  'b Bdd.Env.symbol ->
  'a man -> Apron.Environment.t ->
  'a t -> Apron.Dim.t array -> 'b ApronexprDD.t array -> 'a t option -> 'a t
val substitute_texpr_array :
  'b Bdd.Env.symbol ->
  'a man -> Apron.Environment.t ->
  'a t -> Apron.Dim.t array -> 'b ApronexprDD.t array -> 'a t option -> 'a t

(*
val make_funjoin :
  'a man -> ('a leaf_u, Cudd.User.global) Cudd.User.mexist
val make_funjoin2 :
  'a man -> 'a t -> ('a leaf_u, Cudd.User.global) Cudd.User.mexist
*)
val exist : 'a man -> supp:Cudd.Bdd.vt -> 'a t -> 'a t
val existand :
  'a man ->
  bottom:'a Apron.Abstract0.t Cudd.Mtbddc.unique ->
  supp:Cudd.Bdd.vt -> Cudd.Bdd.vt -> 'a t -> 'a t
val forall : 'a man -> supp:Cudd.Bdd.vt -> 'a t -> 'a t
