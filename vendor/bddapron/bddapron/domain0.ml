(** Boolean/Numerical domain: generic interface *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {3 Generic interface} *)
(*  ********************************************************************** *)

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

let canonicalize ?apron man = man.canonicalize ?apron man.man
let size man = man.size man.man
let print ?print_apron man = man.print ?print_apron
let bottom man = man.bottom man.man
let top man = man.top man.man
let of_apron man = man.of_apron man.man
let of_bddapron man = man.of_bddapron man.man
let is_bottom man = man.is_bottom man.man
let is_top man = man.is_top man.man
let is_leq man = man.is_leq man.man
let is_eq man = man.is_eq man.man
let to_bddapron man = man.to_bddapron man.man
let meet man = man.meet man.man
let join man = man.join man.man
let meet_condition man = man.meet_condition man.man
let assign_lexpr ?relational ?nodependency man = man.assign_lexpr ?relational ?nodependency man.man
let substitute_lexpr man = man.substitute_lexpr man.man
let forget_list man = man.forget_list man.man
let forall_bool_list man = man.forall_bool_list man.man
let widening man = man.widening man.man
let widening_threshold man = man.widening_threshold man.man
let apply_change ~bottom man = man.apply_change ~bottom man.man
let apply_permutation man = man.apply_permutation man.man

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

let mtbdd_of_mtbdddomain (man:('a,'b) Mtbdddomain0.man) : ('a,'b) mtbdd =
  {
    typ = "mtbdd";
    man = man;
    canonicalize = (fun ?apron _ _ -> ());
    size = Mtbdddomain0.size;
    print = Mtbdddomain0.print;
    bottom = Mtbdddomain0.bottom;
    top = Mtbdddomain0.top;
    of_apron = Mtbdddomain0.of_apron;
    of_bddapron = Mtbdddomain0.of_bddapron;
    is_bottom = Mtbdddomain0.is_bottom;
    is_top = Mtbdddomain0.is_top;
    is_leq = Mtbdddomain0.is_leq;
    is_eq = Mtbdddomain0.is_eq;
    to_bddapron = Mtbdddomain0.to_bddapron;
    meet = Mtbdddomain0.meet;
    join = Mtbdddomain0.join;
    meet_condition = Mtbdddomain0.meet_condition;
    assign_lexpr = Mtbdddomain0.assign_lexpr;
    substitute_lexpr = Mtbdddomain0.substitute_lexpr;
    forget_list = Mtbdddomain0.forget_list;
    forall_bool_list = Mtbdddomain0.forall_bool_list;
    widening = Mtbdddomain0.widening;
    widening_threshold = Mtbdddomain0.widening_threshold;
    apply_change = Mtbdddomain0.apply_change;
    apply_permutation = Mtbdddomain0.apply_permutation;
  }

let make_mtbdd ?global (apron:'b Apron.Manager.t) : ('a,'b) mtbdd =
  let man = Mtbdddomain0.make_man ?global apron in
  mtbdd_of_mtbdddomain man

let man_of_mtbdd (man:('a,'b) mtbdd) : ('a,'b,'c,'d) man =
  Obj.magic man
let of_mtbdd (manabs:('a,'b) mtbdd * 'b Mtbdddomain0.t) : ('a,'b,'c,'d) man * 'd t =
  Obj.magic manabs

let man_is_mtbdd (man:('a,'b,'c,'d) man) =
  man.typ="mtbdd"

let man_to_mtbdd (man:('a,'b,'c,'d) man) : ('a,'b) mtbdd =
  if man_is_mtbdd man then
    Obj.magic man
  else
    failwith ""
let to_mtbdd (manabs:('a,'b,'c,'d) man * 'd t) : ('a,'b) mtbdd * 'b Mtbdddomain0.t =
  if man_is_mtbdd (fst manabs) then
    Obj.magic manabs
  else
    failwith ""

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

let bdd_of_bdddomain (man:('a,'b) Bdddomain0.man) : ('a,'b) bdd =
  {
    typ = "bdd";
    man = man;
    canonicalize = (Bdddomain0.canonicalize ~unique:true ~disjoint:true);
    size = Bdddomain0.size;
    print = Bdddomain0.print;
    bottom = Bdddomain0.bottom;
    top = Bdddomain0.top;
    of_apron = Bdddomain0.of_apron;
    of_bddapron = Bdddomain0.of_bddapron;
    is_bottom = Bdddomain0.is_bottom;
    is_top = Bdddomain0.is_top;
    is_leq = Bdddomain0.is_leq;
    is_eq = Bdddomain0.is_eq;
    to_bddapron = Bdddomain0.to_bddapron;
    meet = Bdddomain0.meet;
    join = Bdddomain0.join;
    meet_condition = Bdddomain0.meet_condition;
    assign_lexpr = Bdddomain0.assign_lexpr;
    substitute_lexpr = Bdddomain0.substitute_lexpr;
    forget_list = Bdddomain0.forget_list;
    forall_bool_list = Bdddomain0.forall_bool_list;
    widening = Bdddomain0.widening;
    widening_threshold = Bdddomain0.widening_threshold;
    apply_change = Bdddomain0.apply_change;
    apply_permutation = Bdddomain0.apply_permutation;
  }
let make_bdd (apron:'b Apron.Manager.t) : ('a,'b) bdd =
  let man = Bdddomain0.make_man apron in
  bdd_of_bdddomain man

let man_of_bdd (man:('a,'b) bdd) : ('a,'b,'c,'d) man =
  Obj.magic man
let of_bdd (manabs:('a,'b) bdd * 'b Bdddomain0.t) : ('a,'b,'c,'d) man * 'd t =
  Obj.magic manabs

let man_is_bdd man =
  man.typ="bdd"

let man_to_bdd (man:('a,'b,'c,'d) man) : ('a,'b) bdd =
  if man_is_bdd man then
    Obj.magic man
  else
    failwith ""
let to_bdd (manabs:('a,'b,'c,'d) man * 'd t) : ('a,'b) bdd * 'b Bdddomain0.t =
  if man_is_bdd (fst manabs) then
    Obj.magic manabs
  else
    failwith ""

(*  ********************************************************************** *)
(** {3 Generic functions} *)
(*  ********************************************************************** *)

let man_get_apron (man:('a,'b,'c,'d) man) : 'b Apron.Manager.t =
  if man_is_bdd man then
  let man = man_to_bdd man in
  let bdd = man.man in
  let apron = bdd.Bdddomain0.apron in
   apron
  else if man_is_mtbdd man then
    let man = man_to_mtbdd man in
    let mtbdd = man.man in
    let apron = mtbdd.ApronDD.apron in
    apron
  else
    failwith ""
