(** Extra-operations on formula *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

module O : sig
  module Expr0 : sig
    module Bool : sig
      val to_lconjunction :
	(('a,'b,'c,'d) Env.O.t as 'e) -> ('a,'e) Cond.O.t ->
	'f Expr0.Bool.t ->
	('f Expr0.Bool.t * 'f Expr0.Bool.t) list
      val forget :
	('a, 'b, 'c, 'd) Domain0.man ->
	(('e,'f,'g,'h)  Env.O.t as 'i) -> ('e,'i) Cond.O.t ->
	'j Expr0.Bool.t -> 'e list -> 'j Expr0.Bool.t
    end
  end
  module Expr1 : sig
    module Bool : sig
      val to_lconjunction :
	('a,'b) Cond.O.t ->
	('a,'b) Expr1.O.Bool.t ->
	(('a,'b) Expr1.O.Bool.t * ('a,'b) Expr1.O.Bool.t) list
      val forget :
	('a, 'b, 'c, 'd) Domain0.man ->
	('e,'i) Cond.O.t ->
	('e,'i) Expr1.O.Bool.t -> 'e list -> ('e,'i) Expr1.O.Bool.t
    end
  end
end

module Expr0 : sig
  module Bool : sig
    val to_lconjunction :
      'a Env.t -> 'a Cond.t ->
      'a Expr0.Bool.t ->
      ('a Expr0.Bool.t * 'a Expr0.Bool.t) list
    val forget :
      ('a, 'b, 'c, 'd) Domain0.man ->
      'e Env.t -> 'e Cond.t ->
      'e Expr0.Bool.t -> 'e list -> 'e Expr0.Bool.t
  end
end
module Expr1 : sig
  module Bool : sig
    val to_lconjunction :
      'a Cond.t ->
      'a Expr1.Bool.t ->
      ('a Expr1.Bool.t * 'a Expr1.Bool.t) list
    val forget :
      ('a, 'b, 'c, 'd) Domain0.man ->
      'e Cond.t ->
      'e Expr1.Bool.t -> 'e list -> 'e Expr1.Bool.t
  end
end
