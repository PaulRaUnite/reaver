(** Boolean/Numerical domain with normalized environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

val make_man : ?global:bool -> 'b Apron.Manager.t -> ('a,'b) Mtbdddomain0.man
  (** Makes a BDDAPRON manager from an APRON manager.
      If [global=true] (default: [false]), uses a global (persistent)
      BDD cache for the operations [is_leq], [join], [meet]
      and [exist] (internal).
  *)

include Domainlevel1.Level1 with type ('a,'b) man = ('a,'b) Mtbdddomain0.man
			    and type 'b t0 = 'b Mtbdddomain0.t
