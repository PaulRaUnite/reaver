(** Boolean/Numerical domain with normalized environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

include Domainlevel1.Level1 with type ('a,'b) man = ('a,'b) Bdddomain0.man
			    and type 'b t0 = 'b Bdddomain0.t


val make_man : 'b Apron.Manager.t -> ('a,'b) man
  (** Makes a BDDAPRON manager from an APRON manager, and fills
  options with default values *)

val canonicalize : ?apron:bool -> ?unique:bool -> ?disjoint:bool -> ('a,'b) man -> ('a,'b) t -> unit
  (** Canonicalize an abstract value by ensuring uniqueness and
      disjointness properties. If [apron] is true, then also
      normalize APRON abstract values. By default: [apron=false,
      unique=disjoint=true]. *)
