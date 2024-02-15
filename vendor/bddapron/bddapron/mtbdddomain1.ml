(** Boolean/Numerical domain with normalized environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

module Level1 = Domainlevel1.Make(Mtbdddomain0)
include Level1

let make_man = Mtbdddomain0.make_man
