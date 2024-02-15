(** Boolean/Numerical domain with normalized environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

module Level1 = Domainlevel1.Make(Bdddomain0)
include Level1

let make_man = Bdddomain0.make_man
let canonicalize ?apron ?unique ?disjoint man t =
  Bdddomain0.canonicalize ?apron ?unique ?disjoint man t.Env.val0
