(******************************************************************************)
(* TimeMeas *)
(* measure time *)
(* author: Peter Schrammel *)
(* version: 0.9.1m *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

type timemeas_t = 
  {mutable running: bool; 
   mutable starttime: float;
   mutable accutime: float}

val create : unit -> timemeas_t
val start : timemeas_t -> unit
val stop : timemeas_t -> unit
val reset : timemeas_t -> unit

val print : Format.formatter -> timemeas_t -> unit
