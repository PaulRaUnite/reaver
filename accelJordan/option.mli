(**  Argument, Options and Parsing of command line *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

val inputfilename : string ref
  (** input filename *)
val debug : int ref
  (** debug level *)
val margin : int ref
  (** margin for display *)
type print =
  | Print_box
  | Print_poly
  | Print_boxpoly
val print : print ref
  (** printing format for polyhedra *)

(*  ---------------------------------------------------------------------- *)
(** {3 Fixpoint iteration} *)
(*  ---------------------------------------------------------------------- *)

val analysis_threshold : bool ref

val iteration_depth : int ref
  (** Depth of recursion in iteration. If the depth is deeper, one tries to
      stabilize inner loops first before propagating to enclosing loops.*)

val iteration_guided : bool ref
  (** Guided iteration technique *)

val widening_start : int ref
  (** Number of steps without widening *)

val widening_descend : int ref
  (** Number of descending iterations *)

val dot_fmt : Format.formatter option ref
  (** Optional dot output *)

val logtemplate : int ref
  (** if equal to n, we consider the template a x_1 +/- (1-a) x2 with a=k/2^n  *)

val quadrant : bool ref
  (** if true, take into account interval linear constraints by separating cases/quadrants *)

val approximate : int ref
  (** Approximation algorithm *)
val nbbits : int ref
  (** rounding of constraints after computation of the image of a polyhedron by a vertex matrix *)
val nbbitsu : int ref
  (** idem after computing their convex hull *)

val eps : int ref

(*  ---------------------------------------------------------------------- *)
(** {3 Comparisons} *)
(*  ---------------------------------------------------------------------- *)

val comparison : bool ref
  (** Comparison with std method and derivative closure method *)

(*  ---------------------------------------------------------------------- *)
(** {3 Speclist} *)
(*  ---------------------------------------------------------------------- *)

val speclist : Arg2.entry list
val t : Arg2.t
