(**  Argument, Options and Parsing of command line *)

(* This file is part of the Interproc analyzer, released under GPL license.
   Please read the COPYING file packaged in the distribution.

   Copyright (C) Mathias Argoud, Gaël Lalire, Bertrand Jeannet 2007.
*)

(** input filename *)
let inputfilename = ref ""

(** debug level *)
let debug = ref 0

(** margin for display *)
let margin = ref 78

(** printing format for polyhedra *)
type print =
  | Print_box
  | Print_poly
  | Print_boxpoly
let print = ref Print_boxpoly

(*  ---------------------------------------------------------------------- *)
(** {3 Fixpoint iteration} *)
(*  ---------------------------------------------------------------------- *)

let analysis_threshold = ref false

let iteration_depth = ref 2
let iteration_guided = ref false
let widening_start = ref 1
let widening_descend = ref 2
let dot_fmt = ref None
let logtemplate = ref 1
let eps = ref 0
let quadrant = ref false
let nbbits = ref 0
let nbbitsu = ref 0
let approximate = ref 11

(*  ---------------------------------------------------------------------- *)
(** {3 Comparisons} *)
(*  ---------------------------------------------------------------------- *)

let comparison = ref false
  (** Comparison with std method and derivative closure method *)

(*  ---------------------------------------------------------------------- *)
(** {3 Main specification list} *)
(*  ---------------------------------------------------------------------- *)

let (speclist:Arg2.entry list) =
     [
      (
	"debug",
	Arg2.Set_int(debug),
	"<int>", (" : debug level, from 0 to 5 (default:0)":Arg2.doc)
      );
      (
	"print",
	Arg2.Symbol(
	  ["box";"poly";"boxpoly"],
	  begin fun str ->
	    print := match str with
	    | "box" -> Print_box
	    | "poly" -> Print_poly
	    | "boxpoly" -> Print_boxpoly
	    | _ -> raise (Arg2.Bad ("Wrong argument `"^str^"' to option -print"))
	  end),
      "box|poly|boxpoly", (" : @[print convex polyhedra under the form of@ either its bounding box, itelf, or both (default)@]":Arg2.doc)
      );
      (
	"dot",
	Arg2.String(begin fun filename ->
	  let dotfile = open_out filename in
	  let dotfmt = Format.formatter_of_out_channel dotfile in
	  dot_fmt := Some dotfmt;
	end),
	"<filename>" ,(" : @[activate DOT output to the given file@ (depends on the -debug option)@]":Arg2.doc)
      );
      (
	"cmp",
	Arg2.Bool(begin fun b -> comparison := b end),
	"<bool>", (" : if true, compares with std Kleene iteration and derivative closure method":Arg2.doc)
      );
      (
	"margin",
	Arg2.Set_int(margin),
	"<int>", (" : right margin to use for display":Arg2.doc)
      );
      (
	"depth",
	Arg2.Int(begin fun n ->
	  if n<2 then
	    raise (Arg2.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-depth' expects an integer >= 2"))
	  else
	    iteration_depth := n
	end),
	"<int>", (" : @[depth of recursive iterations@ (default 2, may only be more)@]":Arg2.doc)
      );
      (
	"guided",
	Arg2.Bool(begin fun b -> iteration_guided := b end),
	"<bool>", (" : guided analysis of Gopand and Reps (default: false)":Arg2.doc)
      );
      (
	"threshold",
	Arg2.Bool(begin fun b -> analysis_threshold := b end),
	"<bool>", (" : infers thresholds and then uses widneing with thresholds (default: false)":Arg2.doc)
      );
      (
	"widening",
	Arg2.Tuple([
	  Arg2.Int(begin fun n ->
	    if n<0 then
	      raise (Arg2.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-widening' expects a positive integer for its `widening start' argument"))
	    else
	      widening_start := n
	  end);
	  Arg2.Int(begin fun n ->
	    if n<0 then
	      raise (Arg2.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-widening' expects a positive integer for its `descending' argument"))
	    else
	      widening_descend := n
	  end)
	]),
	"<int><int>", (" : @[specifies usage of delay,@ and nb. of descending steps@ (default: 1 2)@]":Arg2.doc)
      );
     (
	"approx",
	Arg2.Tuple([
	  Arg2.Int(begin fun n ->
	    if n!=10 && n!=11 && n!=12 && n!=20 && n!=21 && n!=22 then
	      raise (Arg2.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-approx' expects 10, 11, 12, 20, 21, 22 for its first argument"))
	    else
	      approximate := n
	  end);
	  Arg2.Int(begin fun n ->
	    if n<0 then
	      raise (Arg2.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-approx' expects a positive integer for its second argument"))
	    else
	      nbbits := n
	  end);
	  Arg2.Int(begin fun n ->
	    if n<0 then
	      raise (Arg2.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-approx' expects a positive integer for its third argument"))
	    else
	      nbbitsu := n
	  end)
	]),
	"<int><int><int>", (" : @[specifies approximation method (10, 11 or 12),@ rounding of constraints on maximum <n> bits after single image computation,@ and after their convex hull@ (default: 0 0, meaning no rounding)@]":Arg2.doc)
      );
      (
	"log",
	Arg2.Int(begin fun n ->
	  if n<0 then
	    raise (Arg2.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-log' expects an integer >= 0"))
	  else
	    logtemplate := n
	end),
	"<int>", (" : @[if equal to n, consider template expressions@ 2^k1 x1 +/- 2^k2 x2@ with 0<=k1,k2<n.@ n=0 generates box constraints,@ n=1 generates octagonal constraints.@]":Arg2.doc)
      );
      (
	"quadrant",
	Arg2.Set_bool(quadrant),
	"<bool>", (" : @[if true, handle precisely interval-linear constraint@ by separating quadrants.@]":Arg2.doc)
      );
     (
	"eps",
	Arg2.Int(begin fun n ->
	  if n<0 then
	    raise (Arg2.Bad ("Wrong argument `"^(string_of_int n)^"'; option `-eps' expects an integer >= 0"))
	  else
	    eps := n
	end),
	"<int>", (" : @[if equal to n, round constraints by adding to them |cst|/2^n .@]":Arg2.doc)
      );
     ]

let t = (("jordan <options> <inputfile>":Arg2.usage_msg),10,speclist,(fun name -> inputfilename := name))
