(******************************************************************************)
(* loc *)
(* data associated to a location of a control flow graph*)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(******************************************************************************)

type t = Env.boolexpr_t

let get_inv loc = loc
let make_inv inv = inv
let refine_inv env loc phi = 
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond loc phi

let print env fmt loc = 
  BddapronUtil.print_boolexpr env.Env.env env.Env.cond fmt loc
