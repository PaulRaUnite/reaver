(******************************************************************************)
(* analysis *)
(* analysis interface *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Analysis";
              Log.level=Log.Debug}

(******************************************************************************)
(** {2 Analysis Module Type } *)
(******************************************************************************)

type direction_t = ApronAccel.direction_t

type bddapron_res_t = (Cfg.locid_t, (Env.boolexpr_t * ApronUtil.linconss_t) list) Mappe.t
type refine_loc_t = refine_bool:bool -> Cfg.locid_t -> Loc.t -> Loc.t option
type print_result_t =  Format.formatter -> unit -> unit
type result_to_bddapron_t = unit -> bddapron_res_t

type analyze_t = Env.t -> Program.cfprog_t -> 
  (bool * refine_loc_t * print_result_t * result_to_bddapron_t)

let bddapron_res_empty = Mappe.empty

module type T = 
  sig
    type analysisparam_t

    val analyze : analysisparam_t -> analyze_t
  end

(*
(* add cfg refinement for new domains here *)
let refine ~refine_bool env locid loc anres =
  let is_bottom s =
    match s with
      |StrictPolPow s -> DomainStd.StrictPolPow.is_bottom env s
      |StrictPolProd s -> DomainStd.StrictPolProd.is_bottom env s
  in
  let to_boolexpr s =
    if refine_bool then
      match s with
        |StrictPolPow s -> DomainStd.StrictPolPow.to_boolexprbool env s
        |StrictPolProd s -> DomainStd.StrictPolProd.to_boolexprbool env s
    else
      match s with
        |StrictPolPow s -> DomainStd.StrictPolPow.to_boolexpr env s
        |StrictPolProd s -> DomainStd.StrictPolProd.to_boolexpr env s
  in
  try
    let s = Mappe.find locid anres in
    if is_bottom s then None
    else Some (Loc.refine_inv env loc (to_boolexpr s))
  with
    Not_found -> None 

(* add intersection check for new domains here *)
let intersects env s boolexpr =
  not 
  (
  match s with
    |StrictPolPow s -> DomainStd.StrictPolPow.is_bottom env
       (DomainStd.StrictPolPow.meet_condition env s boolexpr)
    |StrictPolProd s -> DomainStd.StrictPolProd.is_bottom env
       (DomainStd.StrictPolProd.meet_condition env s boolexpr)
  )

(* add printing for new domains here *)
let print_domain env fmt s =
  match s with
    |StrictPolPow s -> DomainStd.StrictPolPow.print env fmt s
    |StrictPolProd s -> DomainStd.StrictPolProd.print env fmt s
*)

