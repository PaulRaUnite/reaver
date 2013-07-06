(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** framework base: analysis *)

(** analysis direction (forward/backward) *)
type direction_t = ApronAccel.direction_t

(** logico-numerical analysis result *)
type bddapron_res_t = (Cfg.locid_t, (Env.boolexpr_t * ApronUtil.linconss_t) list) Mappe.t

(** callback for refinining the location definition of the CFG 
    by the analysis result *)
type refine_loc_t = refine_bool:bool -> Cfg.locid_t -> Loc.t -> Loc.t option

(** callback for printing the analysis result *)
type print_result_t =  Format.formatter -> unit -> unit

(** callback for getting the analysis result *)
type result_to_bddapron_t = unit -> bddapron_res_t

(** Analysis Interface: function type *)
type analyze_t = Env.t -> Program.cfprog_t -> 
  (bool * refine_loc_t * print_result_t * result_to_bddapron_t)

(** returns the empty analysis result *)
val bddapron_res_empty : bddapron_res_t


(** {2 Analysis Interface } *)

(** parametrizable analysis module *)
module type T = 
  sig
    type analysisparam_t (** analysis parameters *)

    val analyze : analysisparam_t -> analyze_t (** analysis *)
  end


