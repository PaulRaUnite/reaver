(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** framework base: control flow graphs *)

(** {2 Types} *)

(** location id *)
type locid_t = int

(** arc id *)
type arcid_t = int

(** next available location and arc ids *)
type info_t = { mutable locidx : locid_t; mutable arcidx : arcid_t;}

(** CFG *)
type t = (locid_t, arcid_t, Loc.t, Arc.t, info_t) PSHGraph.t


(** {2 Printing} *)

(** print CFG in dot format *)
val print_dot : Env.t -> Format.formatter -> t -> bool -> unit

(** print CFG (only location definitions) *)
val print_short : Env.t -> Format.formatter -> t -> unit


(** {2 Constructors} *)

(** creates a CFG with the given discrete and continuous equations 
    and invariant *)
val make : Env.t -> Env.equs_t -> Env.equs_t -> Env.boolexpr_t -> t

(** creates a CFG partitioned by the given list of expressions *)
val make_partitioned : Env.t -> t -> Env.boolexpr_t list -> Env.boolexpr_t -> t

(** creates an empty CFG *)
val make_empty : unit -> t

(** duplicates a CFG *)
val copy : t -> t

(** computes the map of corresponding locations assuming that cfg1 <= cfg2 *)
val get_locmap : Env.t -> t -> t -> (locid_t * (locid_t list)) list


(** {2 Operations} *)

(** adds a location *)
val add_loc :  Env.t -> t -> Loc.t -> locid_t

(** adds the given number of loccations *)
val add_locs :  Env.t -> t -> Loc.t -> int -> locid_t PSette.t

(** adds an arc *)
val add_arc : Env.t -> t -> locid_t -> locid_t -> Arc.t -> 
   arcid_t

(** adds an arc if it is Boolean-feasible *)
val add_arc_if_reach : Env.t -> t -> locid_t -> locid_t -> Arc.t -> 
  Env.boolexpr_t -> arcid_t option

(** splits the location by the given expression *)
val split_location : Env.t -> t -> locid_t -> Env.boolexpr_t -> 
  Env.boolexpr_t -> locid_t list

(** removes the given arc if it is Boolean-infeasible *)
val remove_infeasible_arc : Env.t -> t ->  Env.boolexpr_t -> arcid_t -> t

(** removes the the Boolean-infeasible arcs *)
val remove_infeasible_arcs : Env.t -> t ->  Env.boolexpr_t -> t

(** removes the the Boolean-infeasible outgoing arcs in the given location *)
val remove_infeasible_arcs_in_loc : Env.t -> t ->  Env.boolexpr_t -> locid_t -> t

(** removes the locations unreachable (in the graph sense) from the locations 
    defined by the given invariant *)
val remove_unreachable_locs : Env.t -> t ->  Env.boolexpr_t -> unit

(** copies the location with the given location id and replaces 
    its associated data *)
val copy_location_with : Env.t -> t -> locid_t -> Loc.t -> 
  Env.boolexpr_t -> t * locid_t


(** updates the data associated to the given location id *)
val update_location_with : Env.t -> t -> locid_t -> Loc.t -> 
  Env.boolexpr_t -> unit

(** {2 Getting locations and arcs} *)

(** returns the data associated to the given location id *)
val get_loc : t -> locid_t -> Loc.t

(** returns the data associated to the given arc id *)
val get_arc : t -> arcid_t -> Arc.t

(** returns the only arc of the CFG *)
val get_only_arc : Env.t -> t -> Arc.t

(** returns the only location of the CFG *)
val get_only_loc : t -> Loc.t

(** returns the only location id of the CFG *)
val get_only_locid : t -> locid_t

(** returns the only predecessor location id of the given arc *)
val get_predlocid : t -> arcid_t -> locid_t

(** returns the only successor location id of the given arc *)
val get_succlocid : t -> arcid_t -> locid_t

(** returns the set of location ids *)
val get_locidset : t -> locid_t PSette.t

(** returns the set of location ids corresponding to the given 
    location definition (invariant) *)
val get_locidset_by_inv : Env.t -> t -> Env.boolexpr_t -> locid_t PSette.t

(** returns the empty set of location ids*)
val empty_locidset : locid_t PSette.t

(** {2 Getting arcs by type} *)

val get_arcs : t -> (Arc.t -> bool) -> 
  (int * Arc.t * int * int) PSette.t

val get_arcs2 : t -> (Arc.t -> (Env.boolexpr_t * Env.equs_t) option) -> 
  (int * Env.boolexpr_t * Env.equs_t * int * int) PSette.t
val get_normal_arcs : t -> 
  (int * Env.boolexpr_t * Env.equs_t * int * int) PSette.t
val get_loop_arcs : t -> 
  (int * Env.boolexpr_t * Env.equs_t * int * int) PSette.t
val get_accel_arcs : t -> 
  (int * Env.boolexpr_t * Env.equs_t * int * int) PSette.t
val get_boolaccel_arcs : t -> 
  (int * Env.boolexpr_t * Env.equs_t * int * int) PSette.t
val get_boolnaccaccel_arcs : t -> 
  (int * Env.boolexpr_t * Env.equs_t * int * int) PSette.t
val get_bool_arcs : t -> 
  (int * Env.boolexpr_t * Env.equs_t * int * int) PSette.t
val get_boolnacc_arcs : t -> 
  (int * Env.boolexpr_t * Env.equs_t * int * int) PSette.t

val get_normal_arcs_in_loc : t -> int ->
  (int * Env.boolexpr_t * Env.equs_t * int * int) PSette.t
val get_flow_arcs_in_loc : t -> int -> 
  (int * Env.boolexpr_t * Env.equs_t) PSette.t

val get_init_arcs : Env.t -> Env.boolexpr_t -> t -> int PSette.t

val get_flow_arc : Env.t -> t -> int ->  (int * Env.boolexpr_t * Env.equs_t) option

(** {2 Miscelleaneous} *)

val cfg_compare : (int, int) PSHGraph.compare
val arcs_compare : int * Arc.t * int * int -> int * Arc.t * int * int -> int
val arcs_compare2 : int * Env.boolexpr_t * Env.equs_t * int * int -> int * Env.boolexpr_t * Env.equs_t * int * int -> int

val locid_dummy : int
val arcid_dummy : int
