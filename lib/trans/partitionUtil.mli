(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** transformation implementation: utilities for partitioning *)

(** computes the partition induced by the given list of predicates *)
val compute_partition_expr_list : Env.t ->  Env.boolexpr_t -> Env.boolexpr_t list -> Env.boolexpr_t list

(** partitions all locations by the given list of predicates *)
val partition_all_by : Env.t -> Cfg.t -> Cfg.locid_t PSette.t -> 
  Env.boolexpr_t -> Env.boolexpr_t list -> unit

(** partitions the given location by the given partition *)
val partition_disjoint_loc : Env.t -> Cfg.t -> Env.boolexpr_t -> Cfg.locid_t -> 
Env.boolexpr_t list -> unit

(** partitions until convergence 
     trying to partition the current location and its successors 
     by a heuristic returning a list of partition equations *) 
val partition_recsucc : Env.t -> Cfg.t -> Env.boolexpr_t -> 
  (Env.t ->  Env.equs_t -> Env.boolexpr_t -> 
  Env.boolexpr_t -> Env.boolexpr_t PSette.t) ->
  Cfg.locid_t PSette.t -> unit

(** computes the partition characterized by the numerical modes with common numerical guards *)
val nummodes1 : Env.vars_t -> Env.t -> Env.equs_t -> Env.boolexpr_t ->  Env.boolexpr_t PSette.t

(** computes the partition characterized by the numerical modes *)
val nummodes2 : Env.vars_t -> Env.t -> Env.equs_t -> Env.boolexpr_t ->  Env.boolexpr_t PSette.t

