(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** analysis implementation: standard analyses with Kleene iteration and widening *)

(** {2 Discrete Standard Analysis } *)

(** analysis parameters: direction, delayed widening start and
      descending iterations *)
type std_param_t = {s_dir:Analysis.direction_t; s_ws:int; s_wd:int}

(** create parameters *)
val make_std_param : Analysis.direction_t -> int -> int -> std_param_t

(** discrete analysis module parametrized with abstract domain *)
module Std(Dom :  Domain.T) : 
  (Analysis.T with type analysisparam_t = std_param_t)


(** {2 Boolean Analysis } *)

(** analysis parameters: direction *)
type bool_param_t = {b_dir:Analysis.direction_t}

(** create parameters *)
val make_bool_param : Analysis.direction_t -> bool_param_t

(** Boolean analysis module *)
module Bool : 
  (Analysis.T with type analysisparam_t = bool_param_t)


(** {2 Hybrid Analysis with Time Elapse} *)

(** analysis parameters: delayed widening start and descending iterations *)
type hyb_param_t = {h_ws:int; h_wd:int}

(** create parameters *)
val make_hyb_param : int -> int -> hyb_param_t

(** hybrid analysis module parametrized with abstract domain *)
module Hyb(Dom :  Domain.T) : 
  (Analysis.T with type analysisparam_t = hyb_param_t)
