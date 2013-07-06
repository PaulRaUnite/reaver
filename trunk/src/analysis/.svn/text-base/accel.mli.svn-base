(** analysis implementation: abstract acceleration *)

(** {2 Discrete Analysis with Abstract Acceleration } *)

(** analysis parameters: direction, delayed widening start,
      delayed widening start in accelerable strongly connected components and
      descending iterations *)
type acc_param_t = {a_dir:Analysis.direction_t; a_ws:int; a_aws:int; a_wd:int}

(** create parameters *)
val make_acc_param : Analysis.direction_t -> int -> int -> int -> acc_param_t

(** analysis module for (logico-numerical) abstract acceleration *)
module Acc(Dom :  Domain.T) : 
  (Analysis.T with type analysisparam_t = acc_param_t)
