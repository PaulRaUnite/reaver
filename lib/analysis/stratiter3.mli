(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** analysis implementation: numerical max-strategy iteration  *)

exception NotSupported of string
exception NotPurelyNumerical

(** {2 module Num: numerical max-strategy iteration} *)
module Num(Dom :  Template.TEMPLATE_T) :
  (Analysis.T with type analysisparam_t = bool)
