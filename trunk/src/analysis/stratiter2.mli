(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** analysis implementation: max-strategy iteration power domain *)

exception NotSupported of string
exception NotPurelyNumerical

type powlognum_param_t = 
    MicroIter   (* with micro-iterations and generalisation*)
  | NoMicroIter (* no micro-iterations, no generalisation *)
  | NoMicroGenIter (* no micro-iterations, but with generalisation *)
  | SymbRows    (* symbolic template rows *)
  | TryImpStrat (* try improved strategy on other template rows *)

(** {2 module PowLognum: logico-numerical max-strategy iteration with power domain} *)
module PowLognum(Dom :  Template.TEMPLATE_T) :
  (Analysis.T with type analysisparam_t = powlognum_param_t)
