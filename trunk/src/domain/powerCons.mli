(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** domain implementation: power domain with linear constraints *)

type apronman_t = Polka.loose Polka.t
type bddapronman_t = (Env.var_t, apronman_t) Bddapron.Domain0.bdd 
type bddaprondoman_t = (Env.var_t, apronman_t,bddapronman_t, Env.var_t Bddapron.Domain0.t) Bddapron.Domain0.man

type cons_t = Apron.Lincons1.t

val cons_true : Env.t -> cons_t
val cons_false : Env.t -> cons_t

module type POWERCONS_T =
  (Domain.T with type doman_param_t = ()
            with type t = Env.var_t Bddapron.Domain0.t
            with type numdomain_t = apronman_t Apron.Abstract0.t
            with type doman_t = bddaprondoman_t * template_t
  ) 

module BoolLin : POWERCONS_T
