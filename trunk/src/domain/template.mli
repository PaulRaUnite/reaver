(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** domain implementation: template polyhedra emulation *)

type apronman_t = Polka.loose Polka.t
type bddapronman_t = (Env.var_t, apronman_t) Bddapron.Domain0.bdd 
type bddaprondoman_t = (Env.var_t, apronman_t,bddapronman_t, Env.var_t Bddapron.Domain0.t) Bddapron.Domain0.man

type elt_t = Apron.Linexpr1.t
type template_t = elt_t list
type cons_t = Apron.Lincons1.t
type bound_t = Apron.Coeff.t

val template_empty : template_t

val template_of_strlist : Env.t -> string list -> template_t

val cons_true : Env.t -> cons_t
val cons_false : Env.t -> cons_t
val cons_instantiate : Env.t -> elt_t -> bound_t -> cons_t
val template_instantiate : Env.t -> template_t -> bound_t list -> ApronUtil.linconss_t

val neginfty : bound_t
val infty : bound_t

val print_bound : Format.formatter -> bound_t -> unit
val print_elt : Format.formatter -> elt_t -> unit
val print_cons : Format.formatter -> cons_t -> unit
val template_print : Format.formatter -> template_t -> unit

val apron_approx_template : Apron.Environment.t -> 'a Apron.Manager.t -> template_t -> 'a ApronUtil.abstract_t -> 'a ApronUtil.abstract_t

val bddapron_approx_template  : Env.t ->
           (Env.var_t, 'a, 'b, 'c) Bddapron.Domain0.man ->
           Apron.Linexpr1.t list ->
           'c Bddapron.Domain0.t -> 'c Bddapron.Domain0.t

module type TEMPLATE_T =
  (Domain.T with type doman_param_t = template_t
            with type t = Env.var_t Bddapron.Domain0.t
            with type numdomain_t = apronman_t Apron.Abstract0.t
            with type doman_t = bddaprondoman_t * template_t
  ) 

module EmuProd : TEMPLATE_T
module EmuPow : TEMPLATE_T
