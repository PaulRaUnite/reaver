(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** framework base: abstract domains *)

exception NotSupported of string (** operation not supported *)

(** {2 Domain Interface } *)

module type T = 
  sig

    (** {2 Types } *)

    type t (** logico-numerical abstract value type *)
    type numdomain_t (** numerical abstract value type *)
    type doman_t (** domain manager type *)
    type doman_param_t (** domain manager parameter type *)


    (** {2 Domain manager operations } *)

    (** creates and returns a domain manager *)
    val make_doman : doman_param_t ->  doman_t

    (** creates and returns a domain manager and initializes the module *)
    val makeinit_doman : doman_param_t -> doman_t

    (** returns the domain manager of the module *)
    val doman : unit -> doman_t

    (** returns the domain manager parameter *)
    val get_doman_param : unit -> doman_param_t


    (** {2 Domain operations } *)

    (** prints an abstract value *)
    val print : ?doman:doman_t -> Env.t -> Format.formatter -> t -> unit

    (** returns the abstract value bottom *)
    val bottom : ?doman:doman_t -> Env.t-> t

    (** returns the abstract value top *)
    val top : ?doman:doman_t -> Env.t -> t

    (** canonicalizes the abstract values *)
    val canonicalize : ?doman:doman_t -> Env.t-> t -> unit

    (** returns true if the given abstract values are equal *)
    val is_eq : ?doman:doman_t -> Env.t -> t -> t -> bool

    (** returns true if the first abstract value is contained 
        in the second one *)
    val is_leq : ?doman:doman_t -> Env.t -> t -> t -> bool

    (** returns true if the given abstract value is bottom *)
    val is_bottom : ?doman:doman_t -> Env.t -> t -> bool

    (** joins two abstract values *)
    val join : ?doman:doman_t -> Env.t -> t -> t -> t

    (** meets two abstract values *)
    val meet : ?doman:doman_t -> Env.t -> t -> t -> t

    (** meets an abstract value with a (mixed) Boolean formula *)
    val meet_condition : ?doman:doman_t -> Env.t-> t -> Env.boolexpr_t -> t

    (** widening operator *)
    val widening : ?doman:doman_t -> Env.t -> t -> t -> t

    (** applies the given discrete transition function (equations) 
        in forward direction *)
    val assign_lexpr : ?doman:doman_t -> Env.t -> t -> Env.equs_t -> t

    (** applies the given discrete transition function (equations)  
        in backward direction *)
    val substitute_lexpr : ?doman:doman_t -> Env.t -> t -> Env.equs_t -> t

    (** existential quantification of the given variables *)
    val forget_list : ?doman:doman_t -> Env.t -> t -> Env.vars_t -> t

    (** applies the given flow relation (continuous equations) *)
    val flow : ?doman:doman_t -> Env.t -> t -> Env.equs_t -> Env.boolexpr_t -> t

    (** abstract acceleration of the given discrete transition function 
        (equations) in the given direction *)
    val accel : ?doman:doman_t -> ?dir:ApronAccel.direction_t -> Env.t -> t -> 
      Env.equs_t -> Env.boolexpr_t -> t


    (** {2 Conversions } *)

    (** converts a numerical abstract value into 
        a logico-numerical abstract value *)
    val of_num : ?doman:doman_t -> Env.t -> numdomain_t -> t

    (** converts a (mixed) Boolean formula into 
        a logico-numerical abstract value *)
    val of_boolexpr : ?doman:doman_t -> Env.t -> Env.boolexpr_t -> t

    (** converts a list of (Boolean formula, numerical abstract value) into 
        a logico-numerical abstract value *)
    val of_boolnumlist : ?doman:doman_t -> Env.t -> (Env.boolexpr_t * numdomain_t) list -> t

    (** converts a logico-numerical abstract value into
       a (mixed) Boolean formula *)
    val to_boolexpr : ?doman:doman_t -> Env.t -> t -> Env.boolexpr_t

    (** converts a logico-numerical abstract value into 
        a list of (Boolean formula, numerical abstract value) *)
    val to_boolnumlist : ?doman:doman_t -> Env.t -> t -> (Env.boolexpr_t * numdomain_t) list


    (** converts a logico-numerical abstract value into
       a (purely) Boolean formula *)
    val to_boolexprbool : ?doman:doman_t -> Env.t -> t -> Env.boolexpr_t

    (** converts a logico-numerical abstract value into 
        a list of (Boolean formula, numerical constraints) *)
    val to_boollinconsslist : ?doman:doman_t -> Env.t -> t -> 
      (Env.boolexpr_t * ApronUtil.linconss_t) list

    (** meets the given abstract value with a (mixed) Boolean formula
        and returns the result as a (mixed) Boolean formula *)
    val meet_to_boolexpr : ?doman:doman_t -> Env.t -> t -> Env.boolexpr_t ->
      Env.boolexpr_t

    (** meets the given abstract value with a (mixed) Boolean formula
        and returns the result as a (purely) Boolean formula *)
    val meetbool_to_boolexpr : ?doman:doman_t -> Env.t -> t -> 
      Env.boolexpr_t -> Env.boolexpr_t
  end

(** {2 Modules for building BddApron-based domains } *)

(** BddApron manager *)
module type BDDAPRON_MAN_T = 
  sig
    type apronman_t
    type 'a bddapronman_t
    type ('a, 'b) man_t = ('a, apronman_t, 'a bddapronman_t, 'b) Bddapron.Domain0.man

    val make_man : unit -> ('a, 'b) man_t
  end

(** domain interface without domain manager parameters *)
module type NOPARAM_T = (T with type doman_param_t = unit)

(** functor type for BddApron-domains *)
module type BDDAPRON_FUNC_T = functor (Man : BDDAPRON_MAN_T) ->
  (NOPARAM_T with type t = Env.var_t Bddapron.Domain0.t
     with type numdomain_t = Man.apronman_t Apron.Abstract0.t
     with type doman_t = (Env.var_t, Env.var_t Bddapron.Domain0.t) Man.man_t
  ) 

(** functor for BddApron-based logico-numerical power domain *)
module Pow : BDDAPRON_FUNC_T

(** functor for BddApron-based logico-numerical product domain *)
module Prod : BDDAPRON_FUNC_T
