open Numutil

type lp
type sol
type basis
val get_value_from_sol : sol -> InfGmpRatio.num 
val get_x_from_sol : sol -> int -> GmpRatio.num
val free_sol : sol -> unit
type comp = Comp_le | Comp_eq

val init : int -> unit
val create_lp : string -> int -> int -> int -> lp 
val free_lp : lp -> unit
val solve : lp -> sol 
val set_cmatcnt : lp -> int -> int -> unit 
val set_cmatbeg : lp -> int -> int -> unit 
val set_cmatind : lp -> int -> int -> unit 
val set_sense : lp -> int -> comp -> unit 
val set_obj : lp -> int -> GmpRatio.num -> unit 
val set_rhs : lp -> int -> GmpRatio.num -> unit 
val set_lower : lp -> int -> GmpRatio.num -> unit 
val set_upper : lp -> int -> GmpRatio.num -> unit 
val set_cmatval : lp -> int -> GmpRatio.num -> unit 
val set_colname : lp -> int -> string -> unit
