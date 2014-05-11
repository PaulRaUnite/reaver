open List
open Numutil

type lp
type sol
type basis
external _get_value_from_sol : sol -> string = "camlidl_qsopt_ex_get_value_from_sol"
let get_value_from_sol sol = 
  let str_value = _get_value_from_sol sol in 
  InfGmpRatio.from_string str_value
external _get_x_from_sol : sol -> int -> string = "camlidl_qsopt_ex_get_x_from_sol"
let get_x_from_sol sol i = 
  let str_value = _get_x_from_sol sol i in 
  GmpRatio.from_string str_value
external free_sol : sol -> unit = "camlidl_qsopt_ex_free_sol"
type comp = Comp_le | Comp_eq
external init : int -> unit = "camlidl_qsopt_ex_qsopt_ex_init"
external create_lp : string -> int -> int -> int -> lp = "camlidl_qsopt_ex_create_lp"
external set_cmatcnt : lp -> int -> int -> unit = "camlidl_qsopt_ex_set_cmatcnt"
external set_cmatbeg : lp -> int -> int -> unit = "camlidl_qsopt_ex_set_cmatbeg"
external set_cmatind : lp -> int -> int -> unit = "camlidl_qsopt_ex_set_cmatind"
external _set_sense : lp -> int -> int -> unit = "camlidl_qsopt_ex_set_sense"
let set_sense lp i comp = 
  match comp with
    Comp_le -> _set_sense lp i 0
  | Comp_eq -> _set_sense lp i 1
external _set_obj : lp -> int -> string -> unit = "camlidl_qsopt_ex_set_obj"
let set_obj lp i q = _set_obj lp i (GmpRatio.to_string q)
external _set_rhs : lp -> int -> string -> unit = "camlidl_qsopt_ex_set_rhs"
let set_rhs lp i q = _set_rhs lp i (GmpRatio.to_string q)
external _set_lower : lp -> int -> string -> unit = "camlidl_qsopt_ex_set_lower"
let set_lower lp i q = _set_lower lp i (GmpRatio.to_string q)
external _set_upper : lp -> int -> string -> unit = "camlidl_qsopt_ex_set_upper"
let set_upper lp i q = _set_upper lp i (GmpRatio.to_string q)
external _set_cmatval : lp -> int -> string -> unit = "camlidl_qsopt_ex_set_cmatval"
let set_cmatval lp i q = _set_cmatval lp i (GmpRatio.to_string q)
external solve : lp -> sol = "camlidl_qsopt_ex_solve"
external free_lp : lp -> unit = "camlidl_qsopt_ex_free_lp"
external set_colname : lp -> int -> string -> unit = "camlidl_qsopt_ex_set_colname"
