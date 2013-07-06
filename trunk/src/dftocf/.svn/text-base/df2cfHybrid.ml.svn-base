(******************************************************************************)
(* df2cfHybrid *)
(* translates a hybrid DF program to a CFG *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Df2cfHybrid";
              Log.level=Log.Debug3}

exception InvalidZeroSemantics of string

type param_t = {disc_sem : ZeroTrans.sem_t; 
                cont_sem : ZeroTrans.sem_t; }

let str_to_sem str =
  match str with
    |"AtZero" -> ZeroTrans.AtZero
    |"Contact" -> ZeroTrans.Contact
    |"Crossing" -> ZeroTrans.Crossing
    |s -> raise (InvalidZeroSemantics s)

let make_param disc_sem cont_sem = {disc_sem; cont_sem}

(* translates a hybrid DF program to a CFG *)
let transform param env dfprog = 
  ZeroTrans.translate env dfprog param.disc_sem param.cont_sem
