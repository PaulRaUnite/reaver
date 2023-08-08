(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** DF to CF implementation: hybrid programs with zero-crossings *)

exception InvalidZeroSemantics of string (** invalid zero-crossing semantics *)

(** translation parameters: semantics of discrete and continuous zero-crossings *)
type param_t = {disc_sem : ZeroTrans.sem_t; 
                cont_sem : ZeroTrans.sem_t; }

(** parses the zero-crossing semantics from a string *)
val str_to_sem : string ->  ZeroTrans.sem_t

(** creates the translation parameters *)
val make_param : ZeroTrans.sem_t ->  ZeroTrans.sem_t -> param_t

(** translates a hybrid DF program to a CFG *)
val transform : param_t -> Program.df2cf_t
