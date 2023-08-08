(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** DF to CF implementation: utilities for zero-crossing translation *)

(** zero-crossing semantics *)
type sem_t = AtZero | Contact | Crossing

(** translates a hybrid program with zero-crossings 
    to a logico-numerical hybrid automaton*)
val translate : Env.t -> Program.dfprog_t -> sem_t -> sem_t -> Env.t * Program.cfprog_t
