(******************************************************************************)
(* df2cfDiscrete *)
(* translates a discrete DF program to a CFG *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Df2cfDiscrete";
              Log.level=Log.Debug3}

let transform env dfprog = 
  let cfprog = Program.make_cfprog 
   (Cfg.make env dfprog.Program.d_disc_equs []
    (Bddapron.Expr0.Bool.dtrue env.Env.env env.Env.cond))
   dfprog.Program.d_disc_equs 
   []
   dfprog.Program.d_init
   dfprog.Program.d_final
   dfprog.Program.d_ass
  in
  (env,cfprog)
