(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

let modes_bool1 vars env cf _  =
  let cfg = cf.Program.c_cfg in
  let iflocs = Cfg.get_locidset_by_inv env cfg 
    (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond 
      cf.Program.c_init cf.Program.c_final) in
  PartitionUtil.partition_all_by env cfg iflocs cf.Program.c_ass 
    (PSette.elements (PartitionUtil.nummodes1 vars env 
      cf.Program.c_disc_equs cf.Program.c_ass));
  cf

let modes_bool2 vars env cf _  =
  let cfg = cf.Program.c_cfg in
  let iflocs = Cfg.get_locidset_by_inv env cfg 
    (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond 
      cf.Program.c_init cf.Program.c_final) in
  PartitionUtil.partition_all_by env cfg iflocs cf.Program.c_ass 
    (PSette.elements (PartitionUtil.nummodes2 vars env 
      cf.Program.c_disc_equs cf.Program.c_ass));
  cf
