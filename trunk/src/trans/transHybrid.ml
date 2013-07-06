(******************************************************************************)
(* transHybrid *)
(* transformations of logico-numerical hybrid automata *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="TransHybrid";
              Log.level=Log.Debug3}


(******************************************************************************)
(** partition by Boolean-defined continuous modes (ignore Boolean inputs) *)
let modes_bool1 vars env cf _  =
  let cfg = cf.Program.c_cfg in
  let iflocs = Cfg.get_locidset_by_inv env cfg 
    (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond 
      cf.Program.c_init cf.Program.c_final) in
  PartitionUtil.partition_all_by env cfg iflocs cf.Program.c_ass 
    (PSette.elements (PartitionUtil.nummodes1 vars env 
      cf.Program.c_cont_equs cf.Program.c_ass));
  cf

(******************************************************************************)
(** partition by Boolean-defined continuous modes 
    (ignore Boolean inputs and numerical constraints) *)
let modes_bool2 vars env cf _  =
  let cfg = cf.Program.c_cfg in
  let iflocs = Cfg.get_locidset_by_inv env cfg 
    (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond 
      cf.Program.c_init cf.Program.c_final) in
  PartitionUtil.partition_all_by env cfg iflocs cf.Program.c_ass 
    (PSette.elements (PartitionUtil.nummodes2 vars env 
      cf.Program.c_cont_equs cf.Program.c_ass));
  cf

(******************************************************************************)
(** partition by numerically convex staying conditions *)
let convex_staycond env cf _ =
  let cfg = cf.Program.c_cfg in
  let convex_staycond_loc loc = 
    let inv = PSHGraph.attrvertex cfg loc in
    let invs = BddapronUtil.boolexpr_to_numconvex_list env.Env.env env.Env.cond 
      (Bddapron.Expr0.Bool.tdrestrict inv env.Env.cond.Bdd.Cond.careset) in
    Log.debug2_o logger (Util.list_print (Env.print_boolexpr env)) 
      "invs=" invs;
    PartitionUtil.partition_disjoint_loc env cfg cf.Program.c_ass loc invs
  in
  PSette.iter (convex_staycond_loc) 
    (Cfg.get_locidset_by_inv env cfg 
      (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond 
        (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond 
          cf.Program.c_init cf.Program.c_final)));
  cf

(******************************************************************************)
(** partition by numerically defined continuous modes *)
let modes_num analyze env cf _ =
  let apronman = Polka.manager_alloc_loose () in
  let (_,_,_,res_to_bddapron) = analyze env cf in
  let anres = res_to_bddapron () in
  let cfg = cf.Program.c_cfg in
  let iflocs = Cfg.get_locidset_by_inv env cfg 
        (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond 
          cf.Program.c_init cf.Program.c_final) in
  let modes_num_loc loc s = 
    if not (PSette.mem loc iflocs) then
      match Cfg.get_flow_arc env cfg loc with
      |Some (_,g,f) ->
      begin
        (* remove all variables not occurring in ODE *)
        let suppvars = List.fold_left (fun supp (_,expr) ->
            PSette.union supp 
              (Bddapron.Expr0.support env.Env.env env.Env.cond expr))
          (Bddapron.Expr0.support env.Env.env env.Env.cond 
            (Bddapron.Expr0.Bool.to_expr g)) f
        in
        let suppvars = BddapronUtil.vars_to_apronvars env.Env.env 
          (PSette.elements 
           (PSette.diff (Util.list2psette (compare) env.Env.ns_vars) suppvars))
        in
        let exprs = List.map (fun (b,n) ->
          Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond b
            (BddapronUtil.apron_to_boolexpr env.Env.env env.Env.cond apronman
              (Apron.Abstract1.forget_array apronman 
                (Apron.Abstract1.of_lincons_array apronman env.Env.apronenv n)
                  suppvars false)))
          s
        in
        Env.compute_careset env;
        let otherlocs = PSette.diff (Cfg.get_locidset cfg) 
          (PSette.singleton (compare) loc) in
        PartitionUtil.partition_all_by env cfg otherlocs cf.Program.c_ass exprs
      end
      |None -> ()
  in
  Mappe.iter (modes_num_loc) anres; 
  cf
