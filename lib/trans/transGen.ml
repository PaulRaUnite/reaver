(******************************************************************************)
(* transGen *)
(* general partitioning techniques and CFG transformations *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="TransGen";
              Log.level=Log.Debug3}

(******************************************************************************)
(** partition into initial, final and other states *)
let initfinal env cf _ =
  let cfg = cf.Program.c_cfg in
  let initial = cf.Program.c_init in
  let final = cf.Program.c_final in
  let assertion = cf.Program.c_ass in
  let inv = Cfg.get_only_loc cfg in
  let binumsupp = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
    env.Env.cond.Bdd.Cond.supp 
    (BddapronUtil.supp_of_vars env.Env.env env.Env.bi_vars) 
  in
  let split_constraints e =
    let e = Cudd.Bdd.existand binumsupp e assertion in
    BddapronUtil.boolexpr_to_numconvex_list env.Env.env env.Env.cond e in
  (* split into initial and other locations *)
  let initial = split_constraints initial in (*(Cudd.Bdd.dand initial inv) in*)
  let final = split_constraints final in (*(Cudd.Bdd.dand final inv) in*)
  Log.debug2_o logger (Util.list_print 
    (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)) 
    "initial=" initial;
  Log.debug2_o logger (Util.list_print 
    (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)) 
    "final=" final;
  let exprs = PartitionUtil.compute_partition_expr_list env inv
    (List.append initial final) in
  Log.debug2_o logger (Util.list_print 
    (BddapronUtil.print_boolexpr env.Env.env env.Env.cond))
    "partition-exprs: " exprs;
  let cfg = Cfg.make_partitioned env cfg exprs assertion in
(*  let finalexpr = List.fold_right 
    (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond )
      final (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond) in
  let initialexpr = List.fold_right 
    (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond)
      initial (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond) in*)
  let initlocs = Cfg.get_locidset_by_inv env cfg cf.Program.c_init in
  let notinitlocs = Cfg.get_locidset_by_inv env cfg 
                      (Cudd.Bdd.dnot cf.Program.c_init) in
  let finallocs = Cfg.get_locidset_by_inv env cfg cf.Program.c_final in
  let notfinallocs = Cfg.get_locidset_by_inv env cfg 
                       (Cudd.Bdd.dnot cf.Program.c_final) in
  (* remove successor arcs of final states *)
  PSette.iter 
    (fun v -> 
      let succs = PSHGraph.succhedge cfg v in
      PSette.iter (PSHGraph.remove_hedge cfg) succs)
    (PSette.diff finallocs notfinallocs);
  (* remove predecessor arcs in init states *)
  PSette.iter 
    (fun v -> 
      let preds = PSHGraph.predhedge cfg v in
      PSette.iter (PSHGraph.remove_hedge cfg) preds)
    (PSette.diff initlocs notinitlocs);
  cf

(******************************************************************************)
(** partitions by the given Boolean expressions *)
let manual exprlist env cf _ =
  let cfg = cf.Program.c_cfg in
  let iflocs = if (PSHGraph.size_vertex cfg)<=1 then PSette.empty (compare)
    else Cfg.get_locidset_by_inv env cfg 
      (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond 
        cf.Program.c_init cf.Program.c_final) 
  in
  Log.debug2_o logger (Util.list_print 
    (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)) 
    "exprlist=" exprlist;
  PartitionUtil.partition_all_by env cfg iflocs 
    cf.Program.c_ass exprlist;
  Cfg.remove_unreachable_locs env cfg cf.Program.c_init; 
  cf

(******************************************************************************)
(** enumerate the states of the given (Boolean) variables *)
let enumerate vars env cf =
  let exprlist = List.flatten
    (List.map 
      (fun v -> 
        match (Bddapron.Env.typ_of_var env.Env.env v) with
	  |`Bool -> [Bddapron.Expr0.Bool.var env.Env.env env.Env.cond v]
	  |`Benum(typ) -> 
             List.map
               (fun label -> 
                 (Bddapron.Expr0.Benum.eq_label env.Env.env env.Env.cond
                   (Bddapron.Expr0.Benum.var env.Env.env env.Env.cond v) label)
               )
               (Array.to_list (Bdd.Enum.labels_of_typ env.Env.env typ))
	  |`Bint(sign,len) -> 
             let (smin,max) = if sign then (int_of_float (1.-.2.**
                                              ((float_of_int len)-.1.)),
                                            int_of_float (2.**
                                              ((float_of_int len)-.1.)-.1.))
                              else (1,int_of_float (2.**
                                      (float_of_int len)-.1.)) in
             let build i max list =
               (Bddapron.Expr0.Bint.eq_int env.Env.env env.Env.cond
                 (Bddapron.Expr0.Bint.var env.Env.env env.Env.cond v) i)::list
             in
             build smin max []
	  |_ -> [])
     vars)
  in
  manual exprlist env cf

(******************************************************************************)
(** transforms the transition functions in a location into transitions per arc
  -- refines the assertions by the destination location *)
let refine_by_destloc env cf _ =
  let cfg = cf.Program.c_cfg in
  let get_primed_var = BddapronUtil.get_primed_var env.Env.env in
  let get_unprimed_var = BddapronUtil.get_unprimed_var env.Env.env in
  let tabarr = MtbddUtil.make_table_action_array env.Env.env in
  let refine_by_destloc_loc sloc sinv succs =
(*    let succs2 = PSette.filter 
          (fun succ -> 
            match (PSHGraph.attrhedge cfg succ) with 
            |Arc.Flow(_)-> false |_ -> true)
          succs
    in*)
    if not(PSette.is_empty succs) then
    begin
     PSette.iter
      (fun arcid -> 
        let arc = PSHGraph.attrhedge cfg arcid in
        match arc with
	  |Arc.Flow(ass,f) -> 
             let f = BddapronUtil.simplify_equs env.Env.env env.Env.cond 
               f sinv in
             (* factorize condiion for non-zero evolution in arc assertion *)
             let numarr = BddapronUtil.product_numequs env.Env.env tabarr f in
             Log.debug_o logger (MtbddUtil.print_array_mtbdd env.Env.env 
               env.Env.cond (Bddapron.Apronexpr.print Env.symbol)) 
               "numarr=" numarr;
             let (ass2,f2) = Array.fold_left
               (fun (ass2,f2) (g,arr) ->
                 if Util.array_exists 
                     (fun a -> 
                       not (Bddapron.Apronexpr.equal Env.symbol a 
                              Bddapron.Apronexpr.zero)) 
                     arr then (ass2,f2)
                 else
                   let notg = Cudd.Bdd.dnot g in
                   (Cudd.Bdd.dand ass notg,
                    BddapronUtil.simplify_equs env.Env.env 
                     env.Env.cond f notg))
               (ass,f) (Cudd.Mtbdd.guardleafs numarr)
             in
             PSHGraph.replace_attrhedge cfg arcid 
               (Arc.Flow(ass2,f2))
	  |_ ->
          begin
            let (ass,f) = Arc.get_ass_equs env (PSHGraph.attrhedge cfg arcid) in
            let f = BddapronUtil.simplify_equs env.Env.env env.Env.cond 
               f sinv in
            let fb = Env.get_bool_equs env f in
            let dlocs = PSHGraph.succvertex cfg arcid in
            assert((Array.length dlocs)==1);
            let dloc = dlocs.(0) in
            let dinv = PSHGraph.attrvertex cfg dloc in
            let sinvb = Cudd.Bdd.exist env.Env.cond.Bdd.Cond.supp sinv in
            let dinvb = Cudd.Bdd.exist env.Env.cond.Bdd.Cond.supp dinv in
            let preass = BddapronAnalysis.bool_pre_assertion2 
              get_primed_var
              env.Env.env env.Env.cond sinvb dinvb fb 
              cf.Program.c_ass env.Env.i_vars in
            let ass2 = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond 
              ass preass in
            Log.debug3_o logger (BddapronUtil.print_boolexpr 
                  env.Env.env env.Env.cond) "preass: " ass2;
            if Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond ass2 then
              PSHGraph.remove_hedge cfg arcid
            else
            begin
              if dloc=sloc then
              begin
                Log.debug3_o logger (BddapronUtil.print_boolexpr 
                  env.Env.env env.Env.cond) "ass2: " ass2;
                Log.debug3_o logger (BddapronUtil.print_equations 
                  env.Env.env env.Env.cond) "eqs: " f;
                let f2 = 
                  if not(Bddapron.Expr0.Bool.is_cst env.Env.env env.Env.cond 
                    ass2) then
                    BddapronUtil.simplify_equs env.Env.env env.Env.cond f ass2 
                  else f
                in
                (* remove identity self-loops  *)
                if BddapronAnalysis.is_id_equs2 
                    get_primed_var get_unprimed_var env.Env.env env.Env.cond 
                    ass2 f2 sinv then
                  PSHGraph.remove_hedge cfg arcid
                else
                  PSHGraph.replace_attrhedge cfg arcid 
                    (Arc.replace_ass_equs arc (ass2,f))
              end
              else
                PSHGraph.replace_attrhedge cfg arcid 
                  (Arc.replace_ass_equs arc (ass2,f))
            end
          end)
        succs
    end
  in 
  PSHGraph.iter_vertex cfg
    (fun loc inv ~pred ~succ ->
      if not (PSette.is_empty succ) then
        refine_by_destloc_loc loc inv succ);
  Cfg.remove_unreachable_locs env cfg cf.Program.c_init;
  cf

(******************************************************************************)
(** boolean backward bisimulation refinement *) 
let boolbacksim env cf _ =
  let cfg = cf.Program.c_cfg in
  let bool_post env f inv assertion =
    let fexprlist = BddapronUtil.get_fexprlist 
      (BddapronUtil.get_primed_var env.Env.env)
      env.Env.env env.Env.cond  (Env.get_bool_equs env f) in
    let forget_supp = Cudd.Bdd.dand env.Env.cond.Bdd.Cond.supp 
      (BddapronUtil.supp_of_vars env.Env.env
        (List.append env.Env.bs_vars env.Env.bi_vars)) in
    let phi = BddapronAnalysis.bool_image2 
      (BddapronUtil.get_unprimed_var env.Env.env) 
      env.Env.env env.Env.cond fexprlist assertion forget_supp inv
    in
    Log.debug2_o logger (Env.print_boolexpr env) 
      "phi=" phi;
    PSette.add phi (PSette.empty (compare))
  in
  PartitionUtil.partition_recsucc env cfg cf.Program.c_ass
    bool_post
    (Cfg.get_locidset_by_inv env cfg 
       (Cudd.Bdd.dor cf.Program.c_init cf.Program.c_final));
  cf

(******************************************************************************)
(** removes boolean inputs and splits arcs *)
(* ACTUALLY moves the boolean inputs into the arc assertion *)
let remove_bool_inputs env cf _  =
  let cfg = cf.Program.c_cfg in
  let remove_bool_inputs_arc (arcid,arc,sloc,dloc) =
    let (ass2,f1) = Arc.get_ass_equs env arc in
    let f2 =   
      if not (Bddapron.Expr0.Bool.is_cst env.Env.env env.Env.cond ass2) then
        BddapronUtil.simplify_equs env.Env.env env.Env.cond f1 ass2
      else f1
    in
    let fn1 = Env.get_num_equs env f2 in
    let tabarr = MtbddUtil.make_table_action_array env.Env.env in
    let fn2 = BddapronUtil.product_numequs env.Env.env tabarr fn1 in
    let fng = Cudd.Mtbdd.guardleafs fn2 in
    let aflist = Array.fold_left 
      (fun af1 (g,a) -> 
        let assg2 = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond g ass2 in
        (Arc.replace_ass_equs arc (assg2,f2))::af1)
      [] fng
    in
    List.iter (fun arc -> let _ = Cfg.add_arc env cfg sloc dloc arc in ()) 
      aflist;
    PSHGraph.remove_hedge cfg arcid  
  in
  PSette.iter (remove_bool_inputs_arc) (Cfg.get_arcs cfg (fun _ -> true));
  cf

(******************************************************************************)
(** splits non-convex guards and transitions *)
let split_arcs env cf _ = 
  let cfg = cf.Program.c_cfg in
  let split_arc (arcid,arc,sloc,dloc) =
    let (ass2,f1) = Arc.get_ass_equs env arc in
    let f2 =   
      if not (Bddapron.Expr0.Bool.is_cst env.Env.env env.Env.cond ass2) then
        BddapronUtil.simplify_equs env.Env.env env.Env.cond f1 ass2
      else f1
    in
    let fn1 = Env.get_num_equs env f2 in
    let tabarr = MtbddUtil.make_table_action_array env.Env.env in
    let fn2 = BddapronUtil.product_numequs env.Env.env tabarr fn1 in
    let fng = Cudd.Mtbdd.guardleafs fn2 in
    let aflist = Array.fold_left 
      (fun af1 (g,_) -> 
        let gass = BddapronUtil.simplify_boolexpr env.Env.env env.Env.cond 
          (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond g ass2) 
          env.Env.cond.Bdd.Cond.careset in
        Log.debug3_o logger (Env.print_boolexpr env) 
          "possibly non-convex g: " gass;
        let afn = (List.fold_left
          (fun af2 g ->
            Log.debug3_o logger (Env.print_boolexpr env) "convex g: " g;
            (Arc.replace_ass_equs arc (g,f2))::af2)
          [] (BddapronUtil.boolexpr_to_dnf env.Env.env gass))
        in 
        List.append afn af1)
      []
      fng
    in
    List.iter (fun arc -> let _ = Cfg.add_arc env cfg sloc dloc arc in ()) 
      aflist;
    PSHGraph.remove_hedge cfg arcid  
  in
  PSette.iter (split_arc) (Cfg.get_arcs cfg (fun _ -> true));
  cf

