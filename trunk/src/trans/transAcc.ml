(******************************************************************************)
(* TransAcc *)
(* transformations of the control flow graph for abstract acceleration *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Trans.Acc";
              Log.level=Log.Debug3}

type decoupling_mode_t = DecoupleNone | DecoupleBool | DecoupleBoolNacc

(******************************************************************************)
(* utilities *)
(******************************************************************************)
(* partition f into accelerable and not-accelerable/boolean equations *)
let split_acc_nonacc env f =
 List.partition 
  (fun (v,expr) ->
    match expr with
      |`Apron(e) -> 
	Array.fold_left
	  (fun res (_,a) -> 
	     res && not (BddapronAccel.is_nonacc_action 
               env.Env.env env.Env.cond env.Env.ni_vars v a))
	  true
	  (Cudd.Mtbdd.guardleafs e)
      |_ -> false)
   f

(******************************************************************************)
(* partition f into numerical and boolean equations *)
let split_num_bool env f =
 List.partition 
   (fun (v,expr) -> match expr with |`Apron(e) -> true  |_ -> false) f

(******************************************************************************)
(* returns the (only) Id successor arc of loc *)
let get_id_succ cfg loc =
  let succs = PSHGraph.succhedge cfg loc in
  let idsuccs = PSette.filter 
    (fun arcid -> match PSHGraph.attrhedge cfg arcid with
      |Arc.Id -> true |_ -> false)
    succs in
  assert((PSette.cardinal idsuccs)==1);
  let idsucc = PSette.choose idsuccs in
  let pred = PSHGraph.predvertex cfg idsucc in
  let succ = PSHGraph.succvertex cfg idsucc in
  assert((Array.length pred)==1);
  assert((Array.length succ)==1);
  (idsucc,pred.(0),succ.(0)) 

(******************************************************************************)
(* returns the accelerable predecessor arcs of loc *)
let get_accel_preds env cfg loc =
  let preds = PSHGraph.predhedge cfg loc in
  let accelpreds = PSette.filter 
    (fun arcid -> match PSHGraph.attrhedge cfg arcid with
      |Arc.Accel(_,_) -> true |_ -> false)
    preds in
  PSette.fold
    (fun arcid set ->
      let pred = PSHGraph.predvertex cfg arcid in
      let succ = PSHGraph.succvertex cfg arcid in
      assert((Array.length pred)==1);
      assert((Array.length succ)==1);
      let (a,f) = Arc.get_ass_equs env (PSHGraph.attrhedge cfg arcid) in
      PSette.add (arcid,a,f,pred.(0),succ.(0)) set
    )
    accelpreds (PSette.empty (fun (a1,_,_,_,_) (a2,_,_,_,_) -> 
      Cfg.cfg_compare.SHGraph.compareh a1 a2))

let get_accel_succs_cnt env cfg loc =
  let succs = PSHGraph.succhedge cfg loc in
  let accelsuccs = PSette.filter 
    (fun arcid -> match PSHGraph.attrhedge cfg arcid with
      |Arc.Accel(_,_) -> true |_ -> false)
    succs in
  PSette.cardinal accelsuccs

let get_nonaccs_cnt env cfg loc =
  let succs = PSHGraph.succhedge cfg loc in
  let nonaccs = PSette.filter 
    (fun arcid -> match PSHGraph.attrhedge cfg arcid with
      |Arc.Nonacc(_,_) -> true |_ -> false)
    succs in
  PSette.cardinal nonaccs


(******************************************************************************)
(* - substitutes boolean state variables in non-identity equations 
   by boolean inputs and
   - substitutes boolean state variables in the guards by boolean inputs 
   - substitutes right-hand side of numerical equations by a numerical input 
       if inputize_num *)
let inputize_tr_fct env (ass,f) inputize_num =
  let substb = List.map
    (fun v -> (v,(Env.get_newinput env v))) env.Env.bs_vars
  in 
  (*Log.debug3_o logger (fun fmt l -> Util.list_print
   (fun fmt (v1,v2) -> Format.pp_print_string fmt ("("^v1^","^v2^")")) fmt l) 
    "substb = " substb;*)
  let bsupp = BddapronUtil.supp_of_vars env.Env.env env.Env.bs_vars in
  let fnew = List.map 
   (fun (v,expr) ->
     Log.debug2_o logger (BddapronUtil.print_equation env.Env.env env.Env.cond)
       "old_eq = " (v,expr);
     let neweq = match expr with
      |`Apron(_) -> 
        if inputize_num then
        begin
          (v,`Apron(Bddapron.Expr0.Apron.var env.Env.env env.Env.cond 
              (Env.get_newinput env v)))
        end
        else
        (v,Bddapron.Expr0.substitute_by_var 
             env.Env.env env.Env.cond expr substb)
    |_ -> 
      begin
      (* if not identity *)
      if not (BddapronUtil.is_id_or_const_equ 
          env.Env.env env.Env.cond (v,expr)) then
        let rexpr = BddapronUtil.expr_forget_supp bsupp expr in
        match rexpr with
	  |`Bool(re) -> 
	   if Bddapron.Expr0.Bool.is_true env.Env.env env.Env.cond re then
	     (* replace true by top = a new unconstrained boolean input *)
           begin
	     (v,Bddapron.Expr0.Bool.to_expr 
	 	(Bddapron.Expr0.Bool.var env.Env.env env.Env.cond 
		  (Env.get_newinput env v)))
           end
	   else (v,rexpr)
	  |_ -> (v,rexpr)
       else (v,expr)
       end 
     in
     Log.debug2_o logger (BddapronUtil.print_equation env.Env.env env.Env.cond) "new_eq = " neweq;
     neweq)
     f
   in
   (BddapronUtil.boolexpr_forget_supp bsupp ass,fnew)

let inputize_guard env g =
  let bvars = Util.list_inter env.Env.b_vars env.Env.s_vars in
  Bddapron.Expr0.Bool.exist env.Env.env env.Env.cond bvars g

(******************************************************************************)
(* loop transformations *)
(******************************************************************************)
(* splits Accel arcs by convex decomposition of their guards *)
(* Remark: guards should be true here, only split assertion *)
let split_arcs env cf _ = 
  let cfg = cf.Program.c_cfg in
  let make_arc arctype a f =
    match arctype with
      |DecoupleNone -> Arc.Accel(a,f)
      |DecoupleBool -> Arc.BoolAccel(a,f)
      |DecoupleBoolNacc -> Arc.BoolNaccAccel(a,f)
  in
  let split_arc arctype (arcid,ass,f,sloc,dloc) =
    let fn = Env.get_num_equs env f in
    let tabarr = MtbddUtil.make_table_action_array env.Env.env in
    let numarr = BddapronUtil.product_numequs env.Env.env tabarr fn in
    let fng = Cudd.Mtbdd.guardleafs numarr in
    let aflist = Array.fold_left 
      (fun af1 (g,_) -> 
        let gass = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond g ass in
        Log.debug3_o logger (Env.print_boolexpr env) 
          "possibly non-convex g: " gass;
        let afn = (List.fold_left
          (fun af2 g ->
            Log.debug3_o logger (Env.print_boolexpr env) "convex g: " g;
            (make_arc arctype g f)::af2)
          [] (BddapronUtil.boolexpr_to_dnf env.Env.env gass))
        in 
        List.append afn af1)
      []
      fng
    in
    List.iter (fun arc -> let _ = Cfg.add_arc env cfg sloc dloc arc in ()) aflist;
    PSHGraph.remove_hedge cfg arcid  
  in
  PSette.iter (split_arc DecoupleNone) (Cfg.get_accel_arcs cfg);
  PSette.iter (split_arc DecoupleBool) (Cfg.get_boolaccel_arcs cfg);
  PSette.iter (split_arc DecoupleBoolNacc) (Cfg.get_boolnaccaccel_arcs cfg);
  cf

(******************************************************************************)
(* removes boolean inputs, 
   splits and 
   classifies the arcs in accelerable and nonaccelerable according to the 
   given decoupling mode*)
(* ACTUALLY moves the boolean inputs into the arc assertion *)
let remove_bool_inputs decoupling_mode env cf _  =
  let cfg = cf.Program.c_cfg in
  let remove_bool_inputs_arc (arcid,ass2,f1,sloc,dloc) =
    if sloc=dloc then
    begin
    (*let binsupp = BddapronUtil.get_boolinput_supp env in  *)
    let inv = PSHGraph.attrvertex cfg sloc in
    let f2 = BddapronUtil.simplify_equs env.Env.env env.Env.cond f1 ass2 in
    let fn1 = Env.get_num_equs env f2 in
    let (vars,_) = Env.split_equs fn1 in
    let tabarr = MtbddUtil.make_table_action_array env.Env.env in
    let fn2 = BddapronUtil.product_numequs env.Env.env tabarr fn1 in
    let fng = Cudd.Mtbdd.guardleafs fn2 in
    let aflist = Array.fold_left 
      (fun af1 (g,a) -> 
        let assg2 = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond g ass2 in
        (* add arc if assertion<>false and it is not an identity loop *)
        let f3 = BddapronUtil.simplify_equs env.Env.env env.Env.cond f2 assg2 in
        let is_boolid = BddapronAnalysis.is_id_equs2 
           (BddapronUtil.get_primed_var env.Env.env) 
           (BddapronUtil.get_unprimed_var env.Env.env) 
           env.Env.env env.Env.cond 
           assg2 (Env.get_bool_equs env f3) inv in
        let is_numid = BddapronUtil.is_id_equs env.Env.env env.Env.cond (Env.get_num_equs env f3) in
        if not (Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond assg2) &&
           not (is_boolid && is_numid) then
        begin
          let is_accel = 
            BddapronAccel.is_acc_actions env.Env.env env.Env.cond 
               env.Env.ni_vars
               (Array.of_list vars) a in
          (* categorize arcs *)
          let newarc = 
            if is_accel then
              if is_boolid then Arc.Accel(assg2,(Env.get_num_equs env f2))
              else 
                match decoupling_mode with
		  |DecoupleNone -> Arc.Nonacc(assg2,f2)
		  |_ -> Arc.BoolAccel(assg2,f2)
            else
              if is_boolid then Arc.Nonacc(assg2,f2)
              else 
                if is_numid then Arc.Nonacc(assg2,f2)
                else 
                  if (match decoupling_mode with 
                       |DecoupleBoolNacc -> true |_ -> false) && 
                     (BddapronAccel.exists_acc_action env.Env.env env.Env.cond 
                       env.Env.ni_vars (Array.of_list vars) a) then 
                    Arc.BoolNaccAccel(assg2,f2) 
                  else Arc.Nonacc(assg2,f2)
          in
          newarc::af1
        end
        else af1)
      [] fng
    in
    Log.debug_o logger (Util.list_print (Arc.print env)) "arc-list: " aflist;
    List.iter (fun arc -> let _ = Cfg.add_arc env cfg sloc dloc arc in ()) aflist;
    PSHGraph.remove_hedge cfg arcid 
    end 
  in
  PSette.iter (remove_bool_inputs_arc) (Cfg.get_normal_arcs cfg);
  cf

(******************************************************************************)
(* expand graph ("flatten") accelerable loops *)
let flatten_loops env cf _ = 
  let cfg = cf.Program.c_cfg in
  let flatten_loops_loc loc inv pred succ loops =
    (* (accelerable loops, non-accelerable loops) according to decoupling mode*)
    let (laccs,lnaccs) = 
      let partfct arcid = match PSHGraph.attrhedge cfg arcid with 
        |Arc.Accel(_,_)|Arc.BoolAccel(_,_)
        |Arc.BoolNaccAccel(_,_) -> true |_ -> false
      in
      List.partition (partfct) (PSette.elements loops)
    in
    (* move successor arcs from loc to newloc --------------------------------*)
    let move_succs newloc =
      List.iter
        (fun h -> 
          let arc = PSHGraph.attrhedge cfg h in
          let succs = PSHGraph.succvertex cfg h in
          assert((Array.length succs)==1);
          let dloc = succs.(0) in       
          PSHGraph.remove_hedge cfg h;
          PSHGraph.add_hedge cfg h arc [|newloc|] [|dloc|])
        (PSette.elements succ)
    in
    (* remove loops in loc ---------------------------------------------------*)
    let remove_loops () = 
      PSette.iter (fun l -> PSHGraph.remove_hedge cfg l) loops in
    (* flatten single accelerable loop ---------------------------------------*)
    let expand_10 () = 
      let arc =  PSHGraph.attrhedge cfg (List.hd laccs) in
      remove_loops ();
      (* add vertex *)
      let newloc = Cfg.add_loc env cfg inv in
      (* reassign successor arcs to new vertex *)
      move_succs newloc;
      (* add accelerating transition *)
      let _ = Cfg.add_arc env cfg loc newloc arc in ()
    in
    (* expand m accelerable and n non-accelerable loops ----------------------*)
    let expand_mn () = 
      let laccarcs = List.map (fun a -> PSHGraph.attrhedge cfg a) laccs in
      let lnaccarcs = List.map (fun a -> PSHGraph.attrhedge cfg a) lnaccs in
      (* remove loops *)
      remove_loops ();
      (* add outgoing location *)
      let newloc = Cfg.add_loc env cfg inv in
      (* move successor arcs to outgoing location *)
      move_succs newloc;
      (* add m-1 interior locations *)
      let newlocs = Cfg.add_locs env cfg inv (List.length laccs) in
      (* add accelerating transitions and 
         add normal transitions from incoming location and
         add identity arcs to outgoing location *)
      PSette.iter 
        (fun v -> 
           List.iter2
             (fun dv arc -> 
               let _ = 
                 if v<>dv then Cfg.add_arc env cfg v dv arc
                 else 
                   let (a,f) = Arc.get_ass_equs env arc in
                   Cfg.add_arc env cfg loc v (Arc.Accel(a,f))
               in ())
             (PSette.elements newlocs) laccarcs;
          let _ = Cfg.add_arc env cfg v newloc Arc.Id in ())
        newlocs;
      (* add identity arc from incoming to outgoing location *)
      let _ = Cfg.add_arc env cfg loc newloc Arc.Id in ();
     (* add non-accelerable transitions from incoming location and
        add non-accelerable loops on ALL interior location *)
     List.iter (fun nloc -> 
     (List.iter 
       (fun arc ->          
         let (a,f) = Arc.get_ass_equs env arc in   
         let _ = Cfg.add_arc env cfg loc nloc (Arc.Normal(a,f)) in
         let _ = Cfg.add_arc env cfg nloc nloc (Arc.Nonacc(a,f)) in ())
       lnaccarcs))
      (PSette.elements newlocs); 

    in
    (* do expansion corresponding to the number 
       of accelerable/non-accelerable loops ----------------------------------*)
    Log.debug2 logger ("(accelerable/non-accelerable) loops = ("^(string_of_int (List.length laccs))^","^(string_of_int (List.length lnaccs))^")");
    match (List.length laccs,List.length lnaccs) with
      |(0,_) -> ()
      |(1,0) -> expand_10 ()
      |(1,_) -> () (* keep e.g. (acc u nacc)*, could expand to (acc nacc* )* *)
      |_ -> expand_mn ()
  in
  (* do for each vertex ------------------------------------------------------*)
  PSHGraph.iter_vertex cfg
    (fun loc inv ~pred ~succ ->
      let loops = PSette.inter pred succ in
      if (PSette.cardinal loops)>0 then
        flatten_loops_loc loc inv 
          (PSette.diff pred loops) (PSette.diff succ loops) loops);
  cf

(******************************************************************************)
(* decouple numerical and boolean transition functions  *)
let decouple_equs env cf _ = 
  let cfg = cf.Program.c_cfg in
  let dlocs = ref(PSette.empty (Pervasives.compare)) in
  (* decouple arc ------------------------------------------------------------*)
  let decouple_tr_fcts_arc boolnacc (arcid,ass,f,sloc,dloc) =
    let f1 = BddapronUtil.simplify_equs env.Env.env env.Env.cond f ass in
    let (fa1,fna1) = if boolnacc then split_acc_nonacc env f1
                     else split_num_bool env f1 in
    PSHGraph.replace_attrhedge cfg arcid (Arc.Accel(ass,fa1));
    if not (PSette.mem dloc !dlocs) then
    begin
      (* memorize locations already treated - otherwise decoupled Bool/BoolNacc
         are duplicated (all Acc-transitions of the same self-loop 
         point to the same location) *)
      dlocs := PSette.add dloc !dlocs;
      let _ = 
        if boolnacc then 
          let (navars,_) = Env.split_equs fna1 in
          let fna2 = List.append (BddapronUtil.get_id_equs_for 
                       env.Env.env env.Env.cond navars) fna1 in
          Cfg.add_arc env cfg dloc dloc (Arc.BoolNacc(ass,fna2))
        else Cfg.add_arc env cfg dloc dloc (Arc.Bool(ass,fna1))
      in ()
    end
  in
  (* do for all arcs according to the decoupling mode ------------------------*)
  PSette.iter (decouple_tr_fcts_arc false) (Cfg.get_boolaccel_arcs cfg);
  PSette.iter (decouple_tr_fcts_arc true) (Cfg.get_boolnaccaccel_arcs cfg);
  cf

(******************************************************************************)
(* inputization *)
let inputize env cf _ =
  let cfg = cf.Program.c_cfg in
  let inputize_arc boolnacc (arcid,ass,f,sloc,dloc) =
    Log.debug3_o logger (Format.pp_print_int) "inputize loc: " sloc;
    let accelpreds = get_accel_preds env cfg sloc in
    let accelsuccscnt = get_accel_succs_cnt env cfg sloc in
    let nonaccscnt = get_nonaccs_cnt env cfg sloc in
    Log.debug3_o logger (Util.list_print (Format.pp_print_int)) 
      "accel_preds: " 
      (List.map (fun (x,_,_,_,_) -> x) (PSette.elements accelpreds));
    (* inputize only if there are no additional Nonacc loops *)
    if nonaccscnt=0 then 
    begin
     (* if there are at least two Accel transitions *)
     if accelsuccscnt>=1 then
     begin
      let f1 = BddapronUtil.simplify_equs env.Env.env env.Env.cond f ass in
      (* inputize in Bool or BoolNacc transition *)
      let newarc1 = 
        if boolnacc then 
          let (a,f) = inputize_tr_fct env (ass,f1) true in Arc.BoolNacc(a,f)
        else 
          let (a,f) = inputize_tr_fct env (ass,f1) false in Arc.Bool(a,f) 
      in
      PSHGraph.remove_hedge cfg arcid;
      let (idsuccid,sloc2,dloc2) = get_id_succ cfg sloc in
      let _ = Cfg.add_arc env cfg sloc dloc2 newarc1 in ();
      PSette.iter (fun (arcid,ass,f,_,_) -> 
        PSHGraph.replace_attrhedge cfg arcid 
          (Arc.Accel(inputize_guard env ass,f))) 
        accelpreds
     end
     (* if there is only one Accel transition *)
     else
     begin
      let (arcida,assa,fa,sloca,dloca) = PSette.choose accelpreds in
      (* add intermediate location *)
      let newloc = Cfg.add_loc env cfg (PSHGraph.attrvertex cfg sloc) in
      PSHGraph.remove_hedge cfg arcida;
      let _ = Cfg.add_arc env cfg sloca newloc (Arc.Accel(assa,fa)) in ();
      let f1 = BddapronUtil.simplify_equs env.Env.env env.Env.cond f ass in
      (* inputize in Bool or BoolNacc transition *)
      let newarc1 = 
        if boolnacc then 
          let (a,f) = inputize_tr_fct env (ass,f1) true in Arc.BoolNacc(a,f)
        else 
          let (a,f) = inputize_tr_fct env (ass,f1) false in Arc.Bool(a,f) 
      in
      PSHGraph.remove_hedge cfg arcid;
      let _ = Cfg.add_arc env cfg newloc dloc newarc1 in
      let _ = Cfg.add_arc env cfg newloc dloc Arc.Id in ()
     end
   end
  in
(* do for all arcs according to the decoupling mode ------------------------*)
 PSette.iter (inputize_arc false) (Cfg.get_bool_arcs cfg);
 PSette.iter (inputize_arc true) (Cfg.get_boolnacc_arcs cfg);
 cf
