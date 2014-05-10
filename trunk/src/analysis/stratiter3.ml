(******************************************************************************)
(* stratiter3 *)
(* numerical max strategy iteration *)
(* author: Peter Schrammel *)
(* version: 0.9.1m *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

exception NotSupported of string
exception NotPurelyNumerical

let iterations = ref(0)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Stratiter3";
              Log.level=Log.Debug3}

let tm_imp = TimeMeas.create ()
let tm_smt = TimeMeas.create ()
let tm_smt_ctx = TimeMeas.create ()
let tm_smt_conv = TimeMeas.create ()
let tm_val = TimeMeas.create ()
let tm_lp = TimeMeas.create ()
let tm_lp_conv = TimeMeas.create ()

type template_t = Template.template_t

type value_t = (Cfg.locid_t, Template.bound_t array) Mappe.t
type strat_t = (Cfg.locid_t, Cfg.arcid_t array) Mappe.t
type open_strats_t = (Cfg.locid_t * int, Cfg.arcid_t PSette.t * Cfg.arcid_t PSette.t) Mappe.t

(* global maps for BDD/MTBDD translation *)
let gtval_mtbddmap = ref (BddapronSmt.mtbddmap_create "gv_")
let val_mtbddmap = ref (BddapronSmt.mtbddmap_create "v_")
let nequ_mtbddmap = ref (BddapronSmt.mtbddmap_create "xe_")
let bexpr_bddmap = ref (BddapronSmt.bddmap_create "bb_")

let boundvar_prefix = "__d"
let arcid_var = "__arcid"

(******************************************************************************)
(* helpers *)
(******************************************************************************)

let print_strategy_elt env cfg fmt arcids = 
  Array.iteri (fun i arcid -> 
    Format.pp_print_newline fmt ();
    Format.pp_print_string fmt "[";
    Format.pp_print_int fmt i;
    Format.pp_print_string fmt "] ";
    let (g,equs) = Arc.get_ass_equs env (Cfg.get_arc cfg arcid) in
    let equs = Env.get_num_equs env equs in
    let equs = BddapronUtil.simplify_equs env.Env.env env.Env.cond equs g in
    BddapronUtil.print_boolexpr env.Env.env env.Env.cond fmt g;
    Format.pp_print_string fmt " -> ";
    BddapronUtil.print_equations env.Env.env env.Env.cond fmt equs)
  arcids

let print_value_elt fmt arr =
  Util.array_print (Template.print_bound) fmt arr

let print_strategy env cfg fmt strat = 
  Format.pp_print_newline fmt ();
  Mappe.print Format.pp_print_int (print_strategy_elt env cfg) fmt strat

let print_values env template fmt (s:value_t) = 
  Format.pp_print_newline fmt ();
  Mappe.print Format.pp_print_int (print_value_elt) fmt s

let print_open_strats fmt (open_strats:open_strats_t) = 
  Mappe.print (fun fmt (l,i) -> Format.pp_print_int fmt l;
                Format.pp_print_string fmt "[";
                Format.pp_print_int fmt i;
                Format.pp_print_string fmt "]")
   (fun fmt (x,y) -> PSette.print Format.pp_print_int fmt x; 
       PSette.print Format.pp_print_int fmt y) fmt open_strats

let benum_careset env = 
  List.fold_left
    (fun res v ->
      match Bddapron.Env.typ_of_var env.Env.env v with
	|`Benum typname  -> 
           let careset = BddapronUtil.get_enum_careset_for_var 
             env.Env.env env.Env.cond v typname in
           Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond res careset 
	|_ -> res)
    (Bddapron.Expr0.Bool.dtrue env.Env.env env.Env.cond) 
    (Util.list_diff env.Env.b_vars env.Env.new_i_vars)

let open_strats_add_new_loc cfg (open_strats:open_strats_t) slocs locid i = 
  let (old,used) = try Mappe.find (locid,i) open_strats 
        with Not_found -> (PSette.empty compare,PSette.empty compare) in
  let newpreds = PSHGraph.predhedge cfg locid in
  let newpreds = PSette.filter (fun arcid -> 
     let pred = (PSHGraph.predvertex cfg arcid).(0) in
     pred=locid)
   newpreds 
  in
  let open_strats = Mappe.add (locid,i) (PSette.diff (PSette.union old newpreds) used,used) open_strats in
  PSette.fold (fun succ (open_strats:open_strats_t) -> 
      let (old,used) = try Mappe.find (succ,i) open_strats 
        with Not_found -> (PSette.empty compare,PSette.empty compare)
      in
      let newpreds = PSHGraph.predhedge cfg succ in
      let newpreds = PSette.filter (fun arcid -> 
             let pred = (PSHGraph.predvertex cfg arcid).(0) in
            Sette.mem pred slocs)
        newpreds 
      in
      Mappe.add (succ,i) 
        (PSette.diff (PSette.union newpreds old) used,used) open_strats)
    (PSHGraph.succ_vertex cfg locid) open_strats 

(* initial must be purely boolean expression *)
let initial_strategy_value env template cfg initial =
  let initlocids = Cfg.get_locidset_by_inv env cfg initial in
  assert ((PSette.cardinal initlocids)=1);
  let initlocid = PSette.choose initlocids in
  let strat:strat_t = Mappe.empty in
  let s = Mappe.add initlocid 
    (Array.map (fun _ -> (Apron.Coeff.Scalar (Apron.Scalar.of_infty (-1)))) 
      (Array.of_list template)) 
    Mappe.empty in
  let open_strats = Util.array_fold_lefti
    (fun i open_strats _ -> 
      open_strats_add_new_loc cfg open_strats (Sette.singleton initlocid) 
        initlocid i)
    Mappe.empty (Array.of_list template)
  in
  (strat,s,open_strats)

let smt_build_gt_value_elt env t_elt (s_elt:Template.bound_t) =
  let nvarmap = List.map (fun v -> (v,v^"'")) env.Env.ns_vars in
  let cons = Template.cons_instantiate env t_elt s_elt in 
(*    Log.debug3_o logger (Template.print_cons) "cons: " cons; *)
  let smtcons = BddapronSmt.apron_lincons_to_smt env.Env.env cons in
  let smtcons = Smt.rename_real_vars nvarmap (Smt.Not smtcons) in
(*  Log.debug3 logger ("smt-gtv: "^(Smt.string_of_formula smtcons)); *)
  smtcons

let smt_build_value_elt env t_elt s_elt =
(*  Log.debug3_o logger Apron.Linexpr1.print "t_elt: " t_elt; *)
  let cons = Template.cons_instantiate env t_elt s_elt in
(*    Log.debug3_o logger (Template.print_cons) "cons: " cons; *)
  let smtcons = BddapronSmt.apron_lincons_to_smt env.Env.env cons in
(*  Log.debug3 logger ("smt-val: "^(Smt.string_of_formula smtcons)); *)
  smtcons
 
let smt_build_value env template (ss:Template.bound_t array) =
  let res = BddapronSmt.smt_and 
    (List.map2
      (fun t_elt s_elt -> 
         smt_build_value_elt env t_elt s_elt)
      template (Array.to_list ss))
  in
  res

let smt_build_equs env g equs = 
  let smtg = BddapronSmt.apron_linconss_to_smt env.Env.env g in
  let smtequs = BddapronSmt.apron_equs_to_smt env.Env.env equs in
  let res = BddapronSmt.smt_and [smtg;smtequs] in
  Log.debug3 logger ("smt-g: "^(Smt.string_of_formula smtg)); 
  Log.debug3 logger ("smt-equs: "^(Smt.string_of_formula res)); 
  res

let smt_build_boolexpr env boolexpr = 
  let res = BddapronSmt.boolexpr_to_smt !bexpr_bddmap env.Env.env env.Env.cond boolexpr in
  let res = Smt.And(res,(BddapronSmt.bddmap_to_smt !bexpr_bddmap)) in
  res

(******************************************************************************)
(* strategy improvement *)
(******************************************************************************)

let get_gequs env doman cfg assertion arcid = 
    let (ass,equs) = Arc.get_ass_equs env (Cfg.get_arc cfg arcid) in
    let ass = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond assertion ass in
    let equs = BddapronUtil.simplify_equs env.Env.env env.Env.cond equs ass in
    let fn = Env.get_num_equs env equs in
(*    Log.debug3_o logger (BddapronUtil.print_equations env.Env.env env.Env.cond)
      "fn: " fn; *)
    let nequs = BddapronUtil.numequs_of_equs env.Env.env env.Env.cond fn in
    let g = BddapronUtil.boolexpr_to_linconss env.Env.env env.Env.cond doman env.Env.b_vars ass in
    (g,nequs)

(* optimised version of Thomas' original algorithm *)
let strategy_improve_loci env doman template cfg assertion 
     (s:value_t) (locid:Cfg.locid_t) i (strat:strat_t)
     (open_strats:open_strats_t) 
     (open_arcs:Cfg.arcid_t PSette.t) =
  let updated = ref false in
  let open_strats = ref open_strats in
  let strat = ref strat in
  let open_arcs = ref open_arcs in
  while not !updated && not (PSette.is_empty !open_arcs) do
  begin
    TimeMeas.start tm_smt_conv;
    let arcid = PSette.choose !open_arcs in
    open_arcs := PSette.remove arcid !open_arcs;
    let srcid = (PSHGraph.predvertex cfg arcid).(0) in
    Log.debug3 logger ("arc: "^(string_of_int arcid)^" ("^(string_of_int srcid)^" -> "^(string_of_int locid)^")"); 
    let (g,equs) = get_gequs env doman cfg assertion arcid in
    let smtequs = smt_build_equs env g equs in
    let benum_careset = benum_careset env in
    let smtenum = smt_build_boolexpr env benum_careset in
    let smtv0 = smt_build_value env template (Mappe.find srcid s) in
    let smtvg0 = smt_build_gt_value_elt env (List.nth template i) 
      (try (Mappe.find locid s).(i) with Not_found ->
         Apron.Coeff.Scalar (Apron.Scalar.of_infty (1)))
    in
    TimeMeas.stop tm_smt_conv;

    TimeMeas.start tm_smt_ctx;
    let ctx1 = BddapronSmt.smt_assert_ctx None smtequs in 
    let ctx1 = BddapronSmt.smt_assert_ctx (Some ctx1) smtenum in
    let ctx1 = BddapronSmt.smt_assert_ctx (Some ctx1) smtv0 in
    let ctx1 = BddapronSmt.smt_assert_ctx (Some ctx1) smtvg0 in
    TimeMeas.stop tm_smt_ctx;

    TimeMeas.start tm_smt;
    let m1 = BddapronSmt.smt_compute_model_ctx env.Env.env env.Env.cond ctx1 in
    Log.debug2_o logger BddapronSmt.smt_print_result "smt-result: " m1;
    TimeMeas.stop tm_smt;
    TimeMeas.start tm_smt_ctx;
    Smt.del_ctx ctx1; 
    TimeMeas.stop tm_smt_ctx;

    match m1 with
      |None -> ()
      |Some _ -> 
      begin
        updated := true;
        Log.debug logger ("improved: ["^(string_of_int i)^"]: "^(string_of_int arcid)^" ("^(string_of_int srcid)^" -> "^(string_of_int locid)^")"); 
        let (opena,used) = Mappe.find (locid,i) !open_strats in
        open_strats := Mappe.add (locid,i) 
          (opena,PSette.add arcid used) !open_strats;
        open_strats := open_strats_add_new_loc cfg !open_strats
          (Sette.add locid (Mappe.maptoset s)) locid i;
        let sarr = try Mappe.find locid !strat 
          with Not_found -> Array.make (List.length template) 0
        in
        sarr.(i) <- arcid;
        strat := Mappe.add locid sarr !strat;
      end
  end
  done;
  (!updated,!strat,!open_strats)


(* algorithm a la GM ESOP'11 *)
let strategy_improve_loci2 env doman template cfg assertion 
     (s:value_t) (locid:Cfg.locid_t) i (strat:strat_t)
     (open_strats:open_strats_t) 
     (open_arcs:Cfg.arcid_t PSette.t) =
  if not (PSette.is_empty open_arcs) then
  begin
    TimeMeas.start tm_smt_conv;
    let f = PSette.fold (fun arcid f -> 
        let srcid = (PSHGraph.predvertex cfg arcid).(0) in
        Log.debug3 logger ("arc: "^(string_of_int arcid)^" ("^(string_of_int srcid)^" -> "^(string_of_int locid)^")"); 
        let (g,equs) = get_gequs env doman cfg assertion arcid in
        let smtequs = smt_build_equs env g equs in
        let smtv0 = smt_build_value env template (Mappe.find srcid s) in
        Smt.Or (f,Smt.And 
          (Smt.Eq (Smt.Real arcid_var,Smt.Const 
               {Smt.num=Big_int.big_int_of_int arcid; 
                Smt.den=Big_int.big_int_of_int 1}),
             Smt.And (smtequs,smtv0))))
      open_arcs Smt.F 
    in

    let benum_careset = benum_careset env in
    let smtenum = smt_build_boolexpr env benum_careset in
    let smtvg0 = smt_build_gt_value_elt env (List.nth template i) 
      (try (Mappe.find locid s).(i) with Not_found ->
         Apron.Coeff.Scalar (Apron.Scalar.of_infty (1)))
    in
    TimeMeas.stop tm_smt_conv;

    TimeMeas.start tm_smt_ctx;
    let ctx1 = BddapronSmt.smt_assert_ctx None f in 
    let ctx1 = BddapronSmt.smt_assert_ctx (Some ctx1) smtenum in
    let ctx1 = BddapronSmt.smt_assert_ctx (Some ctx1) smtvg0 in
    TimeMeas.stop tm_smt_ctx;

    TimeMeas.start tm_smt;
    let m1 = BddapronSmt.smt_compute_model_ctx env.Env.env env.Env.cond ctx1 in
    Log.debug2_o logger BddapronSmt.smt_print_result "smt-result: " m1;
    TimeMeas.stop tm_smt;
    TimeMeas.start tm_smt_ctx;
    Smt.del_ctx ctx1; 
    TimeMeas.stop tm_smt_ctx;

    match m1 with
      |None -> (false,strat,open_strats)
      |Some m -> 
      begin
        let arcid = match Hashtbl.find m arcid_var with
	  |Smt.VRatio v -> Big_int.int_of_big_int v.Smt.num
	  |_ -> assert false
        in
        let srcid = (PSHGraph.predvertex cfg arcid).(0) in
        Log.debug logger ("improved: ["^(string_of_int i)^"]: "^(string_of_int arcid)^" ("^(string_of_int srcid)^" -> "^(string_of_int locid)^")"); 
        let (opena,used) = Mappe.find (locid,i) open_strats in
        let open_strats = Mappe.add (locid,i) 
          (opena,PSette.add arcid used) open_strats in
        let open_strats = open_strats_add_new_loc cfg open_strats
          (Sette.add locid (Mappe.maptoset s)) locid i in
        let sarr = try Mappe.find locid strat 
          with Not_found -> Array.make (List.length template) 0
        in
        sarr.(i) <- arcid;
        (true,Mappe.add locid sarr strat,open_strats)
      end
  end
  else (false,strat,open_strats)

(* for all locs with open strats *)
let strategy_improve ~strategy_improve_loci 
   env doman template cfg assertion strat 
     (open_strats:open_strats_t) s =
  TimeMeas.start tm_imp;
  let (updated,strat,open_strats) = List.fold_left 
    (fun (updated,strat,open_strats) ((locid,i),(open_arcs,_)) -> 
      let (updated1,strat,open_strats) = 
        strategy_improve_loci env doman template cfg assertion s locid i strat open_strats open_arcs in
      (updated or updated1,strat,open_strats))
    (false,strat,open_strats) (Mappe.bindings open_strats)
  in
  TimeMeas.stop tm_imp;
  if updated then Some (strat,open_strats) else None

(******************************************************************************)
(* compute strategy value *)
(******************************************************************************)

let add_suffix s1 s2 v = v^"_"^(string_of_int (-s1))^"_"^(string_of_int (s2))
let add_suffix_primed s1 s2 v = v^"_"^(string_of_int (-s1))^"_"^(string_of_int (s2))^"'"

let add_suffix2 s1 s2 s3 v = v^"_"^(string_of_int (-s1))^"_"^(string_of_int (-s2))^"_"^(string_of_int (s3))
let add_suffix_primed2 s1 s2 s3 v = v^"_"^(string_of_int (-s1))^"_"^(string_of_int (-s2))^"_"^(string_of_int (s3))^"'"

let add_suffix3 s1 s2 s3 s4 v = v^"_"^(string_of_int (-s1))^"_"^(string_of_int (-s2))^"_"^(string_of_int (s3))^"_"^(string_of_int (s4))
let add_suffix_primed3 s1 s2 s3 s4 v = v^"_"^(string_of_int (-s1))^"_"^(string_of_int (-s2))^"_"^(string_of_int (s3))^"_"^(string_of_int (s4))^"'"

let strat_to_lpconss env doman cfg template assertion strat s =  
  (* build constraints *)
  let tconss = Array.map 
        (fun c -> 
          let v = Apron.Var.of_string boundvar_prefix in
          (v,c)) 
        (Array.of_list template)
  in
  let (f,vars) = List.fold_left
    (fun (f,vars) (tgtid,arcids) -> 
      Util.array_fold_lefti (fun i (f,vars) arcid -> 
        let pred = PSHGraph.predvertex cfg arcid in
        let succ = PSHGraph.succvertex cfg arcid in
        assert((Array.length pred)=1);
        assert((Array.length succ)=1);
        assert(tgtid=succ.(0));
        let (g,equs) = get_gequs env doman cfg assertion arcid in
        let ftr = List.append (ApronLp.make_conss_conss (* transition *)
                  ~rename:(add_suffix3 pred.(0) tgtid arcid i) g)
                (List.map (ApronLp.make_conss_equ 
                      ~rename_lhs:(add_suffix_primed3 pred.(0) tgtid arcid i) 
                      ~rename_rhs:(add_suffix3 pred.(0) tgtid arcid i))
                  (Array.to_list equs))
        in
        let ftrl = List.mapi (fun j _ -> 
                 ApronLp.make_conss_lequ (* source location constraints *)
                      ~rename_lhs:(add_suffix pred.(0) j) 
                      ~rename_rhs:(add_suffix3 pred.(0) tgtid arcid i) 
                      tconss.(j))
                 template
        in
        let tcons = tconss.(i) in
        let (v,_) = tcons in
        let svars = (*  bound variables *)
          let sv = Apron.Var.of_string (add_suffix pred.(0) i 
                                      (Apron.Var.to_string v)) in
          if not(Sette.mem sv vars) then Sette.singleton sv else Sette.empty
        in
        let f = List.concat 
         [ftr;ftrl;
          [ApronLp.make_conss_gequ (* target location constraints *)
                      ~rename_lhs:(add_suffix tgtid i) 
                      ~rename_rhs:(add_suffix_primed3  pred.(0) tgtid arcid i) 
                      tcons];
         f] in
      (f,Sette.union (Sette.add (Apron.Var.of_string (add_suffix tgtid i 
                                      (Apron.Var.to_string v))) svars) vars))
      (f,vars) arcids)
    ([],Sette.empty) (Mappe.bindings strat)
  in
  let f = List.fold_left (fun f (locid,sarr) -> (* old value constraints *)
      Util.array_fold_lefti (fun i f bound -> 
          match bound with 
	    |Apron.Coeff.Scalar bb -> 
               if (Apron.Scalar.is_infty bb)=0 then 
               begin
                 let expr =  Apron.Linexpr1.make env.Env.apronenv in
                 Apron.Linexpr1.set_cst expr bound;
                 (ApronLp.make_conss_lequ 
                   ~rename_lhs:(add_suffix locid i) 
                   (Apron.Var.of_string boundvar_prefix,expr))::f
               end
               else f
	    |_ -> assert(false))
        f sarr)
    f (Mappe.bindings s) 
  in
  (f,Array.of_list (Sette.elements vars))

(** computes the value of strategy strat *)
let compute_strategy_value env doman (cfg:Cfg.t) (template:Template.template_t) initial assertion strat s =

  TimeMeas.start tm_val;
  TimeMeas.start tm_lp_conv;
  let (lp_constrs,vars) = 
    strat_to_lpconss env doman cfg template assertion strat s in
  TimeMeas.stop tm_lp_conv;

  Log.debug2_o logger ApronLp.print_lp_conss "conss: " lp_constrs;
  (* list of variables that are trivially infinite (due to value, strat rhs) *)
  Log.debug3_o logger (ApronUtil.print_vars) "vars: " vars;
(*  let unbounded_vars = [||]  in
  Log.debug3_o logger (ApronUtil.print_vars) "unbounded-vars: " unbounded_vars;
*)
  (* test variables whether unbounded *)
  TimeMeas.start tm_lp;
  let bounded_vars = ApronLp.get_bounded_vars lp_constrs vars in
  TimeMeas.stop tm_lp;
  Log.debug2_o logger (ApronUtil.print_vars) "bounded-vars: " bounded_vars;

  (* solve for bounded variables *)
  TimeMeas.start tm_lp;
  let obj_fct = ApronLp.make_obj_fct_max_negsum_vars bounded_vars in
  let (obj_val, sol) = ApronLp.solve lp_constrs obj_fct in 
  TimeMeas.stop tm_lp;
  Log.debug2_o logger Apron.Coeff.print "obj_val: " obj_val;
  Log.debug2_o logger (ApronLp.print_lp_sol bounded_vars) "solution: " sol;


(*  assert (not ((ApronUtil.coeff_is_infty obj_val) or ((ApronUtil.coeff_is_neginfty obj_val))));*)
  (* update for each bound: loc -> value *)
  let s = Mappe.fold
    (fun locid _ s ->
      let sarr = try Mappe.find locid s 
        with Not_found -> 
          Array.make (List.length template) 
            (Apron.Coeff.Scalar (Apron.Scalar.of_infty (-1)))
      in
      List.iteri
        (fun i t_elt -> 
          let v = (Apron.Var.of_string 
                        (add_suffix locid i boundvar_prefix)) in
          let x = 
            if(Util.array_mem v bounded_vars) then sol v 
            else Apron.Coeff.Scalar (Apron.Scalar.of_infty (-1))
          in
          Log.debug2_o logger Apron.Coeff.print "val = " x;
          sarr.(i) <- x)
        template; 
      Mappe.add locid sarr s) strat s
   in
   TimeMeas.stop tm_val;
   s

(** converts a value MTBDD array into a BDDAPRON abstract value   *)
let values_to_abstract env doman cfg template values = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  List.map (fun locid ->
    try
      let bounds = Mappe.find locid values in
      let inv = Loc.get_inv (Cfg.get_loc cfg locid) in
      Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond) "inv = "  inv;
      (locid,Bddapron.Domain0.of_bddapron doman env.Env.env 
         [inv,Apron.Abstract1.abstract0 (Apron.Abstract1.of_lincons_array 
         apronman env.Env.apronenv
         (Template.template_instantiate env template (Array.to_list bounds)))])
    with Not_found -> (locid,Bddapron.Domain0.bottom doman env.Env.env))
    (PSette.elements (Cfg.get_locidset cfg))

(******************************************************************************)
(* main strategy improvement loop *)
(******************************************************************************)

(** the main strategy improvement loop *)
let analyze_num ~smt_disjuncts template env doman cfprog =
  let (strat0,s0,os0) = initial_strategy_value env template cfprog.Program.c_cfg
    cfprog.Program.c_init in
  let strategy_improve_loci = 
    if smt_disjuncts then strategy_improve_loci2
    else strategy_improve_loci
  in
  let strat = ref(strat0) in
  let s = ref(s0) in
  let open_strats = ref(os0) in
  let stable = ref(false) in
  Log.debug_o logger (print_strategy env cfprog.Program.c_cfg) "strategy: " !strat;
  Log.debug_o logger (print_values env template) "value: " !s;
  Log.debug_o logger (print_open_strats) "open_strats: " !open_strats;
  while not !stable do
  begin
    match strategy_improve ~strategy_improve_loci env doman template cfprog.Program.c_cfg cfprog.Program.c_ass !strat !open_strats !s with
    |Some (newstrat,os) ->
      strat := newstrat;
      open_strats := os;
      Log.debug_o logger (print_strategy env cfprog.Program.c_cfg) 
        "strategy: " !strat;
      Log.debug_o logger (print_open_strats) "open_strats: " !open_strats;
      s := compute_strategy_value env doman cfprog.Program.c_cfg template
             cfprog.Program.c_init cfprog.Program.c_ass
             !strat !s; 
      Log.debug_o logger (print_values env template) "value: " !s;
      iterations := !iterations +1;
    |None -> stable := true
  end
  done;
  !s

(******************************************************************************)
(** {2 module Num: numerical max-strategy iteration} *)
(******************************************************************************)
module Num(Dom :  Template.TEMPLATE_T) =
struct
type analysisparam_t = bool

(******************************************************************************)
let refine_loc env anres ~refine_bool locid loc = 
  let s = List.assoc locid anres in
  if Dom.is_bottom env s then None
  else 
    Some (if refine_bool then Dom.meetbool_to_boolexpr env s loc
          else Dom.meet_to_boolexpr env s loc)

let print_result env final anres fmt () = 
  let print_res_loc locid s res =
    Format.pp_print_newline fmt ();
    let strfmt = Format.str_formatter in
    Format.pp_print_string strfmt "LOC ";
    Format.pp_print_int strfmt locid;
    if res then Format.pp_print_string strfmt ": reach = "
    else Format.pp_print_string strfmt ": CONTAINS BAD STATES, reach = ";
    let str1 = Format.flush_str_formatter () in
    Dom.print env strfmt s;
    let str2 = Format.flush_str_formatter () in
    Format.pp_print_string logger.Log.fmt (str1^(Util.string_compact str2))
  in
  List.iter (fun (locid,s) ->
     print_res_loc locid s (Dom.is_bottom env (Dom.meet_condition env s final)))
    anres 
   
let result_to_bddapron env anres () = 
   Util.list2mappe (List.map (fun (locid,s) -> (locid,Dom.to_boollinconsslist env s)) anres)

let is_safe env final anres = 
  let rec check = function
    |[] -> true
    |(_,s)::reslist -> 
      let res = Dom.is_bottom env (Dom.meet_condition env s final) in
      if res then
        check reslist
      else false
  in
  check anres

(******************************************************************************)
let analyze (param:analysisparam_t) env cf =
  let (doman,template) = Dom.doman () in (* here we get the template *)

  Log.info_o logger (Format.pp_print_int) "Template lines:  " (List.length template);

  let values = analyze_num ~smt_disjuncts:param template env doman cf in

  (* print statistics *)
  Log.info_o logger (Format.pp_print_int) "Iterations:  " !iterations;
  Log.info logger "--------------";
  Log.info_o logger (TimeMeas.print) "Strategy improvement time: " tm_imp;
  Log.info_o logger (TimeMeas.print) "Strategy value time:       " tm_val;
  Log.info logger "--------------";
  Log.info_o logger (TimeMeas.print) "SMT formula building time: " tm_smt_conv;
  Log.info_o logger (TimeMeas.print) "SMT context manip. time:   " tm_smt_ctx;
  Log.info_o logger (TimeMeas.print) "SMT solving time:          " tm_smt;
  Log.info_o logger (TimeMeas.print) "LP problem building time:  " tm_lp_conv;
  Log.info_o logger (TimeMeas.print) "LP solving time:           " tm_lp;

  let anres = values_to_abstract env doman cf.Program.c_cfg template values in
  
  (is_safe env cf.Program.c_final anres,
   refine_loc env anres,
   print_result env cf.Program.c_final anres,
   result_to_bddapron env anres)

end
