(******************************************************************************)
(* stratiter2 *)
(* logico-numerical max strategy iteration with native power domain *)
(* author: Peter Schrammel *)
(* version: 0.9.1m *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

(* 
TODOs:
- how to deal with test9.nbac?
*)

(* 
Remarks on various unsuccessful optimisation attempts:
- Boolean encoding of template rows: slower than with numerical vars
- remembering visited strategies is only useful if it helps generating a smaller SMT formula
*)

exception NotSupported of string
exception NotPurelyNumerical

type powlognum_param_t = 
    MicroIter   (* with micro-iterations and generalisation*)
  | NoMicroIter (* no micro-iterations, no generalisation *)
  | NoMicroGenIter (* no micro-iterations, but with generalisation *)
  | SymbRows    (* symbolic template rows *)
  | TryImpStrat (* try improved strategy on other template rows *)

let iterations = ref(0)
let minoriter = ref(0)
let microiter = ref(0)
let tryimpsucc = ref(0)

let template_row_var = "tplt_row"

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Stratiter2";
              Log.level=Log.Debug3}

let tm_strats = TimeMeas.create ()
let tm_imp = TimeMeas.create ()
let tm_smt = TimeMeas.create ()
let tm_smt_ctx = TimeMeas.create ()
let tm_smt_conv = TimeMeas.create ()
let tm_smt_mod = TimeMeas.create ()
let tm_val = TimeMeas.create ()
let tm_lp = TimeMeas.create ()
let tm_lp_conv = TimeMeas.create ()

type template_t = Template.template_t

(* global maps for BDD/MTBDD translation *)
let gtval_mtbddmap = ref (BddapronSmt.mtbddmap_create "gv_")
let val_mtbddmap = ref (BddapronSmt.mtbddmap_create "v_")
let nequ_mtbddmap = ref (BddapronSmt.mtbddmap_create "xe_")
let bequ_bddmap = ref (BddapronSmt.bddmap_create "b_")
let bexpr_bddmap = ref (BddapronSmt.bddmap_create "bb_")

(* current strategy:
     template line -> target bool -> (source bool guard, num guard,action)
*)
type strategy_leaf_t = Env.var_t BddapronUtil.boolexpr_t * ApronUtil.linconss_t * ApronUtil.equs_t
type strategy_elt_t = strategy_leaf_t Cudd.Mtbdd.t
type strategy_elt_table_t = strategy_leaf_t Cudd.Mtbdd.table
type strategy_t = strategy_elt_t array

(* current value: 
     template line -> target bool -> bound
*)
type value_elt_table_t = Template.cons_t Cudd.Mtbdd.table
type value_elt_t = Template.cons_t Cudd.Mtbdd.t
type value_t = value_elt_t array

(* CFG of numerical system for a strategy *)
type info_t = { mutable locidx : int; mutable arcidx : int;}
type stratcfg_t = (int, int, int * Env.var_t BddapronUtil.boolexpr_t, ApronUtil.linconss_t * ApronUtil.equs_t, info_t) PSHGraph.t

(* available strategies = the program *)
type strategies_leaf_t = ApronUtil.linconss_t * ApronUtil.equs_t array
type strategies_t = strategies_leaf_t Cudd.Mtbdd.t 
type strategies_table_t = strategies_leaf_t Cudd.Mtbdd.table

let boundvar_prefix = "__d"

(******************************************************************************)
(* move this to BddapronSmt *)
(******************************************************************************)


(******************************************************************************)
(* cfprog to strategies *)
(******************************************************************************)
let make_strats_table () = Cudd.Mtbdd.make_table ~hash:(Hashtbl.hash) ~equal:(=)
let make_strat_table () = Cudd.Mtbdd.make_table ~hash:(Hashtbl.hash) ~equal:(=)
let make_value_table () = Cudd.Mtbdd.make_table ~hash:(Hashtbl.hash) ~equal:(=)

let make_leaf table l = Cudd.Mtbdd.unique table l

let print_strategy_leaf env fmt (b,g,equs) = 
  BddapronUtil.print_boolexpr env.Env.env env.Env.cond fmt b;
  Format.pp_print_string fmt " /\\ ";
  ApronUtil.print_linconss fmt g;
  Format.pp_print_string fmt " -> ";
  ApronUtil.print_equations fmt equs

let print_value_leaf = Template.print_cons

let print_strategies_leaf env fmt (g,equs) = 
  ApronUtil.print_linconss fmt g;
  Format.pp_print_string fmt " -> ";
  ApronUtil.print_equations fmt equs

let print_strategies env fmt strats = 
  Format.pp_print_newline fmt ();
  MtbddUtil.print_base_mtbdd env.Env.env env.Env.cond
      (print_strategies_leaf env) fmt strats

let print_strategy_elt env fmt (t_elt,s_elt) = 
  Format.pp_print_newline fmt ();
  Template.print_elt fmt t_elt;
  Format.pp_print_string fmt " --> ";
  MtbddUtil.print_base_mtbdd env.Env.env env.Env.cond
    (print_strategy_leaf env) fmt s_elt

let print_strategy env template fmt (strat:strategy_t) = 
  Util.array_iter2 (fun t_elt s_elt -> 
    print_strategy_elt env fmt (t_elt,s_elt))
    (Array.of_list template) strat

let print_values env template fmt (s:value_t) = 
  Util.array_iter2 (fun t_elt s_elt -> 
    Format.pp_print_newline fmt ();
    Template.print_elt fmt t_elt;
    Format.pp_print_string fmt " --> ";
    MtbddUtil.print_base_mtbdd env.Env.env env.Env.cond
      (print_value_leaf) fmt s_elt)
    (Array.of_list template) s

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

(*
let generate_boolencoding n = 
  let rec generate i b n =  
    if n<=1 then ([],[b])
    else
      let v = tplt_prefix^(string_of_int i) in
      let b1 = Smt.And (b,Smt.Bool v) in
      let b2 = Smt.And (b,Smt.Not (Smt.Bool v)) in
      let (v1,e1) = generate (i+1) b1 (n/2) in
      let (v2,e2) = generate (i+1) b2 (n-(n/2)) in
      (List.append (v::v1) v2,List.append e1 e2)
  in
  generate 0 Smt.T n
*)

let build_strategies strat_table env doman assertion equs =
  let fn = Env.get_num_equs env equs in
  let (vars,_) = List.split fn in 
  let apronvars = BddapronUtil.vars_to_apronvars env.Env.env vars in
  let ga = BddapronUtil.numequs_to_guardedactions env.Env.env env.Env.cond ~assertion doman
    fn
  in
  let benum_careset = benum_careset env in
  let ga = List.map (fun (b,g,a) -> 
      (Bddapron.Expr0.Bool.tdrestrict b benum_careset,g,a)) 
    ga
  in
  let ga = List.filter (fun (b,g,_) -> 
      not (Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond b) && 
      not (ApronUtil.linconss_is_unsat g)) 
    ga
  in
  let pexprs = BddapronUtil.generate_boolencoding env.Env.env env.Env.cond env.Env.p_vars (List.length ga) in 
  let ga = List.map2 
    (fun (b,g,a) pexpr ->
      let a = Util.array_map2 
        (fun v a -> (v,BddapronUtil.apronaction_to_linexpr 
                         env.Env.env env.Env.cond a)) 
        apronvars a 
      in  
      (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond b pexpr,(g,a)))
    ga pexprs
  in
  let bcompl = Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond
    (List.fold_left (fun res (b,_) -> 
       Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond res b) 
       (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond) ga) 
  in
  let bcompl = Bddapron.Expr0.Bool.tdrestrict bcompl benum_careset in
  if Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond bcompl then
    MtbddUtil.bddleafarr_to_mtbdd (make_leaf strat_table) env.Env.env 
     (Array.of_list ga)
  else
    MtbddUtil.bddleafarr_to_mtbdd (make_leaf strat_table) env.Env.env 
      (Array.of_list ((bcompl,
                     (ApronUtil.linconss_false env.Env.apronenv,[||]))::ga))

(* initial must be purely boolean expression *)
let initial_strategy_value env template value_table strat_table strats initial =
  let strat = Array.map (fun _ -> 
    let id_equs = ApronUtil.get_id_equations_for env.Env.apronenv 
       (BddapronUtil.vars_to_apronvars env.Env.env env.Env.ns_vars) in
    MtbddUtil.bddleafarr_to_mtbdd (make_leaf strat_table) env.Env.env 
      [|(initial,(Bddapron.Expr0.Bool.dtrue env.Env.env env.Env.cond,
                  ApronUtil.linconss_true env.Env.apronenv,id_equs));
      (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond initial,
        (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond,
          ApronUtil.linconss_false env.Env.apronenv,id_equs))|])
    (Array.of_list template)
  in
  let s = Array.map (fun _ -> 
    MtbddUtil.bddleafarr_to_mtbdd (make_leaf value_table) env.Env.env 
      [|(initial,Template.cons_true env);
        (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond initial,
          Template.cons_false env)|])
    (Array.of_list template)
  in
  (strat,s)

let simplify_strats strats b =
  Cudd.Mtbdd.tdrestrict strats b

let compute_bool_inv env s =
  assert ((Array.length s)>0);
  let bc = Cudd.Mtbdd.guardleafs s.(0) in
  Array.fold_left (fun inv (b,c) -> 
      if not(Apron.Lincons1.is_unsat c) then 
         Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond inv b
      else inv)
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond) bc
 
let get_value_leaf env value_elt boolexpr =
  let value = Cudd.Mtbdd.tdrestrict value_elt boolexpr in
  let leaf = Array.fold_left
    (fun res (b,v) -> v::res)   
    [] (Cudd.Mtbdd.guardleafs value)
  in
  assert((List.length leaf)=1);
  List.hd leaf

let get_strategies_leaf env strats_table strats boolexpr =
(*  let strat = Cudd.Mtbdd.tdrestrict strats boolexpr in *)
  let strat = Cudd.Mtbdd.ite boolexpr strats 
    (Cudd.Mtbdd.cst env.Env.cuddman strats_table 
       (ApronUtil.linconss_false env.Env.apronenv,[||])) in

(*  Log.info_o logger (print_strategies env) "selected strategies-mtbdd: " strat; *)
  let leaf = Array.fold_left
    (fun res (b,(g,a)) ->
      if ApronUtil.linconss_is_unsat g then res
      else (b,g,a)::res)   
    [] (Cudd.Mtbdd.guardleafs strat)
  in
  assert((List.length leaf)=1); (* TODO: sometimes this fails -> check why *)
  List.hd leaf

let update_strategy_elt env strat_table strat boolexpr leaf =
  Cudd.Mtbdd.ite boolexpr 
    (Cudd.Mtbdd.cst env.Env.cuddman strat_table leaf) strat

let smt_build_gt_value_elt env t_elt s_elt =
  let nvarmap = List.map (fun v -> (v,v^"'")) env.Env.ns_vars in
  let leaf_to_smt cons =
(*    let cons = Template.cons_instantiate env t_elt bound in *)
(*    Log.debug3_o logger (Template.print_cons) "tcons: " cons; *)
    let smtcons = BddapronSmt.apron_lincons_to_smt env.Env.env cons in
    let smtcons = YicesSmt.rename_real_vars nvarmap (YicesSmt.Not smtcons) in
(*    Log.debug3 logger ("smt-gt-cons: "^(Smt.string_of_formula smtcons)); *)
    smtcons
  in
  let bs_vars = Util.list2sette env.Env.bs_vars in
  let res = BddapronSmt.mtbdd_to_smt !gtval_mtbddmap ~prime_bool:bs_vars leaf_to_smt env.Env.env env.Env.cond s_elt in
  let res = YicesSmt.And(res,(BddapronSmt.mtbddmap_to_smt !gtval_mtbddmap)) in
(*  Log.debug3 logger ("smt-gt_value_elt: "^(Smt.string_of_formula res)); *)
  res

let smt_build_value_elt env t_elt s_elt =
(*  Log.debug3_o logger Apron.Linexpr1.print "t_elt: " t_elt; *)
  let leaf_to_smt cons =
(*    Log.debug3_o logger (Template.print_cons) "cons: " cons; *)
    let smtcons = BddapronSmt.apron_lincons_to_smt env.Env.env cons in
(*    Log.debug3 logger ("smt-cons: "^(Smt.string_of_formula smtcons)); *)
    smtcons
  in
  let res = BddapronSmt.mtbdd_to_smt !val_mtbddmap leaf_to_smt env.Env.env env.Env.cond s_elt in
(*  Log.debug3 logger ("smt-value_elt: "^(Smt.string_of_formula res)); *)
  res
 
let smt_build_value env template s =
  let res = BddapronSmt.smt_and (Array.to_list (Util.array_map2
    (fun t_elt s_elt -> smt_build_value_elt env t_elt s_elt)
    (Array.of_list template) s)) in
  let res = YicesSmt.And(res,(BddapronSmt.mtbddmap_to_smt !val_mtbddmap)) in
  res

let smt_build_strats env strats = 
  let leaf_to_smt (g,equs) =
    let smtg = BddapronSmt.apron_linconss_to_smt env.Env.env g in
(*    Log.debug3 logger ("smt-g: "^(Smt.string_of_formula smtg)); *)
    let smtequs = BddapronSmt.apron_equs_to_smt env.Env.env equs in
(*    Log.debug3 logger ("smt-equs: "^(Smt.string_of_formula smtequs)); *)
    BddapronSmt.smt_and [smtg;smtequs]
  in
  let res = BddapronSmt.mtbdd_to_smt !nequ_mtbddmap leaf_to_smt env.Env.env env.Env.cond strats in
  let res = YicesSmt.And(res,(BddapronSmt.mtbddmap_to_smt !nequ_mtbddmap)) in
(*  Log.debug3 logger ("smt-strats: "^(Smt.string_of_formula res)); *)
  res

let smt_build_bequs env bequs = 
  let res = BddapronSmt.equs_to_smt !bequ_bddmap env.Env.env env.Env.cond bequs in
  let res = YicesSmt.And(res,(BddapronSmt.bddmap_to_smt !bequ_bddmap)) in
  res

let smt_build_boolexpr env boolexpr = 
  let res = BddapronSmt.boolexpr_to_smt !bexpr_bddmap env.Env.env env.Env.cond boolexpr in
  let res = YicesSmt.And(res,(BddapronSmt.bddmap_to_smt !bexpr_bddmap)) in
  res

let model_filter_vars vars m = List.filter (fun (x,_) -> List.mem x vars) m 
let smt_model_filter_vars vars m = 
  let res = Hashtbl.create (List.length vars) in
  List.iter (fun v -> 
    try Hashtbl.add res v (Hashtbl.find m v)
    with Not_found -> ()) vars;
  res

let model_to_smt_boolexpr env m = 
  BddapronSmt.smt_and (List.map (fun (v,x) -> 
    match x with
    |`Bool x -> 
      if Bddapron.Expr0.Bool.is_true env.Env.env env.Env.cond x then YicesSmt.Bool v 
      else YicesSmt.Not (YicesSmt.Bool v)
    |_ -> raise (NotSupported "numerical and enumerated types")) 
    m)

let smt_model_to_smt_boolexpr m = 
  let bindings = Util.hashtbl_to_list m in
  BddapronSmt.smt_and (List.map (fun (v,x) -> 
    match x with
    |YicesSmt.VBool x -> if x then YicesSmt.Bool v else YicesSmt.Not (YicesSmt.Bool v)
    |YicesSmt.VRatio x -> YicesSmt.Eq (YicesSmt.Real v,YicesSmt.Const x))
    bindings)

(******************************************************************************)
(* strategy improvement *)
(******************************************************************************)

(** computes an improving strategy *)
let strategy_improve_elt strats_table strat_table value_table env 
    template strats ff0 ctx1 strat_i s s_elt t_elt =

  TimeMeas.start tm_smt_conv;
  let smtvg0 = smt_build_gt_value_elt env t_elt s_elt in
(* Log.debug3 logger ("smt-gt_value0: "^(Smt.string_of_formula smtvg0)); *)
  let ff=BddapronSmt.smt_and [smtvg0;ff0] in
  TimeMeas.stop tm_smt_conv;

  TimeMeas.start tm_smt_ctx;
  let assertion_ids = ref [BddapronSmt.smt_assert_retractable_ctx ctx1 smtvg0]
  in
  TimeMeas.stop tm_smt_ctx;

(* Log.debug3 logger ("smt-ff: "^(Smt.string_of_formula ff)); *)

  let sat1 = ref true in
  let found = ref false in
  let strat_elt = ref strat_i in
  while !sat1 do
  begin
    minoriter := !minoriter+1;
    Log.debug_o logger Format.pp_print_int "minor iteration: " !minoriter; 
    TimeMeas.start tm_smt;
    let m1 = BddapronSmt.smt_compute_model_ctx env.Env.env env.Env.cond ctx1 in
    Log.debug2_o logger BddapronSmt.smt_print_result "smt-result1: " m1;
    TimeMeas.stop tm_smt;

    match m1 with 
    | None -> sat1:= false
    | Some mm1 -> 
    begin
      found := true;
      let m = ref mm1 in

      TimeMeas.start tm_smt_mod;
      let nmodel = smt_model_filter_vars env.Env.n_vars mm1 in
      let nff = YicesSmt.instantiate_formula nmodel ff in
      TimeMeas.stop tm_smt_mod;

      TimeMeas.start tm_smt_ctx;
      let ctx2 = ref(BddapronSmt.smt_assert_ctx None nff) in 
      TimeMeas.stop tm_smt_ctx;
  (*    Log.debug3 logger ("smt-ff-inst-n: "^(Smt.string_of_formula !nff)); *)
 
      let sat2 = ref true in
      while !sat2 do
      begin
        TimeMeas.start tm_smt_mod;
        let mm = BddapronSmt.smt_convert_model env.Env.env env.Env.cond !m in
(*        Log.debug3_o logger (BddapronSmt.smt_print_model env.Env.env 
           env.Env.cond) "smt-model: " mm; *)
        let bpvars = List.append env.Env.bs_vars env.Env.p_vars in
(*        Log.debug3_o logger (Util.list_print (Format.pp_print_string)) "bpvars: " bpvars; *)
        let bpmodel = model_filter_vars bpvars mm in
(*        Log.debug2_o logger (BddapronSmt.smt_print_model env.Env.env 
          env.Env.cond) "smt-bp-model: " bpmodel; *)
        let bpexpr = BddapronSmt.model_to_boolexpr env.Env.env env.Env.cond bpmodel in
(*        Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env 
          env.Env.cond) "bp-model-boolexpr: " bpexpr; *)
        let leaf = get_strategies_leaf env strats_table strats bpexpr in
(*        Log.debug2_o logger (print_strategy_leaf env) "leaf: " leaf;*)

        let bpmodel2 = smt_model_filter_vars (BddapronSmt.vars_to_smtvars env.Env.env env.Env.cond bpvars) !m in
        let bprime = YicesSmt.instantiate_formula bpmodel2 nff in
(*         Log.debug3 logger ("smt-bprime: "^(Smt.string_of_formula bprime));*)

        let bprimeexpr = BddapronSmt.smt_to_boolexpr ~unprime:true env.Env.env 
          env.Env.cond bprime in  
        let bisupp = BddapronUtil.supp_of_vars env.Env.env (List.append env.Env.bi_vars env.Env.p_vars) in
        let bprimeexpr = BddapronUtil.boolexpr_forget_supp bisupp bprimeexpr in
(*        Log.debug2_o logger (BddapronUtil.print_boolexpr env.Env.env 
          env.Env.cond) "b'-boolexpr: " bprimeexpr;*)

        strat_elt := update_strategy_elt env strat_table !strat_elt 
          bprimeexpr leaf;

        let blocked_g = smt_build_boolexpr env 
          (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond 
            (BddapronSmt.smt_to_boolexpr env.Env.env env.Env.cond bprime)) in 
        TimeMeas.stop tm_smt_mod;
(*        Log.debug2 logger ("smt-blocked: "^(Smt.string_of_formula blocked_g)); *)
(*        nff := Smt.And (!nff,blocked_g); *)
        TimeMeas.start tm_smt_ctx;
        ctx2 := BddapronSmt.smt_assert_ctx (Some !ctx2) blocked_g;
        assertion_ids := (BddapronSmt.smt_assert_retractable_ctx ctx1 blocked_g)::!assertion_ids;
        TimeMeas.stop tm_smt_ctx;

        TimeMeas.start tm_smt;
        let m2 = BddapronSmt.smt_compute_model_ctx env.Env.env env.Env.cond !ctx2 in
        Log.debug2_o logger BddapronSmt.smt_print_result "smt-result2: " m2;
        begin match m2 with 
        | None -> sat2:= false
        | Some mm2 -> 
         begin
          m := mm2; 
          Log.debug_o logger Format.pp_print_int "micro-iteration: " !microiter;
          microiter := !microiter+1;
         end
        end;
        TimeMeas.stop tm_smt;
      end
      done;
     TimeMeas.start tm_smt_ctx;
     YicesSmt.del_ctx !ctx2;  
     TimeMeas.stop tm_smt_ctx;
    end
  end
  done;
  TimeMeas.start tm_smt_ctx;
  List.iter (fun id -> let _ = BddapronSmt.smt_retract_ctx ctx1 id in ()) 
    !assertion_ids;
  TimeMeas.stop tm_smt_ctx;
  if !found then Some !strat_elt else None

(** computes an improving strategy: version without microiterations *)
let strategy_improve_elt2 strats_table strat_table value_table env 
    template strats ff0 ctx1 strat_i s s_elt t_elt =

  TimeMeas.start tm_smt_conv;
  let smtvg0 = smt_build_gt_value_elt env t_elt s_elt in
(* Log.debug3 logger ("smt-gt_value0: "^(Smt.string_of_formula smtvg0)); *)
  TimeMeas.stop tm_smt_conv;

  TimeMeas.start tm_smt_ctx;
  let assertion_ids = ref[BddapronSmt.smt_assert_retractable_ctx ctx1 smtvg0] in
  TimeMeas.stop tm_smt_ctx;

  let sat1 = ref true in
  let found = ref false in
  let strat_elt = ref strat_i in
  while !sat1 do
  begin
    minoriter := !minoriter+1;
    Log.debug_o logger Format.pp_print_int "minor iteration: " !minoriter; 
    TimeMeas.start tm_smt;
    let m1 = BddapronSmt.smt_compute_model_ctx env.Env.env env.Env.cond ctx1 in
    Log.debug2_o logger BddapronSmt.smt_print_result "smt-result1: " m1;
    TimeMeas.stop tm_smt;

    match m1 with 
    | None -> sat1:= false
    | Some mm1 -> 
    begin
      found := true;

      TimeMeas.start tm_smt_mod;
      let mm1 = BddapronSmt.smt_convert_model env.Env.env env.Env.cond mm1 in
(*        Log.debug3_o logger (BddapronSmt.smt_print_model env.Env.env 
           env.Env.cond) "smt-model: " mm; *)
      let bpvars = List.append env.Env.bs_vars env.Env.p_vars in
(*      Log.debug3_o logger (Util.list_print (Format.pp_print_string)) "bpvars: " bpvars; *)
      let bpmodel = model_filter_vars bpvars mm1 in
(*      Log.debug2_o logger (BddapronSmt.smt_print_model env.Env.env 
          env.Env.cond) "smt-bp-model: " bpmodel; *)
      let bpexpr = BddapronSmt.model_to_boolexpr env.Env.env env.Env.cond bpmodel in
(*        Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env 
          env.Env.cond) "bp-model-boolexpr: " bpexpr; *)
      let leaf = get_strategies_leaf env strats_table strats bpexpr in
(*      Log.debug2_o logger (print_strategy_leaf env) "leaf: " leaf;*)

      let bprimevars = Util.list_inter env.Env.b_vars env.Env.primed_vars in
      let bprimemodel = model_filter_vars bprimevars mm1 in
      let bprimeexpr = BddapronSmt.model_to_boolexpr ~unprime:true env.Env.env env.Env.cond bprimemodel in
      let bisupp = BddapronUtil.supp_of_vars env.Env.env (List.append env.Env.bi_vars env.Env.p_vars) in
      let bprimeexpr = BddapronUtil.boolexpr_forget_supp bisupp bprimeexpr in
(*      Log.debug2_o logger (BddapronUtil.print_boolexpr env.Env.env 
          env.Env.cond) "b'-boolexpr: " bprimeexpr;*)

      strat_elt := update_strategy_elt env strat_table !strat_elt 
          bprimeexpr leaf;

      let blocked_g = smt_build_boolexpr env 
          (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond 
            (BddapronUtil.get_primed_boolexpr 
               (BddapronUtil.get_primed_var env.Env.env)
               env.Env.env env.Env.cond bprimeexpr)) in 
      TimeMeas.stop tm_smt_mod;
(*      Log.debug3 logger ("smt-blocked: "^(Smt.string_of_formula blocked_g));*)

      TimeMeas.start tm_smt_ctx;
      assertion_ids := (BddapronSmt.smt_assert_retractable_ctx 
          ctx1 blocked_g)::!assertion_ids;
      TimeMeas.stop tm_smt_ctx; 
    end
  end
  done;
  TimeMeas.start tm_smt_ctx;
  List.iter (fun id -> let _ = BddapronSmt.smt_retract_ctx ctx1 id in ()) !assertion_ids;
  TimeMeas.stop tm_smt_ctx;
  if !found then Some !strat_elt else None

(** computes an improving strategy: version without microiterations,
      but with generalisation *)
let strategy_improve_elt4 strats_table strat_table value_table env 
    template strats ff0 ctx1 strat_i s s_elt t_elt =

  TimeMeas.start tm_smt_conv;
  let smtvg0 = smt_build_gt_value_elt env t_elt s_elt in
(* Log.debug3 logger ("smt-gt_value0: "^(Smt.string_of_formula smtvg0)); *)
  let ff=BddapronSmt.smt_and [smtvg0;ff0] in
  TimeMeas.stop tm_smt_conv;

  TimeMeas.start tm_smt_ctx;
  let assertion_ids = ref[BddapronSmt.smt_assert_retractable_ctx ctx1 smtvg0] in
  TimeMeas.stop tm_smt_ctx;

  let sat1 = ref true in
  let found = ref false in
  let strat_elt = ref strat_i in
  while !sat1 do
  begin
    minoriter := !minoriter+1;
    Log.debug_o logger Format.pp_print_int "minor iteration: " !minoriter; 
    TimeMeas.start tm_smt;
    let m1 = BddapronSmt.smt_compute_model_ctx env.Env.env env.Env.cond ctx1 in
    Log.debug2_o logger BddapronSmt.smt_print_result "smt-result1: " m1;
    TimeMeas.stop tm_smt;

    match m1 with 
    | None -> sat1:= false
    | Some smt_mm -> 
    begin
      found := true;

      TimeMeas.start tm_smt_mod;
      let mm1 = BddapronSmt.smt_convert_model env.Env.env env.Env.cond smt_mm in
(*        Log.debug3_o logger (BddapronSmt.smt_print_model env.Env.env 
           env.Env.cond) "smt-model: " mm; *)
      let bpvars = List.append env.Env.bs_vars env.Env.p_vars in
(*      Log.debug3_o logger (Util.list_print (Format.pp_print_string)) "bpvars: " bpvars; *)
      let bpmodel = model_filter_vars bpvars mm1 in
(*      Log.debug2_o logger (BddapronSmt.smt_print_model env.Env.env 
          env.Env.cond) "smt-bp-model: " bpmodel; *)
      let bpexpr = BddapronSmt.model_to_boolexpr env.Env.env env.Env.cond bpmodel in
(*        Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env 
          env.Env.cond) "bp-model-boolexpr: " bpexpr; *)
      let leaf = get_strategies_leaf env strats_table strats bpexpr in
(*      Log.debug2_o logger (print_strategy_leaf env) "leaf: " leaf;*)

      let bpnmodel = smt_model_filter_vars (List.append env.Env.n_vars (BddapronSmt.vars_to_smtvars env.Env.env env.Env.cond bpvars)) smt_mm in
      let bprime = YicesSmt.instantiate_formula bpnmodel ff in
(*         Log.debug3 logger ("smt-bprime: "^(Smt.string_of_formula bprime));*)

      let bprimeexpr = BddapronSmt.smt_to_boolexpr ~unprime:true env.Env.env 
          env.Env.cond bprime in  
      let bisupp = BddapronUtil.supp_of_vars env.Env.env (List.append env.Env.bi_vars env.Env.p_vars) in
      let bprimeexpr = BddapronUtil.boolexpr_forget_supp bisupp bprimeexpr in
(*      Log.debug2_o logger (BddapronUtil.print_boolexpr env.Env.env 
          env.Env.cond) "b'-boolexpr: " bprimeexpr;*)

      strat_elt := update_strategy_elt env strat_table !strat_elt 
          bprimeexpr leaf;

      let blocked_g = smt_build_boolexpr env 
          (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond 
            (BddapronUtil.get_primed_boolexpr 
               (BddapronUtil.get_primed_var env.Env.env)
               env.Env.env env.Env.cond bprimeexpr)) in 
      TimeMeas.stop tm_smt_mod;
(*      Log.debug3 logger ("smt-blocked: "^(Smt.string_of_formula blocked_g));*)

      TimeMeas.start tm_smt_ctx;
      assertion_ids := (BddapronSmt.smt_assert_retractable_ctx 
          ctx1 blocked_g)::!assertion_ids;
      TimeMeas.stop tm_smt_ctx; 
    end
  end
  done;
  TimeMeas.start tm_smt_ctx;
  List.iter (fun id -> let _ = BddapronSmt.smt_retract_ctx ctx1 id in ()) !assertion_ids;
  TimeMeas.stop tm_smt_ctx;
  if !found then Some !strat_elt else None

(* strategy improvement *)
let strategy_improve param strats_table strat_table value_table env template bequs assertion strats strat s =
  TimeMeas.start tm_imp;
  TimeMeas.start tm_smt_conv;
  (*  simplify strategies by currently reachable boolean states *)
  let binv = compute_bool_inv env s in
  let bequs = BddapronUtil.simplify_equs env.Env.env env.Env.cond bequs binv in
  let strats = simplify_strats strats binv in 

  (* build SMT formulas *)
  let smtstrats = smt_build_strats env strats in
  let smtbequs = smt_build_bequs env bequs in
  let benum_careset = benum_careset env in
  let smtenum = smt_build_boolexpr env benum_careset in
  let smtv0 = smt_build_value env template s in
(* Log.debug3 logger ("smt-strats: "^(Smt.string_of_formula smtstrats)); *)
(* Log.debug3 logger ("smt-bequs: "^(Smt.string_of_formula smtbequs)); *)
(* Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
(* Log.debug3 logger ("smt-value0: "^(Smt.string_of_formula smtv0)); *)
        "benum-careset: " benum_careset; *)
  let (strategy_improve_elt,ff0) = match param with
    |MicroIter -> (strategy_improve_elt,
                   BddapronSmt.smt_and [smtstrats;smtbequs;smtenum;smtv0])
    |NoMicroGenIter -> (strategy_improve_elt4,
                   BddapronSmt.smt_and [smtstrats;smtbequs;smtenum;smtv0])
    |NoMicroIter -> (strategy_improve_elt2,YicesSmt.F)
    |_ -> assert false
  in
  TimeMeas.stop tm_smt_conv;

  TimeMeas.start tm_smt_ctx;
  let ctx1 = BddapronSmt.smt_assert_ctx None smtv0 in 
  let ctx1 = BddapronSmt.smt_assert_ctx (Some ctx1) smtenum in
  let ctx1 = BddapronSmt.smt_assert_ctx (Some ctx1) smtbequs in
  let ctx1 = BddapronSmt.smt_assert_ctx (Some ctx1) smtstrats in
  TimeMeas.stop tm_smt_ctx;

  let updated = ref(false) in
  List.iteri (fun i t_elt ->
      match strategy_improve_elt strats_table strat_table value_table 
        env template strats ff0 ctx1 strat.(i) s s.(i) t_elt with
      |Some newstrat_elt -> 
(*         Log.debug2_o logger (print_strategy_elt env) "strategy-elt: " 
           (t_elt,newstrat_elt);*)
         strat.(i) <- newstrat_elt; 
         updated := true
      |None -> ())
    template;
  TimeMeas.start tm_smt_ctx;
  YicesSmt.del_ctx ctx1; 
  TimeMeas.stop tm_smt_ctx;
  TimeMeas.stop tm_imp;
  if !updated then Some strat else None

(* strategy improvement, all-symbolic version *)
(*let blocked = ref(Smt.T)*)
let strategy_improve2 strats_table strat_table value_table env template bequs assertion strats strat s =
  TimeMeas.start tm_imp;
  TimeMeas.start tm_smt_conv;
  let binv = compute_bool_inv env s in
  let bequs = BddapronUtil.simplify_equs env.Env.env env.Env.cond bequs binv in
  let strats = simplify_strats strats binv in 

  let smtstrats = smt_build_strats env strats in
  let smtbequs = smt_build_bequs env bequs in
  let benum_careset = benum_careset env in
  let smtenum = smt_build_boolexpr env benum_careset in
  let smtv0 = smt_build_value env template s in
  (* build disjunction of gt values *)

(*
  (* boolean encoding of template rows *)
  let tplt_map = BddapronUtil.generate_boolencoding env.Env.env env.Env.cond 
    env.Env.tplt_vars (List.length template) in
  let tplt_map = Array.of_list tplt_map in
  let smtvg0 = Util.list_fold_lefti (fun i f t_elt -> 
        let smtvg = smt_build_gt_value_elt env t_elt s.(i) in
        Smt.Or (f,Smt.And (smt_build_boolexpr env tplt_map.(i),smtvg)))
      Smt.F template
  in
*)

  let smtvg0 = Util.list_fold_lefti (fun i f t_elt -> 
        let smtvg = smt_build_gt_value_elt env t_elt s.(i) in
        YicesSmt.Or (f,YicesSmt.And 
          (YicesSmt.Eq (YicesSmt.Real template_row_var,YicesSmt.Const 
               {YicesSmt.num=Big_int.big_int_of_int i; 
                YicesSmt.den=Big_int.big_int_of_int 1}),
             smtvg)))
      YicesSmt.F template
  in
  TimeMeas.stop tm_smt_conv;

  TimeMeas.start tm_smt_ctx;
(*  let ctx1 = ref (BddapronSmt.smt_assert_ctx None !blocked) in*)
  let ctx1 = ref (BddapronSmt.smt_assert_ctx None smtv0) in
  ctx1 := BddapronSmt.smt_assert_ctx (Some !ctx1) smtvg0;
  ctx1 := BddapronSmt.smt_assert_ctx (Some !ctx1) smtenum;
  ctx1 := BddapronSmt.smt_assert_ctx (Some !ctx1) smtbequs;
  ctx1 := BddapronSmt.smt_assert_ctx (Some !ctx1) smtstrats;
  TimeMeas.stop tm_smt_ctx;

  let updated = ref(false) in
  let sat = ref(true) in
  while !sat do
  begin
    minoriter := !minoriter+1;
    Log.debug_o logger Format.pp_print_int "minor iteration: " !minoriter; 

    TimeMeas.start tm_smt;
    let m = BddapronSmt.smt_compute_model_ctx env.Env.env env.Env.cond !ctx1 in
    Log.debug_o logger BddapronSmt.smt_print_result "smt-result: " m;
    TimeMeas.stop tm_smt;

    match m with 
    | None -> sat:= false
    | Some mm1 -> 
    begin
      updated := true;

      TimeMeas.start tm_smt_mod;
      let mm = BddapronSmt.smt_convert_model env.Env.env env.Env.cond mm1 in
(*        Log.debug3_o logger (BddapronSmt.smt_print_model env.Env.env 
           env.Env.cond) "smt-model: " mm; *)

(*
      (* boolean encoding of template rows *)
      let tplt_model = model_filter_vars env.Env.tplt_vars mm in
      let btplt = BddapronSmt.model_to_boolexpr env.Env.env env.Env.cond 
        tplt_model in
      let i = Util.array_get_index_of btplt tplt_map in
*)

      let i = match Hashtbl.find mm1 template_row_var with
	  |YicesSmt.VRatio v -> Big_int.int_of_big_int v.YicesSmt.num
	  |_ -> assert false
      in
      Log.debug_o logger Format.pp_print_int "improved template row: " i; 

      let bpvars = List.append env.Env.bs_vars env.Env.p_vars in
(*      Log.debug3_o logger (Util.list_print (Format.pp_print_string)) "bpvars: " bpvars; *)
      let bpmodel = model_filter_vars bpvars mm in
(*      Log.debug2_o logger (BddapronSmt.smt_print_model env.Env.env 
          env.Env.cond) "smt-bp-model: " bpmodel; *)
      let bpexpr = BddapronSmt.model_to_boolexpr env.Env.env env.Env.cond bpmodel in
      let leaf = get_strategies_leaf env strats_table strats bpexpr in
(*      Log.debug2_o logger (print_strategy_leaf env) "leaf: " leaf;*)

      let bprimevars = Util.list_inter env.Env.b_vars env.Env.primed_vars in
      let bprimemodel = model_filter_vars bprimevars mm in
      let bprimeexpr = BddapronSmt.model_to_boolexpr ~unprime:true env.Env.env env.Env.cond bprimemodel in
      let bisupp = BddapronUtil.supp_of_vars env.Env.env (List.append env.Env.bi_vars env.Env.p_vars) in
      let bprimeexpr = BddapronUtil.boolexpr_forget_supp bisupp bprimeexpr in
(*      Log.debug2_o logger (BddapronUtil.print_boolexpr env.Env.env 
          env.Env.cond) "b'-boolexpr: " bprimeexpr;*)

      strat.(i) <- update_strategy_elt env strat_table strat.(i) 
          bprimeexpr leaf;

(*
(* boolean encoding of template rows *)
      let blocked_g = smt_build_boolexpr env 
          (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond 
            (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
               (BddapronUtil.get_primed_boolexpr 
                 (BddapronUtil.get_primed_var env.Env.env)
                 env.Env.env env.Env.cond bprimeexpr)
               btplt))
      in 
*)

      let blocked_g = YicesSmt.Or (smt_build_boolexpr env 
          (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond
            (BddapronUtil.get_primed_boolexpr 
                 (BddapronUtil.get_primed_var env.Env.env)
                 env.Env.env env.Env.cond bprimeexpr)),
          YicesSmt.Not(YicesSmt.Eq (YicesSmt.Real template_row_var,YicesSmt.Const 
               {YicesSmt.num=Big_int.big_int_of_int i; 
                YicesSmt.den=Big_int.big_int_of_int 1}))) 
      in 
(* Remark: globally memorising visited strategies just slows down *)
(*      let blocked_g2 = Smt.Or (smt_build_boolexpr env 
          (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond
           (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
            (BddapronUtil.get_primed_boolexpr 
                 (BddapronUtil.get_primed_var env.Env.env)
                 env.Env.env env.Env.cond bprimeexpr)
            bpexpr)),
          Smt.Not(Smt.Eq (Smt.Real template_row_var,Smt.Const 
               {Smt.num=Big_int.big_int_of_int i; 
                Smt.den=Big_int.big_int_of_int 1}))) 
      in 
      blocked := Smt.And (!blocked,blocked_g2); *)
      TimeMeas.stop tm_smt_mod;

      TimeMeas.start tm_smt_ctx;
      ctx1 := BddapronSmt.smt_assert_ctx (Some !ctx1) blocked_g;
      TimeMeas.stop tm_smt_ctx;
    end
  end
  done; 
  TimeMeas.start tm_smt_ctx;
  YicesSmt.del_ctx !ctx1; 
  TimeMeas.stop tm_smt_ctx;
  TimeMeas.stop tm_imp;
  if !updated then Some strat else None


(** computes an improving strategy: version without microiterations
     and with checking whether a strategy for i-1 improves i *)
let strategy_improve_elt3 models strats_table strat_table value_table env 
    template strats ctx1 strat_i s s_elt t_elt =

  TimeMeas.start tm_smt_conv;
  let smtvg0 = smt_build_gt_value_elt env t_elt s_elt in
(*  Log.debug3 logger ("smt-gt_value0: "^(Smt.string_of_formula smtvg0)); *)
(*  let yvg0 = Smt.formula_to_yexpr ctx1 smtvg0 in *)
  TimeMeas.stop tm_smt_conv;

  TimeMeas.start tm_smt_ctx;
(*  let assertion_ids = ref[Smt.assert_retractable_ctx_yexpr ctx1 yvg0] in*)
  let assertion_ids = ref[BddapronSmt.smt_assert_retractable_ctx ctx1 smtvg0] in
  TimeMeas.stop tm_smt_ctx;

  let mmms = ref(!models) in
  let sat1 = ref true in
  let found = ref false in
  let strat_elt = ref strat_i in
  while !sat1 do
  begin
    minoriter := !minoriter+1;
    Log.debug_o logger Format.pp_print_int "minor iteration: " !minoriter; 
    Log.debug_o logger Format.pp_print_int "number of old models to check: " (List.length !mmms); 
    Log.debug_o logger Format.pp_print_int "total number of models: " (List.length !models); 
   
    TimeMeas.start tm_smt;
    let (check_model,m1) = match !mmms with
      |model::tl -> 
(*        let m = BddapronSmt.smt_check_model2 ctx1 ymodel yvg0 in
        Log.debug3 logger "ymodel: "; Yices.display_model ymodel; 
        Format.pp_print_newline Format.std_formatter (); *)
        let m = BddapronSmt.smt_check_model ctx1 model smtvg0 in 
(*        let m = BddapronSmt.smt_check_model2 ctx1 model 
          (Smt.formula_to_yexpr ctx1 smtvg0) in *)
(*        let m = Smt.compute_model (Smt.And ((smt_model_to_smt_boolexpr model),smtvg0)) in*)
        mmms := tl; (true,m)
      |[] -> (false,
              BddapronSmt.smt_compute_model_ctx env.Env.env env.Env.cond ctx1)
    in       
    Log.debug_o logger BddapronSmt.smt_print_result "smt-result1: " m1;
    TimeMeas.stop tm_smt;

    match m1 with 
    | None -> if not check_model then sat1:= false
    | Some mm1 -> 
    begin
      found := true;

      (* add model to list *)
(*      if not check_model then models := (Smt.get_ymodel ctx1)::!models; *)
      if not check_model then models := mm1::!models
      else tryimpsucc := !tryimpsucc+1;
(*      else Log.info logger "improved strategy reapplied"; *)

      TimeMeas.start tm_smt_mod;
(*      Log.debug3_o logger (Util.hashtbl_print Format.pp_print_string
        (fun fmt x -> Format.pp_print_string fmt (Smt.string_of_value x)))
        "smt-model: " mm1; *)
      let mm1 = BddapronSmt.smt_convert_model env.Env.env env.Env.cond mm1 in
(*        Log.debug3_o logger (BddapronSmt.smt_print_model env.Env.env 
           env.Env.cond) "smt-model: " mm1; *)
      let bpvars = List.append env.Env.bs_vars env.Env.p_vars in
      Log.debug3_o logger (Util.list_print (Format.pp_print_string)) "bpvars: " bpvars; 
      let bpmodel = model_filter_vars bpvars mm1 in
      Log.debug2_o logger (BddapronSmt.smt_print_model env.Env.env 
          env.Env.cond) "smt-bp-model: " bpmodel; 
      let bpexpr = BddapronSmt.model_to_boolexpr env.Env.env env.Env.cond bpmodel in
(*        Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env 
          env.Env.cond) "bp-model-boolexpr: " bpexpr; *)
      let leaf = get_strategies_leaf env strats_table strats bpexpr in
      Log.debug2_o logger (print_strategy_leaf env) "leaf: " leaf;

      let bprimevars = Util.list_inter env.Env.b_vars env.Env.primed_vars in
      let bprimemodel = model_filter_vars bprimevars mm1 in
      let bprimeexpr = BddapronSmt.model_to_boolexpr ~unprime:true env.Env.env env.Env.cond bprimemodel in
      let bisupp = BddapronUtil.supp_of_vars env.Env.env (List.append env.Env.bi_vars env.Env.p_vars) in
      let bprimeexpr = BddapronUtil.boolexpr_forget_supp bisupp bprimeexpr in
      Log.debug2_o logger (BddapronUtil.print_boolexpr env.Env.env 
          env.Env.cond) "b'-boolexpr: " bprimeexpr;

      strat_elt := update_strategy_elt env strat_table !strat_elt 
          bprimeexpr leaf;

      let blocked_g = smt_build_boolexpr env 
          (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond 
            (BddapronUtil.get_primed_boolexpr 
               (BddapronUtil.get_primed_var env.Env.env)
               env.Env.env env.Env.cond bprimeexpr)) in 
      TimeMeas.stop tm_smt_mod;
(*      Log.debug3 logger ("smt-blocked: "^(Smt.string_of_formula blocked_g));*)

      TimeMeas.start tm_smt_ctx;
      assertion_ids := (BddapronSmt.smt_assert_retractable_ctx 
          ctx1 blocked_g)::!assertion_ids;
      TimeMeas.stop tm_smt_ctx;
    end
  end
  done;
  TimeMeas.start tm_smt_ctx;
  List.iter (fun id -> let _ = BddapronSmt.smt_retract_ctx ctx1 id in ()) !assertion_ids;
  TimeMeas.stop tm_smt_ctx;
  if !found then Some !strat_elt else None

(* strategy improvement *)
let strategy_improve3 strats_table strat_table value_table env template bequs assertion strats strat s =
  TimeMeas.start tm_imp;
  TimeMeas.start tm_smt_conv;
  (*  simplify strategies by currently reachable boolean states *)
  let binv = compute_bool_inv env s in
  let bequs = BddapronUtil.simplify_equs env.Env.env env.Env.cond bequs binv in
  let strats = simplify_strats strats binv in 

  (* build SMT formulas *)
  let smtstrats = smt_build_strats env strats in
  let smtbequs = smt_build_bequs env bequs in
  let benum_careset = benum_careset env in
  let smtenum = smt_build_boolexpr env benum_careset in
  let smtv0 = smt_build_value env template s in
(* Log.debug3 logger ("smt-strats: "^(Smt.string_of_formula smtstrats)); *)
(* Log.debug3 logger ("smt-bequs: "^(Smt.string_of_formula smtbequs)); *)
(* Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
(* Log.debug3 logger ("smt-value0: "^(Smt.string_of_formula smtv0)); *)
        "benum-careset: " benum_careset; *)
  TimeMeas.stop tm_smt_conv;

  TimeMeas.start tm_smt_ctx;
  let ctx1 = BddapronSmt.smt_assert_ctx None smtstrats in 
  let ctx1 = BddapronSmt.smt_assert_ctx (Some ctx1) smtbequs in
  let ctx1 = BddapronSmt.smt_assert_ctx (Some ctx1) smtenum in
  let ctx1 = BddapronSmt.smt_assert_ctx (Some ctx1) smtv0 in
  TimeMeas.stop tm_smt_ctx;

  let models = ref [] in 
  let updated = ref false in
  List.iteri (fun i t_elt ->
      match strategy_improve_elt3 models strats_table strat_table value_table 
        env template strats ctx1 strat.(i) s s.(i) t_elt with
      |Some newstrat_elt -> 
(*         Log.debug2_o logger (print_strategy_elt env) "strategy-elt: " 
           (t_elt,newstrat_elt); *)
         strat.(i) <- newstrat_elt; 
         updated := true
      |None -> ())
    template;
  TimeMeas.start tm_smt_ctx;
  YicesSmt.del_ctx ctx1; 
  TimeMeas.stop tm_smt_ctx;
  TimeMeas.stop tm_imp;
  if !updated then Some strat else None

(******************************************************************************)
(* helpers for strategy CFG *)
(******************************************************************************)

let stratcfg_compare = 
  let hcompare = {Hashhe.hash = Hashtbl.hash; Hashhe.equal = (==); } in
  {
    SHGraph.hashv = hcompare; 
    SHGraph.hashh = hcompare;
    SHGraph.comparev = (-);
    SHGraph.compareh = (-);
  }

let stratcfg_make () = PSHGraph.create stratcfg_compare 0 {locidx = 1; arcidx = 1}

(* returns the next available location identifier *)
let stratcfg_next_locid cfg =
  let info = (PSHGraph.info cfg) in
  let id = info.locidx in
  info.locidx <- info.locidx + 1;
  (-id)
(* returns the next available arc identifier *)
let stratcfg_next_arcid cfg =
  let info = (PSHGraph.info cfg) in
  let id = info.arcidx in
  info.arcidx <- info.arcidx + 1;
  id

(* returns the set of locations intersecting with expr *)
let stratcfg_get_locidset_by_inv env cfg expr =
  PSHGraph.fold_vertex cfg 
    (fun locid inv ~pred:_ ~succ:_ locs ->
      if not (Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond
        (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond inv expr)) then
        locid::locs
      else locs)
   []

(* splits a location and returns the location corresponding to expr *)
let stratcfg_split_loc env cfg locid inv1 inv2 =
(*  Log.debug3_o logger Format.pp_print_int "split locid = " locid; *)

  (* add new locations *)
  let loc1 = stratcfg_next_locid cfg in
  PSHGraph.add_vertex cfg loc1 inv1;
  let loc2 = stratcfg_next_locid cfg in
  PSHGraph.add_vertex cfg loc2 inv2;
(*  Log.debug3_o logger Format.pp_print_int "add loc: " loc1;
  Log.debug3_o logger Format.pp_print_int "add loc: " loc2; *)

  (* for all predecessors p of loc 
       (includes self-loops and edges between l1 and l2):
       - add edges from p to l1 and l2
  *)
  PSette.iter 
    (fun pe -> 
      let parr = PSHGraph.predvertex cfg pe in
      assert((Array.length parr)=1);
      if parr.(0)<>locid then
      begin
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg pe) parr [|loc1|]; 
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg pe) parr [|loc2|]; 
      end 
      else
      begin
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg pe) [|loc1|] [|loc2|]; 
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg pe) [|loc2|] [|loc1|]; 
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg pe) [|loc1|] [|loc1|]; 
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg pe) [|loc2|] [|loc2|]; 
      end)
   (PSHGraph.predhedge cfg locid);

  (* remove all edges to loc *)
  PSette.iter 
    (fun e -> PSHGraph.remove_hedge cfg e) 
    (PSHGraph.predhedge cfg locid);

  (* for all successors s of loc:
       - add edges from l1 to s, l2 to s
  *)
  PSette.iter 
    (fun se -> 
      let sarr = PSHGraph.succvertex cfg se in
      assert((Array.length sarr)=1);
      if sarr.(0)<>locid then
      begin
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg se) [|loc1|] sarr; 
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg se) [|loc2|] sarr; 
      end 
    )
    (PSHGraph.succhedge cfg locid);

  (* remove loc *)
  PSHGraph.remove_vertex cfg locid;
  ()
 
let rec stratcfg_split_locs env cfg b =
  match stratcfg_get_locidset_by_inv env cfg b with
  |[] -> 
    let locid = stratcfg_next_locid cfg in
    PSHGraph.add_vertex cfg locid b;
    Log.debug3_o logger Format.pp_print_int "add loc: " locid
  |locid::_ -> 
  begin
    let inv = PSHGraph.attrvertex cfg locid in
    (* can produce 3 locations *)
    let inv1 = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond inv b in
    let inv2 = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond inv 
      (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond b) in
    let inv3 = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond b 
      (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond inv) in
    Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
      "expr = " b;
    Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
      "inv = " inv;
    Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
      "inv1 = " inv1;
    Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
      "inv2 = " inv2;
    Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
      "inv3 = " inv3;
 
    if(not((Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond inv1) or 
         (Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond inv2))) then
      stratcfg_split_loc env cfg locid inv1 inv2;
    if(not(Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond inv3)) then
      stratcfg_split_locs env cfg inv3
   end

let print_arc env fmt (i,g,equs) = 
  Format.pp_print_string fmt ("["^(string_of_int i)^"]: ");
  print_strategies_leaf env fmt (g,equs)

(* adds a transition from bsrc to btgt (splits locations) *)
let stratcfg_add_arc env cfg bsrc btgt arc =
  Log.debug3_o logger (print_arc env) "arc: " arc;
  stratcfg_split_locs env cfg bsrc;
  stratcfg_split_locs env cfg btgt;
  let srcids = stratcfg_get_locidset_by_inv env cfg bsrc in
  let tgtids = stratcfg_get_locidset_by_inv env cfg btgt in
  Log.debug3_o logger (Util.list_print Format.pp_print_int) "srcids: " srcids;
  Log.debug3_o logger (Util.list_print Format.pp_print_int) "tgtids: " tgtids;
  List.iter (fun srcid ->
    List.iter (fun tgtid -> 
      if srcid<>tgtid or (List.length srcids)=1 then
      begin
        let (i,_,_) = arc in Log.debug3_o logger Format.pp_print_int "add arc for i = " i;
        let arcid = stratcfg_next_arcid cfg in
        Log.debug3 logger ("add arc("^(string_of_int arcid)^"): "^(string_of_int srcid)^" --> "^(string_of_int tgtid));
        PSHGraph.add_hedge cfg arcid arc 
                 [|srcid|] [|tgtid|]
      end) tgtids)
    srcids 

let stratcfg_print_dot env fmt cfg print_arcs =
  let print_loc env fmt locid inv =
    let strfmt = Format.str_formatter in
    Format.pp_print_string strfmt ("("^(string_of_int locid)^") ");
    BddapronUtil.print_boolexpr env.Env.env env.Env.cond strfmt inv;
    let str = Format.flush_str_formatter () in
    Format.pp_print_string fmt (Util.string_compact str)
  in
  let print_arc env fmt arcid arc =
    let strfmt = Format.str_formatter in
    Format.pp_print_string strfmt ("("^(string_of_int arcid)^") ");
    Format.pp_print_string strfmt "\\n";
    print_arc env strfmt arc;
    let str = Format.flush_str_formatter () in
    Format.pp_print_string fmt (Util.string_compact str)
  in
  PSHGraph.print_dot
    ~style:"size=\"7.5,10\";center=true;ranksep=0.1;nodesep=0.1;"
    ~hedgestyle:"shape=plaintext,fontsize=10,height=0.01,width=0.01"
    ~vertexstyle:"shape=box,fontsize=10,height=0.01,width=0.01"
    (Format.pp_print_int)
    (Format.pp_print_int)
    (fun fmt locid loc -> print_loc env fmt locid loc)
    (fun fmt arcid arc -> if print_arcs then print_arc env fmt arcid arc 
      else Format.pp_print_string fmt ("("^(string_of_int arcid)^") "))
    fmt cfg

(******************************************************************************)
(* compute strategy value *)
(******************************************************************************)

let update_value_elt env value_table mtbdd boolexpr leaf =
  Cudd.Mtbdd.ite boolexpr 
    (Cudd.Mtbdd.cst env.Env.cuddman value_table leaf) mtbdd

let add_suffix s1 s2 v = v^"_"^(string_of_int (-s1))^"_"^(string_of_int (s2))
let add_suffix_primed s1 s2 v = v^"_"^(string_of_int (-s1))^"_"^(string_of_int (s2))^"'"

let add_suffix2 s1 s2 s3 v = v^"_"^(string_of_int (-s1))^"_"^(string_of_int (-s2))^"_"^(string_of_int (s3))
let add_suffix_primed2 s1 s2 s3 v = v^"_"^(string_of_int (-s1))^"_"^(string_of_int (-s2))^"_"^(string_of_int (s3))^"'"

let strat_to_lp_conss env doman initial template assertion strat s = 
  let boolinputsupp = BddapronUtil.supp_of_vars env.Env.env (List.append env.Env.bi_vars env.Env.p_vars) in
(* compute Boolean source states of current system *)
(*  let inv = Array.fold_left 
      (fun inv strat_elt  -> 
        let bbga = Cudd.Mtbdd.guardleafs strat_elt in
        Array.fold_left (fun inv (b,(bg,_,_)) -> 
           if not(Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond bg) then
             Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond inv b
           else inv)
         inv bbga)
      (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond) strat
  in *) 
  let srcinv = compute_bool_inv env s  in
(*  Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
    "SRC-INV = " srcinv;*)
(* partition according to intersection of source an target booleans *)
  let stratcfg = stratcfg_make () in   
  Array.iteri 
    (fun i strat_elt -> 
      let bbga = Cudd.Mtbdd.guardleafs strat_elt in
      Array.iter (fun (btgt,(bsrc,g,equs)) -> 
        (* remove initial states as target *)
        let btgt = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond btgt
          (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond initial) in
        let bsrc = BddapronUtil.boolexpr_forget_supp boolinputsupp bsrc in
        let bsrc = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond 
          srcinv bsrc in
        if not(Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond bsrc) &&
           not(Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond btgt) then
        begin
          let gass = Bddapron.Expr0.Bool.tdrestrict assertion bsrc in
          let gass = BddapronUtil.boolexpr_to_linconss 
            env.Env.env env.Env.cond doman env.Env.b_vars gass in
          let g = ApronUtil.linconss_append g gass in
          stratcfg_add_arc env stratcfg bsrc btgt (i,g,equs)
        end)
        bbga)
    strat;
  (* remove unreachable locations *)
  PSHGraph.iter_vertex stratcfg
    (fun locid inv ~pred ~succ:_ -> 
      if (PSette.cardinal pred)=0 && 
         (Apron.Lincons1.is_unsat (get_value_leaf env s.(0) inv)) then
        begin
(*          Log.debug_o logger Format.pp_print_int "remove loc " locid;*)
          PSHGraph.remove_vertex stratcfg locid;
        end);

(*  let out_channel = open_out ("t"^(string_of_int !iterations)^".dot") in
  let dotfmt = Format.formatter_of_out_channel out_channel in
  stratcfg_print_dot env dotfmt stratcfg true;
  close_out out_channel;
*)

  (* build constraints *)
  let tconss = Array.map 
        (fun c -> 
          let v = Apron.Var.of_string boundvar_prefix in
          (v,c)) 
        (Array.of_list template)
  in
  let (f,vars) = PSHGraph.fold_hedge stratcfg
    (fun _ (i,g,equs) ~pred ~succ (f,vars) ->
      assert((Array.length pred)=1);
      assert((Array.length succ)=1);
      let ftr = List.append (ApronLp.make_conss_conss (* transition *)
              ~rename:(add_suffix2 pred.(0) succ.(0) i) g)
            (List.map (ApronLp.make_conss_equ 
                      ~rename_lhs:(add_suffix_primed2 pred.(0) succ.(0) i) 
                      ~rename_rhs:(add_suffix2 pred.(0) succ.(0) i))
                  (Array.to_list equs))
      in
      let ftrl = List.append (List.mapi (fun j _ -> 
             ApronLp.make_conss_lequ (* source location constraints *)
                      ~rename_lhs:(add_suffix pred.(0) j) 
                      ~rename_rhs:(add_suffix2 pred.(0) succ.(0) i) 
                      tconss.(j))
             template) ftr
      in
      let tcons = tconss.(i) in
      let (v,_) = tcons in
      let svars = (*  bound variables *)
        let sv = Apron.Var.of_string (add_suffix pred.(0) i 
                                      (Apron.Var.to_string v)) in
        if not(Sette.mem sv vars) then Sette.singleton sv else Sette.empty
      in
      let f = List.concat 
         [ftrl;
          [ApronLp.make_conss_gequ (* target location constraints *)
                      ~rename_lhs:(add_suffix succ.(0) i) 
                      ~rename_rhs:(add_suffix_primed2  pred.(0) succ.(0) i) 
                      tcons];
         f] in
      (f,Sette.union (Sette.add (Apron.Var.of_string (add_suffix succ.(0) i 
                                      (Apron.Var.to_string v))) svars) vars))
    ([],Sette.empty)
  in
  let f = Util.array_fold_lefti (* old value constraints *)
    (fun i f s_elt -> 
      Array.fold_left (fun f (b,cons) -> 
        if (ApronUtil.lincons_is_false cons) or 
           (ApronUtil.lincons_is_true cons) then f 
        else
        begin
          let locids = stratcfg_get_locidset_by_inv env stratcfg b in
          List.fold_left (fun f locid -> 
            let bound = Apron.Coeff.neg (Apron.Lincons1.get_cst cons) in
            let expr =  Apron.Linexpr1.make env.Env.apronenv in
            Apron.Linexpr1.set_cst expr bound;
            (ApronLp.make_conss_lequ 
                   ~rename_lhs:(add_suffix locid i) 
                   (Apron.Var.of_string boundvar_prefix,expr))::f)
            f locids
        end)
      f (Cudd.Mtbdd.guardleafs s_elt))
    f s
  in
  (f,Array.of_list (Sette.elements vars),stratcfg) 

(** computes the value of strategy strat *)
let compute_strategy_value env doman value_table template initial assertion strat s =

  TimeMeas.start tm_val;
  TimeMeas.start tm_lp_conv;
  let (lp_constrs,vars,stratcfg) = 
    strat_to_lp_conss env doman initial template assertion strat s in
  TimeMeas.stop tm_lp_conv;

(*  Log.debug2_o logger ApronLp.print_lp_conss "conss: " lp_constrs; *)
  (* list of variables that are trivially infinite (due to value, strat rhs) *)
(*  Log.debug3_o logger (ApronUtil.print_vars) "vars: " vars; *)
(*  let unbounded_vars = [||]  in
  Log.debug3_o logger (ApronUtil.print_vars) "unbounded-vars: " unbounded_vars;
*)
  (* test variables whether unbounded *)
  TimeMeas.start tm_lp;
  let bounded_vars = ApronLp.get_bounded_vars lp_constrs vars in
  TimeMeas.stop tm_lp;
  Log.debug2_o logger (ApronUtil.print_vars) "bounded-vars: " bounded_vars;

  (* solve for bounded variables *)
  let obj_fct = ApronLp.make_obj_fct_max_negsum_vars bounded_vars in
  TimeMeas.start tm_lp;
  let (obj_val, sol) = ApronLp.solve lp_constrs obj_fct in 
  TimeMeas.stop tm_lp;
  Log.debug2_o logger Apron.Coeff.print "obj_val: " obj_val;
(*  Log.debug2_o logger (ApronLp.print_lp_sol bounded_vars) "solution: " sol;*)


(*  assert (not ((ApronUtil.coeff_is_infty obj_val) or ((ApronUtil.coeff_is_neginfty obj_val))));*)
  (* update for each bound -> boolexpr from strat -> value *)
  PSHGraph.iter_vertex stratcfg
    (fun locid b ~pred:_ ~succ:_ ->
      Log.debug2_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
         "b = " b;
      List.iteri
        (fun i t_elt -> 
          let v = (Apron.Var.of_string 
                        (add_suffix locid i boundvar_prefix)) in
          let x = 
            if(Util.array_mem v bounded_vars) then sol v 
            else Apron.Coeff.Scalar (Apron.Scalar.of_infty (-1))
          in
          Log.debug2_o logger Apron.Coeff.print "val = " x;
          let cons = Template.cons_instantiate env t_elt x in
          s.(i) <- update_value_elt env value_table s.(i) b cons)
        template);
   TimeMeas.stop tm_val;
   s

(** converts a value MTBDD array into a BDDAPRON abstract value   *)
let values_to_abstract env doman values = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  Array.fold_left (fun s s_elt ->
      Bddapron.Domain0.meet doman s 
        (Bddapron.Domain0.of_bddapron doman env.Env.env 
          (Array.to_list (Array.map 
             (fun (b,n) -> 
               let narr = Apron.Lincons1.array_make env.Env.apronenv 1 in
               Apron.Lincons1.array_set narr 0 n;
               (b,Apron.Abstract1.abstract0 (Apron.Abstract1.of_lincons_array 
                    apronman env.Env.apronenv narr)))
             (Cudd.Mtbdd.guardleafs s_elt))))
    )
    (Bddapron.Domain0.top doman env.Env.env) values

(******************************************************************************)
(* main strategy improvement loop *)
(******************************************************************************)

(** the main strategy improvement loop *)
let analyze_pow param template env doman cfprog =
  let equs = cfprog.Program.c_disc_equs in
  let bequs = Env.get_bool_equs env equs in
  let strats_table = make_strats_table () in
  let strat_table = make_strat_table () in
  let value_table = make_value_table () in

  TimeMeas.start tm_strats;
  let strats = build_strategies strats_table env doman cfprog.Program.c_ass equs in
  Log.debug_o logger (print_strategies env) "strategies: " strats;
  let (strat0,s0) = initial_strategy_value env template value_table strat_table strats cfprog.Program.c_init in
(*  Env.cudd_reorder env; *)
  TimeMeas.stop tm_strats;

  let strat = ref(strat0) in
  let s = ref(s0) in
  let stable = ref(false) in
  Log.debug_o logger (print_values env template) "value: " !s;
  while not !stable do
  begin
    iterations := !iterations +1;
    Log.debug_o logger Format.pp_print_int "major iteration: " !iterations; 
    Env.cudd_reorder env;
    let strategy_improve = match param with
      |SymbRows -> strategy_improve2
      |TryImpStrat -> strategy_improve3
      |_ -> strategy_improve param
    in
    match strategy_improve strats_table strat_table value_table env template 
      bequs cfprog.Program.c_ass strats !strat !s with
    |Some newstrat ->
      strat := newstrat;
      Log.debug_o logger (print_strategy env template) "strategy: " !strat;
      s := compute_strategy_value env doman value_table template
             cfprog.Program.c_init cfprog.Program.c_ass
             !strat !s; 
      Log.debug_o logger (print_values env template) "value: " !s;
    |None -> stable := true
  end
  done;
  !s

(******************************************************************************)
(** {2 module PowLognum: logico-numerical numerical max-strategy iteration with power domain} *)
(******************************************************************************)
module PowLognum(Dom :  Template.TEMPLATE_T) =
struct
type analysisparam_t = powlognum_param_t

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
let analyze (param:powlognum_param_t) env cf =
  let (doman,template) = Dom.doman () in (* here we get the template *)

  Log.info_o logger (Format.pp_print_int) "Template lines:  " (List.length template);

  let values = analyze_pow param template env doman cf in

  (* print statistics *)
  Log.info_o logger (Format.pp_print_int) "Major iterations:  " !iterations;
  Log.info_o logger (Format.pp_print_int) "Minor iterations:  " !minoriter;
  Log.info_o logger (Format.pp_print_int) "Micro-iterations:  " !microiter;
  Log.info_o logger (Format.pp_print_int) "Imp. strat. reapp: " !tryimpsucc;
  Log.info logger "--------------";
  Log.info_o logger (TimeMeas.print) "Strategies building time:  " tm_strats;
  Log.info_o logger (TimeMeas.print) "Strategy improvement time: " tm_imp;
  Log.info_o logger (TimeMeas.print) "Strategy value time:       " tm_val;
  Log.info logger "--------------";
  Log.info_o logger (TimeMeas.print) "SMT formula building time: " tm_smt_conv;
  Log.info_o logger (TimeMeas.print) "SMT context manip. time:   " tm_smt_ctx;
(*  Log.info_o logger (Format.pp_print_float) "SMT Yices conversion time: " !Smt.conv_time; *)
  Log.info_o logger (TimeMeas.print) "SMT solving time:          " tm_smt;
  Log.info_o logger (TimeMeas.print) "SMT f. part. eval. time:   " tm_smt_mod;
  Log.info_o logger (TimeMeas.print) "LP problem building time:  " tm_lp_conv;
  Log.info_o logger (TimeMeas.print) "LP solving time:           " tm_lp;

  let s = values_to_abstract env doman values in
  let anres = [Cfg.get_only_locid cf.Program.c_cfg,s] in
  (is_safe env cf.Program.c_final anres,
   refine_loc env anres,
   print_result env cf.Program.c_final anres,
   result_to_bddapron env anres)

end



(*
(** computes an improving strategy: version without generalisation *)
let strategy_improve_elt2 strats_table strat_table value_table env 
    template bequs assertion strats strat_i s s_elt t_elt =
  let smtv0 = smt_build_value env template s in
  Log.debug3 logger ("smt-value0: "^(Smt.string_of_formula smtv0));
  let smtvg0 = smt_build_gt_value_elt env t_elt s_elt in
  Log.debug3 logger ("smt-gt_value0: "^(Smt.string_of_formula smtvg0));
  let smtstrats = smt_build_strats env strats in
  Log.debug3 logger ("smt-strats: "^(Smt.string_of_formula smtstrats));
  let smtbequs = smt_build_bequs env bequs in
  Log.debug3 logger ("smt-bequs: "^(Smt.string_of_formula smtbequs));
  let smtass = smt_build_boolexpr env assertion in
  Log.debug3 logger ("smt-assertion: "^(Smt.string_of_formula smtass));
  let benum_careset = benum_careset env in
  Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
        "benum-careset: " benum_careset;
  let smtenum = smt_build_boolexpr env benum_careset in
  let ff=BddapronSmt.smt_and [smtv0;smtvg0;smtstrats;smtbequs;smtass;smtenum] in
  Log.debug3 logger ("smt-ff: "^(Smt.string_of_formula ff));
  let sat = ref true in
  let found = ref false in
  let strat_elt = ref strat_i in
  let blocked = ref Smt.T in
  while !sat do
  begin
    Log.debug3 logger ("smt-blocked: "^(Smt.string_of_formula !blocked));
    let m = BddapronSmt.smt_compute_model env.Env.env env.Env.cond 
      (Smt.And (ff,!blocked)) in
    Log.debug2_o logger BddapronSmt.smt_print_result "smt-result: " m;
    match m with 
    | None -> sat:= false
    | Some mm -> 
    begin
      let m = BddapronSmt.smt_convert_model env.Env.env env.Env.cond mm in  
      Log.debug2_o logger (BddapronSmt.smt_print_model env.Env.env env.Env.cond)
        "smt-model: " m;
      let pmodel = model_filter_vars env.Env.p_vars m in
      Log.debug2_o logger (BddapronSmt.smt_print_model env.Env.env env.Env.cond)
        "smt-pvars-model: " pmodel;
      let bpvars = BddapronSmt.model_to_boolexpr env.Env.env env.Env.cond pmodel in
      Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
        "pvars-model-boolexpr: " bpvars;
      let leaf = get_strategies_leaf env strats_table strats bpvars in
      Log.debug2_o logger (print_strategy_leaf env) "leaf: " leaf;
      let bprimemodel = model_filter_vars 
        (Util.list_inter env.Env.b_vars env.Env.primed_vars) m in
      Log.debug2_o logger (BddapronSmt.smt_print_model env.Env.env env.Env.cond)
        "smt-b'-model: " bprimemodel;
      let bprime = BddapronSmt.model_to_boolexpr ~unprime:true env.Env.env env.Env.cond bprimemodel in
      Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
        "b'-model-boolexpr: " bprime;

      let nmodel = model_filter_vars env.Env.n_vars mm in
      let nff = Smt.instantiate_formula nmodel ff in
      Log.debug3 logger ("smt-ff-inst-n: "^(Smt.string_of_formula nff));
      let nffexpr = BddapronSmt.smt_to_boolexpr env.Env.env env.Env.cond nff in
      Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
        "smt-ff-inst-n-boolexpr: " nffexpr;

      blocked := Smt.And (!blocked,Smt.Not (model_to_smt_boolexpr env pmodel));
      found := true;
      strat_elt := update_strategy_elt env strat_table !strat_elt bprime leaf;
    end
  end
  done;
  if !found then Some !strat_elt else None
*)



(*
(* version without incremental SMT solving *)
(** computes an improving strategy *)
let strategy_improve_elt strats_table strat_table value_table env 
    template bequs assertion strats strat_i s s_elt t_elt =

  TimeMeas.start tm_smt_conv;
  let smtv0 = smt_build_value env template s in
(* Log.debug3 logger ("smt-value0: "^(Smt.string_of_formula smtv0)); *)
  let smtvg0 = smt_build_gt_value_elt env t_elt s_elt in
(* Log.debug3 logger ("smt-gt_value0: "^(Smt.string_of_formula smtvg0)); *)
  let smtstrats = smt_build_strats env strats in
(* Log.debug3 logger ("smt-strats: "^(Smt.string_of_formula smtstrats)); *)
  let smtbequs = smt_build_bequs env bequs in
(* Log.debug3 logger ("smt-bequs: "^(Smt.string_of_formula smtbequs)); *)
  let smtass = smt_build_boolexpr env assertion in
(* Log.debug3 logger ("smt-assertion: "^(Smt.string_of_formula smtass)); *)
  let benum_careset = benum_careset env in
(* Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
        "benum-careset: " benum_careset; *)
  let smtenum = smt_build_boolexpr env benum_careset in
  let ff=BddapronSmt.smt_and [smtv0;smtvg0;smtstrats;smtbequs;smtass;smtenum] in
(* Log.debug3 logger ("smt-ff: "^(Smt.string_of_formula ff)); *)
  TimeMeas.stop tm_smt_conv;

  let sat1 = ref true in
  let found = ref false in
  let strat_elt = ref strat_i in
  let blocked_u = ref Smt.T in
  while !sat1 do
  begin
    TimeMeas.start tm_smt;
    let m1 = BddapronSmt.smt_compute_model env.Env.env env.Env.cond 
      (Smt.And (ff,!blocked_u)) in
    Log.debug2_o logger BddapronSmt.smt_print_result "smt-result: " m1;
    TimeMeas.stop tm_smt;

    match m1 with 
    | None -> sat1:= false
    | Some mm1 -> 
    begin
      found := true;
      let m = ref mm1 in

      TimeMeas.start tm_smt_mod;
      let nmodel = model_filter_vars env.Env.n_vars mm1 in
      let nff = ref (Smt.instantiate_formula nmodel ff) in
  (*    Log.debug3 logger ("smt-ff-inst-n: "^(Smt.string_of_formula !nff)); *)
      TimeMeas.stop tm_smt_mod;

      let sat2 = ref true in
      while !sat2 do
      begin
        TimeMeas.start tm_smt_mod;
        let m1 = BddapronSmt.smt_convert_model env.Env.env env.Env.cond !m in
(*        Log.debug3_o logger (BddapronSmt.smt_print_model env.Env.env 
           env.Env.cond) "smt-model: " m1; *)
        let bpvars = List.append env.Env.bs_vars env.Env.p_vars in
(*        Log.debug3_o logger (Util.list_print (Format.pp_print_string)) "bpvars: " bpvars; *)
        let bpmodel = model_filter_vars bpvars m1 in
(*        Log.debug3_o logger (BddapronSmt.smt_print_model env.Env.env 
          env.Env.cond) "smt-bp-model: " bpmodel; *)
        let bpexpr = BddapronSmt.model_to_boolexpr env.Env.env env.Env.cond bpmodel in
(*        Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env 
          env.Env.cond) "bp-model-boolexpr: " bpexpr; *)
        let leaf = get_strategies_leaf env strats_table strats bpexpr in
        Log.debug2_o logger (print_strategy_leaf env) "leaf: " leaf;

        let bpmodel2 = model_filter_vars (BddapronSmt.vars_to_smtvars env.Env.env env.Env.cond bpvars) !m in
        let bprime = Smt.instantiate_formula bpmodel2 !nff in
(*         Log.debug3 logger ("smt-bprime: "^(Smt.string_of_formula bprime));*)

        let bprimeexpr = BddapronSmt.smt_to_boolexpr ~unprime:true env.Env.env 
          env.Env.cond bprime in  
        let bisupp = BddapronUtil.supp_of_vars env.Env.env env.Env.bi_vars in
        let bprimeexpr = BddapronUtil.boolexpr_forget_supp bisupp bprimeexpr in
        Log.debug2_o logger (BddapronUtil.print_boolexpr env.Env.env 
          env.Env.cond) "b'-boolexpr: " bprimeexpr;

        strat_elt := update_strategy_elt env strat_table !strat_elt 
          bprimeexpr leaf;

        let blocked_g = smt_build_boolexpr env 
          (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond 
            (BddapronSmt.smt_to_boolexpr env.Env.env env.Env.cond bprime)) in 
        blocked_u := Smt.And (!blocked_u,blocked_g);
(*        Log.debug2 logger ("smt-blocked: "^(Smt.string_of_formula blocked_g)); *)

        nff := Smt.And (!nff,blocked_g);
        TimeMeas.stop tm_smt_mod;

        TimeMeas.start tm_smt;
        let m2 = BddapronSmt.smt_compute_model env.Env.env env.Env.cond !nff in
        Log.debug2_o logger BddapronSmt.smt_print_result "smt-result: " m2;
        begin match m2 with 
        | None -> sat2:= false
        | Some mm2 -> m := mm2 
        end;
        TimeMeas.stop tm_smt;
      end
      done
    end
  end
  done;
  if !found then Some !strat_elt else None
*)

(*
(* splits a location and returns the location corresponding to expr *)
let stratcfg_split_loc env cfg expr locid =
  Log.debug3_o logger Format.pp_print_int "split locid = " locid;
  let inv = PSHGraph.attrvertex cfg locid in
  let (inv1,inv2) = BddapronUtil.split_boolexpr env.Env.env env.Env.cond 
    inv expr in
  Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
    "expr = " expr;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
    "inv = " inv;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
    "inv1 = " inv1;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond)
    "inv2 = " inv2;

 if(not((Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond inv1) or 
         (Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond inv2))) then
 begin
  (* add new locations *)
  let loc1 = stratcfg_next_locid cfg in
  PSHGraph.add_vertex cfg loc1 inv1;
  let loc2 = stratcfg_next_locid cfg in
  PSHGraph.add_vertex cfg loc2 inv2;
  Log.debug3_o logger Format.pp_print_int "add loc: " loc1;
  Log.debug3_o logger Format.pp_print_int "add loc: " loc2;

  (* for all predecessors p of loc 
       (includes self-loops and edges between l1 and l2):
       - add edges from p to l1 and l2
  *)
  PSette.iter 
    (fun pe -> 
      let parr = PSHGraph.predvertex cfg pe in
      assert((Array.length parr)=1);
      if parr.(0)<>locid then
      begin
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg pe) parr [|loc1|]; 
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg pe) parr [|loc2|]; 
      end 
      else
      begin
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg pe) [|loc1|] [|loc2|]; 
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg pe) [|loc2|] [|loc1|]; 
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg pe) [|loc1|] [|loc1|]; 
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg pe) [|loc2|] [|loc2|]; 
      end)
   (PSHGraph.predhedge cfg locid);

  (* remove all edges to loc *)
  PSette.iter 
    (fun e -> PSHGraph.remove_hedge cfg e) 
    (PSHGraph.predhedge cfg locid);

  (* for all successors s of loc:
       - add edges from l1 to s, l2 to s
  *)
  PSette.iter 
    (fun se -> 
      let sarr = PSHGraph.succvertex cfg se in
      assert((Array.length sarr)=1);
      if sarr.(0)<>locid then
      begin
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg se) [|loc1|] sarr; 
        PSHGraph.add_hedge cfg (stratcfg_next_arcid cfg)
          (PSHGraph.attrhedge cfg se) [|loc2|] sarr; 
      end 
    )
    (PSHGraph.succhedge cfg locid);

  (* remove loc *)
  PSHGraph.remove_vertex cfg locid
(*  [loc1]*)
 end
*)
