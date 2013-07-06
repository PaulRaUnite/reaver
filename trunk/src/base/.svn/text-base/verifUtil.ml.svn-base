(******************************************************************************)
(* verifUtil *)
(* verification engine utilities *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="VerifUtil";
              Log.level=Log.Debug}

type trans_t = Env.t -> Program.cfprog_t -> Analysis.result_to_bddapron_t -> 
  Program.cfprog_t
type analysis_t = Analysis.analyze_t

type stratitem_t =
  | Trans of trans_t * string
  | Analysis of analysis_t * string

type strategy_t = stratitem_t list

(******************************************************************************)
(* utilities *)
(******************************************************************************)
(******************************************************************************)
(* refines a CFG according to the given analysis result *)
let refine ?(refine_bool=true) env cfprog refine_loc =
  let cfg = cfprog.Program.c_cfg in
  (* replace invariants *)
  PSHGraph.iter_vertex cfg
    (fun v inv ~pred:_ ~succ:_ ->
      match refine_loc ~refine_bool v inv with
	|None -> PSHGraph.remove_vertex cfg v
	|Some refinv -> PSHGraph.replace_attrvertex cfg v refinv
    );
  (* check feasibility of arcs*)
  let _ = Cfg.remove_infeasible_arcs env cfg cfprog.Program.c_ass in
  (* remove unconnected locations *)
  let (unreach,_) = PSHGraph.reachable_multi 
    Cfg.locid_dummy Cfg.arcid_dummy cfg 
    (Cfg.get_locidset_by_inv env cfg cfprog.Program.c_init) in
  PSHGraph.iter_vertex cfg
    (fun v inv ~pred ~succ ->
      if PSette.mem v unreach then PSHGraph.remove_vertex cfg v);
  cfprog


(*
(******************************************************************************)
(* prints the result per location *)
let print_res_loc env v s contains_bad =
  let strfmt = Format.str_formatter in
  Format.pp_print_string strfmt "LOC ";
  Format.pp_print_int strfmt v;
  if not contains_bad then Format.pp_print_string strfmt ": reach = "
  else Format.pp_print_string strfmt ": CONTAINS BAD STATES, reach = ";
  let str1 = Format.flush_str_formatter () in
  Analysis.print_domain env strfmt s;
  let str2 = Format.flush_str_formatter () in
  Format.pp_print_string logger.Log.fmt (str1^(Util.string_compact str2));
  Format.pp_print_newline logger.Log.fmt ()

(******************************************************************************)
(* prints the overall reachable space *)
(*let print_union_reach env cfprog anres =
  let cfg = cfprog.Program.c_cfg in
  let initial = cfprog.Program.c_init in
  let initstates = Cfg.get_locidset_by_inv env cfg initial in 
  Log.debug_o logger (Domain.O.print env) "union of reachable space: "
    (List.fold_right
      (fun (v,s) result -> Domain.O.join env result s)
      (List.filter (fun (v,_) -> not (PSette.mem v initstates)) anres)
      (Domain.O.bottom env));
  Log.debug_o logger (Format.pp_print_int) "boolean state space size: "
    ((BddapronUtil.bool_space_size env 
      (Cudd.Bdd.exist env.Env.cond.Bdd.Cond.supp 
       (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
        (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond 
          (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond
             param.initial param.final))
        (List.fold_right
          (fun (v,s) result -> Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond 
                               (Domain.O.to_boolexpr env s) result)
          anres
          (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond)))))
      +2)*)


(******************************************************************************)
(* checks the analysis result, returns true if final is not reached *)
let checkres env cfprog dir anres =
  Log.info logger "analysis result:";
  Mappe.fold
    (fun v s result ->
      let contains_bad = Analysis.intersects env s 
         (match dir with
	   |`Forward -> cfprog.Program.c_final
	   |`Backward -> cfprog.Program.c_init)
      in
      print_res_loc env v s contains_bad;
      result && (not contains_bad))
    anres true
*)
