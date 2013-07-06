(******************************************************************************)
(* AnalysisStd *)
(* standard analysis implementation *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="AnalysisStd";
              Log.level=Log.Debug}

(******************************************************************************)
(** {2 module Std: standard analysis} *)
(******************************************************************************)

type std_param_t = {s_dir:Analysis.direction_t; s_ws:int; s_wd:int}
let make_std_param s_dir s_ws s_wd = {s_dir; s_ws; s_wd}

module Std(Dom :  Domain.T) =
struct

type analysisparam_t = std_param_t

(* creates the manager for the fixpoint module *)
let make_fp_manager env cfg initial trfct ws wd = 
{
  Fixpoint.bottom = (fun vertex -> Dom.bottom env);
  Fixpoint.canonical = 
    (fun vertex s -> Dom.canonicalize env s);
  Fixpoint.is_bottom = 
    (fun vertex s -> Dom.is_bottom env s);
  Fixpoint.is_leq = 
    (fun vertex s1 s2 -> Dom.is_leq env s1 s2);
  Fixpoint.join = 
    (fun vertex s1 s2 -> Dom.join env s1 s2);
  Fixpoint.join_list = (fun vertex slist -> 
    List.fold_left (fun res x -> Dom.join env res x) 
                     (Dom.bottom env) slist);
  Fixpoint.widening = (fun vertex s1 s2 -> 
    let dinv = PSHGraph.attrvertex cfg vertex in
    Dom.forget_list env 
      (Dom.meet_condition env 
         (Dom.widening env s1 s2) dinv) env.Env.i_vars);
  Fixpoint.apply = trfct;
  Fixpoint.arc_init = (fun hedge -> ());
  Fixpoint.abstract_init = initial;
  Fixpoint.print_abstract = (fun fmt x -> Dom.print env fmt x);
  Fixpoint.print_arc=(fun fmt () -> ());
  Fixpoint.print_vertex = Format.pp_print_int;
  Fixpoint.print_hedge = Format.pp_print_int;
    
  Fixpoint.accumulate = false;
  Fixpoint.widening_start=ws;
  Fixpoint.widening_descend=wd;

  Fixpoint.print_fmt = logger.Log.fmt;
  Fixpoint.print_analysis = Log.check_level logger Log.Debug;
  Fixpoint.print_component = Log.check_level logger Log.Debug2;
  Fixpoint.print_step = Log.check_level logger Log.Debug2;
  Fixpoint.print_state = Log.check_level logger Log.Debug;
  Fixpoint.print_postpre = Log.check_level logger Log.Debug2;
  Fixpoint.print_workingsets = Log.check_level logger Log.Debug;

  Fixpoint.dot_fmt = None; 
  Fixpoint.dot_vertex = (fun fmt v -> ());
  Fixpoint.dot_hedge = (fun fmt h -> ());
  Fixpoint.dot_attrvertex = (fun fmt v -> ());
  Fixpoint.dot_attrhedge = (fun fmt h -> ()); 
}

(* converts from fixpoint output to a (vertex,abstract value) list *)
let fpout_to_anres env cfg output =
  let locs = Cfg.get_locidset cfg in
  PSette.fold
    (fun v res -> Mappe.add v (PSHGraph.attrvertex output v) res)
    locs (Mappe.empty)

(* fixpoint iteration strategy for standard analysis *)
let make_strategy_std cfg sinit =
  Fixpoint.make_strategy_default 
    ~vertex_dummy:Cfg.locid_dummy ~hedge_dummy:Cfg.arcid_dummy cfg sinit

(* standard transition function *) 
let trfct env dir assertion cfg arcid tabs =
  let dloc = Cfg.get_succlocid cfg arcid in
  let dinv = Loc.get_inv (Cfg.get_loc cfg dloc) in
  assert((Array.length tabs)==1);
  let s = tabs.(0) in
  let (ass,f) = Arc.get_ass_equs env (Cfg.get_arc cfg arcid) in 
  let g = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond 
    env.Env.cond.Bdd.Cond.careset 
    (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond ass assertion) in
  let s = 
    match dir with
      |`Forward -> Dom.assign_lexpr env (Dom.meet_condition env s g) f
      |`Backward -> Dom.meet_condition env
         (Dom.substitute_lexpr env s f) g 
  in
  let s = Dom.forget_list env s env.Env.i_vars in
  let s = Dom.meet_condition env s dinv in
  ((),s)

(******************************************************************************)
(* returns the initial state for the given location *)
let get_initial_state env initial cfg loc =
  let top = Dom.top env in 
  let inv = PSHGraph.attrvertex cfg loc in
  (Dom.meet_condition env top 
     (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond initial inv)) 

(******************************************************************************)
let refine_loc env anres ~refine_bool locid loc = 
  let s = Mappe.find locid anres in
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
  Mappe.iter (fun locid s ->
     print_res_loc locid s (Dom.is_bottom env (Dom.meet_condition env s final)))
    anres 
   
let result_to_bddapron env anres () = 
  Mappe.map (Dom.to_boollinconsslist env) anres

let is_safe env final anres = 
  let rec check = function
    |[] -> true
    |(_,s)::reslist -> 
      let res = Dom.is_bottom env (Dom.meet_condition env s final) in
      if res then
        check reslist
      else false
  in
  check (Util.mappe2list anres)

(******************************************************************************)
let analyze param env cf =
  let cfg = cf.Program.c_cfg in
  let initial = cf.Program.c_init in
  let final = cf.Program.c_final in
  let assertion = cf.Program.c_ass in
  let cfg2 = 
    match param.s_dir with
      |`Forward -> cfg
      |`Backward -> PSHGraph.transpose 
        (fun v inv -> inv) (fun a arc -> arc) (fun info -> info) cfg
  in
  let get_init_state = 
    match param.s_dir with
      |`Forward -> get_initial_state env initial cfg2
      |`Backward -> get_initial_state env final cfg2
  in
  let fpman = make_fp_manager env cfg2 (get_init_state) 
    (trfct env param.s_dir assertion cfg2)
    param.s_ws param.s_wd 
  in
  let sinit = 
    match param.s_dir with
      |`Forward -> Cfg.get_locidset_by_inv env cfg2 initial
      |`Backward -> Cfg.get_locidset_by_inv env cfg2 final
  in
  let strategy = make_strategy_std cfg2 sinit in
  Log.debug2_o logger (FixpointType.print_strategy fpman) "strategy: " strategy;
  let output = Fixpoint.analysis_std fpman cfg2 sinit strategy in
  let anres = fpout_to_anres env cfg2 output in 
  (is_safe env (match param.s_dir with |`Forward -> final |`Backward -> initial)
      anres,
   refine_loc env anres,
   print_result env 
     (match param.s_dir with |`Forward -> final |`Backward -> initial) anres,
   result_to_bddapron env anres)
end


(******************************************************************************)
(** {2 module Bool: Boolean analysis} *)
(******************************************************************************)

type bool_param_t = {b_dir:Analysis.direction_t}
let make_bool_param b_dir = {b_dir}

module Bool =
struct

type analysisparam_t = bool_param_t

let refine_loc env (id,s) ~refine_bool locid loc =
  if not(Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond s) then
    if id=locid then 
      Some (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond s loc)
    else None
  else None

let print_result env (locid,s) result fmt () = 
  Format.pp_print_newline fmt ();
  let strfmt = Format.str_formatter in
  Format.pp_print_string strfmt "LOC ";
  Format.pp_print_int strfmt locid;
  if result then Format.pp_print_string strfmt ": reach = "
  else Format.pp_print_string strfmt ": CONTAINS BAD STATES, reach = ";
  let str1 = Format.flush_str_formatter () in
  BddapronUtil.print_boolexpr env.Env.env env.Env.cond strfmt s;
  let str2 = Format.flush_str_formatter () in
  Format.pp_print_string fmt (str1^(Util.string_compact str2))

let result_to_bddapron env (locid,s) () = 
  let anres = Mappe.empty in
  Mappe.add locid [(s,ApronUtil.linconss_empty env.Env.apronenv)] anres

let is_safe env final s = 
  Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond
    (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond s final)

(******************************************************************************)
let analyze param env cf =
  let cfg = cf.Program.c_cfg in
  let get_primed_var = BddapronUtil.get_primed_var env.Env.env in
  let get_unprimed_var = BddapronUtil.get_unprimed_var env.Env.env in
  let locid = Cfg.get_only_locid cfg in
  let inv = Loc.get_inv (Cfg.get_only_loc cfg) in
  let (_,f) = Arc.get_ass_equs env (Cfg.get_only_arc env cfg) in
  let fb = Env.get_bool_equs env f in
  let s_start = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond inv
    (match param.b_dir with
      |`Forward -> cf.Program.c_init
      |`Backward -> cf.Program.c_final)
  in
  let s = BddapronAnalysis.bool_reach2 ~dir:param.b_dir  
    ~get_primed_var ~get_unprimed_var
    env.Env.env env.Env.cond fb (List.append env.Env.n_vars env.Env.bi_vars) 
    s_start
    cf.Program.c_ass in
  let result = is_safe env cf.Program.c_final s in
  (result,
   refine_loc env (locid,s),
   print_result env (locid,s) result,
   result_to_bddapron env (locid,s))

end


(******************************************************************************)
(** {2 Logico-numerical hybrid analysis with time-elapse *)
(******************************************************************************)

type hyb_param_t = {h_ws:int; h_wd:int}
let make_hyb_param h_ws h_wd = {h_ws; h_wd}

module Hyb(Dom :  Domain.T) =
struct

type analysisparam_t = hyb_param_t

(* creates the manager for the fixpoint module *)
let make_fp_manager env cfg initial trfct ws wd = 
{
  Fixpoint.bottom = (fun vertex -> Dom.bottom env);
  Fixpoint.canonical = 
    (fun vertex s -> Dom.canonicalize env s);
  Fixpoint.is_bottom = 
    (fun vertex s -> Dom.is_bottom env s);
  Fixpoint.is_leq = 
    (fun vertex s1 s2 -> Dom.is_leq env s1 s2);
  Fixpoint.join = 
    (fun vertex s1 s2 -> Dom.join env s1 s2);
  Fixpoint.join_list = (fun vertex slist -> 
    List.fold_left (fun res x -> Dom.join env res x) 
                     (Dom.bottom env) slist);
  Fixpoint.widening = (fun vertex s1 s2 -> 
    let dinv = PSHGraph.attrvertex cfg vertex in
    Dom.forget_list env 
      (Dom.meet_condition env 
         (Dom.widening env s1 s2) dinv) env.Env.i_vars);
  Fixpoint.apply = trfct;
  Fixpoint.arc_init = (fun hedge -> ());
  Fixpoint.abstract_init = initial;
  Fixpoint.print_abstract = (fun fmt x -> Dom.print env fmt x);
  Fixpoint.print_arc=(fun fmt () -> ());
  Fixpoint.print_vertex = Format.pp_print_int;
  Fixpoint.print_hedge = Format.pp_print_int;
    
  Fixpoint.accumulate = false;
  Fixpoint.widening_start=ws;
  Fixpoint.widening_descend=wd;

  Fixpoint.print_fmt = logger.Log.fmt;
  Fixpoint.print_analysis = Log.check_level logger Log.Debug;
  Fixpoint.print_component = Log.check_level logger Log.Debug2;
  Fixpoint.print_step = Log.check_level logger Log.Debug2;
  Fixpoint.print_state = Log.check_level logger Log.Debug;
  Fixpoint.print_postpre = Log.check_level logger Log.Debug2;
  Fixpoint.print_workingsets = Log.check_level logger Log.Debug;

  Fixpoint.dot_fmt = None; 
  Fixpoint.dot_vertex = (fun fmt v -> ());
  Fixpoint.dot_hedge = (fun fmt h -> ());
  Fixpoint.dot_attrvertex = (fun fmt v -> ());
  Fixpoint.dot_attrhedge = (fun fmt h -> ()); 
}

let get_initial_state env initial cfg loc =
  let top = Dom.top env in 
  let inv = PSHGraph.attrvertex cfg loc in
  (Dom.meet_condition env top 
     (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond initial inv)) 

(* converts from fixpoint output to a (vertex,abstract value) list *)
let fpout_to_anres env cfg output =
  let locs = Cfg.get_locidset cfg in
  PSette.fold
    (fun v res -> Mappe.add v (PSHGraph.attrvertex output v) res)
    locs (Mappe.empty)

(* fixpoint iteration strategy for standard analysis *)
let make_strategy_std cfg sinit =
  Fixpoint.make_strategy_default 
    ~vertex_dummy:Cfg.locid_dummy ~hedge_dummy:Cfg.arcid_dummy cfg sinit

(* forward flow transition function *) 
let trfct env assertion cfg hedge tabs =
  let dloc = Array.get (PSHGraph.succvertex cfg hedge) 0 in
  let dinv = PSHGraph.attrvertex cfg dloc in
  assert((Array.length tabs)==1);
  let s = tabs.(0) in
  let s = match PSHGraph.attrhedge cfg hedge with
    |Arc.Normal(ass,f) ->
      Log.debug logger 
        ("processing jump arc ("^(string_of_int hedge)^")...");
      let g = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond ass assertion in
      let f = BddapronUtil.simplify_equs env.Env.env env.Env.cond f g in
      let s = Dom.assign_lexpr env (Dom.meet_condition env s g) f in
      let s = Dom.forget_list env s env.Env.i_vars in
      Dom.meet_condition env s dinv
    |Arc.Flow(stay,f) -> 
      Log.debug logger 
        ("processing flow arc ("^(string_of_int hedge)^")...");
      let stay = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond stay
           (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond dinv assertion)
      in
      Dom.flow env s f stay 
    |_ -> assert(false)
  in
  ((),s)

(******************************************************************************)
let refine_loc env anres ~refine_bool locid loc = 
  let s = Mappe.find locid anres in
  if Dom.is_bottom env s then None
  else 
    Some (if refine_bool then Dom.meetbool_to_boolexpr env s loc
          else Dom.meet_to_boolexpr env s loc)

let print_result env final anres fmt () = 
  let print_res_loc locid s res =
    Format.pp_print_newline logger.Log.fmt ();
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
  Mappe.iter (fun locid s ->
     print_res_loc locid s (Dom.is_bottom env (Dom.meet_condition env s final)))
    anres 
   
let result_to_bddapron env anres () = 
  Mappe.map (Dom.to_boollinconsslist env) anres

let is_safe env final anres = 
  let rec check = function
    |[] -> true
    |(_,s)::reslist -> 
      if Dom.is_bottom env (Dom.meet_condition env s final) then
        check reslist
      else false
  in
  check (Util.mappe2list anres)

(******************************************************************************)
let analyze param env cf =
  let cfg = cf.Program.c_cfg in
  let initial = cf.Program.c_init in
  let final = cf.Program.c_final in
  let assertion = cf.Program.c_ass in
  let get_init_state = get_initial_state env initial cfg in
  let fpman = make_fp_manager env cfg (get_init_state) 
    (trfct env assertion cfg)
    param.h_ws param.h_wd 
  in
  let sinit = Cfg.get_locidset_by_inv env cfg initial in
  let strategy = make_strategy_std cfg sinit in
  Log.debug2_o logger (FixpointType.print_strategy fpman) "strategy: " strategy;
  let output = Fixpoint.analysis_std fpman cfg sinit strategy in
  let anres = fpout_to_anres env cfg output in 
  (is_safe env final anres,
   refine_loc env anres,
   print_result env final anres,
   result_to_bddapron env anres)

end
