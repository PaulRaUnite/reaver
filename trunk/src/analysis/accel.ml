(******************************************************************************)
(* Accel *)
(* analysis with abstract acceleration implementation *)
(* author: Peter Schrammel *)
(* version: 0.9.3 *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Analysis.Accel";
              Log.level=Log.Debug}

(******************************************************************************)
(** {2 module Acc: abstract acceleration} *)
(******************************************************************************)

type acc_param_t = {a_dir:Analysis.direction_t; a_ws:int; a_aws:int; a_wd:int}
let make_acc_param a_dir a_ws a_aws a_wd = {a_dir; a_ws; a_aws; a_wd}

module Acc(Dom : Domain.T) =
struct

type analysisparam_t = acc_param_t

(* creates the manager for the fixpoint module *)
let make_fp_manager env cfg initial trfct = 
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
  Fixpoint.widening = (fun vertex s1 s2 -> Dom.widening env s1 s2);
  Fixpoint.apply = trfct;
  Fixpoint.odiff = None;
  Fixpoint.arc_init = (fun hedge -> ());
  Fixpoint.abstract_init = initial;
  Fixpoint.print_abstract = (fun fmt x -> Dom.print env fmt x);
  Fixpoint.print_arc=(fun fmt () -> ());
  Fixpoint.print_vertex = Format.pp_print_int;
  Fixpoint.print_hedge = Format.pp_print_int;
    
  Fixpoint.accumulate = false;

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

(* fixpoint iteration strategy for abstract acceleration *)
let make_strategy_acc ws wd aws cfg sinit =
  let widening_start cfg l =
    let (_,l) = l in
    let is_accel h = 
      let arc = PSHGraph.attrhedge cfg h in
      match arc with 
        |Arc.Accel(_,_) -> true 
        |_ -> false
    in
    let exists_accel hset =  
      PSette.fold 
        (fun h res -> res || (is_accel h)) 
        hset false
    in
(*    let rec exist_accel l =
      match l with
	|Ilist.Atome(v)::tl -> 
          let res = PSette.fold 
            (fun h res -> res || (is_accel h)) 
            (PSHGraph.succhedge cfg v) false in
          if res then true
	  else exist_accel tl
	|Ilist.List(_)::tl -> exist_accel tl
        |[] -> false
    in *)
    let rec all_accel l =
      match l with
	|Ilist.Atome(v)::tl -> 
          if (exists_accel (PSHGraph.predhedge cfg v)) &&
             (exists_accel (PSHGraph.succhedge cfg v)) then all_accel tl
	  else false
	|Ilist.List(_)::tl -> all_accel tl
        |[] -> true
    in
    if all_accel l then aws else ws
  in
  let flatten ~parent (b,_) = b.FixpointType.widening_start<=0 || 
                              parent.FixpointType.widening_start>0 in
  let filter arcid = 
    match PSHGraph.attrhedge cfg arcid with
    |Arc.Bool(_,_) -> false
    |_ -> true
  in
  FixpointAcc.make_strategy_widening_start 
    ~depth:2
    ~priority:(PSHGraph.Filter (filter))
    ~widening_start:(widening_start cfg)
    ~widening_descend:wd
    ~flatten
    ~vertex_dummy:Cfg.locid_dummy ~hedge_dummy:Cfg.arcid_dummy cfg sinit 

(******************************************************************************)
let arc_normal env dir assertion ass f s =
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
  s

(******************************************************************************)
let trfct env dir assertion cfg arcid tabs =
  let dloc = Cfg.get_succlocid cfg arcid in
  let dinv = Loc.get_inv (Cfg.get_loc cfg dloc) in
  assert((Array.length tabs)==1);
  let s = tabs.(0) in
  let s = match PSHGraph.attrhedge cfg arcid with
    |Arc.Normal(ass,f) ->
      Log.debug logger 
        ("processing normal arc ("^(string_of_int arcid)^")...");
      arc_normal env dir assertion ass f s
    |Arc.Accel(ass,f) -> 
      Log.debug logger 
        ("processing accelerable arc ("^(string_of_int arcid)^")...");
      let g = Bddapron.Expr0.Bool.tdrestrict 
        (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond ass assertion) 
        env.Env.cond.Bdd.Cond.careset 
      in
      Dom.accel ~dir env s f g
    |Arc.Bool(ass,f) -> 
      Log.debug logger 
        ("processing Boolean arc ("^(string_of_int arcid)^")...");
      arc_normal env dir assertion ass 
        (List.append f (BddapronUtil.get_id_equs_for env.Env.env env.Env.cond env.Env.ns_vars)) s
    |Arc.BoolNacc(ass,f) -> 
      Log.debug logger 
      ("processing Boolean/non-accelerable arc ("^(string_of_int arcid)^")...");
      arc_normal env dir assertion ass f s
    |Arc.Nonacc(ass,f) -> 
      Log.debug logger 
       ("processing non-accelerable arc ("^(string_of_int arcid)^")...");
      arc_normal env dir assertion ass f s
    |Arc.Id -> 
      Log.debug logger 
       ("processing identity arc ("^(string_of_int arcid)^")...");
      s
    |_ -> assert(false)
  in
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
    match param.a_dir with
      |`Forward -> cfg
      |`Backward -> PSHGraph.transpose 
        (fun v inv -> inv) (fun a arc -> arc) (fun info -> info) cfg
  in
  let get_init_state = 
    match param.a_dir with
      |`Forward -> get_initial_state env initial cfg2
      |`Backward -> get_initial_state env final cfg2
  in
  let fpman = make_fp_manager env cfg2 (get_init_state) 
    (trfct env param.a_dir assertion cfg2)
  in
  let sinit = 
    match param.a_dir with
      |`Forward -> Cfg.get_locidset_by_inv env cfg2 initial
      |`Backward -> Cfg.get_locidset_by_inv env cfg2 final
  in
  let strategy = make_strategy_acc param.a_ws param.a_wd param.a_aws 
    cfg2 sinit in
  Log.debug2_o logger (FixpointType.print_strategy fpman) "strategy: " strategy;
  let output = Fixpoint.analysis_std fpman cfg2 sinit strategy in
  let anres = fpout_to_anres env cfg2 output in 
  (is_safe env (match param.a_dir with |`Forward -> final |`Backward -> initial)
      anres,
   refine_loc env anres,
   print_result env 
     (match param.a_dir with |`Forward -> final |`Backward -> initial) anres,
   result_to_bddapron env anres)
end
