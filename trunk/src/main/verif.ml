(******************************************************************************)
(* verif *)
(* verification strategy engine *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Verif";
              Log.level=Log.Debug}

exception InvalidStrategy of string
exception InvalidStrategyOption of string
exception InvalidStrategyOptionValue of string

let print_overall = ref false
let check_property = ref true

(******************************************************************************)
(* strategy descriptions *)
(******************************************************************************)
(* add new strategy descriptions here *)

let strats =  (* identifier * short description * long description list *)
  [ 
  (********** PARTITIONING ***********************************************)
   ("pIF",("initial, final and other states", 
           "partition by initial, final and other states"));
   ("pE",("enumeration of Boolean states",
          "enumerate (non initial/non final) Boolean states, options: v=<vars>...restrict enumeration to variables"));
   ("pM",("manual partitioning",
          "manual partitioning, options: e=<exprs>...partitioning predicates"));
   ("pMD",("discrete numerical modes (Boolean-defined)",
           "partition by discrete numerical modes (Boolean-defined), "^
              "options: v=<vars>...restrict to given numerical variables, "^
              "i=<forget>...bi=ignore Boolean inputs (default), bic=ignore Boolean inputs and numerical constraints"));
   ("pMHB",("continuous modes (Boolean-defined)",
           "partition by continuous modes (Boolean-defined), "^
              "options: v=<vars>...restrict to given numerical variables, "^
              "i=<forget>...bi=ignore Boolean inputs (default), bic=ignore Boolean inputs and numerical constraints"));
   ("pMHN",("continuous modes (numerically defined)",
           "partition by continuous modes (numerically defined), "^
              "options: d=<dom>...domain (default: FdpI), "^
              "ws=<widening start>, wd=<descending iterations>"));
   ("pQ",("enumeration of hybrid translation variables",
          "enumerate hybrid translation state variables"));
   ("pS",("convex staying conditions",
          "split into convex staying conditions")); 

   ("tR",("relational abstractions",
          "relational abstractions")); 

   ("pB",("boolean backward bisimulation",
          "refine partition by boolean backward bisimulation"));

  (********** PREPROCESSING ***********************************************)
   ("rT",("refine transition functions to arcs",
          "refine transition functions to arcs"));
   ("rB",("remove boolean inputs",
           "remove boolean inputs"));
   ("rS",("split non-convex guards",
           "split non-convex guards"));

   ("rAB",("remove boolean inputs and categorize accelerable loops",
          "remove boolean inputs and categorize accelerable loops, "^
             "options: d=<decoupling mode>, O...no decoupliing, "^
             "B...boolean/accelerable decoupling, "^
             "N...boolean+non-accelerable/accelerable decoupling (default)"));
   ("rAS",("split non-convex numerical guards in accelerable self-loops",
          "split non-convex numerical guards in accelerable self-loops"));
   ("rAF",("flatten accelerable self-loops",
          "flatten accelerable self-loops"));
   ("rAD",("decouple accelerable from non-accelerable or Boolean self-loops",
          "decouple accelerable from non-accelerable or Boolean self-loops"));
   ("rAI",("inputization for decoupled self-loops",
          "inputization for decoupled self-loops")); 

  (********** ANALYSIS ***********************************************)
  (********** discrete ***********************************************)
   ("aB",("boolean analysis",
           "boolean analysis, options: b...backward"));
   ("aS",("standard analysis",
           "standard analysis, options: d=<domain>, b...backward, ws=<widening start>, wd=<descending iterations>"));
   ("aA",("analysis with abstract acceleration",
           "analysis with abstract acceleration, options:d=<domain>, b...backward, ws=<widening start>, wd=<descending iterations>, aws=<widening start for accelerable transitions>"));

  (********** ANALYSIS ***********************************************)
  (********** hybrid ***********************************************)
   ("aH",("hybrid analysis with time elapse",
           "hybrid analysis with time elapse, options: d=<domain>, ws=<widening start>, wd=<descending iterations>")); 
  ]

let get_short_stratdesc s =
  let (desc,_) = List.assoc s strats in
  desc

let dir_to_string = function 
  |`Forward -> "forward"
  |`Backward -> "backward"

let print_strategies fmt () =
  List.iter
    (fun (s,(_,longdesc)) -> 
      Format.fprintf fmt "@[ ";
      Util.print_fixed fmt 5 s;
      Format.fprintf fmt "@[<hov>";
      Util.print_breakable fmt longdesc;
      Format.fprintf fmt "@]@]@.")
    strats

(******************************************************************************)
(* domains *)
(******************************************************************************)

let domain_descs =
  [("P",("convex polyhedra","convex polyhedra, options: l...without strict inequalities, p...power domain"));
   ("O",("octagons","octagons, options: p...power domain"));
   ("I",("intervals","intervals, options: p...power domain"));
   ("TE",("template polyhedra (emulation)","template polyhedra (emulation), options: t=<template expressions>, p...power domain"));
   ("FdpI",("finitely disjunctive partitioned interval domain","finitely disjunctive partitioned interval domain"));
  ]

let print_domains fmt () =
  List.iter
    (fun (s,(_,longdesc)) -> 
      Format.fprintf fmt "@[ ";
      Util.print_fixed fmt 5 s;
      Format.fprintf fmt "@[<hov>";
      Util.print_breakable fmt longdesc;
      Format.fprintf fmt "@]@]@.")
    domain_descs

let get_domain_desc dom = 
 let (domstr,_) = ParseParams.parse_param dom in
  Pervasives.fst (List.assoc domstr domain_descs)

let parse_domain env dom = 
  let (domstr,options) = 
    try ParseParams.parse_param dom 
    with _ -> raise (InvalidStrategyOptionValue dom)
  in
  let defaults = Util.list2mappe [("l","s");("p","c");("t","OCT")] in
  let domstr = domstr^
    (if domstr="P" then 
       ParseParams.get_option options defaults "l" (function ""->"l" |x->x)
     else "")^
    (ParseParams.get_option options defaults "p" (function ""->"p" |x->x))
  in
  match domstr with
    |"Psc" -> let _ = DomainStd.StrictPolProd.makeinit_doman () in
        (module DomainStd.StrictPolProd : Domain.T)
    |"Psp" -> let _ = DomainStd.StrictPolPow.makeinit_doman () in
        (module DomainStd.StrictPolPow : Domain.T)
    |"Plc" -> let _ = DomainStd.LoosePolProd.makeinit_doman () in
        (module DomainStd.LoosePolProd : Domain.T)
    |"Plp" -> let _ = DomainStd.LoosePolPow.makeinit_doman () in
        (module DomainStd.LoosePolPow : Domain.T)
    |"Oc" -> let _ = DomainStd.OctProd.makeinit_doman () in
        (module DomainStd.OctProd : Domain.T)
    |"Op" -> let _ = DomainStd.OctPow.makeinit_doman () in
        (module DomainStd.OctPow : Domain.T)
    |"Ic" -> let _ = DomainStd.BoxProd.makeinit_doman () in
        (module DomainStd.BoxProd : Domain.T)
    |"Ip" -> let _ = DomainStd.BoxPow.makeinit_doman () in
        (module DomainStd.BoxPow : Domain.T)
    |_ -> raise (InvalidStrategyOptionValue dom)

let parse_bddapron_domain env dom = 
  let (domstr,options) = 
    try ParseParams.parse_param dom 
    with _ -> raise (InvalidStrategyOptionValue dom)
  in
  let defaults = Util.list2mappe [("l","s");("p","c")] in
  let domstr = domstr^
    (if domstr="P" then 
       ParseParams.get_option options defaults "l" (function ""->"l" |x->x)
     else "")^
    (ParseParams.get_option options defaults "p" (function ""->"p" |x->x))
  in
  match domstr with
    |"Psc" -> let _ = DomainStd.StrictPolProd.makeinit_doman () in
        (module DomainStd.StrictPolProd : Domain.NOPARAM_T)
    |"Psp" -> let _ = DomainStd.StrictPolPow.makeinit_doman () in
        (module DomainStd.StrictPolPow : Domain.NOPARAM_T)
    |"Plc" -> let _ = DomainStd.LoosePolProd.makeinit_doman () in
        (module DomainStd.LoosePolProd : Domain.NOPARAM_T)
    |"Plp" -> let _ = DomainStd.LoosePolPow.makeinit_doman () in
        (module DomainStd.LoosePolPow : Domain.NOPARAM_T)
    |"Oc" -> let _ = DomainStd.OctProd.makeinit_doman () in
        (module DomainStd.OctProd : Domain.NOPARAM_T)
    |"Op" -> let _ = DomainStd.OctPow.makeinit_doman () in
        (module DomainStd.OctPow : Domain.NOPARAM_T)
    |"Ic" -> let _ = DomainStd.BoxProd.makeinit_doman () in
        (module DomainStd.BoxProd : Domain.NOPARAM_T)
    |"Ip" -> let _ = DomainStd.BoxPow.makeinit_doman () in
        (module DomainStd.BoxPow : Domain.NOPARAM_T)
    |_ -> raise (InvalidStrategyOptionValue dom)

(******************************************************************************)
(* parsing of strategy options *)
(******************************************************************************)
(* add parsing of strategy options here *)

let build_call_pE env options =
  let defaults = Util.list2mappe [("v","")] in
  let vars = ParseParams.get_option options defaults "v" 
    (ParseParams.comma_str_to_strlist) in
  let vars = if Util.list_is_empty vars then env.Env.bs_vars else vars in
  TransGen.enumerate vars

let build_call_pM env options =
  let defaults = Util.list2mappe [("e","")] in
  let exprlist = ParseParams.get_option options defaults "e" 
    (ParseParams.comma_str_to_strlist) in
  let exprs = ParseParams.strlist_to_boolexprlist env exprlist in
  TransGen.manual exprs
  
let build_call_pMD env options =
  let defaults = Util.list2mappe [("v","");("i","bi")] in
  let vars = ParseParams.get_option options defaults "v" 
    (ParseParams.comma_str_to_strlist) in
  let vars = if Util.list_is_empty vars then env.Env.ns_vars else vars in
  match ParseParams.get_option options defaults "i" (fun x -> x) with
    |"bi" -> TransDiscrete.modes_bool1 vars
    |"bic" -> TransDiscrete.modes_bool2 vars
    |x -> raise (InvalidStrategyOptionValue x)

let build_call_pMHB env options =
  let defaults = Util.list2mappe [("v","");("i","bi")] in
  let vars = ParseParams.get_option options defaults "v" 
    (ParseParams.comma_str_to_strlist) in
  let vars = if Util.list_is_empty vars then env.Env.ns_vars else vars in
  match ParseParams.get_option options defaults "i" (fun x -> x) with
    |"bi" -> TransHybrid.modes_bool1 vars
    |"bic" -> TransHybrid.modes_bool2 vars
    |x -> raise (InvalidStrategyOptionValue x)

let build_call_pMHN env options = 
  let defaults = Util.list2mappe 
    [("d","FdpI");("ws","0");("wd","1")] in
  let ws = ParseParams.get_option options defaults "ws" (int_of_string) in
  let wd = ParseParams.get_option options defaults "wd" (int_of_string) in
  let dom = ParseParams.get_option options defaults "d" (fun x -> x) in
  let module Dom = (val (parse_domain env dom): Domain.T) in
  let module An = AnalysisStd.Hyb(Dom) in
  VerifUtil.Trans(TransHybrid.modes_num 
   (An.analyze (AnalysisStd.make_hyb_param ws wd)),
      ((get_short_stratdesc "pMHN")^" analyzed with "^(get_domain_desc dom)))

let build_call_rAB env options =
  let defaults = Util.list2mappe [("d","N")] in
  let decouplemode = ParseParams.get_option options defaults "d" (function
      |"N" -> TransAcc.DecoupleBoolNacc
      |"B" -> TransAcc.DecoupleBool
      |"O" -> TransAcc.DecoupleNone
      |x -> raise (InvalidStrategyOptionValue x)) in
  TransAcc.remove_bool_inputs decouplemode

let parse_aS env options = 
  let defaults = Util.list2mappe 
    [("b","f");("d","P:s,c");("ws","2");("wd","2")] in
  let dir = ParseParams.get_option options defaults "b" 
    (function |"f" -> `Forward |_-> `Backward) in
  let ws = ParseParams.get_option options defaults "ws" (int_of_string) in
  let wd = ParseParams.get_option options defaults "wd" (int_of_string) in
  let dom = ParseParams.get_option options defaults "d" (fun x -> x) in
  let module Dom = (val (parse_domain env dom): Domain.T) in
  let module An = AnalysisStd.Std(Dom) in
  VerifUtil.Analysis(
    An.analyze (AnalysisStd.make_std_param dir ws wd),
      ((get_short_stratdesc "aS")^" "^(dir_to_string dir)^
        " with "^(get_domain_desc dom)))

let parse_aB options =
  let defaults = Util.list2mappe [("b","f")] in
  let dir = ParseParams.get_option options defaults "b" 
    (function |"f" -> `Forward |_-> `Backward) in
  VerifUtil.Analysis(AnalysisStd.Bool.analyze (AnalysisStd.make_bool_param dir),
    ((get_short_stratdesc "aB")^" "^(dir_to_string dir)))

let parse_aA env options = 
  let defaults = Util.list2mappe 
    [("b","f");("d","P:s,c");("ws","2");("aws","7");("wd","2")] in
  let dir = ParseParams.get_option options defaults "b" 
    (function |"f" -> `Forward |_-> `Backward) in
  let ws = ParseParams.get_option options defaults "ws" (int_of_string) in
  let aws = ParseParams.get_option options defaults "aws" (int_of_string) in
  let wd = ParseParams.get_option options defaults "wd" (int_of_string) in
  let dom = ParseParams.get_option options defaults "d" (fun x -> x) in
  let module Dom = (val (parse_bddapron_domain env dom): Domain.NOPARAM_T) in
  let module An = Accel.Acc(Dom) in
  VerifUtil.Analysis(
    An.analyze (Accel.make_acc_param dir ws aws wd),
      ((get_short_stratdesc "aA")^" "^(dir_to_string dir)^
        " with "^(get_domain_desc dom)))
 
let parse_aH env options = 
  let defaults = Util.list2mappe 
    [("d","P:s,c");("ws","2");("wd","2")] in
  let ws = ParseParams.get_option options defaults "ws" (int_of_string) in
  let wd = ParseParams.get_option options defaults "wd" (int_of_string) in
  let dom = ParseParams.get_option options defaults "d" (fun x -> x) in
  let module Dom = (val (parse_domain env dom): Domain.T) in
  let module An = AnalysisStd.Hyb(Dom) in
  VerifUtil.Analysis(
    An.analyze (AnalysisStd.make_hyb_param ws wd),
      ((get_short_stratdesc "aH")^" "^
        " with "^(get_domain_desc dom)))

(******************************************************************************)
(* parses a strategy from a string *)
(******************************************************************************)

(* add new strategies here *)
let str_to_strategy env str =
  let ss = ParseParams.semicolon_str_to_strlist str in
  if Util.list_is_empty ss then 
    raise (InvalidStrategy( "empty strategy. nothing to do"));
  List.map (fun s ->
    let p = try ParseParams.parse_param s 
      with  Pervasives.Exit | Parse.Lex_error | Parsing.Parse_error -> raise (InvalidStrategy s)
    in
    match p with

   (********** PARTITIONING ***********************************************)
     |("pIF",_) -> (VerifUtil.Trans(TransGen.initfinal,
                    get_short_stratdesc "pIF"))
     |("pE",options) -> (VerifUtil.Trans( 
                    build_call_pE env options,
                    get_short_stratdesc "pE"))
     |("pM",options) -> (VerifUtil.Trans( 
                    build_call_pM env options,
                    get_short_stratdesc "pM"))
     |("pMD",options) -> (VerifUtil.Trans( 
                    build_call_pMD env options,
                    get_short_stratdesc "pMD"))
     |("pMHB",options) -> (VerifUtil.Trans( 
                    build_call_pMHB env options,
                    get_short_stratdesc "pMHB"))
     |("pMHN",options) -> build_call_pMHN env options
     |("pQ",_) -> (VerifUtil.Trans(TransGen.enumerate
                    env.Env.cont_q_vars,
                    get_short_stratdesc "pQ"))
     |("pS",_) -> (VerifUtil.Trans(TransHybrid.convex_staycond,
                    get_short_stratdesc "pS")) 
     |("tR",_) -> (VerifUtil.Trans(RelAbstr.transform,
                    get_short_stratdesc "tR")) 
     |("pB",_) -> (VerifUtil.Trans(TransGen.boolbacksim,
                    get_short_stratdesc "pB")) 

  (********** PREPROCESSING ***********************************************)
      |("rT",_) -> (VerifUtil.Trans(TransGen.refine_by_destloc,
                   get_short_stratdesc "rT"))
      |("rAB",options) -> (VerifUtil.Trans(
                    build_call_rAB env options,
                    get_short_stratdesc "rAB"))
      |("rAS",_) -> (VerifUtil.Trans(TransAcc.split_arcs,
                    get_short_stratdesc "rAS"))
      |("rAF",_) -> (VerifUtil.Trans(TransAcc.flatten_loops,
                    get_short_stratdesc "rAF"))
      |("rAD",_) -> (VerifUtil.Trans(TransAcc.decouple_equs,
                    get_short_stratdesc "rAD"))
      |("rAI",_) -> (VerifUtil.Trans(TransAcc.inputize, 
                    get_short_stratdesc "rAI")) 

      |("rB",_) -> (VerifUtil.Trans(TransGen.remove_bool_inputs,
                    get_short_stratdesc "rB"))
      |("rS",_) -> (VerifUtil.Trans(TransGen.split_arcs,
                    get_short_stratdesc "rS"))
 
  (********** ANALYSIS ***********************************************)
  (********** discrete ***********************************************)
      |("aS",options) -> parse_aS env options
      |("aB",options) -> parse_aB options
      |("aA",options) -> parse_aA env options

  (********** ANALYSIS ***********************************************)
  (********** hybrid *************************************************)
      |("aH",options) -> parse_aH env options

  (*******************************************************************)
      |(s,_) -> raise (InvalidStrategy("unknown strategy item '"^s^"'"))
    )
    ss


(******************************************************************************)
(* run the strategy *)
(******************************************************************************)

(* display CFG info *)
let display_cfg_size cfg = 
  let (cntv,cnth,_,_) = PSHGraph.size cfg in
  Log.info logger ("CFG ("^(string_of_int cntv)^" location(s), "^
     (string_of_int cnth)^" arc(s)")

(* run the verification strategy *)
let run env strategy cfprog =
  let rec do_run strats cfprog get_anres =
    match strats with
      |[] -> (false,get_anres,cfprog)
      |VerifUtil.Trans(trans,description)::tailstrats -> 
         Log.info logger ("transformation '"^description^"'");
         let cfprog = trans env cfprog get_anres in
         display_cfg_size cfprog.Program.c_cfg;            
         Log.debug_o logger (Cfg.print_short env) "CFG: " cfprog.Program.c_cfg;
         do_run tailstrats cfprog get_anres
      |VerifUtil.Analysis(analysis,description)::tailstrats -> 
         Log.info logger ("analysis '"^description^"'");
         let (result,refine_loc,print_result,get_anres) = 
           analysis env cfprog in
         Log.info_o logger (Format.pp_print_bool) 
           ("analysis '"^description^"' returned ") result;
         Log.info_o logger (print_result) "analysis result: " ();
         (* VerifUtil.checkres env cfprog direction anres*)
         if result && !check_property
            or (match tailstrats with |[] -> true |_ -> false) 
            then 
         begin
           if !print_overall then 
             VerifUtil.print_overall_reach env cfprog get_anres; 
           (result,get_anres,cfprog)
         end
         else
         begin
           (* Env.cudd_reorder env; *)
           let cfprog = VerifUtil.refine env cfprog (refine_loc) in
           display_cfg_size cfprog.Program.c_cfg;            
           Log.debug_o logger (Cfg.print_short env) "Refined CFG: " 
             cfprog.Program.c_cfg;
           do_run tailstrats cfprog get_anres
        end
  in
  do_run strategy cfprog (fun () -> Analysis.bddapron_res_empty)
