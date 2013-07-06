(******************************************************************************)
(* partitionUtil *)
(* partitioning utilities *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="PartitionUtil";
              Log.level=Log.Debug3}

(******************************************************************************)
(** computes all intersections of the given expressions  *)
let compute_partition_expr_list env expr0 exprs =
  let rec compute exprset exprs =
    match exprs with
      |[] -> exprset
      |e1::etl ->
        let ne1 = Cudd.Bdd.dnot e1 in
        let s = PSette.fold (fun e2 s -> 
          let ee1 = Cudd.Bdd.tdrestrict 
            (Cudd.Bdd.dand e1 e2) env.Env.cond.Bdd.Cond.careset in
          let ee2 = Cudd.Bdd.tdrestrict 
            (Cudd.Bdd.dand ne1 e2) env.Env.cond.Bdd.Cond.careset in
          if not(Cudd.Bdd.is_false ee1) && not(Cudd.Bdd.is_true ee1) then
            let s =  PSette.add ee1 s in
            if not(Cudd.Bdd.is_false ee2) && not(Cudd.Bdd.is_true ee2) then
              let s = PSette.add ee2 s in
              PSette.remove e2 s
            else s
          else  
            if not(Cudd.Bdd.is_false ee2) && not(Cudd.Bdd.is_true ee2) then
              PSette.add ee2 s
            else s)
          exprset exprset
        in
        compute s etl
  in 
  PSette.elements (compute 
    (PSette.singleton (compare) expr0) 
    exprs)

(******************************************************************************)
(** checks whether the splitting constraint would yield reasonable partitions *)
let check_partition_expr env inv expr =
  let expr1 = Bddapron.Expr0.Bool.tdrestrict 
    (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond inv expr)
    env.Env.cond.Bdd.Cond.careset
  in
  let expr2 = Bddapron.Expr0.Bool.tdrestrict 
    (BddapronUtil.normalize_benumneg env.Env.env env.Env.cond
         (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond inv 
           (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond expr)))
    env.Env.cond.Bdd.Cond.careset
  in
  not(Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond expr1) &&
  not(Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond expr2)

(******************************************************************************)
(** partitions a location by the given expression *)
let partition_location_by env cfg loc partition_expr assertion =
  let inv = PSHGraph.attrvertex cfg loc in
  Log.debug_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond) 
    "partition_expr = " partition_expr; 
  (* if partition expression != true (or false which is nonsense) 
     then do partitioning *)
  if check_partition_expr env inv partition_expr then 
    let _ = Cfg.split_location env cfg loc partition_expr assertion in ()
  else
    if not (Bddapron.Expr0.Bool.is_true env.Env.env env.Env.cond 
       partition_expr) then 
      Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond) 
        "partition_expr invalid: " partition_expr

(***********let partition_locs_by env cfg assertion exprs locsinv = 
  PSette.iter 
    (fun expr -> 
      let locs = Cfg.get_locs_by env cfg locsinv in
      PSette.iter 
       (fun loc -> 
         let inv = PSHGraph.attrvertex cfg loc in
         if PartitionUtil.check_partition_expr env inv expr then 
         begin
           Log.debug_o logger (BddapronUtil.print_bdd env) 
             "partition location: " inv;
           Log.debug_o logger (BddapronUtil.print_bdd env) 
             "partition_expr = " expr; 
           Cfg.split_location2 env cfg loc expr assertion
         end)
       locs)
     exprs
*******************************************************************)
(** partitions according to a given list of expressions *) 
let partition_all_by env cfg iflocs assertion exprlist =
  List.iter
    (fun expr -> 
       PSette.iter 
         (fun locid -> partition_location_by env cfg locid expr assertion) 
         (PSette.diff (Cfg.get_locidset cfg) iflocs))
    exprlist

(******************************************************************************)
(** partitions the given location according to a given list of disjoint 
     expressions *) 
let rec partition_disjoint_loc env cfg assertion loc exprlist =
  match exprlist with
    |[] -> ()
    |e::ee -> 
    begin
      let inv = PSHGraph.attrvertex cfg loc in
      (* check whether expr is a valid splitting expression *)
      let newloc = 
        if check_partition_expr env inv e then
        begin
          let newlocs = Cfg.split_location env cfg loc e assertion in
          assert((List.length newlocs)=2);
          List.hd (List.tl newlocs)
        end
        else loc
      in
      partition_disjoint_loc env cfg assertion newloc ee
    end

(******************************************************************************)
let partition_locs_by env cfg assertion exprs locsinv = 
  PSette.iter 
    (fun expr -> 
      let locs = Cfg.get_locidset_by_inv env cfg locsinv in
      PSette.iter 
       (fun loc -> 
         let inv = PSHGraph.attrvertex cfg loc in
         if check_partition_expr env inv expr then 
         begin
           Log.debug_o logger (Env.print_boolexpr env) 
             "partition location: " inv;
           Log.debug_o logger (Env.print_boolexpr env) 
             "partition_expr = " expr; 
           let _ = Cfg.split_location env cfg loc expr assertion in ()
         end)
       locs)
     exprs

(******************************************************************************)
let partition_locationsucc env cfg assertion loc heur =
  let inv = PSHGraph.attrvertex cfg loc in
  try begin
   let (_,f) = Arc.get_ass_equs env (PSHGraph.attrhedge cfg
      (PSette.choose (PSHGraph.succhedge cfg loc)))
    in
  let exprs = heur env f inv assertion in
  (* inv \/ \/_succs inv_succ  *)
  let locsinv = PSette.fold 
    (fun l invs -> Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond 
      (PSHGraph.attrvertex cfg l) invs)
    (PSHGraph.succ_vertex cfg loc)
    inv
  in
  partition_locs_by env cfg assertion exprs locsinv
  end
  with Not_found -> ()
(*    Log.warn_o logger (Env.print_boolexpr env) 
       "location has no outgoing edges: " inv*)

(******************************************************************************)
(** partitions until convergence 
     trying to partition the current location and its successors 
     by a heuristic returning a list of partition equations *) 
let partition_recsucc env cfg assertion heur iflocs =
  let doneset = ref (PSette.empty compare) in
  let todoset = ref (PSette.diff (Cfg.get_locidset cfg) iflocs) in
  while not (PSette.is_empty !todoset) do 
  begin 
    let loc = PSette.choose !todoset in
    partition_locationsucc env cfg assertion loc heur;
    let currentset = Cfg.get_locidset cfg in
    doneset := PSette.add loc !doneset;
    todoset := PSette.diff currentset !doneset;
    Log.debug_o logger (PSette.print Format.pp_print_int) 
      "todoset = " !todoset
  end done

(******************************************************************************)
(** find a combination of actions 
   with the same numerical constraints in the guards*)
let nummodes1 vars env f assertion =
  let fn = Env.get_num_equs env f in
  Log.debug3_o logger (Env.print_equations env) "nequs=" fn;
  let binsupp = BddapronUtil.supp_of_vars env.Env.env env.Env.bi_vars in
  let fn = List.filter (fun (v,e) -> List.mem v vars) fn in
  let tabarr = MtbddUtil.make_table_action_array env.Env.env in
  let numarr = BddapronUtil.product_numequs env.Env.env tabarr fn in
  Log.debug3_o logger (MtbddUtil.print_array_mtbdd env.Env.env env.Env.cond (MtbddUtil.print_action env.Env.env))  "num-arr: " numarr;
  let tabarrset = MtbddUtil.make_table_action_arrset env.Env.env in
  let numarrset = MtbddUtil.arraymtbdd_to_arrsetmtbdd (compare) tabarrset 
    numarr in
  Log.debug3_o logger (MtbddUtil.print_arrset_mtbdd env.Env.env env.Env.cond (MtbddUtil.print_action env.Env.env))  "num-arr-set1: " numarrset;
  let numarrset = MtbddUtil.arrsetmtbdd_exists tabarrset numarrset binsupp in
  Log.debug3_o logger (MtbddUtil.print_arrset_mtbdd env.Env.env env.Env.cond (MtbddUtil.print_action env.Env.env))  "num-arr-set2: " numarrset;
  let boolguards = MtbddUtil.get_guards_mtbdd numarrset in
  let boolguards = List.map (Cudd.Bdd.exist env.Env.cond.Bdd.Cond.supp) 
    boolguards in
  let boolguards = Util.list2psette (compare) boolguards in
  Log.debug3_o logger (Format.pp_print_int)  "number of guards: " 
    (PSette.cardinal boolguards);
  Log.debug3_o logger (PSette.print ~sep:";" (BddapronUtil.print_boolexpr 
    env.Env.env env.Env.cond)) "guardset: " boolguards;
  boolguards

(******************************************************************************)
(** find a combination of actions *)
let nummodes2 vars env f assertion =
  let fn = Env.get_num_equs env f in
  Log.debug3_o logger (Env.print_equations env) "nequs=" fn;
  let binumsupp = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
    env.Env.cond.Bdd.Cond.supp 
    (BddapronUtil.supp_of_vars env.Env.env env.Env.bi_vars) 
  in
  let fn = List.filter (fun (v,e) -> List.mem v vars) fn in
  let tabarr = MtbddUtil.make_table_action_array env.Env.env in
  let numarr = BddapronUtil.product_numequs env.Env.env tabarr fn in
  Log.debug3_o logger (MtbddUtil.print_array_mtbdd env.Env.env env.Env.cond (MtbddUtil.print_action env.Env.env))  "num-arr: " numarr;
  let tabarrset = MtbddUtil.make_table_action_arrset env.Env.env in
  let numarrset = MtbddUtil.arraymtbdd_to_arrsetmtbdd (compare) tabarrset 
    numarr in
  Log.debug3_o logger (MtbddUtil.print_arrset_mtbdd env.Env.env env.Env.cond (MtbddUtil.print_action env.Env.env))  "num-arr-set1: " numarrset;
  let numarrset = MtbddUtil.arrsetmtbdd_exists tabarrset numarrset binumsupp in
  Log.debug3_o logger (MtbddUtil.print_arrset_mtbdd env.Env.env env.Env.cond (MtbddUtil.print_action env.Env.env))  "num-arr-set2: " numarrset;
  let boolguards = MtbddUtil.get_guards_mtbdd numarrset in
  Log.debug3_o logger (Format.pp_print_int)  "number of guards: " 
    (List.length boolguards);
  let boolguards = Util.list2psette (compare) boolguards in
  Log.debug3_o logger (PSette.print (BddapronUtil.print_boolexpr 
    env.Env.env env.Env.cond)) "guardset: " boolguards;
  boolguards
