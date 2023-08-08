(******************************************************************************)
(* cfg *)
(* control flow graph*)
(* author: Peter Schrammel *)
(* version: 0.9.3 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Cfg";
              Log.level=Log.Debug3}

(******************************************************************************)
(** {2 Types} *)
(******************************************************************************)

type locid_t = int
type arcid_t = int
type info_t = { mutable locidx : locid_t; mutable arcidx : arcid_t;}
type t = (locid_t, arcid_t, Loc.t, Arc.t, info_t) PSHGraph.t

(******************************************************************************)
(* Internal helpers *)
(******************************************************************************)
(* method used to check the feasibility of arcs *)
let check_reach env g f phi1 phi2 = BddapronAnalysis.check_bool_reach2 
  (BddapronUtil.get_primed_var env.Env.env) env.Env.env env.Env.cond
  g (Env.get_bool_equs env f) phi1 phi2

let locid_dummy = (1)
let arcid_dummy = (-1)
let cfg_compare = 
  let hcompare = {Hashhe.hash = Hashtbl.hash; Hashhe.equal = (==); } in
  {
    SHGraph.hashv = hcompare; 
    SHGraph.hashh = hcompare;
    SHGraph.comparev = (-);
    SHGraph.compareh = (-);
  }

let make_info () = {locidx = 1; arcidx = 1}

(* returns the next available location identifier *)
let next_locid cfg =
  let info = (PSHGraph.info cfg) in
  let id = info.locidx in
  info.locidx <- info.locidx + 1;
  (-id)
(* returns the next available arc identifier *)
let next_arcid cfg =
  let info = (PSHGraph.info cfg) in
  let id = info.arcidx in
  info.arcidx <- info.arcidx + 1;
  id

(******************************************************************************)
(** {2 Printing} *)
(******************************************************************************)

let print_loc env fmt locid loc =
  let strfmt = Format.str_formatter in
  Format.pp_print_string strfmt ("("^(string_of_int locid)^") ");
  Loc.print env strfmt loc;
  let str = Format.flush_str_formatter () in
  Format.pp_print_string fmt (Util.string_compact str)

let print_arc env fmt arcid arc =
  Format.pp_print_string fmt ("("^(string_of_int arcid)^") ");
  Arc.print_type env fmt arc;
  Format.pp_print_string fmt "\\n";
  Arc.print env fmt arc

let print_dot env fmt cfg print_arcs =
  PSHGraph.print_dot
    ~style:"size=\"7.5,10\";center=true;ranksep=0.1;nodesep=0.1;"
    ~hedgestyle:"shape=plaintext,fontsize=10,height=0.01,width=0.01"
    ~vertexstyle:"shape=box,fontsize=10,height=0.01,width=0.01"
    (Format.pp_print_int)
    (Format.pp_print_int)
    (fun fmt locid loc -> print_loc env fmt locid loc)
    (fun fmt arcid arc -> if print_arcs then print_arc env fmt arcid arc 
      else 
	begin 
          Format.pp_print_string fmt ("("^(string_of_int arcid)^") ");
          Arc.print_type env fmt arc
        end)
    fmt cfg

let print_short env fmt cfg =
  let (cntv,cnth,_,_) = PSHGraph.size cfg in
  Format.pp_print_int fmt cntv;
  Format.pp_print_string fmt " locations and ";
  Format.pp_print_int fmt cnth;
  Format.pp_print_string fmt " arcs";

  PSHGraph.iter_vertex cfg
    (fun v loc ~pred:pred ~succ:succ ->
      Format.pp_print_newline fmt ();
      Format.pp_print_string fmt "LOC ";
      Format.pp_print_int fmt v;
      Format.pp_print_string fmt ": arcs(in/out/loop)=(";
      let arcpred = (PSette.cardinal pred) in
      let arcsucc = (PSette.cardinal succ) in
      let arcloop = List.length (List.filter
          (fun h ->
            let sv = Array.get (PSHGraph.succvertex cfg h) 0 in
            (sv=v))
        (Util.psette2list succ)) 
      in
      Format.pp_print_int fmt (arcpred-arcloop);
      Format.pp_print_string fmt ",";
      Format.pp_print_int fmt (arcsucc-arcloop);
      Format.pp_print_string fmt ",";
      Format.pp_print_int fmt arcloop;
      Format.pp_print_string fmt "): ";
      Loc.print env fmt loc)

(******************************************************************************)
(** {2 Operations on locations} *)
(******************************************************************************)


let empty_locidset = PSette.empty (cfg_compare.SHGraph.comparev)
let get_locidset cfg =
  let set = ref (empty_locidset) in
  PSHGraph.iter_vertex
    cfg
    (fun locid i ~pred:p ~succ:s -> set := PSette.add locid !set);
  !set

(******************************************************************************)

let get_only_locid cfg = 
  assert((PSHGraph.size_vertex cfg)=1);
  (-1)

let get_only_loc cfg =
  assert((PSHGraph.size_vertex cfg)=1);
  PSHGraph.attrvertex cfg (-1)


(******************************************************************************)
let get_locidset_by_inv env cfg expr =
  PSette.filter
    (fun locid ->
      let inv = Loc.get_inv (PSHGraph.attrvertex cfg locid) in
      not (Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond
        (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond inv expr))
    )
    (get_locidset cfg)  

(******************************************************************************)
(* adds a location *)
let add_loc env cfg loc =
  let locid = next_locid cfg in
  Log.debug3_o logger (Loc.print env) 
    ("add loc: locid="^(string_of_int locid)^", loc=") loc;
  PSHGraph.add_vertex cfg locid loc;
  locid

(******************************************************************************)
(* adds n locations *)
let add_locs env cfg inv n =
  let newlocs = ref(empty_locidset) in
  for i=1 to n do 
  begin
    let locid = next_locid cfg in
    PSHGraph.add_vertex cfg locid inv;
    newlocs := PSette.add locid !newlocs;
  end done;
  !newlocs

(******************************************************************************)
(* removes locations that are unconnnected or 
   not reachable from the init locations via a path *) 
let remove_unreachable_locs env cfg initial = 
  let locs = PSette.diff (get_locidset cfg) (get_locidset_by_inv env cfg initial) in
  PSette.iter
    (fun v ->
      let preds = PSHGraph.pred_vertex cfg v in
      if ((PSette.cardinal preds)=0) || 
         (((PSette.cardinal preds)=1)&&((PSette.choose preds)=v)) then
        PSHGraph.remove_vertex cfg v)
    locs


(******************************************************************************)
(** {2 Retrieving and filtering arcs} *)
(******************************************************************************)

let get_arc cfg arcid = PSHGraph.attrhedge cfg arcid
let get_loc cfg locid = PSHGraph.attrvertex cfg locid

let get_succlocid cfg arcid = 
  let arr = PSHGraph.succvertex cfg arcid in
  assert((Array.length arr)=1);
  arr.(0)

let get_predlocid cfg arcid = 
  let arr = PSHGraph.predvertex cfg arcid in
  assert((Array.length arr)=1);
  arr.(0)

let get_only_arc env cfg =
  assert((PSHGraph.size_hedge cfg)>=1);
  PSHGraph.attrhedge cfg 1

(******************************************************************************)
let arcs_compare (a1,_,_,_) (a2,_,_,_) =
      cfg_compare.SHGraph.compareh a1 a2
let arcs_compare2 (a1,_,_,_,_) (a2,_,_,_,_) =
      cfg_compare.SHGraph.compareh a1 a2
let flowarcs_compare (a1,_,_) (a2,_,_) =
      cfg_compare.SHGraph.compareh a1 a2

let get_arcs cfg predicate =
  PSHGraph.fold_hedge cfg
    (fun arcid arc ~pred ~succ arcset ->
      match predicate arc with
	|true -> 
          assert((Array.length pred)=1);
          assert((Array.length succ)=1);
          PSette.add (arcid,arc,pred.(0),succ.(0)) arcset
	|false -> arcset)
    (PSette.empty arcs_compare)

let get_arcs2 cfg predicate =
  PSHGraph.fold_hedge cfg
    (fun arcid arc ~pred ~succ arcset ->
      match predicate arc with
	|Some(ass,f) -> 
          assert((Array.length pred)=1);
          assert((Array.length succ)=1);
          PSette.add (arcid,ass,f,pred.(0),succ.(0)) arcset
	|None -> arcset)
    (PSette.empty arcs_compare2)

let get_normal_arcs cfg = 
  get_arcs2 cfg (fun arc ->  match arc with 
    |Arc.Normal(ass,f) -> Some(ass,f) |_ -> None)
let get_loop_arcs cfg = 
  get_arcs2 cfg (fun arc ->  match arc with 
    |Arc.Loop(ass,f) -> Some(ass,f) |_ -> None)
let get_accel_arcs cfg = 
  get_arcs2 cfg (fun arc ->  match arc with 
    |Arc.Accel(ass,f) -> Some(ass,f) |_ -> None)
let get_boolaccel_arcs cfg = 
  get_arcs2 cfg (fun arc ->  match arc with 
    |Arc.BoolAccel(ass,f) -> Some(ass,f) |_ -> None)
let get_boolnaccaccel_arcs cfg = 
  get_arcs2 cfg (fun arc ->  match arc with 
    |Arc.BoolNaccAccel(ass,f) -> Some(ass,f) |_ -> None)
let get_bool_arcs cfg = 
  get_arcs2 cfg (fun arc ->  match arc with 
    |Arc.Bool(ass,f) -> Some(ass,f) |_ -> None)
let get_boolnacc_arcs cfg = 
  get_arcs2 cfg (fun arc ->  match arc with 
    |Arc.BoolNacc(ass,f) -> Some(ass,f) |_ -> None)

let get_init_arcs env initial cfg = PSette.fold 
    (fun loc set -> PSette.union set (PSHGraph.succhedge cfg loc))
    (get_locidset_by_inv env cfg initial) (PSette.empty (compare))

let get_normal_arcs_in_loc cfg loc = 
  PSHGraph.fold_hedge cfg
    (fun arcid arc ~pred ~succ arcset ->
      match arc with
	|Arc.Normal(ass,f) -> 
          assert((Array.length pred)=1);
          if pred.(0)=loc then
          begin
            assert((Array.length succ)=1);
            PSette.add (arcid,ass,f,pred.(0),succ.(0)) arcset
          end
          else arcset
	|_ -> arcset)
    (PSette.empty arcs_compare2)
let get_flow_arcs_in_loc cfg loc = 
  PSHGraph.fold_hedge cfg
    (fun arcid arc ~pred ~succ arcset ->
      match arc with
	|Arc.Flow(ass,f) -> 
          assert((Array.length pred)=1);
          if pred.(0)=loc then
          begin
            assert((Array.length succ)=1);
            PSette.add (arcid,ass,f) arcset
          end
          else arcset
	|_ -> arcset)
    (PSette.empty flowarcs_compare)

(******************************************************************************)
(* returns the flow transition of a location *)
let get_flow_arc env cfg loc =
  try
    let arcid = PSette.choose (PSette.filter 
      (fun arcid -> 
         match PSHGraph.attrhedge cfg arcid with
  	 |Arc.Flow(_) -> true |_ -> false) 
      (PSHGraph.succhedge cfg loc)) 
    in
    let (a,f) = Arc.get_ass_equs env (PSHGraph.attrhedge cfg arcid) in
    Some (arcid,a,f)
  with Not_found -> None
(*    (0,Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond,[]) *)

(******************************************************************************)
(** {2 Adding and (re)moving arcs} *)
(******************************************************************************)

(******************************************************************************)
(* add_arc2 *)
(* adds an arc *)
let add_arc env cfg sloc dloc arc =
  let arcid = next_arcid cfg in
  Log.debug3_o logger (Format.pp_print_int) ("adding arc ("^(string_of_int sloc)^" -> "^(string_of_int dloc)^"): ") arcid;
  Log.debug3_o logger (Arc.print env) "arc = " arc;
  PSHGraph.add_hedge cfg arcid arc
    ~pred:[|sloc|] ~succ:[|dloc|];
  arcid

(******************************************************************************)
(* add_arc *)
(* adds an arc (if the destination location is reachable *)
let add_arc_if_reach env cfg sloc dloc arc assertion =
  match arc with
    |Arc.Flow(_,_) -> Some(add_arc env cfg sloc dloc arc)
    |_ ->
    begin
      let sinv =  Loc.get_inv (PSHGraph.attrvertex cfg sloc) in
      let dinv =  PSHGraph.attrvertex cfg dloc in
      let  afl = Arc.simplify env arc sinv in
      let (ass,fl) = Arc.get_ass_equs env afl in
      (* check boolean reachability *)
      let sinvb = Cudd.Bdd.exist env.Env.cond.Bdd.Cond.supp sinv in
      let dinvb = Cudd.Bdd.exist env.Env.cond.Bdd.Cond.supp dinv in
      if check_reach env
          (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
          (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond ass assertion)
           env.Env.cond.Bdd.Cond.careset) fl sinvb dinvb
      then
        Some (add_arc env cfg sloc dloc afl)
      else None
    end

(******************************************************************************)
(* add_arc4 *)
(* adds an arc (if the destination location is reachable *)
let add_arc_if_reach2 env cfg sloc sreach dloc af =
  let dinv =  PSHGraph.attrvertex cfg dloc in
  if BddapronAnalysis.check_bool_reach3 env.Env.env env.Env.cond 
    sreach dinv then 
    Some (add_arc env cfg sloc dloc af)
  else None

(******************************************************************************)

let preds_succs_loops cfg loc =
  let pred = PSHGraph.predhedge cfg loc in
  let succ = PSHGraph.succhedge cfg loc in
  let loops = PSette.inter pred succ in
  (PSette.diff pred loops,PSette.diff succ loops,loops)

let preds_succs_loops2 cfg pred succ =
  let loops = PSette.inter pred succ in
  (PSette.diff pred loops,PSette.diff succ loops,loops)

(* moves the successor arcs from loc to newloc *)
let move_succs_to env cfg loc newloc succs =
 PSette.iter
  (fun s ->
    let dloc = PSHGraph.succvertex cfg s in
    let afp = PSHGraph.attrhedge cfg s in
    PSHGraph.remove_hedge cfg s;
    PSHGraph.add_hedge cfg s afp [|newloc|] dloc)
  succs

(* removes arcs from loc *)
let remove_arcs cfg loc arcidset =
 PSette.iter (fun s -> PSHGraph.remove_hedge cfg s) arcidset
   
(******************************************************************************)
(* removes the arc if it is boolean infeasible *) 
let remove_infeasible_arc env cfg assertion arcid =
  let arc = PSHGraph.attrhedge cfg arcid in
  begin match arc with
    |Arc.Flow(_,_) -> ()
    |Arc.Id ->
    begin
      let dinv= PSHGraph.attrvertex cfg ((PSHGraph.succvertex cfg arcid).(0)) in
      let sinv= PSHGraph.attrvertex cfg ((PSHGraph.predvertex cfg arcid).(0)) in
      if Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond
        (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond dinv      
          (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond sinv
            (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond assertion
               env.Env.cond.Bdd.Cond.careset)))
        then PSHGraph.remove_hedge cfg arcid
    end
    |_ -> 
    begin
      let (ass,f) = Arc.get_ass_equs env arc in
      let dinv= PSHGraph.attrvertex cfg ((PSHGraph.succvertex cfg arcid).(0)) in
      let sinv= PSHGraph.attrvertex cfg ((PSHGraph.predvertex cfg arcid).(0)) in
      if not(check_reach env 
        (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
          (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond ass assertion)
             env.Env.cond.Bdd.Cond.careset)
          f sinv dinv)   
        then PSHGraph.remove_hedge cfg arcid
    end
  end;
  cfg

(* removes boolean infeasible arcs *) 
let remove_infeasible_arcs env cfg assertion =
  PSHGraph.iter_hedge cfg
    (fun arcid arc ~pred ~succ ->
      let arc = PSHGraph.attrhedge cfg arcid in
      match arc with
        |Arc.Flow(_,_) -> ()
        |_ -> 
        begin
          let (ass,f) = Arc.get_ass_equs env arc in
          let dinv = PSHGraph.attrvertex cfg (succ.(0)) in
          let sinv = PSHGraph.attrvertex cfg (pred.(0)) in
          if not(check_reach env 
            (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
              (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond ass assertion)
                env.Env.cond.Bdd.Cond.careset)
              f sinv dinv) 
            then PSHGraph.remove_hedge cfg arcid
        end);
   cfg

(* removes boolean infeasible incoming/outgoing arcs in loc *) 
let remove_infeasible_arcs_in_loc env cfg assertion loc =
  PSette.iter 
    (fun a -> let _ = remove_infeasible_arc env cfg assertion a in ())
    (PSette.union (PSHGraph.succhedge cfg loc) (PSHGraph.predhedge cfg loc));
  cfg

let update_location_with env cfg loc inv assertion =
  Log.debug3_o logger (Env.print_boolexpr env) 
    ("update location: "^(string_of_int loc)^" newinv = ") inv;

  let succarcs = PSHGraph.succhedge cfg loc in
  let predarcs = PSette.diff (PSHGraph.predhedge cfg loc) succarcs in

  PSette.iter 
    (fun arcid -> 
      let (ass,f) = Arc.get_ass_equs env (PSHGraph.attrhedge cfg arcid) in
      let fb = Env.get_bool_equs env f in
      let sinv = PSHGraph.attrvertex cfg (get_predlocid cfg arcid) in
      if not(BddapronAnalysis.check_bool_reach2 
        (BddapronUtil.get_primed_var env.Env.env)
        env.Env.env env.Env.cond    
        (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
          (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond ass assertion)
          env.Env.cond.Bdd.Cond.careset)
        fb sinv inv) 
      then PSHGraph.remove_hedge cfg arcid)
    predarcs;
  PSette.iter 
    (fun arcid -> 
      let succ = PSHGraph.succvertex cfg arcid in
      assert((Array.length succ)=1);
      let arc = PSHGraph.attrhedge cfg arcid in
      let (ass,f) = Arc.get_ass_equs env arc in
      let dinv = PSHGraph.attrvertex cfg (succ.(0)) in
      let fs2 = Arc.replace_ass_equs arc (ass,BddapronUtil.simplify_equs 
        env.Env.env env.Env.cond f inv) in
      let fexprlist = BddapronUtil.get_fexprlist 
        (BddapronUtil.get_primed_var env.Env.env)
          env.Env.env env.Env.cond (Env.get_bool_equs env f) in
      let ass2 = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
            (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond ass assertion)
               env.Env.cond.Bdd.Cond.careset
      in
      let freach1 = BddapronAnalysis.get_freach env.Env.env env.Env.cond 
        dinv ass2 fexprlist in
      let _ = add_arc_if_reach2 env cfg loc freach1 
        (get_succlocid cfg arcid) fs2 in
      PSHGraph.remove_hedge cfg arcid)
    succarcs;
  ()

let copy_location_with env cfg loc1 inv2 assertion =
  let loc2 = add_loc env cfg (Loc.make_inv inv2) in
  Log.debug3_o logger (Env.print_boolexpr env) 
    "add location: inv = " inv2;

  let predarcs = PSHGraph.predhedge cfg loc1 in
  let succarcs = PSette.diff (PSHGraph.succhedge cfg loc1) predarcs in
  let fid = BddapronUtil.get_id_equs_for env.Env.env env.Env.cond 
    env.Env.s_vars in

  (* for all predecessors p of loc 
       (includes self-loops and edges between l1 and l2):
       - add edges from p to l1 and l2
  *)
  PSette.iter 
    (fun pe -> 
      let parr = PSHGraph.predvertex cfg pe in
      if (Array.length parr)>0 then
      begin
        let fl = PSHGraph.attrhedge cfg pe in
        Array.iter
          (fun p -> 
            if p<>loc1 then 
              let _ = add_arc_if_reach env cfg p loc2 fl assertion in ()
            else
            begin
              let _ = add_arc_if_reach env cfg loc2 loc2 fl assertion in
              match fl with
		|Arc.Flow(ass,f) -> 
                  let flid = Arc.Normal(ass,fid) in
                  let _ = add_arc_if_reach env cfg loc2 loc1 flid assertion in
                  let _ = add_arc_if_reach env cfg loc1 loc2 flid assertion in 
                  ()
		|_ -> 
                  let _ = add_arc_if_reach env cfg loc2 loc1 fl assertion in
                  let _ = add_arc_if_reach env cfg loc1 loc2 fl assertion in ()
(*              PSHGraph.remove_hedge cfg pe;
              add_arc env cfg loc1 loc1 fl assertion *)
            end)
          parr
        end)
    predarcs;

  (* for all successors s of loc:
       - add edges from l1 to s, l2 to s
  *)
  PSette.iter 
    (fun se -> 
      let sarr = PSHGraph.succvertex cfg se in
      if (Array.length sarr)>0 then
      begin
        let fs = PSHGraph.attrhedge cfg se in
        let (ass,f) = Arc.get_ass_equs env fs in
        let fs2 = Arc.replace_ass_equs fs (ass,BddapronUtil.simplify_equs 
          env.Env.env env.Env.cond f inv2) in
        let fexprlist = BddapronUtil.get_fexprlist 
          (BddapronUtil.get_primed_var env.Env.env)
          env.Env.env env.Env.cond (Env.get_bool_equs env f) in
        let ass2 = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
            (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond ass assertion)
               env.Env.cond.Bdd.Cond.careset
        in
        let freach1 = BddapronAnalysis.get_freach env.Env.env env.Env.cond
          inv2 ass2 fexprlist in

        Array.iter
          (fun s -> let _ = add_arc_if_reach2 env cfg loc2 freach1 s fs2 in ())
          sarr
      end)
    succarcs;
  (cfg,loc2)

(******************************************************************************)
(** {2 Splitting Locations}  *)
(******************************************************************************)

(******************************************************************************)
(* splits a vertex (location) of the CFG by the given expression phi *)
(* factorizes the computation of the transition relation for successors *)
let split_location env cfg loc phi assertion =
  let inv = PSHGraph.attrvertex cfg loc in

  Log.debug3_o logger (Loc.print env) "split location: " inv;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond) 
    "split location by: " phi;

  let (inv1,inv2) = BddapronUtil.split_boolexpr env.Env.env env.Env.cond 
    inv phi in
  if not((Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond inv1) || 
         (Bddapron.Expr0.Bool.is_false env.Env.env env.Env.cond inv2)) then
  begin

  (* add new locations *)
  let loc1 = add_loc env cfg inv1 in
  let loc2 = add_loc env cfg inv2 in

  (* for all predecessors p of loc 
       (includes self-loops and edges between l1 and l2):
       - add edges from p to l1 and l2
  *)
  PSette.iter 
    (fun pe -> 
      let parr = PSHGraph.predvertex cfg pe in
      if (Array.length parr)>0 then
      begin
        let fl = PSHGraph.attrhedge cfg pe in
        match fl with
	  |Arc.Flow(_) -> 
          begin
            let _ = add_arc_if_reach env cfg loc1 loc1 fl assertion in
            let _ = add_arc_if_reach env cfg loc2 loc2 fl assertion in ()
          end
	  |_ -> 
          begin
          Array.iter
          (fun p -> 
            if p<>loc then 
              let _ = add_arc_if_reach env cfg p loc1 fl assertion in ()
            else
            begin
              let _ = add_arc_if_reach env cfg loc1 loc1 fl assertion in
              let _ = add_arc_if_reach env cfg loc2 loc1 fl assertion in ()
            end
            )
            parr;
          Array.iter
          (fun p -> 
            if p<>loc then 
              let _ = add_arc_if_reach env cfg p loc2 fl assertion in ()
            else
            begin
              let _ = add_arc_if_reach env cfg loc2 loc2 fl assertion in
              let _ = add_arc_if_reach env cfg loc1 loc2 fl assertion in ()
            end
           )
          parr
         end
        end
    )
   (PSHGraph.predhedge cfg loc);

  (* remove all edges to loc *)
  PSette.iter 
    (fun e -> PSHGraph.remove_hedge cfg e) 
    (PSHGraph.predhedge cfg loc);

  (* for all successors s of loc:
       - add edges from l1 to s, l2 to s
  *)
  PSette.iter 
    (fun se -> 
      let sarr = PSHGraph.succvertex cfg se in
      if (Array.length sarr)>0 then
      begin
        let fs = PSHGraph.attrhedge cfg se in
        let (ass,f) = Arc.get_ass_equs env fs in
        let fs1 = Arc.simplify env fs inv1 in
        let fs2 = Arc.simplify env fs inv2 in
        let fexprlist = BddapronUtil.get_fexprlist
          (BddapronUtil.get_primed_var env.Env.env) 
          env.Env.env env.Env.cond
          (Env.get_bool_equs env f) in
        let ass2 = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
            (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond ass assertion)
               env.Env.cond.Bdd.Cond.careset
        in
        let freach1 = BddapronAnalysis.get_freach env.Env.env env.Env.cond 
          inv1 ass2 fexprlist in
        let freach2 = BddapronAnalysis.get_freach env.Env.env env.Env.cond 
          inv2 ass2 fexprlist in

        Array.iter
          (fun s -> 
            let _ = add_arc_if_reach2 env cfg loc1 freach1 s fs1 in 
            let _ = add_arc_if_reach2 env cfg loc2 freach2 s fs2 in ()
          )
          sarr
      end
    )
    (PSHGraph.succhedge cfg loc);

  (* remove loc *)
  PSHGraph.remove_vertex cfg loc;
  [loc1;loc2]
  end
  else [loc]

(******************************************************************************)
(** {2 Creating a CFG} *)
(******************************************************************************)
(** creates a CFG with the invariant inv and a discrete selfloop df and 
     continuous evolution cf *)
let make env df cf inv =
  let cfg = PSHGraph.create cfg_compare 0 (make_info ()) in
  let locid = add_loc env cfg (Loc.make_inv inv) in
  let _ = add_arc env cfg locid locid 
    (Arc.Normal(Bddapron.Expr0.Bool.dtrue env.Env.env env.Env.cond,df)) in
  if not (Util.list_is_empty cf) then 
  begin
    let _ = add_arc env cfg locid locid 
    (Arc.Flow((Bddapron.Expr0.Bool.dtrue env.Env.env env.Env.cond),cf)) in ();
  end;
  cfg

(******************************************************************************)
(** creates an empty CFG *)
let make_empty () = PSHGraph.create cfg_compare 0 (make_info ())

(******************************************************************************)
(* creates an CFG partitioned by the given partition-exprs *)
let make_partitioned env (cfg:t) exprs assertion =
  match exprs with
    |[] -> cfg
    |expr::exprs -> 
    begin
      let loc = get_only_locid cfg in
      PSHGraph.replace_attrvertex cfg loc expr;
      let flowarc = get_flow_arc env cfg loc in
      let (_,ass,jump,_,_) = PSette.choose (get_normal_arcs cfg) in
      List.iter
        (fun e ->
          let loc = add_loc env cfg e in
          begin match flowarc with |None -> () |Some (_,stay,flow) -> 
            let _ = add_arc env cfg loc loc (Arc.Flow(stay,flow)) in ();
          end;
          let _ = add_arc_if_reach env cfg loc loc 
            (Arc.Normal (ass,jump)) assertion in
          PSHGraph.iter_vertex cfg
            (fun dloc _ ~pred:_ ~succ:_ -> 
               Log.debug3_o logger Format.pp_print_int "dloc: " dloc;
               if loc<>dloc then
                 let _ = add_arc_if_reach env cfg loc dloc 
                   (Arc.Normal (ass,jump)) assertion in 
                 let _ = add_arc_if_reach env cfg dloc loc 
                   (Arc.Normal (ass,jump)) assertion in()))
        exprs;
      cfg
    end
 
(******************************************************************************)
(** duplicates a CFG *)
let copy cfg = PSHGraph.copy (fun _ loc -> loc) (fun _ arc -> arc) 
  (fun info -> info) cfg

(******************************************************************************)
(** computes the map of corresponding locations assuming that cfg1 <= cfg2 *)
let get_locmap env cfg1 cfg2 =
  let locs1 = PSette.elements (get_locidset cfg1) in
  let locs2 = PSette.elements (get_locidset cfg2) in
  let is_contained cfg2 inv1 loc2 =
    let inv2 = PSHGraph.attrvertex cfg2 loc2 in
    Bddapron.Expr0.Bool.is_leq env.Env.env env.Env.cond inv2 inv1
  in
  List.map (fun loc1 ->
    let inv1 = PSHGraph.attrvertex cfg1 loc1 in
    (loc1,List.filter (is_contained cfg2 inv1) locs2))
  locs1
