(******************************************************************************)
(* stratiter *)
(* wrapper for (max) strategy iteration *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

exception NotSupported of string
exception NotPurelyNumerical

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Stratiter";
              Log.level=Log.Debug3}

(******************************************************************************)
(* Reduction to numerical CFG *)
(******************************************************************************)

let extract_num_equs env cfg initial assertion =
  Log.debug3 logger "extracting numerical equations...";
  let boolsupp = BddapronUtil.supp_of_vars env.Env.env env.Env.b_vars in
  let extract_num_equs_arc make_arc check_actions (arcid,ass,f,sloc,dloc) =
    let fb = Env.get_bool_equs env f in
    let sinv = Loc.get_inv (Cfg.get_loc cfg sloc) in
    let dinv = Loc.get_inv (Cfg.get_loc cfg dloc) in
    let dinvfb = Bddapron.Expr0.Bool.substitute env.Env.env env.Env.cond 
                  dinv fb in
    let ass = Cudd.Bdd.dand assertion ass in
    let g = Cudd.Bdd.dand ass (Cudd.Bdd.dand sinv dinvfb) in
    Log.debug3_o logger (Env.print_boolexpr env) "g: " g;
    if not (Cudd.Bdd.is_false g) then
      begin
	let fn = BddapronUtil.simplify_equs env.Env.env env.Env.cond 
          (Env.get_num_equs env f) g in
	Log.debug3_o logger (Env.print_equations env) "fn: " fn;
	let (vars,_) = Env.split_equs fn in
        let tabarr = MtbddUtil.make_table_action_array env.Env.env in
        let numarr = BddapronUtil.product_numequs env.Env.env tabarr fn in
        let tabarrset = MtbddUtil.make_table_action_arrset env.Env.env in
        let fnset = MtbddUtil.arraymtbdd_to_arrsetmtbdd (compare) 
          tabarrset numarr in
        Log.debug3_o logger (MtbddUtil.print_arrset_mtbdd env.Env.env 
             env.Env.cond (MtbddUtil.print_action env.Env.env))  
          "fnset: " fnset;
	let fnite = Cudd.Mtbdd.ite g fnset 
          (MtbddUtil.arrsetmtbdd_empty ~compare_array:compare tabarrset
             env.Env.cuddman) in
        Log.debug3_o logger (MtbddUtil.print_arrset_mtbdd env.Env.env 
            env.Env.cond (MtbddUtil.print_action env.Env.env))  
          "fnite: " fnite;
        let fnnum = MtbddUtil.arrsetmtbdd_exists tabarrset fnite boolsupp in
        Log.debug3_o logger (MtbddUtil.print_arrset_mtbdd env.Env.env 
            env.Env.cond (MtbddUtil.print_action env.Env.env))  
          "fnnum: " fnnum;
	let guardleaves = Cudd.Mtbdd.guardleafs fnnum in
	let aflist = Array.fold_left 
	  (fun arclist (gg,arrset) -> 
	    if PSette.is_empty arrset then arclist
	    else
	    begin
	      let ass2 = Cudd.Bdd.tdrestrict (Cudd.Bdd.dand g gg) 
                env.Env.cond.Bdd.Cond.careset in
              let ass2list = BddapronUtil.boolexpr_to_dnf env.Env.env ass2 in
              List.fold_left (fun arclist ass2 -> 
	        PSette.fold (fun arr arclist -> 
                  if (sloc<>dloc) or 
                     (check_actions vars (Array.to_list arr))  then
		    let f2 = List.map2 
		      (fun v a -> (v,
		        `Apron (BddapronUtil.apronexpr_to_apronexprDD 
		  	   env.Env.env a)))
		      vars (Array.to_list arr)
		    in
		    (make_arc (ass2,f2))::arclist
                  else arclist)
		arrset arclist)
                arclist ass2list
	    end)
  	   [] guardleaves
         in
         List.iter (fun arc -> 
           let _ = Cfg.add_arc env cfg sloc dloc arc in ()) aflist;
      end;
     PSHGraph.remove_hedge cfg arcid  
  in

(* check for identities and zero-evolution*)
  let rec not_is_identity_actions vars actions =
    match (vars,actions) with
      |([],[]) -> false
      |(v::vars,a::actions) -> 
      begin
        match a with
	  |Bddapron.Apronexpr.Lin(a) ->
            if a=(Bddapron.Apronexpr.Lin.var v)
              then not_is_identity_actions vars actions
            else true 
	  |_ -> true
      end 
      |_ -> assert false
  in
  let rec not_is_zero_actions vars actions =
    match (vars,actions) with
      |([],[]) -> false
      |(v::vars,a::actions) -> 
      begin
        match a with
	  |Bddapron.Apronexpr.Lin(a) ->
            if a=Bddapron.Apronexpr.Lin.zero
              then not_is_identity_actions vars actions
            else true 
	  |_ -> true
      end 
      |_ -> assert false
  in

  (* extract num_tr_fcts *)
  PSette.iter (extract_num_equs_arc (fun (a,f) -> Arc.Normal(a,f))
      not_is_identity_actions) 
    (Cfg.get_arcs2 cfg (fun arc -> match arc with 
      |Arc.Normal(a,f) -> Some (a,f) 
      |_ -> None));
  PSette.iter (extract_num_equs_arc (fun (a,f) -> Arc.Flow(a,f))
      not_is_zero_actions) 
    (Cfg.get_arcs2 cfg (fun arc -> match arc with 
                         |Arc.Flow(a,f) -> Some (a,f) |_ -> None));
  (* remove unconnected locations *)
  let (unreach,_) = PSHGraph.reachable_multi 
    Cfg.locid_dummy Cfg.arcid_dummy cfg 
    (Cfg.get_locidset_by_inv env cfg initial) in
  PSHGraph.iter_vertex cfg
    (fun v inv ~pred ~succ ->
      if PSette.mem v unreach then PSHGraph.remove_vertex cfg v);
  cfg

let print_locmap fmt locmap =
  List.iter (fun (loc,locs) ->
    Format.pp_print_newline fmt ();
    Format.pp_print_int fmt loc; 
    Format.pp_print_string fmt " --> "; 
    Util.list_print (Format.pp_print_int) fmt locs) locmap

let print_anres env doman fmt anres = 
  let print_res_loc locid s =
    Format.pp_print_newline fmt ();
    let strfmt = Format.str_formatter in
    Format.pp_print_string strfmt "LOC ";
    Format.pp_print_int strfmt locid;
    Format.pp_print_string strfmt ": reach = ";
    let str1 = Format.flush_str_formatter () in
    Bddapron.Domain0.print doman env.Env.env strfmt s;
    let str2 = Format.flush_str_formatter () in
    Format.pp_print_string logger.Log.fmt (str1^(Util.string_compact str2))
  in
  List.iter (fun (locid,s) -> print_res_loc locid s) anres 

(* refines a CFG according to the given analysis result *)
let boolrefine env doman cfg initial assertion anres =
  Log.debug3 logger "refining by analysis result...";
  (* replace invariants *)
  let locmap = List.fold_left
    (fun locmap (v,s) -> 
      if Bddapron.Domain0.is_bottom doman s then 
      begin
        PSHGraph.remove_vertex cfg v; 
        (v,[])::locmap
      end
      else
	begin 
        let inv = PSHGraph.attrvertex cfg v in
        let slist = Bddapron.Domain0.to_bddapron doman s in
        let (cfg,locs) = List.fold_left
          (fun (cfg,locs) (b,_) ->
            let newinv = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond inv
              b in
            let (cfg,newv) = Cfg.copy_location_with env cfg v newinv 
              assertion in
            (cfg,newv::locs))
          (cfg,[]) slist in
        PSHGraph.remove_vertex cfg v;
        (v,locs)::locmap
      end)
    [] anres
  in
  (* check feasibility of arcs*)
  (* Cfg.remove_infeasible_arcs env cfg param.assertion; *)
  (* remove unconnected locations *)
  let (unreach,_) = PSHGraph.reachable_multi 
    Cfg.locid_dummy Cfg.arcid_dummy cfg 
    (Cfg.get_locidset_by_inv env cfg initial) in
  PSHGraph.iter_vertex cfg
    (fun v inv ~pred ~succ ->
      if PSette.mem v unreach then PSHGraph.remove_vertex cfg v);
  (cfg,locmap)

let rec find_in_locmap locmap v =
  match locmap with
    |[] -> (v,[])
    |(mv,locs)::locmaptl -> 
      if List.mem v locs then (mv,List.filter (fun x -> x<>v) locs)
      else find_in_locmap locmaptl v

let split_locs_by_flow_arcs env cfg locmap assertion =
  Log.debug3 logger "splitting locations by flow transitions...";
  PSHGraph.fold_vertex cfg
    (fun v inv ~pred ~succ (cfg,locmap) ->
      let flowarcs = Cfg.get_flow_arcs_in_loc cfg v in
      if (PSette.cardinal flowarcs)<=1 then (cfg,locmap)
      else
        let cfg = PSette.fold (fun (arcid,_,_) cfg ->
            PSHGraph.remove_hedge cfg arcid; cfg) flowarcs cfg in
        let (mappedv,locs) = find_in_locmap locmap v in
        let (cfg,newlocs) = 
          PSette.fold (fun (_,ass,f) (cfg,locs) ->
            let (cfg,newv) = Cfg.copy_location_with env cfg v inv 
              assertion in
            let _ = Cfg.add_arc env cfg newv newv (Arc.Flow(ass,f)) in 
            let locs = newv::locs in
            (cfg,locs))
          flowarcs (cfg,[])
        in
        let cfg = List.fold_left (fun cfg v1 -> 
          List.fold_left (fun cfg v2 -> 
            if v1<>v2 then
              let _ = Cfg.add_arc env cfg v1 v2 Arc.Id in 
              cfg
            else cfg) cfg newlocs)
          cfg newlocs
        in
        PSHGraph.remove_vertex cfg v;
        let locmap = List.remove_assoc mappedv locmap in
        (cfg,(mappedv,List.append newlocs locs)::locmap))
  (cfg,locmap)

(* reduce CFG to numerical equations *)
let reduce_to_numcfg anres env doman cfg initial assertion = 
  let numcfg = PSHGraph.copy 
    (fun v inv -> inv) (fun a arc -> arc) (fun info -> info) cfg in
  let (numcfg,locmap) = boolrefine env doman numcfg initial assertion anres in
  let numcfg = extract_num_equs env numcfg initial assertion in
  let (numcfg,locmap) = split_locs_by_flow_arcs env numcfg locmap assertion in
  Log.debug3_o logger (print_locmap) "locmap: " locmap;
(*    let dotfile = open_out "tr.dot" in
    let dotfmt = Format.formatter_of_out_channel dotfile in
      Cfg.print_dot env dotfmt numcfg true; *)
  (numcfg,locmap)

(******************************************************************************)
(* Interface matching *)
(******************************************************************************)

type numres_t = (int * Polka.loose Polka.t Apron.Abstract1.t) list 

module MyHa = Maxstrat.Ha.StringLocStringVarHybridAutomaton

(* converts APRON coefficients into STRATIMP coefficients *)
let scalar_to_numinf c =
  if (Apron.Scalar.is_infty c>0) then Maxstratutil.Num.InfGmpRatio.Infty
  else if (Apron.Scalar.is_infty c<0) then Maxstratutil.Num.InfGmpRatio.Neginfty
  else if Apron.Scalar.equal_int c 1 then Maxstratutil.Num.InfGmpRatio.one
  else if Apron.Scalar.equal_int c (-1) then 
    Maxstratutil.Num.InfGmpRatio.minus_one
  else Maxstratutil.Num.InfGmpRatio.Num(
         Maxstratutil.Num.GmpRatio.from_string (Apron.Scalar.to_string c))

let scalar_to_num c =
  if Apron.Scalar.equal_int c 1 then Maxstratutil.Num.GmpRatio.one
  else if Apron.Scalar.equal_int c (-1) then 
    Maxstratutil.Num.GmpRatio.minus_one
  else Maxstratutil.Num.GmpRatio.from_string 
    (Apron.Scalar.to_string c)
 
let coeff_to_num c = 
(*  Log.debug3_o logger (Apron.Coeff.print) "coeff_to_num: " c; *)
  if Apron.Coeff.is_zero c then Maxstratutil.Num.GmpRatio.zero
  else
  match c with
    |Apron.Coeff.Scalar(c) -> scalar_to_num c 
    |_-> raise (NotSupported "interval coefficients")

(* converts APRON coefficients into STRATIMP coefficients *)
let num_to_coeff c = 
  Apron.Coeff.s_of_mpqf 
    (Mpqf.of_string (Maxstratutil.Num.GmpRatio.to_string c))

(* converts an APRON linexpr1 to a STRATIMP expression *)
let linexpr1_to_expr env linexpr =
(*  Log.debug3_o logger (Apron.Linexpr1.print) "linexpr1_to_expr: linexpr=" linexpr; *)
  let vars = (* Util.list_inter env.Env.s_vars *) env.Env.n_vars in
  let const = Apron.Linexpr1.get_cst linexpr in 
  let constterm = if Apron.Coeff.is_zero const then [] 
    else [MyHa.expr_const (coeff_to_num const)] in
  let terms = List.fold_left
      (fun terms var  -> 
        let c = Apron.Linexpr1.get_coeff linexpr (Apron.Var.of_string var) in
        if Apron.Coeff.is_zero c then terms
        else if Apron.Coeff.equal_int c 1 then (MyHa.expr_var var)::terms
        else 
          (MyHa.expr_mul [MyHa.expr_const (coeff_to_num c); 
                          MyHa.expr_var var])::terms)
      constterm vars
  in
  let res = if Util.list_is_empty terms then MyHa.expr_const MyHa.zero 
    else MyHa.expr_add terms  in
(*  Log.debug3_o logger (Format.pp_print_string) "linexpr1_to_expr: expr=" 
    (MyHa.string_of_expr res); *)
  res

(* converts an APRON linexpr1 to a STRATIMP expression *)
let linexpr1_to_neg_expr env linexpr =
(*  Log.debug3_o logger (Apron.Linexpr1.print) "linexpr1_to_neg_expr: linexpr=" linexpr; *)
  let vars = (* Util.list_inter env.Env.s_vars *) env.Env.n_vars in
  let const = Apron.Linexpr1.get_cst linexpr in 
  let constterm = if Apron.Coeff.is_zero const then [] 
    else [MyHa.expr_const (coeff_to_num (Apron.Coeff.neg const))] in
  let terms = List.fold_left
      (fun terms var -> 
        let c = Apron.Linexpr1.get_coeff linexpr (Apron.Var.of_string var) in
        if Apron.Coeff.is_zero c then terms
        else if Apron.Coeff.equal_int c (-1) then (MyHa.expr_var var)::terms
        else 
          (MyHa.expr_mul [MyHa.expr_const (coeff_to_num (Apron.Coeff.neg c)); 
                          MyHa.expr_var var])::terms)
      constterm vars 
  in
  let res = if Util.list_is_empty terms then MyHa.expr_const MyHa.zero 
    else MyHa.expr_add terms  in
(*  Log.debug3_o logger (Format.pp_print_string) "linexpr1_to_neg_expr: expr=" 
    (MyHa.string_of_expr res); *)
  res

(* converts an APRON lincons1 to a STRATIMP constraint *)
let lincons1_to_cons env lincons =
  let expr = linexpr1_to_expr env (Apron.Lincons1.get_linexpr1 lincons) in
  match Apron.Lincons1.get_typ lincons with
    |Apron.Lincons0.EQ -> 
      MyHa.atom_eq expr (MyHa.expr_const MyHa.zero)
    |Apron.Lincons0.SUPEQ -> 
      MyHa.atom_geq expr (MyHa.expr_const  MyHa.zero)
    |_ -> raise (NotSupported "strict inequalities")

(* converts an APRON abstract value into a numerical STRATIMP predicate *)
let linconss_to_pform env linconss = 
  MyHa.pform_and (Array.to_list 
    (Array.mapi 
      (fun i _ -> 
        MyHa.pform_atom (lincons1_to_cons env 
          (Apron.Lincons1.array_get linconss i)))
      linconss.Apron.Lincons1.lincons0_array))

(* converts a logico-numerical predicate into a numerical STRATIMP predicate *)
let boolexpr_to_pform env doman boolexpr = 
  let linconss = BddapronUtil.boolexpr_to_linconss env.Env.env env.Env.cond 
    doman env.Env.b_vars boolexpr in
  linconss_to_pform env linconss

(* extracts and converts the numerical equations of the transition function 
   to STRATIMP assignments *)
let equs_to_assignment env f = 
  let numf = BddapronUtil.numequs_of_equs env.Env.env env.Env.cond f in
  List.map (fun (v,e) -> (Apron.Var.to_string v,
                          linexpr1_to_expr env e)) (Array.to_list numf)

(* converts an APRON linexpr1 to a diffinequ STRATIMP expression *)
let linexpr1_to_diffinequexpr env linexpr : MyHa.DiffIneq.expr =
(*  Log.debug3_o logger (Apron.Linexpr1.print) "linexpr1_to_expr: linexpr=" linexpr; *)
  let vars = (* Util.list_inter env.Env.s_vars *) env.Env.n_vars in
  let const = Apron.Linexpr1.get_cst linexpr in 
  let constterm = if Apron.Coeff.is_zero const then [] 
    else [MyHa.DiffIneq.expr_const (coeff_to_num const)] in
  let terms = List.fold_left
      (fun terms var  -> 
        let c = Apron.Linexpr1.get_coeff linexpr (Apron.Var.of_string var) in
        if Apron.Coeff.is_zero c then terms
        else if Apron.Coeff.equal_int c 1 then 
          (MyHa.DiffIneq.expr_var (MyHa.DiffIneqVar.Var var))::terms
        else 
          (MyHa.DiffIneq.expr_mul [MyHa.DiffIneq.expr_const (coeff_to_num c); 
           MyHa.DiffIneq.expr_var (MyHa.DiffIneqVar.Var var)])::terms)
      constterm vars 
  in
  let res = if Util.list_is_empty terms then       MyHa.DiffIneq.expr_const MyHa.DiffIneq.zero 
    else MyHa.DiffIneq.expr_add terms  in
(*  Log.debug3_o logger (Format.pp_print_string) "linexpr1_to_expr: expr=" 
    (MyHa.string_of_expr res); *)
  res

(* converts an APRON lincons1 to a STRATIMP atomci constraint *)
let lincons1_to_atom env lincons =
  let expr = linexpr1_to_diffinequexpr env 
    (Apron.Lincons1.get_linexpr1 lincons) in
  match Apron.Lincons1.get_typ lincons with
    |Apron.Lincons0.EQ -> 
      MyHa.DiffIneq.atom_eq expr (MyHa.DiffIneq.expr_const MyHa.zero)
    |Apron.Lincons0.SUPEQ -> 
      MyHa.DiffIneq.atom_geq expr (MyHa.DiffIneq.expr_const  MyHa.zero)
    |_ -> raise (NotSupported "strict inequalities")

(* converts a logico-numerical predicate into a list of numerical STRATIMP atomic constraints *)
let boolexpr_to_atomlist env doman boolexpr : MyHa.DiffIneq.atom list = 
  let linconss = BddapronUtil.boolexpr_to_linconss env.Env.env env.Env.cond 
    doman env.Env.b_vars boolexpr in
  Array.to_list 
    (Array.mapi 
      (fun i _ -> lincons1_to_atom env 
          (Apron.Lincons1.array_get linconss i))
      linconss.Apron.Lincons1.lincons0_array)


(* extracts and converts the numerical equations of the transition function 
   to STRATIMP differential equations *)
let equs_to_diffequs env f : MyHa.DiffIneq.atom list = 
  let numf = BddapronUtil.numequs_of_equs env.Env.env env.Env.cond f in
  List.map 
    (fun (v,e) -> 
      (MyHa.DiffIneq.atom_eq
          (MyHa.DiffIneq.expr_var 
            (MyHa.DiffIneqVar.DiffVar (Apron.Var.to_string v)))
          (linexpr1_to_diffinequexpr env e)))
        (Array.to_list numf)

(* converts a CFG into a STRATIMP HA *)
let cfg_to_ha env doman cfg initloc =
  Log.debug3_o logger (Cfg.print_short env) "CFG: " cfg;
  let ha1 = PSHGraph.fold_vertex cfg 
    (fun locid _ ~pred ~succ ha -> 
      let ha = MyHa.add_loc (string_of_int locid) ha in
      let flowarcs = Cfg.get_flow_arcs_in_loc cfg locid in
      if PSette.is_empty flowarcs then
        MyHa.add_ev ((string_of_int locid),[MyHa.DiffIneq.atom_false]) ha
      else
        PSette.fold (fun (_,ass,f) ha ->
          let inv = Loc.get_inv (Cfg.get_loc cfg locid) in
          let staycond = boolexpr_to_atomlist env doman 
            (Cudd.Bdd.dand ass inv) in
          let diffequs = equs_to_diffequs env f in
          MyHa.add_ev (string_of_int locid,List.append staycond diffequs) ha)
        flowarcs ha)
    MyHa.empty 
  in
  let ha2 = PSHGraph.fold_hedge cfg 
    (fun h arc ~pred ~succ ha -> 
      match arc with
	|Arc.Flow(_,_) -> ha
	|_ -> 
        begin
          let (ass,f) = Arc.get_ass_equs env arc in
          let ass2 = Cudd.Bdd.dand ass (PSHGraph.attrvertex cfg pred.(0)) in
          let trans = (boolexpr_to_pform env doman ass2, 
             equs_to_assignment env 
             (BddapronUtil.simplify_equs env.Env.env env.Env.cond f ass)) 
          in   
          MyHa.add_trans (string_of_int pred.(0),trans,string_of_int succ.(0)) 
            ha
        end)
    ha1
  in
  let ha3 = MyHa.set_init_loc (string_of_int initloc) ha2 in
  MyHa.add_vars (Util.list_inter env.Env.s_vars env.Env.n_vars) ha3

(* converts templates into STRATIMP templates *)
let template_convert env t = List.map (linexpr1_to_neg_expr env) t 

let get_local_template local_templates loc =
 try List.assoc loc local_templates 
 with Not_found -> []

(******************************************************************************)
(* Conversion variable assignments from/to polyhedra*)
(******************************************************************************)

(* converts the STRATIMP result into an analysis result *)
let varass_to_res env doman cfg global_template local_templates var_ass = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  List.map 
    (fun loc ->
      let locarr = 
        List.map
          (fun cons ->
            let value = Maxstrat.Stratimp.get var_ass 
              (Maxstrat.Stratimp.V,string_of_int loc,
               linexpr1_to_neg_expr env cons) in
            match value with 
              |Maxstratutil.Num.InfGmpRatio.Infty -> Template.cons_true env
              |Maxstratutil.Num.InfGmpRatio.Neginfty -> Template.cons_false env
              |Maxstratutil.Num.InfGmpRatio.Num(v) -> 
                Template.cons_instantiate env cons (num_to_coeff v)
          )
          (List.append global_template (get_local_template local_templates loc))
      in
      Log.debug2_o logger (Util.list_print (Apron.Lincons1.print)) 
        ("result(loc="^(string_of_int loc)^") = ") locarr;

      let locearr = 
        { Apron.Lincons1.lincons0_array = Array.of_list 
            (List.map (fun c -> Apron.Lincons1.get_lincons0 c) locarr);
          array_env = env.Env.apronenv; }
      in
      (* loc -> bool x num *)
      (loc,Apron.Abstract1.of_lincons_array apronman env.Env.apronenv
             locearr))
    (PSette.elements (Cfg.get_locidset cfg))

(* transforms (loc -> polyhedron) to (variable assignment w.r.t. bounds of template polyhedra) *)
let anres_to_varass env doman global_template local_templates anres =
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let vars =  [Maxstrat.Stratimp.V;Maxstrat.Stratimp.A;Maxstrat.Stratimp.B] in
  let get_max s t =
    let minmax = Apron.Abstract1.bound_linexpr apronman s t in
    let max = scalar_to_numinf minmax.Apron.Interval.sup in
(*    Log.debug3_o logger (Format.pp_print_string) 
      "max = " (Maxstratutil.Num.InfGmpRatio.to_string max); *)
    max
  in
  let make_varass f sloc s template varass =
    List.fold_left
      (fun varass t  -> 
(*        Log.debug3_o logger (Apron.Linexpr1.print) "template = " t; *)
        let tex = linexpr1_to_expr env t in 
        let max = f s t in
(*        Log.debug3 logger 
          ("max = "^(Maxstratutil.Num.InfGmpRatio.to_string max)); *)
        List.fold_left 
          (fun varass v -> ((v,sloc,tex),max)::varass)
          varass vars)
      varass template 
  in
  let loc_to_varass loc s =    
(*    Log.debug3_o logger (Format.pp_print_int) "loc = " loc;
    Log.debug3_o logger (Apron.Abstract1.print) "s = " s; *)
    let sloc = string_of_int loc in
    let template = List.append global_template 
      (get_local_template local_templates loc) in
    if Apron.Abstract1.is_top apronman s then 
      make_varass (fun s t -> Maxstratutil.Num.InfGmpRatio.Infty) 
        sloc s template [] 
    else if Apron.Abstract1.is_bottom apronman s then 
      make_varass (fun s t -> Maxstratutil.Num.InfGmpRatio.Neginfty) 
        sloc s template [] 
    else make_varass (get_max) sloc s template []
  in
  List.flatten (List.map (fun (loc,s) -> loc_to_varass loc s) anres)

let numanres_to_anres env doman anres locmap numanres =
  Log.debug3_o logger (print_anres env doman) "anres = " anres;
  Log.debug3_o logger (print_locmap) "locmap = " locmap;
  List.map (fun (loc,s) -> 
    let slist = List.rev (Bddapron.Domain0.to_bddapron doman s) in
    let locs = List.assoc loc locmap in
    if Util.list_is_empty slist then
    begin
      assert((List.length locs)=(List.length slist)); 
      (loc,Bddapron.Domain0.bottom doman env.Env.env)
    end
    else
    begin
      (* !!! relies on the order of elements in locs and slist !!! *)
      assert(((List.length locs) mod (List.length slist))=0);
      let n = (List.length locs) / (List.length slist) in
      let rec iter slist locs news i =
        if i>=n then 
          match slist with 
          |_::[] -> news
          |_::stl -> iter stl locs news 0
	  |_ -> assert false
        else
          match (locs,slist) with
	  |(l::ltl,(sb,_)::_) -> 
             let ns = try
               Bddapron.Domain0.of_bddapron doman env.Env.env
                 [(sb,(Apron.Abstract1.abstract0 (List.assoc l numanres)))]
               with Not_found -> Bddapron.Domain0.bottom doman env.Env.env
             in
             iter slist ltl (Bddapron.Domain0.join doman ns news) (i+1)
	  |_ -> assert false
      in
      let news = 
        iter slist locs (Bddapron.Domain0.bottom doman env.Env.env) 0 in
      Log.debug3_o logger (Bddapron.Domain0.print doman env.Env.env) 
             ("loc "^(string_of_int loc)^" --> ") news;
      (loc,news)
    end)
    anres

let numanres_to_anres2 env doman cfg numanres =
  List.map (fun loc -> 
    let inv = PSHGraph.attrvertex cfg loc in
    let s =  
      try
        Bddapron.Domain0.meet_condition doman env.Env.env  env.Env.cond
          (Bddapron.Domain0.of_apron doman env.Env.env 
             (Apron.Abstract1.abstract0 (List.assoc loc numanres))) inv
      with Not_found -> Bddapron.Domain0.bottom doman env.Env.env
    in
    Log.debug3_o logger (Bddapron.Domain0.print doman env.Env.env) 
      ((string_of_int loc)^" --> ") s;
    (loc,s))
  (PSette.elements (Cfg.get_locidset cfg))
 
let anres_to_numanres env doman anres locmap =
  Log.debug3_o logger (print_anres env doman) "anres = " anres;
  List.fold_left 
    (fun numanres (loc,s)  -> 
      Log.debug3_o logger (Format.pp_print_int) "loc = " loc;
      let slist = List.rev (Bddapron.Domain0.to_bddapron doman s) in
      let locs = List.assoc loc locmap in
      if Util.list_is_empty locs then 
      begin
        assert((List.length locs)=(List.length slist)); 
        numanres
      end
      else
      begin
        (* !!! relies on the order of elements in locs and slist !!! *)
        assert(((List.length locs) mod (List.length slist))=0);
        let n = (List.length locs) / (List.length slist) in
        let rec iter numres slist locs i =
          if i>=n then 
            match slist with 
	      |_::[] -> numres
	      |_::stl -> iter numres stl locs 0
	      |_ -> assert false
          else
            match (locs,slist) with
	      |(l::ltl,(sb,sn)::_) -> 
              begin
                Log.debug3_o logger (Bddapron.Domain0.print doman env.Env.env) 
                  ((string_of_int l)^" --> ") 
                  (Bddapron.Domain0.of_bddapron doman env.Env.env [(sb,sn)]);
                let abs = {Apron.Abstract1.abstract0=sn; 
                           Apron.Abstract1.env=env.Env.apronenv} in
                iter ((l,abs)::numres) slist ltl (i+1)
              end
	      |_ -> assert false
        in 
        iter numanres slist locs 0
       end)
     [] anres

(******************************************************************************)
(* Solve system *)
(******************************************************************************)

let max_solve env doman cfg initloc global_template local_templates anres =
  Maxstratutil.Verb.set_verb "max-strategies";
  Maxstratutil.Verb.set_verb "variable-assignment";
(*  Maxstratutil.Verb.set_verb "lp"; *)
  let ha = cfg_to_ha env doman cfg initloc in
  Log.debug2 logger (MyHa.string_of_ha ha);

  let globaltmp = template_convert env global_template in
  Log.debug2_o logger (Util.list_print (Format.pp_print_string)) 
    "global templates: " (List.map MyHa.string_of_expr globaltmp);

  let localtmps loc = template_convert env
    (get_local_template local_templates (int_of_string loc))
  in

  let var_ass0 = anres_to_varass env doman global_template 
    local_templates anres in 
(*  let var_ass0 = [] in *)

  let var_ass = Maxstrat.Stratimp.solve_with_initial_var_ass 
    ha globaltmp localtmps var_ass0 in
  varass_to_res env doman cfg global_template local_templates var_ass

let analyze_num template oldanres locmap env doman cfg initial =
  Maxstratutil.Verb.enabled := Log.check_level logger Log.Debug2;
  let initlocidset = Cfg.get_locidset_by_inv env cfg initial in
  if (PSette.cardinal initlocidset)>1 then 
    raise (NotSupported "multiple initial locations");
  (* TODO: enable several initial locations *)
  let initloc = PSette.choose (initlocidset) in
  let oldnumanres = anres_to_numanres env doman oldanres locmap in
  let newnumanres = max_solve env doman cfg initloc template [] oldnumanres in
  if Util.list_is_empty oldanres then 
    numanres_to_anres2 env doman cfg newnumanres
  else numanres_to_anres env doman oldanres locmap newnumanres

(******************************************************************************)
(** {2 module Num: (discrete/hybrid) numerical max-strategy iteration} *)
(******************************************************************************)
module Num(Dom :  Template.TEMPLATE_T) =
struct
type analysisparam_t = unit

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
  let cfg = cf.Program.c_cfg in
  let initial = cf.Program.c_init in
  let assertion = cf.Program.c_ass in
  let final = cf.Program.c_final in
  let (doman,template) = Dom.doman () in
  let (cfg2,locmap) = reduce_to_numcfg [] env doman cfg initial assertion in
  if not(Util.list_is_empty locmap) then raise NotPurelyNumerical;
  let anres = analyze_num template [] locmap env doman cfg2 initial in
  (is_safe env final anres,
   refine_loc env anres,
   print_result env final anres,
   result_to_bddapron env anres)

end

(******************************************************************************)
(** {2 module Lognum: (discrete/hybrid) 
        logico-numerical max-strategy iteration} *)
(******************************************************************************)

module Lognum(Dom :  Template.TEMPLATE_T) =
struct

type analysisparam_t = unit

(******************************************************************************)
let bool_is_leq env s1 s2 = 
  let b1 = Dom.to_boolexprbool env s1 in
  let b2 = Dom.to_boolexprbool env s2 in
  Bddapron.Expr0.Bool.is_leq env.Env.env env.Env.cond b1 b2

let bool_is_eq env s1 s2 = 
  let b1 = Dom.to_boolexprbool env s1 in
  let b2 = Dom.to_boolexprbool env s2 in
  Bddapron.Expr0.Bool.is_eq env.Env.env env.Env.cond b1 b2

(* creates the manager for the fixpoint module *)
let make_fp_manager env initial trfct = 
{
  Fixpoint.bottom = 
    (fun vertex -> Dom.bottom env);
  Fixpoint.canonical = 
    (fun vertex s -> Dom.canonicalize env s);
  Fixpoint.is_bottom = 
    (fun vertex s -> Dom.is_bottom env s);
  Fixpoint.is_leq = (fun vertex s1 s2 -> bool_is_leq env s1 s2);
  Fixpoint.join = (fun vertex s1 s2 -> Dom.join env s1 s2);
  Fixpoint.join_list = 
    (fun vertex slist -> 
      List.fold_left (fun res x -> Dom.join env res x)
        (Dom.bottom env) slist);
  Fixpoint.widening = (fun vertex s1 s2 -> Dom.widening env s1 s2);
  Fixpoint.apply = trfct;
  Fixpoint.arc_init = (fun hedge -> ());
  Fixpoint.abstract_init = initial;
  Fixpoint.print_abstract = (fun fmt x -> Dom.print env fmt x);
  Fixpoint.print_arc=(fun fmt () -> ());
  Fixpoint.print_vertex = Format.pp_print_int;
  Fixpoint.print_hedge = Format.pp_print_int;
    
  Fixpoint.accumulate = false;
  Fixpoint.widening_start=1000000;
  Fixpoint.widening_descend=(-1);

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
    (fun v res -> (v,PSHGraph.attrvertex output v)::res)
    locs []

(* fixpoint iteration strategy for standard analysis *)
let make_strategy_std cfg sinit =
  Fixpoint.make_strategy_default 
    ~vertex_dummy:Cfg.locid_dummy ~hedge_dummy:Cfg.arcid_dummy cfg sinit

(* standard transition function *) 
let trfct env assertion cfg arcid tabs =
  let dloc = Cfg.get_succlocid cfg arcid in
  let dinv = Loc.get_inv (Cfg.get_loc cfg dloc) in
  assert((Array.length tabs)==1);
  let s = tabs.(0) in
  let arc = Cfg.get_arc cfg arcid in
  match arc with
    |Arc.Flow(ass,f) -> ((),s)
    |_ -> 
       let (ass,f) = Arc.get_ass_equs env arc in 
       let g = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond 
         env.Env.cond.Bdd.Cond.careset 
         (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond ass assertion) in
       let s = Dom.assign_lexpr env (Dom.meet_condition env s g) f in
       let s = Dom.forget_list env s env.Env.i_vars in
       let s = Dom.meet_condition env s dinv in
       ((),s)

(* returns the initial state for the given location *)
let get_initial_state env initial cfg loc =
  let top = Dom.top env in 
  let inv = PSHGraph.attrvertex cfg loc in
  (Dom.meet_condition env top 
     (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond initial inv)) 

(* do standard analysis with widening and without abstract acceleration *)
let analyze_propagate_bounded env cfg assertion get_init_state =
  let trfct = trfct env assertion cfg in
  let fpman = make_fp_manager env (get_init_state) trfct in
  let sinit = PSette.filter
    (fun locid -> not (Dom.is_bottom env (get_init_state locid)))
    (Cfg.get_locidset cfg)
  in
  let strategy = make_strategy_std cfg sinit in
  let output = Fixpoint.analysis_std fpman cfg sinit strategy in
  let res = fpout_to_anres env cfg output in
  res

(******************************************************************************)
let anres_is_bool_eq env a1 a2 =
  let rec anres_is_eq_loc a1 a2 =
    match a1 with
      |[] -> true
      |(v,s1)::ra1 -> 
        try 
          let s2 = List.assoc v a2 in
          if bool_is_eq env s1 s2 then anres_is_eq_loc ra1 a2 else false
        with Not_found -> Dom.is_bottom env s1
  in
  anres_is_eq_loc a1 a2

let anres_is_eq env a1 a2 =
  let rec anres_is_eq_loc a1 a2 =
    match a1 with
      |[] -> true
      |(v,s1)::ra1 -> 
        if (try 
             let s2 = List.assoc v a2 in
             Dom.is_eq env s1 s2
            with Not_found -> Dom.is_bottom env s1) then
          anres_is_eq_loc ra1 a2
        else false
  in
  anres_is_eq_loc a1 a2

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
  let cfg = cf.Program.c_cfg in
  let initial = cf.Program.c_init in
  let assertion = cf.Program.c_ass in
  let final = cf.Program.c_final in
  let (doman,template) = Dom.doman () in
  let anres_prev = ref(List.map (fun loc -> (loc,Dom.bottom env))
(*    (fun loc -> (loc,get_initial_state env initial cfg loc)) *)
      (PSette.elements (Cfg.get_locidset cfg))) in
  let anres = ref(analyze_propagate_bounded env cfg assertion
    (get_initial_state env initial cfg)) in
  Log.debug2_o logger (fun fmt s -> print_result env final s fmt ()) 
      "anres_prev = " !anres_prev;
  Log.debug2_o logger (fun fmt s -> print_result env final s fmt ()) 
      "anres = " !anres;
  let cnt = ref(0) in
  while not (anres_is_eq env !anres !anres_prev) do
  begin
    let (numcfg,locmap) = reduce_to_numcfg !anres env doman 
      cfg initial assertion in
    anres := analyze_num template !anres locmap env doman numcfg initial;
    Log.debug2_o logger (fun fmt s -> print_result env final s fmt ()) 
      "anres_num = " !anres;
    let get_init_state locid =
      try List.assoc locid !anres
      with Not_found -> Dom.bottom env 
    in
    anres_prev := !anres;
    anres := analyze_propagate_bounded env cfg assertion get_init_state;
    Log.debug2_o logger (fun fmt s -> print_result env final s fmt ()) 
      "anres_prev = " !anres_prev;
    Log.debug2_o logger (fun fmt s -> print_result env final s fmt ()) 
      "anres = " !anres;
    cnt := !cnt + 1;
  end done;
  Log.debug2_o logger (Format.pp_print_int) "main iterations = " !cnt;
  (is_safe env final !anres,
   refine_loc env !anres,
   print_result env final !anres,
   result_to_bddapron env !anres)
end

