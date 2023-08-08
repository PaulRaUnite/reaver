(******************************************************************************)
(* BddapronSmt *)
(* translation of BDD APRON formulas to SMT formulas *)
(* author: Peter Schrammel *)
(* version: 0.9.3 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="BddapronToSmt";
              Log.level=Log.Debug3}

exception NotSupported of string

let id_prefix = "__"
let id_prefix_length = String.length id_prefix

type model_t = string BddapronUtil.equs_t

(* maps for BDD translation *)
type 'a bddmap_t = {mapb : ('a BddapronUtil.boolexpr_t,
                            ('a * YicesSmt.formula)) Hashhe.t;
                    mutable lastidb : int; 
                    prefixb : 'a}
let bddmap_new_var bddmap = bddmap.lastidb <- bddmap.lastidb+1; bddmap.prefixb^"tmp"^(string_of_int bddmap.lastidb)
let bddmap_insert bddmap bdd var = Hashhe.add bddmap.mapb bdd var
let bddmap_get bddmap bdd = fst (Hashhe.find bddmap.mapb bdd)
let bddmap_create prefix = 
  {mapb = Hashhe.create 42; lastidb = -1; prefixb = prefix} 

type 'a inverse_bddmap_t = ('a, 'a BddapronUtil.boolexpr_t) Hashhe.t
let inverse_bddmap_insert ibddmap var bdd = Hashhe.add ibddmap var bdd
let inverse_bddmap_get ibddmap var = Hashhe.find ibddmap var
let inverse_bddmap_create () = Hashhe.create 42

(* maps for MTBDD translation *)
type ('a, 'b) mtbddmap_t = {mapm : ('b Cudd.Mtbdd.t,('a * YicesSmt.formula))Hashhe.t;
                            mutable lastidm : int;
                            prefixm:'a}
let mtbddmap_new_var mtbddmap = mtbddmap.lastidm <- mtbddmap.lastidm+1; mtbddmap.prefixm^"tmp"^(string_of_int mtbddmap.lastidm)
let mtbddmap_insert mtbddmap mtbdd var = Hashhe.add mtbddmap.mapm mtbdd var
let mtbddmap_get mtbddmap mtbdd = fst (Hashhe.find mtbddmap.mapm mtbdd)
let mtbddmap_create prefix : ('a, 'b) mtbddmap_t = {mapm = Hashhe.create 42; lastidm = -1; prefixm = prefix} 


let cst_to_string c =
  match c with
    |Apron.Coeff.Scalar(c) -> Apron.Scalar.to_string c
    |_ -> raise (NotSupported "interval constants")

let cst_to_ratio c =
  match c with
    |Apron.Coeff.Scalar(c) -> 
    begin
      match c with 
	|Apron.Scalar.Mpqf cc -> 
          let den = Mpqf.get_den cc in
          let num = Mpqf.get_num cc in
          if (Mpz.fits_int_p den) && (Mpz.fits_int_p num) then
            {YicesSmt.num=Big_int.big_int_of_int (Mpz.get_int num); 
             YicesSmt.den=Big_int.big_int_of_int (Mpz.get_int den)}
          else YicesSmt.ratio_of_string (Apron.Scalar.to_string c)
        |_ -> YicesSmt.ratio_of_string (Apron.Scalar.to_string c)
    end 
    |_ -> raise (NotSupported "interval constants")

let linexpr1_to_negexpr env linexpr =
  let vars = BddapronUtil.apronvars_to_vars env (ApronUtil.vars_of_env (Bddapron.Env.apron env)) in
  let const = Apron.Linexpr1.get_cst linexpr in 
  let constterm = if Apron.Coeff.is_zero const then [] 
    else [YicesSmt.Const (cst_to_ratio (Apron.Coeff.neg const))] in
  let terms = List.fold_left
      (fun terms var -> 
        let c = Apron.Linexpr1.get_coeff linexpr (Apron.Var.of_string var) in
        if Apron.Coeff.is_zero c then terms
        else if Apron.Coeff.equal_int c (-1) then (YicesSmt.Real var)::terms
        else 
          YicesSmt.Mul (cst_to_ratio (Apron.Coeff.neg c),
                   YicesSmt.Real var)::terms)
      constterm vars 
  in
  let res = if Util.list_is_empty terms then YicesSmt.Const YicesSmt.ratio_zero
    else YicesSmt.create_sum terms  in
  res

let linexpr1_to_expr env linexpr =
  let vars = BddapronUtil.apronvars_to_vars env (ApronUtil.vars_of_env (Bddapron.Env.apron env)) in
  let const = Apron.Linexpr1.get_cst linexpr in 
  let constterm = if Apron.Coeff.is_zero const then [] 
    else [YicesSmt.Const (cst_to_ratio const)] in
  let terms = List.fold_left
      (fun terms var -> 
        let c = Apron.Linexpr1.get_coeff linexpr (Apron.Var.of_string var) in
        if Apron.Coeff.is_zero c then terms
        else if Apron.Coeff.equal_int c 1 then (YicesSmt.Real var)::terms
        else 
          YicesSmt.Mul (cst_to_ratio c,
                   YicesSmt.Real var)::terms)
      constterm vars 
  in
  let res = if Util.list_is_empty terms then YicesSmt.Const YicesSmt.ratio_zero
    else YicesSmt.create_sum terms  in
  res

let cons_is_true (cons:ApronUtil.lincons_t) = 
  let t = Apron.Lincons1.make 
    (Apron.Linexpr1.make (Apron.Lincons1.get_env cons)) Apron.Lincons0.EQ
  in
  cons=t

let cons_is_false = Apron.Lincons1.is_unsat

let lincons1_to_cons env lincons =
  if cons_is_false lincons then YicesSmt.F 
  else
    if cons_is_true lincons then YicesSmt.T 
    else 
      match Apron.Lincons1.get_typ lincons with
      |Apron.Lincons0.EQ -> YicesSmt.Eq (linexpr1_to_negexpr env (Apron.Lincons1.get_linexpr1 lincons),YicesSmt.Const YicesSmt.ratio_zero)
      |Apron.Lincons0.SUPEQ -> YicesSmt.Le (linexpr1_to_negexpr env (Apron.Lincons1.get_linexpr1 lincons),YicesSmt.Const YicesSmt.ratio_zero)
      |Apron.Lincons0.SUP -> YicesSmt.Gt (linexpr1_to_expr env (Apron.Lincons1.get_linexpr1 lincons),YicesSmt.Const YicesSmt.ratio_zero)
      |_ -> raise (NotSupported "disequalities, modulo")

let boolvar_to_smtvar ?(prime=false) env cond v = 
  try
  let id = if prime then (PMappe.find (v^"'") env.Bdd.Env.vartid).(0)
    else (PMappe.find v env.Bdd.Env.vartid).(0) 
  in
  let vv = id_prefix^(string_of_int id) in
(*  Log.debug3 logger ("boolvar: "^v^(if prime then "'" else "")^" --> "^vv);*)
  vv
  with Not_found -> assert false

let enumvar_to_smtvar ?(prime=false) env cond v index = 
  try
  let ids = if prime then PMappe.find (v^"'") env.Bdd.Env.vartid 
    else PMappe.find v env.Bdd.Env.vartid 
  in
  let id = ids.(index) in
  let vv = id_prefix^(string_of_int id) in
(*  Log.debug3 logger ("enumvar: "^v^(if prime then "'" else "")^"."^(string_of_int index)^" --> "^vv);*)
  vv
  with Not_found -> assert false

let id_to_smt ?(prime_bool=Sette.empty) env (cond:string BddapronUtil.cond_t) id = 
  try
  begin
    let v = PMappe.find id env.Bdd.Env.idcondvar in
    let prime = Sette.mem v prime_bool in
    match Bdd.Env.typ_of_var env v with 
    |`Bool -> YicesSmt.Bool (boolvar_to_smtvar ~prime env cond v)
    |`Benum typ -> 
    begin         
      let ids = PMappe.find v env.Bdd.Env.vartid in
      let index = Util.array_get_index_of id ids in
      YicesSmt.Bool (enumvar_to_smtvar ~prime env cond v index)
    end
    |_ -> assert false
  end
  with Not_found -> 
    begin                         (* TODO: clarify where this "true" is from?!*)
    let c = match (Bdd.Cond.cond_of_idb cond (id,true)) with 
      |`Apron c -> Bddapron.Apronexpr.Condition.to_apron1 env.Bdd.Env.symbol
         (Bddapron.Env.apron env) c
    in
    let lincons =  match c with
      |`Lin c -> c
      |_ -> raise (NotSupported "non-linear expressions")
    in
    lincons1_to_cons env lincons
  end

let vars_to_smtvars env cond vars = 
  let var_to_smtvar res v = 
    match Bdd.Env.typ_of_var env v with 
    |`Bool -> (boolvar_to_smtvar env cond v)::res
    |`Benum _ -> 
      let ids = PMappe.find v env.Bdd.Env.vartid in
      Array.fold_left (fun res id -> 
        (id_prefix^(string_of_int id))::res)
        res ids
    |_ -> raise (NotSupported "numerical variables and bitvectors not supported")
  in
  List.fold_left var_to_smtvar [] vars


let mtbdd_to_smt ?(prime_bool=Sette.empty) mtbddmap leaf_to_smt env cond mtbdd = 
  let rec traverse mtbdd : YicesSmt.formula =
(*    Log.debug_o logger (MtbddUtil.print_base_mtbdd env cond
			  (Template.print_bound)) "mtbdd = " mtbdd; *)
    try YicesSmt.Bool (mtbddmap_get mtbddmap mtbdd)
    with Not_found ->
      begin match Cudd.Mtbdd.inspect mtbdd with
      | Cudd.Mtbdd.Ite(id,dthen,delse) ->
        let f1 = traverse dthen in
        let f2 = traverse delse in
        let v = mtbddmap_new_var mtbddmap in
        mtbddmap_insert mtbddmap mtbdd (v,YicesSmt.cases (id_to_smt ~prime_bool env cond id) f2 f1);
        YicesSmt.Bool v
      | Cudd.Mtbdd.Leaf(leaf) -> 
        let v = mtbddmap_new_var mtbddmap in
        mtbddmap_insert mtbddmap mtbdd (v,leaf_to_smt (Cudd.Mtbdd.get leaf));
        YicesSmt.Bool v
     end
  in
  traverse mtbdd 

let mtbddmap_to_smt mtbddmap =
  Hashhe.fold (fun _ (v,e) res -> 
    YicesSmt.And(res,(YicesSmt.bool_let v e)))
    mtbddmap.mapm YicesSmt.T

let bddmap_to_smt bddmap =
  Hashhe.fold (fun _ (v,e) res -> 
    YicesSmt.And(res,(YicesSmt.bool_let v e)))
    bddmap.mapb YicesSmt.T

let boolexpr_to_smt bddmap env cond boolexpr : YicesSmt.formula = 
  let rec traverse bdd : YicesSmt.formula =
    let is_compl = Cudd.Bdd.is_complement bdd in
    let bdd = if is_compl then Cudd.Bdd.dnot bdd else bdd in
    try 
      let v = YicesSmt.Bool (bddmap_get bddmap bdd) in
      if is_compl then YicesSmt.Not v else v
    with Not_found ->
      begin match Cudd.Bdd.inspect bdd with
      | Cudd.Bdd.Ite(id,dthen,delse) ->
        let f1 = if Cudd.Bdd.is_cst dthen then 
            if (Cudd.Bdd.is_false dthen) then YicesSmt.F else YicesSmt.T
                 else traverse dthen
        in
        let f2 = if Cudd.Bdd.is_cst delse then 
                   if (Cudd.Bdd.is_false delse) then YicesSmt.F else YicesSmt.T
                 else traverse delse
        in
        let v = bddmap_new_var bddmap in
        bddmap_insert bddmap bdd (v,YicesSmt.cases (id_to_smt env cond id) f2 f1);
        if is_compl then YicesSmt.Not (YicesSmt.Bool v) else YicesSmt.Bool v
        | Cudd.Bdd.Bool(b) -> if b then if is_compl then YicesSmt.F else YicesSmt.T  
                              else if is_compl then YicesSmt.T else YicesSmt.F
     end
  in
  traverse boolexpr  

let equs_to_smt bddmap env cond equs =
  YicesSmt.create_and (List.flatten (List.map
    (fun (v,e) -> 
      match e with
	|`Bool(expr) -> [YicesSmt.bool_let (boolvar_to_smtvar ~prime:true env cond v)
                                     (boolexpr_to_smt bddmap env cond expr)]
	|`Benum(expr) -> 
           List.mapi 
             (fun i b -> 
                YicesSmt.bool_let (enumvar_to_smtvar ~prime:true env cond v i)
                  (boolexpr_to_smt bddmap env cond b)
             )
             (Array.to_list expr.Bdd.Enum.reg)
        |_ -> raise (NotSupported "numerical and bitvector types"))
    equs))

let apron_lincons_to_smt = lincons1_to_cons

let apron_linconss_to_smt env linconss = 
  YicesSmt.create_and (Array.to_list 
    (Array.mapi 
      (fun i _ -> 
        (lincons1_to_cons env (Apron.Lincons1.array_get linconss i)))
      linconss.Apron.Lincons1.lincons0_array))

let apron_equs_to_smt env equs = 
  YicesSmt.create_and (List.map
    (fun (v,e) -> YicesSmt.Eq (YicesSmt.Real ((Apron.Var.to_string v)^"'"),
                          (linexpr1_to_expr env e)))
    (Array.to_list equs))

let smt_and = YicesSmt.create_and

let smt_convert_model env cond m =
      (* parse model into BDDAPRON types *)
      let model = ref [] in
      let mm = Util.hashtbl_to_array ("",YicesSmt.VBool true) m in
      Array.sort (fun (v1,_) (v2,_) -> compare v1 v2) mm;
      let i = ref 0 in
      while !i<(Array.length mm) do
      begin
        let (v,value) = mm.(!i) in
        begin
        match value with
	|YicesSmt.VBool value -> 
        if((String.length v)>id_prefix_length && 
           (String.sub v 0 id_prefix_length)=id_prefix) then
        begin
          let id = int_of_string (String.sub v id_prefix_length
                                    ((String.length v)-id_prefix_length)) in
          let v = PMappe.find id env.Bdd.Env.idcondvar in
          match Bddapron.Env.typ_of_var env v with 
	  |`Bool -> model := 
            (v, `Bool (Bddapron.Expr0.Bool.of_bool env cond value))::!model;
	  |`Benum typ -> 
          begin
            let size = Array.length (PMappe.find v env.Bdd.Env.vartid) in
            let reg = Array.make size 
              (Bddapron.Expr0.Bool.of_bool env cond value) in
            for k=1 to size-1 do 
            begin
              i:=!i+1;
              let (var,value) = mm.(!i) in
              match value with 
	      |YicesSmt.VBool value -> reg.(k) <- 
                     Bddapron.Expr0.Bool.of_bool env cond value
	      |_ -> assert(false)
            end
            done;
            model := (v,`Benum {Bdd.Enum.typ=typ; Bdd.Enum.reg=reg})::!model;
          end
          |_ -> assert(false)
        end
	|YicesSmt.VRatio value -> (* convert to APRON Coeff *)
          let x = if (Big_int.is_int_big_int value.YicesSmt.num) &&
                     (Big_int.is_int_big_int value.YicesSmt.den) then
              Apron.Coeff.s_of_frac 
                (Big_int.int_of_big_int value.YicesSmt.num)
                (Big_int.int_of_big_int value.YicesSmt.den)
            else
              Apron.Coeff.s_of_mpq (Mpq.of_string (YicesSmt.string_of_ratio value))
          in
          model :=  (v, `Apron (Bddapron.Expr0.Apron.cst env cond x))::!model;
        end;
        i:=!i+1;
      end
      done;
      !model

(** computes a model for the SMT formula *)
let smt_compute_model (env:string BddapronUtil.env_t) (cond:string BddapronUtil.cond_t) f : YicesSmt.interpretation option =
  YicesSmt.compute_model f

(** adds a formula to the given context *)
let smt_assert_ctx ctx f = 
  match ctx with
    |None -> YicesSmt.create_ctx f
    |Some ctx -> YicesSmt.assert_ctx ctx f

(** computes a model for the SMT formula in the given context*)
let smt_compute_model_ctx env cond ctx =
  YicesSmt.compute_model_ctx ctx


let smt_print_model env cond fmt m =
  List.iter (fun (v,x) -> 
             Format.pp_print_newline fmt (); 
             env.Bdd.Env.symbol.Bdd.Env.print fmt v;
             Format.pp_print_string fmt " = ";
             Bddapron.Expr0.print env cond fmt x)  
          m

let smt_print_result fmt m =
  match m with
    | None -> Format.pp_print_string fmt "UNSAT";
    | Some _ -> Format.pp_print_string fmt "SAT"

let smt_to_boolexpr ?(unprime=false) env cond f : 'a BddapronUtil.boolexpr_t = 
(*  Log.debug3 logger "smt_to_boolexp()"; *)
  let tmpmap = Hashhe.create 42 in
  let rec buildtmp = function
    | YicesSmt.Let (v,f) -> Hashhe.add tmpmap v f 
    | YicesSmt.And (f1,f2) -> begin buildtmp f1; buildtmp f2; end
    | YicesSmt.Or (f1,f2) -> begin buildtmp f1; buildtmp f2; end
    | YicesSmt.Not f -> buildtmp f
    | _ -> ()
  in
  buildtmp f;
(*  Log.debug3_o logger (Hashhe.print Format.pp_print_string 
    (fun fmt x -> Format.pp_print_string fmt (YicesSmt.string_of_formula x))) 
    "tmpmap = " tmpmap; *)
  let ibddmap = inverse_bddmap_create () in
  let rec traverse = function
    | YicesSmt.T -> Bddapron.Expr0.Bool.dtrue env cond  
    | YicesSmt.F -> Bddapron.Expr0.Bool.dfalse env cond
    | YicesSmt.Bool v -> 
      begin
      try inverse_bddmap_get ibddmap v
      with Not_found ->
        try 
          let f = Hashhe.find tmpmap v in
          let bdd = (traverse f) in
          inverse_bddmap_insert ibddmap v bdd;
          bdd
        with Not_found -> 
        begin
          let id = int_of_string (String.sub v id_prefix_length ((String.length v)-id_prefix_length)) in
          let id = if unprime then
            begin
(*              Log.debug3_o logger (Format.pp_print_int) "unprime id: " id;*)
              try
              begin
                let v = PMappe.find id env.Bdd.Env.idcondvar in
                if (String.sub v ((String.length v)-1) 1)="'" then
                begin
(*                  Log.debug3 logger ("unprime v: "^v);*)
                  let ids = PMappe.find v env.Bdd.Env.vartid in
(*                  Log.debug3_o logger (Util.array_print (Format.pp_print_int)) "ids: " ids; *)
                  let index = Util.array_get_index_of id ids in
                  let pids = PMappe.find (String.sub v 0 ((String.length v)-1)) env.Bdd.Env.vartid in
(*                  Log.debug3_o logger (Util.array_print (Format.pp_print_int)) "pids: " pids;*)
                  pids.(index)
                end
                else id
              end
              with Not_found -> assert false
            end
            else id 
          in
(*          Log.debug3_o logger (Format.pp_print_int) "id: " id; *)
          Cudd.Bdd.ithvar env.Bdd.Env.cudd id
	end
      end
    | YicesSmt.Let (v,f) -> 
    if((String.length v)>id_prefix_length && (String.sub v 0 id_prefix_length)=id_prefix) then
    begin
      let id = int_of_string (String.sub v id_prefix_length ((String.length v)-id_prefix_length)) in
      let id = if unprime then
            begin
(*              Log.debug3_o logger (Format.pp_print_int) "unprime id: " id;*)
              try
              begin
                let v = PMappe.find id env.Bdd.Env.idcondvar in
                if (String.sub v ((String.length v)-1) 1)="'" then
                begin
(*                  Log.debug3 logger ("unprime v: "^v);*)
                  let ids = PMappe.find v env.Bdd.Env.vartid in
(*                  Log.debug3_o logger (Util.array_print (Format.pp_print_int)) "ids: "  ids;*)
                  let index = Util.array_get_index_of id ids in
                  let pids = PMappe.find (String.sub v 0 ((String.length v)-1)) env.Bdd.Env.vartid in
(*                  Log.debug3_o logger (Util.array_print (Format.pp_print_int)) "pids: " pids;*)
                  pids.(index)
                end
                else id
              end
              with Not_found -> assert false
            end
        else id 
      in
(*      Log.debug3_o logger (Format.pp_print_int) "id: " id;*)
      Bddapron.Expr0.Bool.eq env cond
              (Cudd.Bdd.ithvar env.Bdd.Env.cudd id) (traverse f)
    end
    else Bddapron.Expr0.Bool.dtrue env cond
    | YicesSmt.And (f1,f2) -> Bddapron.Expr0.Bool.dand env cond
                       (traverse f1) (traverse f2)
    | YicesSmt.Or (f1,f2) -> Bddapron.Expr0.Bool.dor env cond
                       (traverse f1) (traverse f2)
    | YicesSmt.Not f -> Bddapron.Expr0.Bool.dnot env cond (traverse f)
    | YicesSmt.Eq (e1,e2) | YicesSmt.Gt (e1,e2) | YicesSmt.Le (e1,e2) -> 
       raise (NotSupported "numerical constraints") 
  in
  let res = traverse f in
(*  Log.debug3_o logger (Hashhe.print Format.pp_print_string 
    (BddapronUtil.print_boolexpr env cond)) 
    "ibddmap = " ibddmap; *)
  res

let model_to_boolexpr ?(unprime=false) env cond model = 
  List.fold_left 
    (fun res (v,x) -> 
      match x with
	|`Bool x -> 
           let v = if unprime then String.sub v 0((String.length v)-1) else v in
           let v = Bddapron.Expr0.Bool.var env cond v in
           if Bddapron.Expr0.Bool.is_true env cond x then
             Bddapron.Expr0.Bool.dand env cond res v
           else
             Bddapron.Expr0.Bool.dand env cond res
               (Bddapron.Expr0.Bool.dnot env cond v)
	|`Benum x -> 
           let v = if unprime then String.sub v 0((String.length v)-1) else v in
           let v = Bddapron.Expr0.Benum.var env cond v in
           Bddapron.Expr0.Bool.dand env cond res
             (Bddapron.Expr0.Benum.eq env cond v x)
        |_ -> raise (NotSupported "numerical models"))
    (Bddapron.Expr0.Bool.dtrue env cond) model

let smt_assert_retractable_ctx = YicesSmt.assert_retractable_ctx
let smt_retract_ctx = YicesSmt.retract_ctx

let smt_check_model ctx model f = 
  let ff = YicesSmt.instantiate_formula model f in
  match ff with
    |YicesSmt.T -> Some model 
    |YicesSmt.F -> None
    |_ -> 
    begin
   (*  Log.warn logger "not fully evaluated"; *)
      match YicesSmt.compute_model ff with
	|Some _ -> Some model
	|None -> None
(*       Log.debug3 logger ("evaluated formula: "^(YicesSmt.string_of_formula ff));
       assert false *)
    end

let smt_check_model2 ctx ymodel yexpr = 
  if YicesSmt.evaluate_in_ymodel ymodel yexpr then
    Some (YicesSmt.ymodel_to_model ctx ymodel)
  else None
