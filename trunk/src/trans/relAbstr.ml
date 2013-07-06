(******************************************************************************)
(* relAbstr *)
(* relational abstractions of flow transitions *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="RelAbstr";
              Log.level=Log.Debug3}

(*
  implementation restricted to diagonal matrices
*)

type trans_type_t = 
  | Zero 
  | RealEigen1 of Apron.Coeff.t * Apron.Coeff.t
  | Other

(******************************************************************************)
(* Helpers *)
(******************************************************************************)
 let rec check_coeffs env e vdim dims ivars j 
   (has_v_only,is_reset,has_inputs) =
   if j>=dims then (has_v_only,is_reset,has_inputs)
   else
     let zero_coeff = Apron.Coeff.is_zero (Apron.Linexpr0.get_coeff e j) in
     let is_input = Util.array_mem (Apron.Environment.var_of_dim env j) ivars in
     match (vdim=j,zero_coeff,is_input) with
      |(false,false,false) -> check_coeffs env e vdim dims ivars (j+1) 
                                (false,false,has_inputs)
      |(true ,false,false) -> check_coeffs env e vdim dims ivars (j+1) 
                                (has_v_only,false,has_inputs)
      |(_    ,false,true ) -> check_coeffs env e vdim dims ivars (j+1) 
                                (has_v_only,is_reset,true)
      |_                   -> check_coeffs env e vdim dims ivars (j+1) 
                                (has_v_only,is_reset,has_inputs)

let get_transition_type_equ (v,e1) ivars =
  let e = Apron.Linexpr1.get_linexpr0 e1 in
  let env = Apron.Linexpr1.get_env e1 in
  let dims = ApronUtil.dims_of_env env in
  let vdim = Apron.Environment.dim_of_var env v in
  let c = Apron.Linexpr0.get_coeff e vdim in
  let d = Apron.Linexpr0.get_cst e in
  let has_nonzero_cst = not (Apron.Coeff.is_zero d) in
  match ((check_coeffs env e vdim dims ivars 0 
            (true,true,false)),has_nonzero_cst) with
    |((true,_,_),_) -> RealEigen1(c,d)
    |((_,true,false),false) -> Zero
    |_ -> Other

(******************************************************************************)
let transform_equ env (v,g,a) = 
  let theenv = env in
  let env,cond = env.Env.env, env.Env.cond in
  match get_transition_type_equ (BddapronUtil.var_to_apronvar env v,a) 
           (BddapronUtil.vars_to_apronvars env theenv.Env.ni_vars) 
  with
     |Zero -> (v,Bddapron.Expr0.Bool.dtrue env cond,
                 Bddapron.Expr0.var env cond v)
     |RealEigen1(c,_) -> 
       let newinvar = Env.get_newinput theenv v in
       let gg = 
         (* c>0: *)
         if Apron.Coeff.cmp c (Apron.Coeff.s_of_int 0)>=0 then
            (* cx+Txi+d>0 /\ xx>=x /\ x'=xx *)
           let gpos = Bddapron.Expr0.Bool.dand env cond
             (BddapronUtil.lincons_to_boolexpr env cond 
               (Apron.Lincons1.make a (Apron.Lincons0.SUP)))
             (Bddapron.Expr0.Apron.supeq env cond
               (Bddapron.Expr0.Apron.sub env cond
                  (Bddapron.Expr0.Apron.var env cond newinvar)
                  (Bddapron.Expr0.Apron.var env cond v)))
           in
           (* cx+Txi+d<0 /\ xx<=x /\ x'=xx *)
           let gneg = Bddapron.Expr0.Bool.dand env cond
             (Bddapron.Expr0.Bool.dnot env cond
               (BddapronUtil.lincons_to_boolexpr env cond 
                 (Apron.Lincons1.make a (Apron.Lincons0.SUPEQ))))
             (Bddapron.Expr0.Apron.supeq env cond
               (Bddapron.Expr0.Apron.sub env cond
                  (Bddapron.Expr0.Apron.var env cond v)
                  (Bddapron.Expr0.Apron.var env cond newinvar)))
           in
           Bddapron.Expr0.Bool.dor env cond gpos gneg
         (* c<0: *)
         else
         begin
           let a0 = Apron.Linexpr1.copy a in
           Apron.Linexpr1.set_coeff a0 (BddapronUtil.var_to_apronvar env v) 
             (Apron.Coeff.s_of_int 0);
           (* cx+Txi+d>0 /\ x<=xx<Txi+d /\ x'=xx *)
           let gpos = 
             Bddapron.Expr0.Bool.dand env cond
               (BddapronUtil.lincons_to_boolexpr env cond 
                 (Apron.Lincons1.make a (Apron.Lincons0.SUP)))
               (Bddapron.Expr0.Bool.dand env cond
                 (Bddapron.Expr0.Apron.supeq env cond
                   (Bddapron.Expr0.Apron.sub env cond
                     (Bddapron.Expr0.Apron.var env cond newinvar)
                     (Bddapron.Expr0.Apron.var env cond v)))
                 (Bddapron.Expr0.Apron.sup env cond
                   (Bddapron.Expr0.Apron.sub env cond
                     (BddapronUtil.linexpr_to_apronexprDD env a0)
                     (Bddapron.Expr0.Apron.var env cond newinvar))))
           in
           (* cx+Txi+d<0 /\ Txi+d<xx<=x /\ x'=xx *)
           let gneg = 
             Bddapron.Expr0.Bool.dand env cond
               (Bddapron.Expr0.Bool.dnot env cond
                 (BddapronUtil.lincons_to_boolexpr env cond 
                   (Apron.Lincons1.make a (Apron.Lincons0.SUPEQ))))
               (Bddapron.Expr0.Bool.dand env cond
                 (Bddapron.Expr0.Apron.supeq env cond
                   (Bddapron.Expr0.Apron.sub env cond
                     (Bddapron.Expr0.Apron.var env cond v)
                     (Bddapron.Expr0.Apron.var env cond newinvar)))
                 (Bddapron.Expr0.Apron.sup env cond
                   (Bddapron.Expr0.Apron.sub env cond
                     (Bddapron.Expr0.Apron.var env cond newinvar)
                     (BddapronUtil.linexpr_to_apronexprDD env a0))))
           in
           Bddapron.Expr0.Bool.dor env cond gpos gneg
         end
       in
       let ggg = Bddapron.Expr0.Bool.dand env cond gg
         (Bddapron.Expr0.Bool.dand env cond g 
           (Bddapron.Expr0.Bool.substitute_by_var env cond g [(v,newinvar)]))
       in
       (v,ggg,Bddapron.Expr0.var env cond newinvar)
     |_ ->  
       let newinvar = Env.get_newinput theenv v in
       let ggg = Bddapron.Expr0.Bool.dand env cond g 
         (Bddapron.Expr0.Bool.substitute_by_var env cond g [(v,newinvar)])
       in
       (v,ggg,
          Bddapron.Expr0.var env cond newinvar)

 
(******************************************************************************)
(* Interface *)
(******************************************************************************)
let transform env cf _  =
  let cfg = cf.Program.c_cfg in
  let transform_loc locid = 
    match Cfg.get_flow_arc env cfg locid with
      |Some(arcid,ass,f) ->
      begin
        let cequs = List.map 
          (fun (v,e) ->
            match e with
	      |`Apron(e) -> 
                let gl = Cudd.Mtbdd.guardleafs e in
                (v,Array.fold_left 
                  (fun ee (g,a) ->
(*                    let g = Cudd.Bdd.dand g ass in*)
                    let a = BddapronUtil.apronaction_to_linexpr 
                      env.Env.env env.Env.cond a in
                    let (_,g,a) = transform_equ env (v,g,a) in
                    Bddapron.Expr0.ite env.Env.env env.Env.cond g a ee)
                     (Bddapron.Expr0.var env.Env.env env.Env.cond v) 
                  gl)
	      |_ -> assert(false))
          f
        in
        Log.debug_o logger (BddapronUtil.print_boolexpr 
                      env.Env.env env.Env.cond) "rel.abs. ass = " ass;
        let ass = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond ass 
         (Bddapron.Expr0.Bool.substitute_by_var env.Env.env env.Env.cond ass 
            (List.map (fun v -> (v,Env.get_newinput env v)) env.Env.ns_vars))
        in
        let ncequs = BddapronUtil.get_id_equs_for env.Env.env env.Env.cond 
         (Util.list_diff env.Env.s_vars (Pervasives.fst (List.split f)))
        in
        let equs = List.append cequs ncequs in
        PSHGraph.replace_attrhedge cfg arcid (Arc.Normal(ass,equs))
      end
      |None -> () 
  in
  PSette.iter (transform_loc) 
    (Cfg.get_locidset_by_inv env cfg 
      (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond 
        (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond 
          cf.Program.c_init cf.Program.c_final)));
(*    Env.cudd_reorder env;*)
  cf

