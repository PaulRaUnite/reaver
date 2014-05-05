(******************************************************************************)
(* zeroTrans *)
(* translates zero-crossings to hybrid automata *)
(* author: Peter Schrammel *)
(* version: 0.9.3 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="ZeroTrans";
              Log.level=Log.Debug3}

(* 
s_i' = \/_j phiZ_ij -> Phi_ij
phiZ_ij:  /\_p z_p ... conjunction of positive and negative zero-crossings
all but one phiZ_j contain at least one positive zero-crossing

(1) collect positive zero-crossings from all equations i
(2) translate positive zero-crossings: (above-below-ready)
(3) replace zero-crossings by guards in equations 
*)

open Program

type sem_t = AtZero | Contact | Crossing
type kind_t = Continuous of Env.var_t | 
          Discrete of Env.var_t | 
          DiscCont of Env.var_t * Env.var_t

type guardexpr_t = GContinuous of Env.boolexpr_t | 
          GDiscrete of Env.boolexpr_t | 
          GDiscCont of Env.boolexpr_t * Env.boolexpr_t

type zspec_t = 
{
  zvar : Env.var_t;  (* zero-crossing placeholder variable *)
  cont_sem : sem_t;    (* continuous semantics *)
  disc_sem : sem_t;    (* discrete semantics *)
  zdef : Env.numexpr_t; (* zero-crossing expression *)
  qvars : kind_t;      (* q-variables (for location encoding) for 
                          Continuous, Discrete and DiscCont zcs *)
}

let q_state3 = "_QState3"
let l_above = "_above"
let l_below = "_below"
let l_ready = "_ready"

(******************************************************************************)
(* utilities *)
(******************************************************************************)

type zero_trans_t = Env.t -> Env.numexpr_t -> Env.boolexpr_t

(******************************************************************************)
let get_epsilon env = Bddapron.Expr0.Apron.cst env.Env.env env.Env.cond 
  (Apron.Coeff.s_of_float Env.epsilon)

(******************************************************************************)
let gt_zero env e = Bddapron.Expr0.Apron.sup env.Env.env env.Env.cond e
let lt_zero env e = Bddapron.Expr0.Apron.sup env.Env.env env.Env.cond 
  (Bddapron.Expr0.Apron.negate env.Env.env env.Env.cond e)
let geq_zero (env:Env.t) (e:Env.numexpr_t) : Env.boolexpr_t = 
  Bddapron.Expr0.Apron.supeq env.Env.env env.Env.cond e
let leq_zero env e = Bddapron.Expr0.Apron.supeq env.Env.env env.Env.cond 
  (Bddapron.Expr0.Apron.negate env.Env.env env.Env.cond e)
let eq_zero env e = Bddapron.Expr0.Apron.eq env.Env.env env.Env.cond e
let neq_zero env e = Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond 
  (Bddapron.Expr0.Apron.eq env.Env.env env.Env.cond e)
let leqeps_zero env e = Bddapron.Expr0.Apron.supeq env.Env.env env.Env.cond 
  (Bddapron.Expr0.Apron.sub env.Env.env env.Env.cond (get_epsilon env) e)
let leqleqeps_zero env e = 
  match BddapronUtil.apronexprDD_to_apronexpr env.Env.env e with
    |Bddapron.Apronexpr.Lin linexpr -> 
       let newlinexpr = Bddapron.Apronexpr.Lin.sub Env.symbol
         (Bddapron.Apronexpr.Lin.cst (Mpqf.of_float Env.epsilon))
         linexpr
       in
       Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
         (Bddapron.Expr0.Apron.supeq env.Env.env env.Env.cond
            (BddapronUtil.apronexpr_to_apronexprDD env.Env.env
                (Bddapron.Apronexpr.Lin newlinexpr)))
         (Bddapron.Expr0.Apron.supeq env.Env.env env.Env.cond e)
    |_ -> Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond
  (Bddapron.Expr0.Apron.supeq env.Env.env env.Env.cond 
    (Bddapron.Expr0.Apron.sub env.Env.env env.Env.cond (get_epsilon env) e))
  (Bddapron.Expr0.Apron.supeq env.Env.env env.Env.cond e)

(******************************************************************************)
(* replaces the zeros in expr using the given zero translator *)
let replace_zeros_boolexpr (trans_zero:zero_trans_t) (env:Env.t) 
  zero_defs (expr: Env.boolexpr_t) :  Env.boolexpr_t = 
  let zero_defs = List.map (fun (v,z) -> (v,
                       Bddapron.Expr0.Bool.to_expr (trans_zero env z))) 
                    zero_defs
  in
  Bddapron.Expr0.Bool.substitute env.Env.env env.Env.cond expr zero_defs

(******************************************************************************)
(* checks whether expr contains input variables *)
let zero_expr_contains_inputs env zero_defs expr =
  let iset = Util.list2psette (compare) env.Env.i_vars in
  let zs = Bddapron.Expr0.support env.Env.env env.Env.cond (`Bool expr) in
  PSette.fold
    (fun z accu -> 
      let zd = Env.get_zero_def zero_defs z in
      let supp = Bddapron.Expr0.support env.Env.env env.Env.cond (`Apron zd) in
      accu && not (PSette.is_empty (PSette.inter supp iset)))
    zs true

(******************************************************************************)
(* filter for splitting continuous equations into those with
    non-constant and constant continuous variables *)
let filter_noncst_cst env (v,f) = 
  match f with
    |`Apron(e) -> 
       let guardleaves = Cudd.Mtbdd.guardleafs e in
       if (Array.length guardleaves)<>1 then true
       else
         let (_,l) = guardleaves.(0) in
         not (Bddapron.ApronexprDD.is_zero env.Env.env l)
    |_ -> assert(false)

(******************************************************************************)
(* printing and conversion *)
(******************************************************************************)

(******************************************************************************)
(* semantics *)
let print_sem fmt sem = 
  Format.pp_print_string fmt 
    (match sem with
      |AtZero -> "At-zero" 
      |Contact -> "Contact" 
      |Crossing -> "Crossing")

(******************************************************************************)
let print_kind fmt qvars = 
  Format.pp_print_string fmt 
    (match qvars with
      |Continuous(_) -> "Continuous" 
      |Discrete(_) -> "Discrete" 
      |DiscCont(_,_) -> "Discrete/Continuous")

let print_zdef env fmt zdef = 
      Bddapron.Expr0.Apron.print env.Env.env env.Env.cond fmt zdef

let print_zspec env fmt zspec =
    Format.pp_print_string fmt "zvar=";
    Format.pp_print_string fmt zspec.zvar;
    Format.pp_print_string fmt ", zdef=";
    print_zdef env fmt zspec.zdef;
    Format.pp_print_string fmt ", kind=";
    print_kind fmt zspec.qvars

let print_zguardmap env fmt zguardmap = Util.list_print
  (fun fmt (zvar,g) ->  
    Format.pp_print_string fmt zvar;
    Format.pp_print_string fmt " -> ";
    Bddapron.Expr0.print env.Env.env env.Env.cond fmt g)
  fmt zguardmap


(******************************************************************************)
(* Translation Helpers *)
(******************************************************************************)

let bool_t env = Bddapron.Expr0.Bool.dtrue env.Env.env env.Env.cond
let bool_f env = Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond
let bool_or env e1 e2 = Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond e1 e2

let apron_var env q = Bddapron.Expr0.Apron.var env.Env.env env.Env.cond q
let apron_expr env expr = Cudd.Mtbdd.cst_u env.Env.cuddman
  (Cudd.Mtbdd.unique env.Env.env.Bdd.Env.ext.Bddapron.Env.table expr)
let apron_ite env c e1 e2 = 
  Bddapron.Expr0.Apron.ite env.Env.env env.Env.cond c e1 e2

let benum_var env q = Bddapron.Expr0.Benum.var env.Env.env env.Env.cond q
let benum_label env l = 
  Bdd.Enum.of_label env.Env.env l
let benum_eq_label env q label = 
  Bddapron.Expr0.Benum.eq_label env.Env.env env.Env.cond 
    (Bddapron.Expr0.Benum.var env.Env.env env.Env.cond q) label
let benum_eq_labels env q labels = 
  List.fold_right
    (fun label e -> 
      bool_or env e (Bddapron.Expr0.Benum.eq_label env.Env.env env.Env.cond 
       (Bddapron.Expr0.Benum.var env.Env.env env.Env.cond q) label))
    labels (bool_f env)
let bool_and env e1 e2 = Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond e1 e2
let bool_not env e = Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond e
let bool_var env q = Bddapron.Expr0.Bool.var env.Env.env env.Env.cond q
let bool_closed_not env doman e = BddapronUtil.boolexpr_topoclose env.Env.env env.Env.cond doman (bool_not env e)
let bool_init env einit e =
  Bddapron.Expr0.Bool.ite env.Env.env env.Env.cond 
    (Bddapron.Expr0.Bool.var env.Env.env env.Env.cond Env.init_var) einit e
let benum_init env einit e =
  Bddapron.Expr0.Benum.ite env.Env.env env.Env.cond 
    (Bddapron.Expr0.Bool.var env.Env.env env.Env.cond Env.init_var) einit e
let bool_ite env c e1 e2 = 
  Bddapron.Expr0.Bool.ite env.Env.env env.Env.cond c e1 e2
let benum_ite env c e1 e2 = 
  Bddapron.Expr0.Benum.ite env.Env.env env.Env.cond c e1 e2

let trans_zero env translator zdef = translator env zdef

(******************************************************************************)
(* computes the set of purely continuous variables: 
       discrete transition = idenitity except for init *)
let get_purecontset env disc_equs cont_equs = 
  let (conteqvars,_) = List.split cont_equs in
  List.fold_right
      (fun v s ->
        let dexpr = match List.assoc v disc_equs with 
          | `Apron(expr) -> expr | _ -> assert(false)
        in
        let guardleaves = Cudd.Mtbdd.guardleafs dexpr in
        (* all actions must be identity except for 
           that one with condition "_init" *)
        let has_init = List.mem Env.init_var env.Env.s_vars in
        let rec check i =
          if i<(Array.length guardleaves) then
            let (g,l) = guardleaves.(i) in
            if has_init && (Bddapron.Expr0.Bool.is_true env.Env.env env.Env.cond
                 (Bddapron.Expr0.Bool.eq env.Env.env env.Env.cond g 
                   (bool_var env Env.init_var))) || 
               (Bddapron.Apronexpr.equal Env.symbol l
                     (Bddapron.Apronexpr.var Env.symbol 
                       (Bddapron.Env.typ_of_var env.Env.env) v))
            then check (i+1)
            else false
          else true
        in
        if check 0 then PSette.add v s
	else s)
      conteqvars (PSette.empty (compare))

let get_contset env cont_equs = 
  let (conteqvars,_) = List.split cont_equs in
  Util.list2psette (compare) conteqvars

(******************************************************************************)
let guardexprs env zspec =
  let guardexpr_disc q = 
    let zexpr = match zspec.disc_sem with
      |AtZero |Contact -> trans_zero env (geq_zero) zspec.zdef
      |Crossing -> trans_zero env (gt_zero) zspec.zdef
    in
    bool_and env zexpr (bool_var env q)
  in
  let guardexpr_cont q = 
    match zspec.cont_sem with
      |AtZero -> bool_and env 
           (trans_zero env (eq_zero) zspec.zdef)
           (bool_var env q)  
      |Contact -> bool_and env 
           (trans_zero env (eq_zero) zspec.zdef)   
           (benum_eq_label env q l_ready)
      |Crossing -> bool_and env 
           (trans_zero env (leqleqeps_zero) zspec.zdef)   
           (benum_eq_label env q l_ready)
  in
  match zspec.qvars with
    |Continuous(q) -> GContinuous(guardexpr_cont q)
    |DiscCont(q1,q2) -> GDiscCont(guardexpr_disc q1, guardexpr_cont q2)
    |Discrete(q) -> GDiscrete(guardexpr_disc q)

(******************************************************************************)
let guardexprs_to_guard env gexprs = 
  match gexprs with
    |GContinuous(g) |GDiscrete(g) -> g
    |GDiscCont(g1,g2) -> bool_or env g1 g2

(******************************************************************************)
let staying env zspec = 
  match zspec.qvars with
    |Continuous(q) |DiscCont(_,q) ->
    begin
      match zspec.cont_sem with
       |AtZero -> bool_or env
              (bool_and env (bool_not env (bool_var env q))
                (trans_zero env (geq_zero) zspec.zdef))            
              (bool_and env (bool_var env q)
                (trans_zero env(leq_zero) zspec.zdef))            
       |Contact -> bool_or env 
              (bool_and env (benum_eq_label env q l_above)
                (trans_zero env (geq_zero) zspec.zdef))
              (bool_and env (benum_eq_labels env q [l_below;l_ready])
                (trans_zero env (leq_zero) zspec.zdef))
       |Crossing -> bool_or env (bool_or env
              (bool_and env (benum_eq_label env q l_above)
                (trans_zero env (geq_zero) zspec.zdef))
              (bool_and env (benum_eq_label env q l_below)
                (trans_zero env (leq_zero) zspec.zdef)))
              (bool_and env (benum_eq_label env q l_ready)
                (trans_zero env (leqleqeps_zero) zspec.zdef))      
    end
    |_ -> bool_t env

(******************************************************************************)
let q_equation env zspec guard = 
  let q_equation_disc q = 
    let zexpr = match zspec.disc_sem with
      |AtZero |Crossing -> trans_zero env (leq_zero) zspec.zdef
      |Contact -> trans_zero env (lt_zero) zspec.zdef
    in
    [(q,`Bool (bool_init env (bool_f env) zexpr))]
  in
  let q_equation_cont q = 
    match zspec.cont_sem with
      |AtZero -> 
	let qb0 = Env.get_q_bool_i_var zspec.zvar 0 in
	let qb1 = Env.get_q_bool_i_var zspec.zvar 1 in
	let qb2 = Env.get_q_bool_i_var zspec.zvar 2 in
	let qexpr = bool_init env 
	   (bool_var env qb0)
	   (bool_ite env (bool_var env q)
	     (bool_ite env (bool_var env qb1) 
	       (bool_t env) (bool_not env guard))
	     (bool_ite env (bool_var env qb2) 
	       (trans_zero env (leq_zero) zspec.zdef) 
	       (bool_f env)))
	in [(q,`Bool qexpr)]
      |Contact -> 
	let qb0 = Env.get_q_bool_i_var zspec.zvar 0 in
	let qb1 = Env.get_q_bool_i_var zspec.zvar 1 in
	let qb2 = Env.get_q_bool_i_var zspec.zvar 2 in
	let qb3 = Env.get_q_bool_i_var zspec.zvar 3 in
	let qb4 = Env.get_q_bool_i_var zspec.zvar 4 in
	let qexpr = benum_init env 
	  (benum_ite env 
	    (bool_var env qb0)
	    (benum_label env l_above) (benum_label env l_below))
	  (benum_ite env
	    (benum_eq_label env q l_above)
	    (benum_ite env 
	      (bool_and env (bool_var env qb1)
		(trans_zero env (leq_zero) zspec.zdef)) (* eq *)
	      (benum_label env l_below) (benum_label env l_above))
	    (benum_ite env
	      (benum_eq_label env q l_below)
	      (benum_ite env 
		(bool_and env (bool_var env qb2)
		  (trans_zero env (lt_zero) zspec.zdef))
		(benum_label env l_ready) (benum_label env l_below))
	      (* l_ready *)
		(benum_ite env (bool_and env (bool_var env qb3) guard)
		  (benum_ite env (bool_var env qb4) 
		    (benum_label env l_above) (benum_label env l_below))
		  (benum_label env l_ready))))
       in [(q,`Benum qexpr)]
      |Crossing ->
	let qb0 = Env.get_q_bool_i_var zspec.zvar 0 in
	let qb1 = Env.get_q_bool_i_var zspec.zvar 1 in
	let qb2 = Env.get_q_bool_i_var zspec.zvar 2 in
	let qb3 = Env.get_q_bool_i_var zspec.zvar 3 in
	let qb4 = Env.get_q_bool_i_var zspec.zvar 4 in
	let qexpr = benum_init env 
	  (benum_ite env 
	    (bool_var env qb0)
	    (benum_label env l_above) (benum_label env l_below))
	  (benum_ite env
	    (benum_eq_label env q l_above)
	    (benum_ite env 
	      (bool_and env (bool_var env qb1)
		(trans_zero env (leq_zero) zspec.zdef)) (* eq *)
	      (benum_label env l_below) (benum_label env l_above))
	    (benum_ite env
	      (benum_eq_label env q l_below)
	      (benum_ite env 
		(bool_and env (bool_var env qb2) 
		  (trans_zero env (geq_zero) zspec.zdef)) (*eq*)
		(benum_label env l_ready) (benum_label env l_below))
	      (benum_ite env (bool_and env (bool_var env qb3) guard)
		(benum_ite env (bool_var env qb4) 
		  (benum_label env l_above) (benum_label env l_below))
		(benum_label env l_ready))))
       in [(q,`Benum qexpr)]
  in
  match zspec.qvars with
    |Discrete(q) -> q_equation_disc q
    |Continuous(q) -> q_equation_cont q
    |DiscCont(q1,q2) -> List.append (q_equation_disc q1) (q_equation_cont q2)

(******************************************************************************)
let cont_interruption env doman zspec gexprs = 
  match gexprs with
    |GDiscrete(g) |GDiscCont(g,_) -> bool_closed_not env doman g
    |_ -> bool_t env

(******************************************************************************)
(* translation tools *)
(******************************************************************************)

(******************************************************************************)
(* compute zero-crossings specs *)
let get_zero_specs env dfprog cont_sem disc_sem zvars =
  let iset = Util.list2psette (compare) env.Env.i_vars in
  (* purely continuous variables: 
       discrete transition = idenitity except for init *)
  let contset = get_contset env dfprog.d_cont_equs in
  let purecontset = get_purecontset env dfprog.d_disc_equs dfprog.d_cont_equs in

  (* computes the zero-crossing spec for a zero-conjunction list *)
  let comp_spec zvar = 
    let zdef = Env.get_zero_def dfprog.d_zero_defs zvar in  
    let supp = Bddapron.Expr0.support env.Env.env env.Env.cond (`Apron zdef) in
    (* -- distinguish purely cont, purely disc and cont+disc zero-crossings *)
    (* purely continuous zero-crossings: 
         all state variables are purely continuous 
         => translation only for continuous zero-crossing *)
    let all_cont = PSette.is_empty 
      (PSette.diff (PSette.diff supp purecontset) iset)
    in
    (* purely discrete zero-crossings: 
         no continuous state variables
         => translation only for discrete zero-crossing *)
    (* continuous and discrete zero-crossings: 
        => translation for both types *)
    let no_cont = PSette.is_empty (PSette.inter supp contset) in
    (* -- parametrized by semantics *)
    let qvars =  
      if all_cont then Continuous(Env.get_q_var zvar true)
      else if no_cont then Discrete(Env.get_q_var zvar false)
      else DiscCont(Env.get_q_var zvar false,Env.get_q_var zvar true)
    in
    let zspec = 
      {zvar; disc_sem; cont_sem; zdef; qvars} in
    zspec
  in
  (* iterate through zero-crossing list *)
  List.map (comp_spec) zvars

(******************************************************************************)
(* add new variables *)
let extend_environment env param zspecs =
  (* add type for q variables to environment *)
  Bddapron.Env.add_typ_with env.Env.env q_state3 
    (`Benum [|l_above;l_below;l_ready|]);

  (* creates a list of qivars_cnt boolean input variables *)
  let create_q_bool_i_varstyp_list zid qivars_cnt =
    let l = ref [] in    
    for i=0 to qivars_cnt-1 do
      l := (Env.get_q_bool_i_var zid i,`Bool)::!l
    done;
    !l
  in
  
  (* computes variables for a zero-crossing spec *)
  let gen_variables_for_zspec zspec  =
    let (new_disc_q_varlist,new_cont_q_varlist) = match zspec.qvars with
      |Discrete(q) -> ([q],[])
      |Continuous(q) -> ([],[q])
      |DiscCont(q1,q2) -> ([q1],[q2])
    in

    let (new_q_varstyp,qivars_cnt) = match zspec.qvars with
      |Discrete(q) -> ([(q,`Bool)],1)
      |Continuous(q) -> 
      begin
        match zspec.cont_sem with
          |AtZero -> ([(q,`Bool)],3)
          |Contact |Crossing -> ([(q,`Benum(q_state3))],5)
      end
      |DiscCont(q1,q2) ->
      begin
        match zspec.cont_sem with
	  |AtZero -> ([(q1,`Bool);(q2,`Bool)],3)
 	  |Contact |Crossing -> ([(q1,`Bool);(q2,`Benum(q_state3))],5)
      end
    in
    let new_qi_varstyp = create_q_bool_i_varstyp_list zspec.zvar qivars_cnt in
    (new_q_varstyp, new_qi_varstyp, 
     new_disc_q_varlist, new_cont_q_varlist)
  in

  (* iterate through zero-crossing specs *)
  let (q_varstyp, qi_varstyp, disc_q_varlist, cont_q_varlist) =
    List.fold_right 
      (fun zspec 
         (q_varstyp, qi_varstyp, disc_q_varlist, cont_q_varlist) ->
         let (new_q_varstyp, new_qi_varstyp,  
              new_disc_q_varlist, new_cont_q_varlist) = 
           gen_variables_for_zspec zspec in
         (List.append q_varstyp new_q_varstyp, 
          List.append qi_varstyp new_qi_varstyp, 
          List.append disc_q_varlist new_disc_q_varlist, 
          List.append cont_q_varlist new_cont_q_varlist))
     zspecs ([],[],[],[]) in
  (* variable lists *)
  let new_i_q_varstyp = List.map (fun (v,t) -> 
    (Env.get_newinput env v,t)) q_varstyp in
  let primed_q_varstyp = List.map (fun (v,t) -> 
    (BddapronUtil.get_primed_var env.Env.env v,t)) q_varstyp in
  let q_varlist = List.append disc_q_varlist cont_q_varlist in
  let (new_i_q_varlist,_) = List.split new_i_q_varstyp in
  let (primed_q_varlist,_) = List.split primed_q_varstyp in
  let (qi_varlist,_) = List.split qi_varstyp in
  (* add the variables *)
  let varstyp = List.concat [q_varstyp;primed_q_varstyp;new_i_q_varstyp;
    qi_varstyp] in
  (* permute existing expressions *)
  let perm = Bddapron.Env.add_vars_with env.Env.env varstyp in
  let newparam = Program.apply_env_permutation env param perm in
  (* create new env structure *)
  let newenv = 
    {Env.env = env.Env.env;
     Env.cond = env.Env.cond;
     Env.cuddman = env.Env.cuddman;
     Env.apronenv = env.Env.apronenv;
     Env.s_apronenv = env.Env.s_apronenv;
     Env.i_apronenv = env.Env.i_apronenv;
     Env.s_vars = List.append env.Env.s_vars q_varlist;
     Env.new_i_vars = List.append env.Env.new_i_vars new_i_q_varlist;
     Env.primed_vars = List.append env.Env.primed_vars primed_q_varlist;
     Env.i_vars = List.concat [env.Env.i_vars;qi_varlist];
     Env.b_vars = List.concat [env.Env.b_vars;q_varlist;new_i_q_varlist;
            primed_q_varlist;qi_varlist];
     Env.n_vars = env.Env.n_vars;
     Env.bi_vars = List.concat [env.Env.bi_vars;qi_varlist];
     Env.ni_vars = env.Env.ni_vars;
     Env.bs_vars = List.concat [env.Env.bs_vars;q_varlist];
     Env.ns_vars = env.Env.ns_vars;
     Env.zero_vars = env.Env.zero_vars;
     Env.disc_q_vars = disc_q_varlist;
     Env.cont_q_vars = cont_q_varlist;
     Env.q_i_vars = qi_varlist;
  }
  in
  (newenv,newparam,perm)

(******************************************************************************)
(* TODO??? how to deal with init var ??? *) 
let translate_disc_eqs env zguardmap f =
  let translate_disc_eq (v,e) =
    (v,Bddapron.Expr0.substitute env.Env.env env.Env.cond e zguardmap)
  in
  List.map (translate_disc_eq) f

let translate_cont_eqs env continter f =
  let translate_cont_eq (v,e) =
    (v,Bddapron.Expr0.ite env.Env.env env.Env.cond continter e 
         (`Apron (Bddapron.Expr0.Apron.cst env.Env.env env.Env.cond 
                   (Apron.Coeff.s_of_int 0))))
  in
  List.map (translate_cont_eq) f

let translate_boolexpr env zguardmap e =
  Bddapron.Expr0.Bool.substitute env.Env.env env.Env.cond e zguardmap


(******************************************************************************)
(* translate *)
(******************************************************************************)
let translate env dfprog disc_sem cont_sem =
  Log.debug_o logger (print_sem) "using continuous semantics: " cont_sem;
  Log.debug_o logger (print_sem) "using discrete semantics: " disc_sem;

  (* factorization according to zero-crossings = 
       MTBDD product of discrete equations involving zero-crossings, 
       zero-crossing placeholders are right below the root *)
  let (f_no_z,f_z) = List.partition
    (fun (_,e) -> PSette.is_empty (PSette.inter 
      (Bddapron.Expr0.support env.Env.env env.Env.cond e)
      (Util.list2psette (compare) env.Env.zero_vars)))
    dfprog.d_disc_equs
  in
  let zspecs =  get_zero_specs env dfprog cont_sem disc_sem env.Env.zero_vars in
  let (env,param,perm) = extend_environment env dfprog zspecs in
  let f_no_z = Program.permute_equations perm f_no_z in
  let f_z = Program.permute_equations perm f_z in
  let init_z = Program.permute_boolexpr perm dfprog.d_init in
  let final_z = Program.permute_boolexpr perm dfprog.d_final in
  let ass_z = Program.permute_boolexpr perm dfprog.d_ass in
  Env.cudd_group env.Env.env (Env.get_s_varsmap env);
  Env.cudd_make_varmap env.Env.env (Util.mappe2list (Env.get_s_varsmap env));

  let doman = Bddapron.Domain0.man_of_bdd (Bddapron.Domain0.make_bdd 
    ((Polka.manager_alloc_strict ())))  in 
  (* translates the given zero-crossing spec *)
  let do_zero_trans (gexprlist,zguardmap,q_equations,staycond,continter) zspec = 
    Log.debug_o logger (print_zspec env) 
      "translating zero-crossing: " zspec;
    let gexprs = guardexprs env zspec in
    let guard = guardexprs_to_guard env gexprs in

    let new_q_equations = q_equation env zspec guard in
    (* - for discrete zero-crossings: interruption of continuous evolution
       - add staying conditions to staycond *)
    let new_staycond = staying env zspec in
    let new_continter = cont_interruption env doman zspec gexprs in 
    Env.cudd_reorder env; 

    Log.debug_o logger (Env.print_boolexpr env) "guard: "guard;
    Log.debug_o logger (Env.print_equations env) 
      "q_equations: " new_q_equations;
    Log.debug_o logger (Env.print_boolexpr env) 
      "staying condition: " new_staycond;

    (gexprs::gexprlist,
     (zspec.zvar,`Bool guard)::zguardmap,
     List.append q_equations new_q_equations,
     (bool_and env staycond new_staycond),
     (bool_and env continter new_continter))
  in

  (* iterate through zero-crossing list *)
  let (_,zguardmap,q_equations,staycond,continter) =
    List.fold_left (do_zero_trans) ([],[],[],bool_t env,bool_t env) zspecs in

  Log.debug_o logger (print_zguardmap env)  "z-guard-map: " zguardmap;

  Env.compute_careset env;

(*  Log.debug2_o logger Bddapron.Env.print 
     "environment: " env.Env.env;
  Log.debug2_o logger (Bddapron.Cond.print env.Env.env)
     "conditions: " env.Env.cond;
  Log.debug2_o logger (fun f e -> Bddapron.Env.print_order e f)
     "env-order: " env.Env.env;*)

  let new_disc_equs = BddapronUtil.simplify_equs env.Env.env env.Env.cond
    (List.concat 
    [f_no_z;
     translate_disc_eqs env zguardmap f_z;
     q_equations]) 
    env.Env.cond.Bdd.Cond.careset
  in
 
  let new_cont_equs = BddapronUtil.simplify_equs env.Env.env env.Env.cond
    (translate_cont_eqs env continter dfprog.d_cont_equs) 
    env.Env.cond.Bdd.Cond.careset
  in

  (*    env.Env.cond.Bdd.Cond.careset*)

  Log.debug_o logger (Env.print_equations env) 
      "translated discrete equations: " new_disc_equs;
  let staycond = Cudd.Bdd.tdrestrict staycond env.Env.cond.Bdd.Cond.careset in
  Log.debug_o logger (Env.print_boolexpr env) 
      "staying condition: " staycond;
  let cfprog = Program.make_cfprog
    (Cfg.make env new_disc_equs new_cont_equs 
      (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond 
       (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond staycond
          (Bddapron.Expr0.Bool.dnot env.Env.env env.Env.cond
            (Bddapron.Expr0.Bool.var env.Env.env env.Env.cond Env.init_var)))
       (Bddapron.Expr0.Bool.var 
                 env.Env.env env.Env.cond Env.init_var)))
    new_disc_equs new_cont_equs
    (translate_boolexpr env zguardmap init_z)
    (translate_boolexpr env zguardmap final_z)
    (translate_boolexpr env zguardmap ass_z)
  in  
  Log.info logger "Translation to logico-numerical hybrid automata done.";
  (env,cfprog)


