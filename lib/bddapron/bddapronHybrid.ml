(******************************************************************************)
(* BddapronHybrid *)
(* logico-numerical analysis utilities for hybrid transitions *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="BddapronHybrid";
              Log.level=Log.Debug3}

type ('a, 'b, 'c, 'd) cont_elapse_t = 
  'a BddapronUtil.env_t ->         (** BddApron environment *)
  'a BddapronUtil.cond_t ->        (** BddApron conditions *)
  ('a, 'b, 'c, 'd) BddapronUtil.doman_t -> (** BddApron domain manager *)
  'a BddapronUtil.boolexpr_t ->    (** guard (including assertion) *)
  'a BddapronUtil.equs_t ->        (** differential equations *)
  'a BddapronUtil.vars_t ->        (** numerical input variables *)
  'd BddapronUtil.abstract_t ->    (** start abstract value *)
  'd BddapronUtil.abstract_t       (* result abstract value *)

(******************************************************************************)
(* Internal helpers *)
(******************************************************************************)
(* partitions the list of BDD APRON equations into an array of 
   constant (resp. constant with inputs) APRON equations and
   a array of variables with other equations*)
let equs_to_numequs_partition env cond ivars equs  =
  let (cst,noncst) = List.fold_right 
    (fun (v,expr) (cst,noncst) ->
      match expr with 
        |`Apron(e) -> 
        begin
           let v = BddapronUtil.var_to_apronvar env v in
           try 
             let linexpr1 = BddapronUtil.apronexpr_to_linexpr env cond e in
             match ApronAccel.get_transition_type_equ (v,linexpr1) ivars with
	       |ApronAccel.Reset -> ((v, linexpr1)::cst,noncst)
               |_ -> (cst,v::noncst)
	   with BddapronUtil.NotLinearAction _ -> (cst,v::noncst)
        end
        |_ -> (cst,noncst))
    equs ([],[])
  in
  (Array.of_list cst,Array.of_list noncst)


(******************************************************************************)
(* Time elapse *)
(******************************************************************************)
(** polyhedral time-elapse operator for constant dynamics (forget otherwise),
    also takes into account expressions dependent on inputs and 
    discrete resp. dot v = 0 variables  *)
(* computes translation polyhedron (dot x, dot n)
   with the variables n with dot n = 0 and x with dot x <> 0: 
   exists n,xi : 
     convexify(exists b,beta: C /\ dot x = f^c /\ dot n = 0 /\ A /\ 
                  (exists x: S))
*)
let cont_elapse2 ?(convexify=true)
    (get_primed_var:('a BddapronUtil.var_t -> 'a BddapronUtil.var_t)) 
    (env:'a BddapronUtil.env_t) 
    (cond:'a BddapronUtil.cond_t) 
    (doman:('a, 'b, 'c, 'd) BddapronUtil.doman_t) 
    (staycond:'a BddapronUtil.boolexpr_t) 
    (equs:'a BddapronUtil.equs_t) 
    (ivars:'a BddapronUtil.vars_t) 
    (s:'d BddapronUtil.abstract_t) 
    : 'd BddapronUtil.abstract_t 
    =
  (* variables *)
  let apronenv = Bddapron.Env.apron env in
  let (contvars,_) = List.split equs in
  let vars = BddapronUtil.apronvars_to_vars env 
    (ApronUtil.vars_of_env apronenv) in
  let primed_vars = List.map (get_primed_var) vars in
  let svars = Util.list_diff vars (List.append ivars primed_vars) in
  let discvars = Util.list_diff svars contvars in
  let npnvars = Util.list_diff vars primed_vars in
(*  let boolvars = BddapronUtil.boolvars_of_env env in *)
  Log.debug3_o logger (BddapronUtil.print_vars env) 
    "non-primed vars = " npnvars;
  (* non-constant continuous variables *)
  let (vnd0,_) = List.split (List.filter 
                              (BddapronUtil.is_nonzero_equ env) equs) in
  Log.debug3_o logger (BddapronUtil.print_vars env) 
     "non-zero dynamics vars = " vnd0;
  (* add identities for non-continuous variables *)
  let equs = List.append equs 
    (BddapronUtil.get_zero_equs_for env cond discvars) in
  (* current values without non-constant variables *)
  let s_vd0 = Bddapron.Domain0.forget_list doman env s vnd0 in
  Log.debug3_o logger (BddapronUtil.print_abstract env doman) "s_vd0 = " s_vd0;
  (* make conjunction of equations /\_i x_i'=e_i *)
  let fexpr = BddapronUtil.get_fexpr get_primed_var env cond 
     equs in
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "fexpr = " fexpr;
  (* "weaken" assertion into g(x)/\ g(xi) *)
  let staycond = BddapronUtil.boolexpr_to_abstract env cond doman staycond in
  let istaycond = Bddapron.Domain0.forget_list doman env staycond svars in
  let sstaycond = Bddapron.Domain0.forget_list doman env staycond ivars in
  let wstaycond = Bddapron.Domain0.meet doman istaycond sstaycond in
  Log.debug3_o logger (BddapronUtil.print_abstract env doman) 
    "wstaycond = " wstaycond;
 (* compute the abstract relation (dot x, x, n, xi) *)
  let s_full = Bddapron.Domain0.meet_condition doman env cond
    (Bddapron.Domain0.meet doman s_vd0 wstaycond) fexpr in
  Log.debug3_o logger (BddapronUtil.print_abstract env doman) "s_full: " s_full;
(*  let s_num = Bddapron.Domain0.forget_list doman env s_full boolvars in
  Log.debug3_o logger (BddapronUtil.print_abstract env doman) "s_num: " s_num;*)
  (* compute the convex polyhedron in dot x *)
  let dotx = Bddapron.Domain0.forget_list doman env s_full npnvars in
  let dotx = if convexify then 
      BddapronUtil.abstract_convexify env cond doman dotx 
    else dotx 
  in
  Log.debug3_o logger (BddapronUtil.print_abstract env doman) "dotx: " dotx;
  (* extract numerical part *)
  let bddapron = Bddapron.Domain0.to_bddapron doman dotx in
  if Util.list_is_empty bddapron then s
  else
  begin
    let bd = List.map (fun (bb,dd) ->
      let d = ApronUtil.abstract1_of_abstract0 apronenv dd in
        (* replace primed by unprimed variables *)
        let d = ApronUtil.rename_primed_to_unprimed 
          (fun v -> BddapronUtil.var_to_apronvar env
            (get_primed_var (BddapronUtil.apronvar_to_var env v)))
          d (BddapronUtil.vars_to_apronvars env svars) in
        Log.debug3_o logger (ApronUtil.print_abstract) "d = " d;
        (bb,d))
      bddapron
    in 
    (* extract numerical abstract value *)
    let bn = Bddapron.Domain0.to_bddapron doman s in
    let bnres = List.fold_left
      (fun bnres (bb,nn) ->
        let nn1 = {Apron.Abstract1.abstract0 = nn; 
                 Apron.Abstract1.env = apronenv} in
        List.fold_left (fun bnres (b,d) -> 
          (* compute elapse *)
          let nn2 = ApronUtil.elapse nn1 d in
          Log.debug3_o logger (ApronUtil.print_abstract) "n-elapse = " nn2;
          (Cudd.Bdd.dand bb b,
           Apron.Abstract1.abstract0 nn2)::bnres)
          bnres bd)
      [] bn
    in
    (* build logico-numerical abstract value *)
    let res:'d BddapronUtil.abstract_t = 
      Bddapron.Domain0.of_bddapron doman env bnres
    in
    res
  end

(******************************************************************************)
(** polyhedral time-elapse operator for constant dynamics (forget otherwise) *)
let cont_elapse1 env cond doman staycond equs ivars s = 
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let apronenv = Bddapron.Env.apron env in
  let (contvars,_) = List.split equs in
  let vars = ApronUtil.vars_of_env apronenv in
  let ivars = BddapronUtil.vars_to_apronvars env ivars in
  let svars = Util.array_diff vars ivars in
  let noncontvars = Util.array_diff svars (BddapronUtil.vars_to_apronvars env contvars) in
  let boolvars = BddapronUtil.boolvars_of_env env in
  (* extract the condition over numerical inputs from the staying condition *)
  let numstaycond = BddapronUtil.boolexpr_to_linconss env cond doman 
    boolvars staycond in
  let istaycond = Apron.Abstract1.to_lincons_array apronman 
    (ApronUtil.project_forget 
      (Apron.Abstract1.of_lincons_array apronman apronenv numstaycond) 
      svars) 
  in

  (* extract APRON equations and 
     classify them w.r.t. constant (resp. with inputs) dynamics and 
                          non-constant dyn. *)
  let (csteqs,noncstvars) = equs_to_numequs_partition env cond ivars equs in

  (* add =0 equations for non-continous variables *)
  let csteqs = Array.append csteqs 
    (ApronUtil.get_zero_equations_for apronenv noncontvars) in
  Log.debug3_o logger (ApronUtil.print_vars) "noncontvars = " noncontvars;
  Log.debug3_o logger (ApronUtil.print_equations) "csteqs = " csteqs;
  (* compute the translator *)
  let d = ApronUtil.get_translator apronman csteqs istaycond ivars in
  Log.debug3_o logger (ApronUtil.print_abstract) "d = " d;
  (* extract numerical abstract value *)
  let bn = Bddapron.Domain0.to_bddapron doman s in
  let bnres = List.map
    (fun (bb,nn) ->
      let nn1 = ApronUtil.abstract1_of_abstract0 apronenv nn in
      (* elapse and remove variables with non-constant dynamics *)
      let nn2 = ApronUtil.elapse nn1 d in
      let nn3 = ApronUtil.project_forget nn2 noncstvars in
      (bb,Apron.Abstract1.abstract0 nn3))
    bn
  in
  (* build logico-numerical abstract value *)
  Bddapron.Domain0.of_bddapron doman env bnres


(******************************************************************************)
(** simplest time-elapse operator 
      (forget everything about continuous variables) *)
let cont_elapse0 env cond doman staycond equs ivars s = 
  let (vars,_) = List.split equs in
  Bddapron.Domain0.forget_list doman env s vars

(******************************************************************************)
(* Flow transition operator *)
(******************************************************************************)
(** computes the time elapse of s by eqs up to staycond *)
let flow ~trans_doman env cond doman
        ?(cont_elapse = (cont_elapse2 (BddapronUtil.get_primed_var env)))
        staycond equs ivars s = 
  Log.debug3_o logger (BddapronUtil.print_abstract env doman) "flow: s = " s;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) 
    "flow: staycond = " staycond;
  Log.debug3_o logger (BddapronUtil.print_equations env cond) 
    "flow: equs = " equs;
  Log.debug3_o logger (BddapronUtil.print_vars env) "flow: ivars = " ivars;

  (* compute time elapse *)
  let selapse = cont_elapse env cond trans_doman staycond equs ivars 
    (BddapronUtil.change_domain env doman s trans_doman) in
  Log.debug3_o logger (BddapronUtil.print_abstract env trans_doman) 
    "s-elapse: " selapse;
  (* project staying condition on state variables *)
  let sstaycond = BddapronUtil.boolexpr_forget_vars env cond trans_doman
    ivars staycond in
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) 
    "sstaycond = " sstaycond;
  (* intersect with staying condition *)
  let res = Bddapron.Domain0.meet_condition trans_doman env cond 
    selapse sstaycond in
  Log.debug3_o logger (BddapronUtil.print_abstract env trans_doman) 
    "flow(s) in trans_doman = " res;
  let res = BddapronUtil.change_domain env trans_doman res doman in
  Log.debug3_o logger (BddapronUtil.print_abstract env doman) "flow(s) = " res;
  res
