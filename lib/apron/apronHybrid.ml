(******************************************************************************)
(* ApronHybrid *)
(* analysis utilities for hybrid transitions *)
(* author: Peter Schrammel *)
(* version: 0.9.3 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="ApronHybrid";
              Log.level=Log.Debug3}

type 'a cont_elapse_t = 'a ApronUtil.man_t -> ApronUtil.linconss_t -> 
  ApronUtil.equs_t -> 
  ApronUtil.vars_t -> 'a ApronUtil.abstract_t -> 
  'a ApronUtil.abstract_t


(******************************************************************************)
(* internal utilities *)
(******************************************************************************)
(** checks whether the given equation represents a non-constant diff. equation*)
let has_noncst_dynamics env ivars (v,e) =
  let dims = ApronUtil.dims_of_env env in
  let e = Apron.Linexpr1.get_linexpr0 e in
  let rec check_coeff j =
    if j>=dims then false
    else
      if (Apron.Coeff.is_zero (Apron.Linexpr0.get_coeff e j)) ||
         (Util.array_mem (Apron.Environment.var_of_dim env j) ivars)
        then check_coeff (j+1)
      else true
  in
  check_coeff 0

(** checks whether the given equation represents a diff. equation dot x <> 0 *)
let has_nonzero_dynamics env ivars (v,e) =
  let dims = ApronUtil.dims_of_env env in
  let e = Apron.Linexpr1.get_linexpr0 e in
  let rec check_coeff j =
    if j>=dims then false
    else
      if (Apron.Coeff.is_zero (Apron.Linexpr0.get_coeff e j)) 
        then check_coeff (j+1)
      else true
  in
  (not (Apron.Coeff.is_zero (Apron.Linexpr0.get_cst e))) ||
  (check_coeff 0)

(******************************************************************************)
(** returns the variables of diff. equations with dot x <> 0 *)
let get_nonzero_dynamics_vars env eqs ivars =
  Array.of_list (fst (List.split 
    (List.filter (has_nonzero_dynamics env ivars) (Array.to_list eqs))))

(******************************************************************************)
(** returns the variables of diff. equations with dot x <> 0 *)
let get_noncst_dynamics_vars env eqs ivars =
  Array.of_list (fst (List.split 
    (List.filter (has_noncst_dynamics env ivars) (Array.to_list eqs))))


(******************************************************************************)
(* continuous time elapse operators *)
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
let cont_elapse2 get_primed_var trans_man staycond eqs ivars s = 
  let env = Apron.Abstract1.env s in
  let vars = ApronUtil.vars_of_env env in
  let (contvars,_) = Util.array_split eqs in
  let primed_vars = Array.map (get_primed_var) vars in
  let discvars = Util.array_diff vars 
    (Array.concat [contvars;ivars;primed_vars]) in
  (* non-constant continuous variables *)
  let vnd0 = get_nonzero_dynamics_vars env eqs ivars in
  Log.debug3_o logger (ApronUtil.print_vars) "non-zero dynamics vars = " vnd0;
  let npnvars = Util.array_diff vars primed_vars in
  Log.debug3_o logger (ApronUtil.print_vars) "non-primed vars = " npnvars;
  let svars = Util.array_diff npnvars ivars in

  (* add =0 equations for non-continuous variables *)
  let eqs = Array.append eqs 
    (ApronUtil.get_zero_equations_for env discvars) in
  (* current values without non-constant variables *)
  let s_vd0 = ApronUtil.project_forget s vnd0 in
  Log.debug3_o logger (ApronUtil.print_abstract) "s_vd0 = " s_vd0;

  (* equations to conjunction of constraints*)
  let fexpr = ApronUtil.get_fexpr (get_primed_var) env eqs in
  Log.debug3_o logger (ApronUtil.print_linconss) "fexpr = " fexpr;

  (* need to "weaken" staycond first into g(x)/\ g(xi) *)
  let staycond = Apron.Abstract1.of_lincons_array trans_man env staycond in
  let istaycond = ApronUtil.project_forget staycond svars in
  let sstaycond = ApronUtil.project_forget staycond ivars in
  let wstaycond = Apron.Abstract1.meet trans_man istaycond sstaycond in
  Log.debug3_o logger (ApronUtil.print_abstract) "weak staycond = " wstaycond;
  (* compute the abstract relation (dot x, x, n, xi) *)
  let s_rel = Apron.Abstract1.meet_lincons_array trans_man 
    (Apron.Abstract1.meet trans_man s_vd0 wstaycond) fexpr in
  Log.debug3_o logger (ApronUtil.print_abstract) "s_rel = " s_rel;
  (* compute the convex polyhedron in dot x *)
  let dotx = ApronUtil.project_forget s_rel npnvars in
  let d = ApronUtil.rename_primed_to_unprimed (get_primed_var) dotx svars in
  Log.debug3_o logger (ApronUtil.print_abstract) "d = " d;
  (* compute elapse *)
  let res = ApronUtil.elapse_with_domain s d in
  res

(******************************************************************************)
(** polyhedral time-elapse operator for constant dynamics (forget otherwise) *)
let cont_elapse1 trans_man staycond eqs ivars s = 
  let env = Apron.Abstract1.env s in
  let vars = ApronUtil.vars_of_env env in
  let (contvars,_) = Util.array_split eqs in
  let noncontvars = Util.array_diff vars 
    (Array.append (Array.append contvars ivars) 
                          (Array.map (ApronUtil.get_primed_var) vars)) in
  let svars = Util.array_diff vars ivars in
  (* get assertion on input variables from staying condition *)
  let istaycond = Apron.Abstract1.to_lincons_array trans_man
    (ApronUtil.project_forget 
      (Apron.Abstract1.of_lincons_array trans_man env staycond) svars) in
  Log.debug3_o logger (ApronUtil.print_linconss) "i-staycond = " istaycond;
  (* get variables with non-constant dyn. *)
  let noncstvars = get_noncst_dynamics_vars env eqs ivars in
  Log.debug3_o logger (ApronUtil.print_vars) "non-cst dynamics vars = " noncstvars;
  (* add =0 equations for non-continous variables *)
  let eqs = Array.append eqs 
    (ApronUtil.get_zero_equations_for env noncontvars) in
  (* compute translator *)
  let d = ApronUtil.get_translator trans_man eqs istaycond ivars in
  Log.debug3_o logger (ApronUtil.print_abstract) "d = " d;
  let s2 = ApronUtil.elapse_with_domain s d in
  ApronUtil.project_forget s2 noncstvars

(******************************************************************************)
(** simplest time-elapse operator 
     (forget everything about continuous variables) *)
let cont_elapse0 trans_man staycond eqs ivars s = 
  let (contvars,_) = Util.array_split eqs in
  ApronUtil.project_forget s contvars



(******************************************************************************)
(* flow transition operator *)
(******************************************************************************)
(** computes the time elapse of s by eqs up to staycond *)
let flow ~trans_man
         ?(cont_elapse=(cont_elapse2 ApronUtil.get_primed_var))
        staycond eqs ivars s = 
  Log.debug3_o logger (ApronUtil.print_abstract) "flow: s = " s;
  Log.debug3_o logger (ApronUtil.print_linconss) "flow: staycond = " staycond;
  Log.debug3_o logger (ApronUtil.print_equations) "flow: eqs = " eqs;
  Log.debug3_o logger ApronUtil.print_vars "flow: ivars = " ivars;

  let env = Apron.Abstract1.env s in

  (* compute time elapse *)
  let selapse = cont_elapse trans_man staycond eqs ivars 
    (ApronUtil.change_domain s trans_man) in
  Log.debug3_o logger (ApronUtil.print_abstract) "s-elapse: " selapse;
  (* project staying condition on state variables *)
  let sstaycond = ApronUtil.project_forget 
    (Apron.Abstract1.of_lincons_array trans_man env staycond) ivars in
  (* intersect with staying condition *)
  let res = Apron.Abstract1.meet trans_man selapse sstaycond in
  Log.debug3_o logger (ApronUtil.print_abstract) "flow(s) in trans_man = " res;
  let res = ApronUtil.change_domain res (Apron.Abstract1.manager s) in
  Log.debug3_o logger (ApronUtil.print_abstract) "flow(s) = " res;
  res

