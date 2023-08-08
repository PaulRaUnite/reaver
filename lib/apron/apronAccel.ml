(******************************************************************************)
(* ApronAccel *)
(* numerical acceleration *)
(* author: Peter Schrammel *)
(* version: 0.9.3 *)
(* remarks: *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="ApronAccel";
              Log.level=Log.Debug3}

type direction_t = [ `Forward | `Backward ]
type transtype_t = 
    |Identity
    |Reset
    |Translation
    |TransReset
    |NonAcc

(******************************************************************************)
(* printing *)
(******************************************************************************)

(** prints the direction *)
let print_direction fmt dir = 
  match dir with
    |`Forward -> Format.pp_print_string fmt "`Forward"
    |`Backward -> Format.pp_print_string fmt "`Backward"

(** prints the transition type *)
let print_transtype fmt transtype = 
  match transtype with
    |Identity -> Format.pp_print_string fmt "Identity"
    |Reset -> Format.pp_print_string fmt "Reset"
    |Translation -> Format.pp_print_string fmt "Translation"
    |TransReset -> Format.pp_print_string fmt "TransReset"
    |NonAcc -> Format.pp_print_string fmt "NonAcc"

(******************************************************************************)
(* image computation *)
(******************************************************************************)

(******************************************************************************)
(** computes tau(s) according to eqs *)
let post g eqs ivars s =
  let (vars,exprs) = Util.array_split eqs in
  let man =  Apron.Abstract1.manager s in
  let sg = Apron.Abstract1.meet_lincons_array man s g in
  Log.debug3_o logger (ApronUtil.print_abstract) "post: s/\\g = " sg;
  let spost = Apron.Abstract1.assign_linexpr_array man sg vars exprs None in
  ApronUtil.project_forget spost ivars

(******************************************************************************)
(** computes tau-1(s) according to eqs *)
let pre g eqs ivars s =
  let (vars,exprs) = Util.array_split eqs in
  let man =  Apron.Abstract1.manager s in
  let spre = Apron.Abstract1.substitute_linexpr_array man s vars exprs None in
  Log.debug3_o logger (ApronUtil.print_abstract) "pre: f^-1(s) = " spre;
  let spreg = Apron.Abstract1.meet_lincons_array man spre g in
  ApronUtil.project_forget spreg ivars

(******************************************************************************)
(** computes the post- or pre-image of s according to tau: g -> eqs *)
let image ?(dir=`Forward) g eqs ivars s =
  Log.debug3_o logger ApronUtil.print_abstract "image: s = " s;
  Log.debug3_o logger (print_direction) "image: dir = " dir;
  Log.debug3_o logger ApronUtil.print_linconss "image: g = "g;
  Log.debug3_o logger ApronUtil.print_equations "image: eqs = " eqs;
  Log.debug3_o logger ApronUtil.print_vars "image: ivars = " ivars;
  let res = match dir with 
    |`Forward -> post g eqs ivars s 
    |`Backward -> pre g eqs ivars s 
  in
  Log.debug3_o logger ApronUtil.print_abstract "image(s) = " res;
  res

(******************************************************************************)
(* acceleration *)
(******************************************************************************)

(******************************************************************************)
(** computes the forward acceleration of a translation *)
let acc_trans_forward g gs eqs ivars d s =
  let man = Apron.Abstract1.manager s in
  Log.debug3_o logger (ApronUtil.print_abstract) "s = " s;
  (* S/\G *)
  let sg = Apron.Abstract1.meet_lincons_array man s gs in
  Log.debug3_o logger (ApronUtil.print_abstract) "s/\\g = " sg;
  (* S/\G elapse D *)
  let sgd = ApronUtil.elapse_with_domain sg d in
  Log.debug3_o logger (ApronUtil.print_abstract) "s/\\g elapse d = " sgd;
  (* tau(S/\G elapse D)) *)
  let res = post g eqs ivars sgd in
  res

(******************************************************************************)
(** computes the backward acceleration of a translation *)
let acc_trans_backward g gs eqs ivars d s = 
  let man = Apron.Abstract1.manager s in
  let dd = ApronUtil.neg d in
  Log.debug3_o logger (ApronUtil.print_abstract) "s = " s;
  Log.debug3_o logger (ApronUtil.print_abstract) "-d = " dd;
  (* pre(S) *)
  let sp = pre g eqs ivars s in
  Log.debug3_o logger (ApronUtil.print_abstract) "pre(s) = " sp;
  (* pre(S) elapse -D *)
  let spd = ApronUtil.elapse_with_domain sp dd in
  Log.debug3_o logger (ApronUtil.print_abstract) "pre(s) elapse -d = " spd;
  (* (pre(S) elapse -D)/\G *)
  let spdg = Apron.Abstract1.meet_lincons_array man spd gs in
  ApronUtil.project_forget spdg ivars

(******************************************************************************)
(** computes the forward acceleration of a translation with resets *)
let acc_transreset_forward g gs eqs ivars reset_vars d s =
  let man = Apron.Abstract1.manager s in
  let env =  Apron.Abstract1.env s in
  let vars = ApronUtil.vars_of_env env in
  let trans_vars = Util.array_diff vars reset_vars in
  Log.debug3_o logger (ApronUtil.print_vars) "not reset_vars = " trans_vars;
  (* Dt, Dr *)
  let dt = ApronUtil.project_to_zero d reset_vars in
  Log.debug3_o logger (ApronUtil.print_abstract) "dt = " dt;
  let dr = ApronUtil.change_domain (ApronUtil.project_forget d trans_vars)man in
  Log.debug3_o logger (ApronUtil.print_abstract) "dr = " dr;

  (* tau(S) *)
  let taus = post g eqs ivars s in 
  Log.debug3_o logger (ApronUtil.print_abstract) "tau(s) = " taus;

  (* tau(S)/\G *)
  let sg = Apron.Abstract1.meet_lincons_array man taus gs in
  Log.debug3_o logger (ApronUtil.print_abstract) "tau(s)/\\g = " sg;

  let tauo  = 
    if Apron.Abstract1.is_bottom man sg then taus
    else
    begin
      (* (tau(S)/\G elapse D^t)+D^r *)
      let sgd = Apron.Abstract1.meet man
        (ApronUtil.project_forget 
          (ApronUtil.elapse_with_domain sg dt) reset_vars) 
        dr in
      Log.debug3_o logger (ApronUtil.print_abstract) 
        "(tau(S)/\\G elapse D^t)+D^r = " sgd;

      (* tau(((tau(S)/\\G elapse D^t)+D^r)) *)
      let sgdgd =  post g eqs ivars sgd in
      Log.debug3_o logger (ApronUtil.print_abstract) 
        "tau(((tau(S)/\\G elapse D^t)+D^r)) = " sgdgd;
      sgdgd
    end
  in

  (* tau(S) \\/ tau(((tau(S)/\\G elapse D^t)+D^r)) *) 
  let res = Apron.Abstract1.join man taus tauo in 
  res

(******************************************************************************)
(** computes the backward acceleration of a translation with resets*)
let acc_transreset_backward g gs eqs ivars reset_vars d s = 
  let man = Apron.Abstract1.manager s in
  let env =  Apron.Abstract1.env s in
  let vars = ApronUtil.vars_of_env env in
  let trans_vars = Util.array_diff vars reset_vars in
  Log.debug3_o logger (ApronUtil.print_vars) "not reset_vars = " trans_vars;
  (* pre(S) *)
  let sp = pre g eqs ivars s in 
  Log.debug3_o logger (ApronUtil.print_abstract) "pre(s) = " sp;

  if Apron.Abstract1.is_bottom man sp then s
  else
  begin
    let ddt = ApronUtil.neg (ApronUtil.project_to_zero d reset_vars) in
    Log.debug3_o logger (ApronUtil.print_abstract) "-dt = " ddt;
    let dr = ApronUtil.change_domain 
      (ApronUtil.project_forget d trans_vars) man in
    Log.debug3_o logger (ApronUtil.print_abstract) "dr = " dr;

    (* pre(s)|t elapse Dt *)
    let spd = ApronUtil.elapse_with_domain
      (ApronUtil.project_forget sp reset_vars) ddt in
    Log.debug3_o logger (ApronUtil.print_abstract) "pre(s)|t elapse Dt = " spd;

    (* (pre(s)|t elapse Dt)/\\gs\\dr *)
    let spdg = Apron.Abstract1.meet_lincons_array man
      (Apron.Abstract1.meet man 
        (ApronUtil.elapse_with_domain 
           (ApronUtil.project_forget sp reset_vars) ddt) 
        dr)
      gs in
    Log.debug3_o logger (ApronUtil.print_abstract) 
      "(pre(s)|t elapse Dt)/\\g/\\dr = " spdg;

    (* pre((pre(S)|t elapse Dt)/\\g/\\dr) *)
    let spdgp = pre g eqs ivars spdg in 
    Log.debug3_o logger (ApronUtil.print_abstract) 
      "pre((pre(s)|t elapse Dt)/\\g/\\dr) = " spdgp;
    (* pre(S)\/pre(...) *)
    let res = Apron.Abstract1.join man sp spdgp in
    res
  end

(******************************************************************************)
(** computes the weakest guard (cartesian product of state and input variables*)
let get_weakest_guard trans_man svars ivars g =
  let env = g.Apron.Lincons1.array_env in
  let g = Apron.Abstract1.of_lincons_array trans_man env g in
  (Apron.Abstract1.to_lincons_array trans_man 
    (ApronUtil.project_forget g ivars),
   Apron.Abstract1.to_lincons_array trans_man 
    (ApronUtil.project_forget g svars))

(******************************************************************************)
(* checks whether the given equation is the identity *)
let is_id_equ (v,e) =
  let env = Apron.Linexpr1.get_env e in
  let dims = ApronUtil.dims_of_env env in
  let e = Apron.Linexpr1.get_linexpr0 e in
  let vdim = Apron.Environment.dim_of_var env v in
  let rec check_coeff j =
    if j>=dims then true
    else
      let c = Apron.Linexpr0.get_coeff e j in
      if (vdim<>j) && (Apron.Coeff.is_zero c) ||
        (vdim=j) && (Apron.Coeff.equal_int c 1) then check_coeff (j+1)
      else false
  in
  (Apron.Coeff.is_zero (Apron.Linexpr0.get_cst e)) &&
  (check_coeff 0)

(******************************************************************************)
(* checks whether the given equation is a reset equation *)
let is_reset_equ idvars (v,e) =
  let env = Apron.Linexpr1.get_env e in
  let dims = ApronUtil.dims_of_env env in
  let e = Apron.Linexpr1.get_linexpr0 e in
  let vdim = Apron.Environment.dim_of_var env v in
  let rec check_coeff j =
    if j>=dims then true
    else
      let c = Apron.Linexpr0.get_coeff e j in
      if (vdim<>j) && ((Apron.Coeff.is_zero c) ||
         (Util.array_mem (Apron.Environment.var_of_dim env j) idvars)) ||
        (vdim=j) && (Apron.Coeff.is_zero c) then check_coeff (j+1)
      else false
  in
  check_coeff 0

(******************************************************************************)
(* returns the variables which are identities in eqs *)
let get_id_vars eqs =
  Array.of_list (Array.fold_left 
    (fun idvars (v,e) ->
      if is_id_equ (v,e) then v::idvars else idvars)
    [] eqs)

(******************************************************************************)
(* returns the variables which are reset in eqs *)
let get_reset_vars idvars eqs =
  Array.of_list (Array.fold_left 
    (fun rvars (v,expr) ->
      if is_reset_equ idvars (v,expr) then v::rvars
      else rvars)
    [] eqs)

(******************************************************************************)
(* acceleration operator *)
(******************************************************************************)
(** compute accelerated transition *)
let acc ?(dir=`Forward) 
        ~trans_man 
        g eqs ivars s = 
  Log.debug3_o logger (ApronUtil.print_abstract) "acc: s = " s;
  Log.debug3_o logger print_direction "acc: dir = " dir;
  Log.debug3_o logger (ApronUtil.print_linconss) "acc: g = " g;
  Log.debug3_o logger (ApronUtil.print_equations) "acc: eqs = " eqs;
  Log.debug3_o logger ApronUtil.print_vars "acc: ivars = " ivars;
 
  let env = Apron.Abstract1.env s in
  let man = Apron.Abstract1.manager s in
  let vars = ApronUtil.vars_of_env env in
  let svars = Util.array_diff vars ivars in

  (* compute weakest guard *)
  let (gs,gi) = get_weakest_guard trans_man svars ivars g in
  Log.debug3_o logger (ApronUtil.print_linconss) "acc: gs = " gs;
  Log.debug3_o logger (ApronUtil.print_linconss) "acc: gi = " gi;
  (* get id and reset vars *)
  let id_vars = get_id_vars eqs in
  let reset_vars = get_reset_vars id_vars eqs in
  let d = 
    if (Array.length id_vars)>0 then 
    begin
      let nonid_vars = Array.append ivars (Util.array_diff svars id_vars) in
      (* compute constraints for id_vars *)
      let sid = ApronUtil.project_forget s nonid_vars in
      let gg = Apron.Abstract1.to_lincons_array man 
        (Apron.Abstract1.meet_lincons_array man sid gi) in
      (* compute translator d *)
      let nonid_eqs = (Array.of_list (List.filter 
         (fun (v,_) -> Util.array_mem v nonid_vars) (Array.to_list eqs))) in
      let d = ApronUtil.get_translator trans_man nonid_eqs gg 
        (Array.append ivars id_vars) in
      (* add zero-constraints for id_vars to translator *)
      Apron.Abstract1.meet_lincons_array trans_man d 
           (ApronUtil.get_zero_linconss_for env id_vars) 
    end
    else
      ApronUtil.get_translator trans_man eqs gi ivars 
  in
  Log.debug3_o logger (ApronUtil.print_abstract) "translator d = " d;
  (* accelerate *)
  let res = if (Array.length reset_vars)=0 then 
      match dir with 
        |`Forward -> acc_trans_forward g gs eqs ivars d s
        |`Backward -> acc_trans_backward g gs eqs ivars d s
    else 
      match dir with 
        |`Forward -> acc_transreset_forward g gs eqs ivars reset_vars d s
        |`Backward -> acc_transreset_backward g gs eqs ivars reset_vars d s
  in
  Log.debug3_o logger (ApronUtil.print_abstract) "acc: res = " res;
  res

 
(******************************************************************************)
(* checking accelerability *)
(******************************************************************************)
let rec check_coeffs env e vdim dims ivars j 
   (is_nonacc,is_state_id,has_inputs) =
   if j>=dims then (is_nonacc,is_state_id,has_inputs)
   else
     let zero_coeff = Apron.Coeff.is_zero (Apron.Linexpr0.get_coeff e j) in
     let one_coeff = Apron.Coeff.equal_int 
       (Apron.Linexpr0.get_coeff e j) 1 in
     let is_input = Util.array_mem 
       (Apron.Environment.var_of_dim env j) ivars in
     match (vdim=j,zero_coeff,one_coeff,is_input) with
      |(false,false,_    ,false) -> (true     ,is_state_id,has_inputs)
      |(false,false,_    ,true ) -> check_coeffs env e vdim dims ivars (j+1) 
                                    (is_nonacc,is_state_id,true      )
      |(false,true ,_    ,_    ) -> check_coeffs env e vdim dims ivars (j+1) 
                                    (is_nonacc,is_state_id,has_inputs)
      |(true ,false,false,_    ) -> (true     ,is_state_id,has_inputs)
      |(true ,true ,false,_    ) -> check_coeffs env e vdim dims ivars(j+1) 
                                    (is_nonacc,false      ,has_inputs)
      |(true ,_    ,true ,_    ) -> check_coeffs env e vdim dims ivars (j+1) 
                                    (is_nonacc,is_state_id,has_inputs)

(******************************************************************************)
(** determines the transition type of a single equation *)
let get_transition_type_equ (v,e1) ivars =
  let e = Apron.Linexpr1.get_linexpr0 e1 in
  let env = Apron.Linexpr1.get_env e1 in
  let dims = ApronUtil.dims_of_env env in
  let vdim = Apron.Environment.dim_of_var env v in
  let has_nonzero_cst = 
        not (Apron.Coeff.is_zero (Apron.Linexpr0.get_cst e)) in
  match ((check_coeffs env e vdim dims ivars 0 
            (false,true,false)),has_nonzero_cst) with
    |((true,_,_),_) -> NonAcc
    |((false,true,false),false) -> Identity
    |((false,true,_),_) -> Translation
    |((false,false,_),_) -> Reset

(******************************************************************************)
(** determines the transition type of the transition function equs *)
let get_transition_type equs ivars =
  let id_vars = get_id_vars equs in
  let idivars = Array.append ivars id_vars in
  let rec check_equ i (has_reset,has_trans) =
    if i>=(Array.length equs) then
      match (has_reset,has_trans) with
	|(false,false) -> Identity
	|(false,true) -> Translation
	|(true,true) -> TransReset
	|(true,false) -> Reset
    else
    begin
      let (v,e1) = equs.(i) in
      let e = Apron.Linexpr1.get_linexpr0 e1 in
      let env = Apron.Linexpr1.get_env e1 in
      let dims = ApronUtil.dims_of_env env in
      let vdim = Apron.Environment.dim_of_var env v in
      let has_nonzero_cst = 
        not (Apron.Coeff.is_zero (Apron.Linexpr0.get_cst e)) in
      match ((check_coeffs env e vdim dims idivars 0 
                 (false,true,false)),has_nonzero_cst) with
	|((true,_,_),_) ->  NonAcc
	|((false,true,false),false) -> check_equ (i+1) (has_reset,has_trans)
	|((false,true,_),_) -> check_equ (i+1) (has_reset,true)
	|((false,false,_),_) -> check_equ (i+1) (true,has_trans)
    end
  in
  check_equ 0 (false,false)

(******************************************************************************)
let is_acc equs ivars = 
  match (get_transition_type equs ivars) with
    |Translation |TransReset -> true
    |_ -> false

(******************************************************************************)
let is_trivial equs ivars = 
  match (get_transition_type equs ivars) with
    |Identity |Reset -> true
    |_ -> false

(******************************************************************************)
let is_nonacc equs ivars = 
  match (get_transition_type equs ivars) with
    |NonAcc -> true
    |_ -> false
