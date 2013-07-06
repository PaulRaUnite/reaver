(******************************************************************************)
(* BddapronAccel *)
(* numerical acceleration in a logico-numerical context*)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* remarks: *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="BddapronAccel";
              Log.level=Log.Debug3}

type direction_t = ApronAccel.direction_t

(******************************************************************************)
(* acceleration operator *)
(******************************************************************************)
(** compute accelerated transition *)
let acc ?(dir=`Forward) 
        ~trans_apronman 
        env cond doman
        g equs i_num_vars s = 
  Log.debug3_o logger (BddapronUtil.print_abstract env doman) "acc: s = " s;
  Log.debug3_o logger (ApronAccel.print_direction) "acc: dir = " dir;
  Log.debug3_o logger (BddapronUtil.print_boolexpr env cond) "acc: g = " g;
  Log.debug3_o logger (BddapronUtil.print_equations env cond) "acc: equs = " 
    equs;
  Log.debug3_o logger (BddapronUtil.print_vars env) 
    "acc: i_num_vars = " i_num_vars;

  let sgb = Bddapron.Domain0.meet_condition doman env cond s 
    (BddapronUtil.boolexpr_remove_cond 
      (BddapronUtil.supp_of_vars env (BddapronUtil.boolvars_of_env env)) g) in
  let sres = 
    if not (Bddapron.Domain0.is_bottom doman sgb) then
    begin
      let apronenv = Bddapron.Env.apron env in
      let boolvars = BddapronUtil.boolvars_of_env env in

      let equs = BddapronUtil.simplify_equs env cond equs g in
      let gn = BddapronUtil.boolexpr_to_linconss env cond doman boolvars g in
      let eqs = BddapronUtil.numequs_of_equs env cond equs in
      let slist = Bddapron.Domain0.to_bddapron doman sgb in
      let sacclist = List.map
        (fun (sb,sn) ->
          (* compute post_\oplus_numequs *)
          let sn1 = {Apron.Abstract1.abstract0=sn; 
	      Apron.Abstract1.env=apronenv} in
          let sn2 = ApronAccel.acc ~dir ~trans_man:trans_apronman 
              gn eqs (BddapronUtil.vars_to_apronvars env i_num_vars) sn1 in
          (sb,Apron.Abstract1.abstract0 sn2))
        slist 
      in
      Bddapron.Domain0.of_bddapron doman env sacclist
    end
    else sgb
  in
  Log.debug3_o logger (BddapronUtil.print_abstract env doman) 
    "acc: post_oplus(s) = " sres;
  sres

(******************************************************************************)
(* checking for accelerability *)
(******************************************************************************)

let get_transition_type_action env cond i_num_vars v apronaction =
  let linexpr = BddapronUtil.apronaction_to_linexpr env cond apronaction in
  ApronAccel.get_transition_type_equ 
    (BddapronUtil.var_to_apronvar env v,linexpr) 
    (BddapronUtil.vars_to_apronvars env i_num_vars)

let get_transition_type_actions env cond i_num_vars vars apronactions =
  let eqs = Util.array_map2 
    (fun v a -> 
       (BddapronUtil.var_to_apronvar env v,
        BddapronUtil.apronaction_to_linexpr env cond a)) 
    vars apronactions in
  ApronAccel.get_transition_type eqs 
    (BddapronUtil.vars_to_apronvars env  i_num_vars)

let get_transition_type_numequs env cond i_num_vars numequs =
  try
  begin
    let eqs = Array.of_list (List.map 
      (fun (v,expr) -> 
        match expr with
        |`Apron(expr) -> (BddapronUtil.var_to_apronvar env v,
                           BddapronUtil.apronexpr_to_linexpr env cond expr)
        |_ -> assert(false))
      numequs)
    in
    ApronAccel.get_transition_type eqs 
      (BddapronUtil.vars_to_apronvars env i_num_vars)
  end
  with BddapronUtil.NotLinearAction _ -> ApronAccel.NonAcc


(******************************************************************************)
(* TODO: does not take into account dependencies on unmodified variables *)
let is_acc_action env cond i_num_vars v apronaction = 
  match (get_transition_type_action env cond i_num_vars v apronaction) with
    |ApronAccel.Translation |ApronAccel.TransReset -> true
    |_ -> false

let is_acc_actions env cond i_num_vars vars apronactions = 
  match (get_transition_type_actions env cond i_num_vars vars apronactions) with
    |ApronAccel.Translation |ApronAccel.TransReset -> true
    |_ -> false

let is_acc_numequs env cond i_num_vars numequs = 
  match (get_transition_type_numequs env cond i_num_vars numequs) with
    |ApronAccel.Translation |ApronAccel.TransReset -> true
    |_ -> false

(******************************************************************************)
let is_trivial_action env cond i_num_vars v apronaction = 
  match (get_transition_type_action env cond i_num_vars v apronaction) with
    |ApronAccel.Identity |ApronAccel.Reset -> true
    |_ -> false

let is_trivial_actions env cond i_num_vars vars apronactions = 
  match (get_transition_type_actions env cond i_num_vars vars apronactions) with
    |ApronAccel.Identity |ApronAccel.Reset -> true
    |_ -> false

let is_trivial_numequs env cond i_num_vars numequs = 
  match (get_transition_type_numequs env cond i_num_vars numequs) with
    |ApronAccel.Identity |ApronAccel.Reset -> true
    |_ -> false

(******************************************************************************)
let is_nonacc_action env cond i_num_vars v apronaction = 
  match (get_transition_type_action env cond i_num_vars v apronaction) with
    |ApronAccel.NonAcc -> true
    |_ -> false

let is_nonacc_actions env cond i_num_vars vars apronactions = 
  match (get_transition_type_actions env cond i_num_vars vars apronactions) with
    |ApronAccel.NonAcc -> true
    |_ -> false

let is_nonacc_numequs env cond i_num_vars numequs = 
  match (get_transition_type_numequs env cond i_num_vars numequs) with
    |ApronAccel.NonAcc -> true
    |_ -> false

(******************************************************************************)
(* returns true if there is an accelerable action *)
(* TODO: does not take into account dependencies on unmodified variables *)
let exists_acc_action env cond i_num_vars vars apronactions =
  Util.array_exists2 (is_acc_action env cond i_num_vars) 
    vars apronactions
