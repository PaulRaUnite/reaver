(******************************************************************************)
(* ApronUtil *)
(* utilities for manipulating APRON entities *)
(* author: Peter Schrammel *)
(* version: 0.9.3 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="ApronUtil";
              Log.level=Log.Debug3}

type 'a man_t = 'a Apron.Manager.t
type env_t = Apron.Environment.t
type linexpr_t = Apron.Linexpr1.t
type lincons_t = Apron.Lincons1.t
type linconss_t = Apron.Lincons1.earray
type var_t = Apron.Var.t
type vars_t = Apron.Var.t array
type equ_t = var_t * linexpr_t
type equs_t = equ_t array
type 'a abstract_t = 'a Apron.Abstract1.t

(******************************************************************************)
(* printing functions *)
(******************************************************************************)
(** prints an APRON linear equation *)
let print_equation fmt (v,e) =
  Apron.Var.print fmt v;
  Format.pp_print_string fmt "' = "; 
  Apron.Linexpr1.print fmt e

(******************************************************************************)
(** prints an APRON abstract value *)
let print_abstract = Apron.Abstract1.print

(******************************************************************************)
(** prints APRON linear equations *)
let print_equations fmt equs = 
  Array.iter 
    (fun eq -> print_equation fmt eq;
      Format.pp_print_string fmt "; "
    ) equs

(******************************************************************************)
(** prints an array of APRON variables *)
let print_vars fmt vars =
  Format.pp_print_string fmt "[";
  Array.iter
    (fun v -> Apron.Var.print fmt v;
       Format.pp_print_string fmt "; ")
    vars;
  Format.pp_print_string fmt "]"

(******************************************************************************)
(** prints APRON linear constraints *)
let print_linconss fmt linconss = Apron.Lincons1.array_print fmt linconss


(******************************************************************************)
(* utilities *)
(******************************************************************************)
(** primes a variable *)
let get_primed_var v = Apron.Var.of_string ((Apron.Var.to_string v)^"'")

(******************************************************************************)
(** builds the constraint array  v'=e from the given equations *)
let get_fexpr get_primed_var env eqs =
  let minus_one = Apron.Coeff.s_of_int (-1) in
  let len = Array.length eqs in
  let fexpr = Apron.Lincons1.array_make env len in
  for i=0 to len-1 do begin
    let (v,e) = eqs.(i) in
    let lincons = Apron.Lincons1.make e Apron.Lincons1.EQ in
    Apron.Lincons1.set_coeff lincons (get_primed_var v) minus_one;
    Apron.Lincons1.array_set fexpr i lincons;
  end done;
  fexpr

(******************************************************************************)
(** gets the array of variables in the APRON environment *)
let vars_of_env env =
  let (ivars,rvars) = Apron.Environment.vars env in
  Array.append ivars rvars

(******************************************************************************)
(** returns the sub-environment containing the given variables *)
let env_of_vars env vars =
  let (ivars,rvars) = 
    List.partition (fun v -> 
      match Apron.Environment.typ_of_var env v with
	|Apron.Environment.INT -> true |_ -> false)
    (Array.to_list vars)
  in
  Apron.Environment.make (Array.of_list ivars) (Array.of_list rvars)

(******************************************************************************)
(** returns the number of dimensions of env *)
let dims_of_env env = 
  let dim = Apron.Environment.dimension env in
  dim.Apron.Dim.intd+dim.Apron.Dim.reald

(******************************************************************************)
(** checks whether the expression depends on at least one of the given 
    variables *)
let linexpr_depends_on_some_var e vars =
  let dims = dims_of_env (Apron.Linexpr1.get_env e) in
  let e = Apron.Linexpr1.get_linexpr0 e in
  let rec check_coeffs j =
    if j>=dims then false 
    else 
      if Apron.Coeff.is_zero (Apron.Linexpr0.get_coeff e j) then
        check_coeffs (j+1) 
      else true
  in
  check_coeffs 0

(******************************************************************************)
(** creates the linexpr 0 *)
let make_zero_linexpr env = 
  let dims = dims_of_env env in
  let zero = Apron.Coeff.s_of_int 0 in
  let linexpr = Apron.Linexpr0.make (Some dims) in
  Apron.Linexpr0.set_cst linexpr zero;
  for i=0 to dims-1 do 
    Apron.Linexpr0.set_coeff linexpr i zero;
  done;
  {Apron.Linexpr1.linexpr0 = linexpr; Apron.Linexpr1.env = env}

(******************************************************************************)
(** creates equations (v,0) for the given variables *)
let get_zero_equations_for env vars =
  Array.map (fun v ->  (v,make_zero_linexpr env)) vars

(******************************************************************************)
(** returns the identity transition function for a set of variables *)
let get_id_equations_for env vars =
  Array.map (fun v -> 
      let cons = make_zero_linexpr env in
      Apron.Linexpr1.set_coeff cons v (Apron.Coeff.s_of_int 1);
      (v,cons)) 
    vars

(******************************************************************************)
(** creates the constraints (v=0) for the given variables *)
let get_zero_linconss_for env vars =
  let len = Array.length vars in
  let one = Apron.Coeff.s_of_int 1 in
  let conss = Apron.Lincons1.array_make env len in
  for i=0 to len-1 do
    let expr = Apron.Linexpr1.make env in
    Apron.Linexpr1.set_coeff expr vars.(i) one;
    let cons = Apron.Lincons1.make expr Apron.Lincons0.EQ in
    Apron.Lincons1.array_set conss i cons
  done;
  conss

(******************************************************************************)
(** APRON abstract bottom *)
let bottom0 man env = 
  let dim = Apron.Environment.dimension env in
  Apron.Abstract0.bottom man dim.Apron.Dim.intd 
    dim.Apron.Dim.reald

(******************************************************************************)
let abstract1_of_abstract0 env s =
  {Apron.Abstract1.abstract0 = s;
   Apron.Abstract1.env = env}

(******************************************************************************)
let lincons1_of_lincons0 env c =
  {Apron.Lincons1.lincons0 = c;
   Apron.Lincons1.env = env}

(******************************************************************************)
(** rename primed variables to unprimed variables in the abstract value *)
let rename_primed_to_unprimed get_primed_var abstract1 vars =
  let env = Apron.Abstract1.env abstract1 in
  let man = Apron.Abstract1.manager abstract1 in
  let zero = Apron.Coeff.s_of_int 0 in
  let indexmap = 
    Array.map (fun v -> 
        (Apron.Environment.dim_of_var env (get_primed_var v),
         Apron.Environment.dim_of_var env v))
      vars in
  let earr = Apron.Abstract1.to_lincons_array man abstract1 in
  Array.iter 
    (fun lincons0 ->
      let linexpr0 = lincons0.Apron.Lincons0.linexpr0 in
       Array.iter
         (fun (iprimed,i) ->
           Apron.Linexpr0.set_coeff linexpr0 i 
             (Apron.Linexpr0.get_coeff linexpr0 iprimed);
           Apron.Linexpr0.set_coeff linexpr0 iprimed zero)
         indexmap)
    earr.Apron.Lincons1.lincons0_array;
  let dim = Apron.Environment.dimension env in
  {Apron.Abstract1.abstract0 = 
    Apron.Abstract0.of_lincons_array man dim.Apron.Dim.intd 
      dim.Apron.Dim.reald earr.Apron.Lincons1.lincons0_array;
   Apron.Abstract1.env = env}

(******************************************************************************)
(* operations on abstract values *)
(******************************************************************************)
(** converts the abstract value s to the domain of the given manager *)
let change_domain s man =
  let sman = Apron.Abstract1.manager s in
  let sarr = Apron.Abstract1.to_lincons_array sman s in
  Apron.Abstract1.of_lincons_array man (Apron.Abstract1.env s) sarr

(******************************************************************************)
(** generalized time elapse = s+t*d, t>=0 *)
let elapse s d =
  let man = Apron.Abstract1.manager d in
  let dgen = Apron.Abstract1.to_generator_array man d in
  for i=0 to (Apron.Generator1.array_length dgen)-1 do
    let g1 = Apron.Generator1.array_get dgen i in
    if Apron.Generator1.get_typ g1 == Apron.Generator0.VERTEX then
      Apron.Generator1.set_typ g1 Apron.Generator0.RAY
  done;
  Apron.Abstract1.add_ray_array man s dgen

(******************************************************************************)
(** generalized time elapse in the domain of d, 
    the domain of s may be different *)
let elapse_with_domain s d =
  let dman = Apron.Abstract1.manager d in
  let snew = elapse (change_domain s dman) d in
  change_domain snew (Apron.Abstract1.manager s)

(******************************************************************************)
(** projects out the given variables from s, but keeps their dimensions *)
let project_forget s forget_vars =
Apron.Abstract1.forget_array (Apron.Abstract1.manager s) s forget_vars false

(******************************************************************************)
(** projects the zero_vars of s onto the hyperplane 
    where all zero_vars=0, i.e. (exists zero_vars.S)/\(/\zero_vars=0) *)
let project_to_zero s zero_vars =
  let man = Apron.Abstract1.manager s in
  let env = Apron.Abstract1.env s in
  let cp1 = Apron.Coeff.Scalar (Apron.Scalar.of_int 1) in
  let lexpr0 = Array.fold_left
    (fun lexprs v ->
      let e = Apron.Linexpr1.make env in
      Apron.Linexpr1.set_coeff e v cp1;
      e::lexprs)
    [] zero_vars 
  in
  let linconsarr = Apron.Lincons1.array_make env (Array.length zero_vars) in
  Array.iteri (fun i lc -> Apron.Lincons1.array_set linconsarr i 
                (Apron.Lincons1.make lc Apron.Lincons1.EQ))
    (Array.of_list lexpr0);
  let s0 = Apron.Abstract1.of_lincons_array man env linconsarr in
  Apron.Abstract1.meet man 
    (Apron.Abstract1.forget_array man s zero_vars false) s0

(******************************************************************************)
(** negation of an abstract value: x in -S <=> -x in S *)
let neg s =
  let man = Apron.Abstract1.manager s in
  let env = Apron.Abstract1.env s in
  let linconsarr = Apron.Abstract1.to_lincons_array man s in
  for i=0 to (Apron.Lincons1.array_length linconsarr)-1 do
  begin
    let lincons = Apron.Lincons1.array_get linconsarr i in
    let linexpr = Apron.Lincons1.get_linexpr1 lincons in
    Apron.Linexpr1.iter
     (fun c v -> Apron.Linexpr1.set_coeff linexpr v (Apron.Coeff.neg c))
     linexpr
  end done;
  Apron.Abstract1.of_lincons_array man env linconsarr

(******************************************************************************)
(** returns the translator d in the domain man *)
let get_translator man eqs g ivars =
(*  Log.debug3_o logger (print_equations) "eqs = " eqs;
  Log.debug3_o logger (print_linconss) "g = " g;
  Log.debug3_o logger print_vars "ivars = " ivars;*)
  let env = Apron.Lincons1.array_get_env g in
  let dt = Apron.Lincons1.array_make env (Array.length eqs) in
  let minus_one = Apron.Coeff.s_of_int (-1) in
  Array.iteri
    (fun j eq ->
      let (v,expr) = eq in
      let newexpr = Apron.Linexpr1.copy expr in
      Apron.Linexpr1.set_coeff newexpr v minus_one;
      Apron.Lincons1.array_set dt j 
        (Apron.Lincons1.make newexpr Apron.Lincons1.EQ)
    )
    eqs;
  (* J xi <= k /\ d = T xi + u *)
  let dxi = Apron.Abstract1.meet_lincons_array man 
    (Apron.Abstract1.of_lincons_array man env g) dt in
 project_forget dxi ivars

(******************************************************************************)
let lincons0_is_eq c1 c2 =
  (c1.Apron.Lincons0.typ = c2.Apron.Lincons0.typ) &&
  ((Apron.Linexpr0.compare 
      c1.Apron.Lincons0.linexpr0 c2.Apron.Lincons0.linexpr0)=0)

let linconss_is_eq a1 a2 =
  ((Apron.Lincons1.array_get_env a1)=(Apron.Lincons1.array_get_env a2)) &&
  (List.fold_right2 
    (fun c1 c2 res -> res && (lincons0_is_eq c1 c2))
    (Array.to_list a1.Apron.Lincons1.lincons0_array)
    (Array.to_list a2.Apron.Lincons1.lincons0_array)
    true)

let linconss_is_unsat linconss = 
  ((Array.length linconss.Apron.Lincons1.lincons0_array)=1 && 
       (Apron.Lincons1.is_unsat 
         (lincons1_of_lincons0 linconss.Apron.Lincons1.array_env 
            linconss.Apron.Lincons1.lincons0_array.(0))))

let lincons_is_false = Apron.Lincons1.is_unsat
let lincons_is_true c = 
  lincons0_is_eq (Apron.Lincons1.get_lincons0 c) 
    (Apron.Lincons0.make (Apron.Linexpr0.make None)
       Apron.Lincons0.SUPEQ)

let linconss_empty env = 
  {Apron.Lincons1.lincons0_array=[||];Apron.Lincons1.array_env=env}

let linconss_false env = 
  {Apron.Lincons1.lincons0_array=[|Apron.Lincons1.get_lincons0 (Apron.Lincons1.make_unsat env)|];
   Apron.Lincons1.array_env=env}

let linconss_true = linconss_empty

let linconss_append l1 l2 = 
  {Apron.Lincons1.lincons0_array=Array.append 
    l1.Apron.Lincons1.lincons0_array l2.Apron.Lincons1.lincons0_array;
   Apron.Lincons1.array_env=l1.Apron.Lincons1.array_env}

(******************************************************************************)
let coeff_is_infty c = 
  match c with 
    |Apron.Coeff.Scalar s -> (Apron.Scalar.is_infty s)>0
    |Apron.Coeff.Interval i -> (Apron.Scalar.is_infty i.Apron.Interval.inf)>0

let coeff_is_neginfty c = 
  match c with 
    |Apron.Coeff.Scalar s -> (Apron.Scalar.is_infty s)<0
    |Apron.Coeff.Interval i -> (Apron.Scalar.is_infty i.Apron.Interval.inf)<0


(******************************************************************************)
(* OLD STUFF *)
(******************************************************************************)
(* removes the dimensions of the given variables from s *) 
(*let project_remove s remove_vars =
  let spenv = Apron.Environment.remove (Apron.Abstract1.env s) remove_vars in
  Apron.Abstract1.change_environment (Apron.Abstract1.manager s) s spenv false
*)

(******************************************************************************)
(* adds the abstract values s1 and s2 point-by-point (Minkowski sum) *)
(*let add s1 s2 = 
  let man = (Apron.Abstract1.manager s1) in
  assert(Apron.Environment.equal (Apron.Abstract1.env s1) 
           (Apron.Abstract1.env s2));
  let (s1ivars,s1rvars) = Apron.Environment.vars (Apron.Abstract1.env s1) in
  let to_prime1 v = Apron.Var.of_string ((Apron.Var.to_string v)^"_") in
  let to_prime2 v = Apron.Var.of_string ((Apron.Var.to_string v)^"__") in
  let (s1nivars,s1nrvars) = 
    ((Array.map to_prime1 s1ivars),(Array.map to_prime1 s1rvars)) in
  let (s2nivars,s2nrvars) = 
    ((Array.map to_prime2 s1ivars),(Array.map to_prime2 s1rvars)) in

  let s1vars = Array.append s1ivars s1rvars in
  let s1nvars = Array.append s1nivars s1nrvars in 
  let s2nvars = Array.append s2nivars s2nrvars in 
  let s12nivars = Array.append (Array.append s1ivars s1nivars) s2nivars in
  let s12nrvars = Array.append (Array.append s1rvars s1nrvars) s2nrvars in
  let s12nenv = Apron.Environment.make s12nivars s12nrvars in
  let xvars = Array.mapi
    (fun i v -> (v,Array.get s1nvars i,Array.get s2nvars i)) s1vars in

  let s1n = Apron.Abstract1.change_environment man
    (Apron.Abstract1.rename_array man s1
      s1vars s1nvars)
    s12nenv false in

  let s2n = Apron.Abstract1.change_environment man 
    (Apron.Abstract1.rename_array man s2
      s1vars s2nvars)
    s12nenv false in
  (* P' /\ Q''*)
  let s1n2n = Apron.Abstract1.meet man s1n s2n in
  (* /\x=x'+x'' *)
  let xexx = 
    let n = Array.length s1vars in
    let cm1 = Apron.Coeff.Scalar (Apron.Scalar.of_int (-1)) in
    let cp1 = Apron.Coeff.Scalar (Apron.Scalar.of_int 1) in
    let linarr = Apron.Lincons1.array_make s12nenv n in
    Array.iteri 
      (fun i xline ->
        let (x,x1,x2) = xline in
        let linexpr = Apron.Linexpr1.make s12nenv in
        Apron.Linexpr1.set_array linexpr 
          (Array.of_list [(cp1,x);(cm1,x1);(cm1,x2)]) None;
        let lincons = Apron.Lincons1.make linexpr Apron.Lincons1.EQ in
        Apron.Lincons1.array_set linarr i lincons)
        xvars;
    linarr in
  (* P' /\ Q'' /\ /\x=x'+x'' *)
  let s12n = Apron.Abstract1.meet_lincons_array man s1n2n xexx in
  (* exists x',x''. P' /\ Q'' /\ /\x=x'+x'' *)
  Apron.Abstract1.change_environment  man s12n (Apron.Abstract1.env s1) false
    *)
