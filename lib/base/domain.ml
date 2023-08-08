(******************************************************************************)
(* Domain *)
(* wrapper for abstract domains *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Base.Domain";
              Log.level=Log.Debug3}

exception NotSupported of string

(******************************************************************************)
(** {2 Domain Interface } *)
(******************************************************************************)

module type T = 
  sig
    type t
    type numdomain_t
    type doman_t
    type doman_param_t

    val make_doman : doman_param_t ->  doman_t
    val makeinit_doman : doman_param_t -> doman_t
    val doman : unit -> doman_t
    val get_doman_param : unit -> doman_param_t

    val print : ?doman:doman_t -> Env.t -> Format.formatter -> t -> unit

    val bottom : ?doman:doman_t -> Env.t-> t
    val top : ?doman:doman_t -> Env.t -> t

    val canonicalize : ?doman:doman_t -> Env.t-> t -> unit

    val is_eq : ?doman:doman_t -> Env.t -> t -> t -> bool
    val is_leq : ?doman:doman_t -> Env.t -> t -> t -> bool
    val is_bottom : ?doman:doman_t -> Env.t -> t -> bool

    val join : ?doman:doman_t -> Env.t -> t -> t -> t
    val meet : ?doman:doman_t -> Env.t -> t -> t -> t
    val meet_condition : ?doman:doman_t -> Env.t-> t -> Env.boolexpr_t -> t
    val widening : ?doman:doman_t -> Env.t -> t -> t -> t
    val assign_lexpr : ?doman:doman_t -> Env.t -> t -> Env.equs_t -> t
    val substitute_lexpr : ?doman:doman_t -> Env.t -> t -> Env.equs_t -> t
    val forget_list : ?doman:doman_t -> Env.t -> t -> Env.vars_t -> t

    val flow : ?doman:doman_t -> Env.t -> t -> Env.equs_t -> Env.boolexpr_t -> t
    val accel : ?doman:doman_t -> ?dir:ApronAccel.direction_t -> Env.t -> t -> Env.equs_t -> Env.boolexpr_t -> t

    val of_num : ?doman:doman_t -> Env.t -> numdomain_t -> t
    val of_boolexpr : ?doman:doman_t -> Env.t -> Env.boolexpr_t -> t
    val of_boolnumlist : ?doman:doman_t -> Env.t -> (Env.boolexpr_t * numdomain_t) list -> t

    val to_boolexpr : ?doman:doman_t -> Env.t -> t -> Env.boolexpr_t
    val to_boolnumlist : ?doman:doman_t -> Env.t -> t -> (Env.boolexpr_t * numdomain_t) list
    val to_boolexprbool : ?doman:doman_t -> Env.t -> t -> Env.boolexpr_t
    val to_boollinconsslist : ?doman:doman_t -> Env.t -> t -> (Env.boolexpr_t * ApronUtil.linconss_t) list

    val meet_to_boolexpr : ?doman:doman_t -> Env.t -> t -> Env.boolexpr_t -> Env.boolexpr_t
    val meetbool_to_boolexpr : ?doman:doman_t -> Env.t -> t -> Env.boolexpr_t -> 
      Env.boolexpr_t
  end

module type BDDAPRON_MAN_T = 
  sig
    type apronman_t
    type 'a bddapronman_t
    type ('a, 'b) man_t = ('a, apronman_t, 'a bddapronman_t, 'b) Bddapron.Domain0.man

    val make_man : unit -> ('a, 'b) man_t

  end

module type NOPARAM_T = (T with type doman_param_t = unit)
module type BDDAPRON_FUNC_T = functor (Man : BDDAPRON_MAN_T) ->
  (NOPARAM_T with type t = Env.var_t Bddapron.Domain0.t
     with type numdomain_t = Man.apronman_t Apron.Abstract0.t
     with type doman_t = (Env.var_t, Env.var_t Bddapron.Domain0.t) Man.man_t
  ) 


(******************************************************************************)
(** Bddapron t: B^p -> A *)
(******************************************************************************)
module Pow(Man: BDDAPRON_MAN_T) =
struct
type t = Env.var_t Bddapron.Domain0.t
type numdomain_t = Man.apronman_t Apron.Abstract0.t
type doman_t = (Env.var_t, Env.var_t Bddapron.Domain0.t) Man.man_t
type doman_param_t = unit

let domanref = ref(None)

let make_doman () = Man.make_man ()

let makeinit_doman () = 
  let d = Man.make_man () in
  domanref := Some d;
  d

let doman () = 
  match !domanref with
    |Some d -> d
    |None -> assert(false)

let get_doman_param () = ()

let meet_condition ?(doman=doman()) env s e = 
    Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond s e

let is_bottom ?(doman=doman()) env s = Bddapron.Domain0.is_bottom doman s

let print ?(doman=doman()) env fmt s = Bddapron.Domain0.print doman env.Env.env fmt s

let is_eq ?(doman=doman()) env s1 s2 = Bddapron.Domain0.is_eq doman s1 s2

let bottom ?(doman=doman()) env = Bddapron.Domain0.bottom doman env.Env.env 

let canonicalize ?(doman=doman()) env s = Bddapron.Domain0.canonicalize doman s

let is_leq ?(doman=doman()) env s1 s2 = Bddapron.Domain0.is_leq doman s1 s2

let join ?(doman=doman()) env s1 s2 = Bddapron.Domain0.join doman s1 s2
let meet ?(doman=doman()) env s1 s2 = Bddapron.Domain0.meet doman s1 s2

let widening ?(doman=doman()) env s1 s2 = Bddapron.Domain0.widening doman s1 s2

let assign_lexpr ?(doman=doman()) env s equs = 
  let (vars,exprs) = Env.split_equs equs in
  Bddapron.Domain0.assign_lexpr 
      doman env.Env.env env.Env.cond s 
      vars exprs None

let forget_list ?(doman=doman()) env s vars = 
    Bddapron.Domain0.forget_list doman env.Env.env s  vars

let substitute_lexpr ?(doman=doman()) env s equs = 
  let (vars,exprs) = Env.split_equs equs in
  Bddapron.Domain0.substitute_lexpr 
      doman env.Env.env env.Env.cond s 
      vars exprs None

let flow ?(doman=doman()) env s equs staycond = 
  let trans_doman = doman in
  let cont_elapse = BddapronHybrid.cont_elapse2 ~convexify:false
    (BddapronUtil.get_primed_var env.Env.env) in
  BddapronHybrid.flow ~trans_doman env.Env.env env.Env.cond doman
        ~cont_elapse staycond equs env.Env.i_vars s

let accel ?(doman=doman()) ?(dir=`Forward) env s equs g = 
  let trans_apronman = Bddapron.Domain0.man_get_apron doman in
  join ~doman env s (BddapronAccel.acc ~dir ~trans_apronman 
    env.Env.env env.Env.cond doman g equs env.Env.ni_vars s)

let top ?(doman=doman()) env = Bddapron.Domain0.top doman env.Env.env 

let of_boolnumlist ?(doman=doman()) env bnlist = 
  Bddapron.Domain0.of_bddapron doman env.Env.env bnlist

let of_num ?(doman=doman()) env apron = 
  Bddapron.Domain0.of_apron doman env.Env.env apron

let of_boolexpr ?(doman=doman()) env bexpr =  
  Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond
    (top env) bexpr

let to_boolexprbool ?(doman=doman()) env s =
  let l = Bddapron.Domain0.to_bddapron doman s in
  List.fold_left 
    (fun bacc (be,_)-> Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc be)
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond)
    l

let to_boolexpr ?(doman=doman()) env s =
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let l = Bddapron.Domain0.to_bddapron doman s in
  List.fold_left 
    (fun bacc (be,ne)-> 
       Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc 
         (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond be
           (BddapronUtil.apron_to_boolexpr env.Env.env env.Env.cond
             apronman
             (ApronUtil.abstract1_of_abstract0 env.Env.apronenv ne))))
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond)
    l

let to_boolnumlist ?(doman=doman()) env s = Bddapron.Domain0.to_bddapron doman s

let to_boollinconsslist ?(doman=doman()) env s =
  let apronman = Bddapron.Domain0.man_get_apron doman in
  List.map (fun (b,n) -> (b,Apron.Abstract1.to_lincons_array apronman
                        (ApronUtil.abstract1_of_abstract0 env.Env.apronenv n)))
    (Bddapron.Domain0.to_bddapron doman s)

let meet_to_boolexpr ?(doman=doman()) env s bexpr = 
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond bexpr (to_boolexpr env s)
let meetbool_to_boolexpr ?(doman=doman()) env s bexpr = 
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond bexpr 
    (to_boolexprbool env s)
end

(******************************************************************************)
(** product t: p(B^p) x A, based on Bddapron t *)
(******************************************************************************)
module Prod(Man: BDDAPRON_MAN_T) =
struct
type t = Env.var_t Bddapron.Domain0.t
type numdomain_t = Man.apronman_t Apron.Abstract0.t
type doman_t = (Env.var_t, Env.var_t Bddapron.Domain0.t) Man.man_t
type doman_param_t = unit

let domanref = ref(None)

let make_doman () = Man.make_man ()

let makeinit_doman () = 
  let d = Man.make_man () in
  domanref := Some d;
  d

let doman () = 
  match !domanref with
    |Some d -> d
    |None -> assert(false)

let get_doman_param () = ()

let print ?(doman=doman()) env fmt s = Bddapron.Domain0.print doman env.Env.env fmt s

let convexify ?(doman=doman()) env s =
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let l = Bddapron.Domain0.to_bddapron doman s in
  let (b,n) = List.fold_left 
    (fun (bacc,nacc) (be,ne) -> 
      (Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc be,
       Apron.Abstract0.join apronman nacc ne))
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond,
     ApronUtil.bottom0 apronman env.Env.apronenv)
    l
  in
  Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond 
    (Bddapron.Domain0.of_apron doman env.Env.env n) b

let meet_condition ?(doman=doman()) env  s e = 
  convexify env 
    (Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond s e)

let is_bottom ?(doman=doman()) env s = Bddapron.Domain0.is_bottom doman s


let is_eq ?(doman=doman()) env s1 s2 = Bddapron.Domain0.is_eq doman s1 s2

let bottom ?(doman=doman()) env = Bddapron.Domain0.bottom doman env.Env.env 
let top ?(doman=doman()) env = Bddapron.Domain0.top doman env.Env.env 

let canonicalize ?(doman=doman()) env s = Bddapron.Domain0.canonicalize doman s

let is_leq ?(doman=doman()) env s1 s2 = Bddapron.Domain0.is_leq doman s1 s2

let join ?(doman=doman()) env s1 s2 = convexify env (Bddapron.Domain0.join doman s1 s2)
let meet ?(doman=doman()) env s1 s2 = convexify env (Bddapron.Domain0.meet doman s1 s2)

let widening ?(doman=doman()) env s1 s2 = 
  convexify env (Bddapron.Domain0.widening doman s1 s2)

let assign_lexpr ?(doman=doman()) env s equs = 
  let (vars,exprs) = Env.split_equs equs in 
  convexify env 
    (Bddapron.Domain0.assign_lexpr doman env.Env.env env.Env.cond s 
       vars exprs None)

let forget_list ?(doman=doman()) env s vars = 
  convexify env 
    (Bddapron.Domain0.forget_list doman env.Env.env s  vars)

let substitute_lexpr ?(doman=doman()) env s equs = 
  let (vars,exprs) = Env.split_equs equs in 
  convexify env 
    (Bddapron.Domain0.substitute_lexpr doman env.Env.env env.Env.cond s 
       vars exprs None)

let flow ?(doman=doman()) env s equs staycond = 
  let trans_doman = doman in
  let cont_elapse = BddapronHybrid.cont_elapse2 ~convexify:false
    (BddapronUtil.get_primed_var env.Env.env) in
  convexify env 
    (BddapronHybrid.flow ~trans_doman env.Env.env env.Env.cond doman
        ~cont_elapse staycond equs env.Env.i_vars s)

let accel ?(doman=doman()) ?(dir=`Forward) env s equs g = 
  let trans_apronman = Bddapron.Domain0.man_get_apron doman in
  join ~doman env s (BddapronAccel.acc ~dir ~trans_apronman 
      env.Env.env env.Env.cond doman g equs env.Env.ni_vars s)

let of_boolnumlist ?(doman=doman()) env bnlist = 
  match bnlist with
    |(bexpr,apron)::[] -> 
       Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond 
         (Bddapron.Domain0.of_apron doman env.Env.env apron) bexpr
    |_ -> convexify env (Bddapron.Domain0.of_bddapron doman env.Env.env bnlist)

let of_num ?(doman=doman()) env apron = 
  Bddapron.Domain0.of_apron doman env.Env.env apron

let of_boolexpr ?(doman=doman()) env bexpr =  
  Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond
    (top env) bexpr

let to_boolexprbool ?(doman=doman()) env s =
  let l = Bddapron.Domain0.to_bddapron doman s in
  List.fold_left 
    (fun bacc (be,_)-> Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc be)
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond)
    l

let to_boolexpr ?(doman=doman()) env s =
  let apronman = Bddapron.Domain0.man_get_apron doman in
  let l = Bddapron.Domain0.to_bddapron doman s in
  List.fold_left 
    (fun bacc (be,ne)-> 
       Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc 
         (Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond be
           (BddapronUtil.apron_to_boolexpr env.Env.env env.Env.cond
             apronman
             (ApronUtil.abstract1_of_abstract0 env.Env.apronenv ne))))
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond)
    l

let to_boolnumlist ?(doman=doman()) env s = Bddapron.Domain0.to_bddapron doman s
let to_boollinconsslist ?(doman=doman()) env s =
  let apronman = Bddapron.Domain0.man_get_apron doman in
  List.map (fun (b,n) -> (b,Apron.Abstract1.to_lincons_array apronman
                        (ApronUtil.abstract1_of_abstract0 env.Env.apronenv n)))
    (Bddapron.Domain0.to_bddapron doman s)

let meet_to_boolexpr ?(doman=doman()) env s bexpr = 
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond bexpr (to_boolexpr env s)
let meetbool_to_boolexpr ?(doman=doman()) env s bexpr = 
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond bexpr 
    (to_boolexprbool env s)
end



