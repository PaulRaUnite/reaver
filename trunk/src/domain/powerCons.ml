(******************************************************************************)
(* powerCons *)
(* power domain with linear constraints *)
(* author: Peter Schrammel *)
(* version: 0.9.1m *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Domain.PowerCons";
              Log.level=Log.Debug3}

type apronman_t = Polka.loose Polka.t
type bddapronman_t = (Env.var_t, apronman_t) Bddapron.Domain0.bdd 
type bddaprondoman_t = (Env.var_t, apronman_t,bddapronman_t, Env.var_t Bddapron.Domain0.t) Bddapron.Domain0.man

type cons_t = Apron.Lincons1.t

(******************************************************************************)
(* Helpers *)
(******************************************************************************)

(* constraint that is always true *)
let cons_true env = Apron.Lincons1.make 
  (Apron.Linexpr1.make env.Env.apronenv) Apron.Lincons0.EQ

(* constraint that is always false *)
let cons_false env = Apron.Lincons1.make_unsat env.Env.apronenv

(******************************************************************************)
(** {2 module BoolLin: power domain with linear constraints} *)
(******************************************************************************)

module BoolLin =
struct
type t = Env.var_t Bddapron.Domain0.t

type numdomain_t = apronman_t Apron.Abstract0.t

type doman_param_t = ()
type doman_t = bddaprondoman_t

let make_man () : bddaprondoman_t = 
  let bman: bddapronman_t = 
    Bddapron.Domain0.make_bdd (Polka.manager_alloc_loose ()) in
  Bddapron.Domain0.man_of_bdd bman

let domanref = ref(None)

let make_doman () = make_man ()

let makeinit_doman () = 
  let d = make_man () in
  domanref := Some d;
  d

let doman () = 
  match !domanref with
    |Some d -> d
    |None -> assert(false)

let get_doman_param () = ()

(*TODO*)
let meet_condition ?(doman=doman()) env  s e = 
  let (doman,_) = doman in
  Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond s e

let is_bottom ?(doman=doman()) env s = 
  let (doman,_) = doman in
  Bddapron.Domain0.is_bottom doman s

let print ?(doman=doman()) env fmt s = 
  let (doman,_) = doman in
  Bddapron.Domain0.print doman env.Env.env fmt s

let is_eq ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.is_eq doman s1 s2

let bottom ?(doman=doman()) env = 
  let (doman,_) = doman in
  Bddapron.Domain0.bottom doman env.Env.env 

let top ?(doman=doman()) env = 
  let (doman,_) = doman in
  Bddapron.Domain0.top doman env.Env.env 

let canonicalize ?(doman=doman()) env s = 
  let (doman,_) = doman in
  Bddapron.Domain0.canonicalize doman s

let is_leq ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.is_leq doman s1 s2

let join ?(doman=doman()) env s1 s2 = 
  let (doman,template) = doman in
  Bddapron.Domain0.join doman  
       (bddapron_approx_template env doman template s1)
       (bddapron_approx_template env doman template s2)

let meet ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.meet doman s1 s2

let widening ?(doman=doman()) env s1 s2 = 
  let (doman,_) = doman in
  Bddapron.Domain0.widening doman s1 s2

let assign_lexpr ?(doman=doman()) env s equs = 
  let (doman,_) = doman in
  let (vars,exprs) = Env.split_equs equs in 
  Bddapron.Domain0.assign_lexpr doman env.Env.env env.Env.cond s 
       vars exprs None

let forget_list ?(doman=doman()) env s vars = 
  let (doman,_) = doman in
  Bddapron.Domain0.forget_list doman env.Env.env s  vars

let substitute_lexpr ?(doman=doman()) env s equs = 
  let (doman,_) = doman in
  let (vars,exprs) = Env.split_equs equs in 
  Bddapron.Domain0.substitute_lexpr doman env.Env.env env.Env.cond s 
       vars exprs None

let flow ?(doman=doman()) env s equs staycond = 
  raise (Domain.NotSupported "Template.Pow.flow")
let accel ?(doman=doman()) ?(dir=`Forward) env s equs g = 
  raise (Domain.NotSupported "Template.Pow.accel")

let of_boolnumlist ?(doman=doman()) env bnlist = 
  let (doman,_) = doman in
  match bnlist with
    |(bexpr,apron)::[] -> 
       Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond 
         (Bddapron.Domain0.of_apron doman env.Env.env apron) bexpr
    |_ -> Bddapron.Domain0.of_bddapron doman env.Env.env bnlist

let of_num ?(doman=doman()) env apron = 
  let (doman,_) = doman in
  Bddapron.Domain0.of_apron doman env.Env.env apron

let of_boolexpr ?(doman=doman()) env bexpr =  
  let (doman,_) = doman in
  Bddapron.Domain0.meet_condition doman env.Env.env env.Env.cond
    (top env) bexpr

let to_boolexprbool ?(doman=doman()) env s =
  let (doman,_) = doman in
  let l = Bddapron.Domain0.to_bddapron doman s in
  List.fold_left 
    (fun bacc (be,_)-> Bddapron.Expr0.Bool.dor env.Env.env env.Env.cond bacc be)
    (Bddapron.Expr0.Bool.dfalse env.Env.env env.Env.cond)
    l

let to_boolexpr ?(doman=doman()) env s =
  let (doman,_) = doman in
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

let to_boolnumlist ?(doman=doman()) env s = 
  let (doman,_) = doman in
  Bddapron.Domain0.to_bddapron doman s

let to_boollinconsslist ?(doman=doman()) env s =
  let (doman,_) = doman in
  let apronman = Bddapron.Domain0.man_get_apron doman in
  List.map (fun (b,n) -> (b,Apron.Abstract1.to_lincons_array apronman
                        (ApronUtil.abstract1_of_abstract0 env.Env.apronenv n)))
    (Bddapron.Domain0.to_bddapron doman s)

let meet_to_boolexpr ?(doman=doman()) env s bexpr = 
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond bexpr 
    (to_boolexpr ~doman env s)

let meetbool_to_boolexpr ?(doman=doman()) env s bexpr = 
  Bddapron.Expr0.Bool.dand env.Env.env env.Env.cond bexpr 
    (to_boolexprbool ~doman env s)
end
