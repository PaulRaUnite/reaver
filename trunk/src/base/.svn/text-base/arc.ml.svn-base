(******************************************************************************)
(* arc *)
(* data associated to an arc of a control flow graph*)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Arc";
              Log.level=Log.Debug3}

(******************************************************************************)
(** {2 Types} *)
(******************************************************************************)

type t = 
    |Normal of Env.boolexpr_t * Env.equs_t 
    |Loop of Env.boolexpr_t * Env.equs_t
    |Accel of Env.boolexpr_t * Env.equs_t
    |BoolAccel of Env.boolexpr_t * Env.equs_t
    |BoolNaccAccel of Env.boolexpr_t * Env.equs_t
    |Bool of Env.boolexpr_t * Env.equs_t
    |BoolNacc of Env.boolexpr_t * Env.equs_t
    |Nonacc of Env.boolexpr_t * Env.equs_t
    |Id
    |Flow of Env.boolexpr_t * Env.equs_t
    |Apron of Env.boolexpr_t * (ApronUtil.equs_t * Env.equs_t)

(******************************************************************************)
(** {2 Printing} *)
(******************************************************************************)

let print_type env fmt arc =
  let mode = 
    match arc with 
      |Normal(_,_) -> ""
      |Loop(_,_) -> "[Loop]"
      |Accel(_,_) -> "[Accel]"
      |BoolAccel(_,_) -> "[BoolAccel]"
      |BoolNaccAccel(_,_) -> "[BoolNaccAccel]"
      |Bool(_,_) -> "[Bool]"
      |BoolNacc(_,_) -> "[BoolNacc]"
      |Nonacc(_,_) -> "[Nonacc]"
      |Id -> "[Id]"
      |Flow(_,_) -> "[Flow]"
      |Apron(_,_) -> "[DiscFlow]"
 in
 Format.pp_print_string fmt mode

let print env fmt arc =
   match arc with 
    |Normal(a,f)|Loop(a,f)|Accel(a,f)|BoolAccel(a,f)
      |BoolNaccAccel(a,f)|Bool(a,f)|BoolNacc(a,f)|Nonacc(a,f) -> 
      if not(Bddapron.Expr0.Bool.is_true env.Env.env env.Env.cond a) then
      begin
        let strfmt = Format.str_formatter in
	Bddapron.Expr0.Bool.print env.Env.env env.Env.cond strfmt a;
        let str = Format.flush_str_formatter () in
        Format.pp_print_string fmt ((Util.string_compact str)^" -->\\n")
      end;
      let strfmt = Format.str_formatter in
      List.iter 
	(fun equ ->
	  let (v,expr) = equ in
	  Format.pp_print_string strfmt v;
	  Format.pp_print_string strfmt "'=";
	  Bddapron.Expr0.print env.Env.env env.Env.cond strfmt expr;
	  Format.pp_print_string strfmt "; ") 
      (BddapronUtil.simplify_equs env.Env.env env.Env.cond f a);
      let str = Format.flush_str_formatter () in
      Format.pp_print_string fmt (Util.string_compact str)
    |Id -> ()
    |Apron(a,(fn,fb)) -> 
      if not(Bddapron.Expr0.Bool.is_true env.Env.env env.Env.cond a) then
      begin
        let strfmt = Format.str_formatter in
	Bddapron.Expr0.Bool.print env.Env.env env.Env.cond strfmt a;
        let str = Format.flush_str_formatter () in
        Format.pp_print_string fmt ((Util.string_compact str)^" -->\\n")
      end;
      let strfmt = Format.str_formatter in
      Array.iter 
	(fun equ ->
	  let (v,expr) = equ in
	  Apron.Var.print strfmt v;
	  Format.pp_print_string strfmt "'=";
	  Apron.Linexpr1.print strfmt expr;
	  Format.pp_print_string strfmt "; ") 
        fn;
      List.iter 
	(fun equ ->
	  let (v,expr) = equ in
	  Format.pp_print_string strfmt v;
	  Format.pp_print_string strfmt "'=";
	  Bddapron.Expr0.print env.Env.env env.Env.cond strfmt expr;
	  Format.pp_print_string strfmt "; ") 
        fb;
      let str = Format.flush_str_formatter () in
      Format.pp_print_string fmt (Util.string_compact str)
    |Flow(a,f) -> 
      if not(Bddapron.Expr0.Bool.is_true env.Env.env env.Env.cond a) then
      begin
        let strfmt = Format.str_formatter in
	Bddapron.Expr0.Bool.print env.Env.env env.Env.cond strfmt a;
        let str = Format.flush_str_formatter () in
        Format.pp_print_string fmt ((Util.string_compact str)^" -->\\n")
      end;
      let strfmt = Format.str_formatter in
      List.iter 
	(fun equ ->
	  let (v,expr) = equ in
	  Format.pp_print_string strfmt ".";
	  Format.pp_print_string strfmt v;
	  Format.pp_print_string strfmt "=";
	  Bddapron.Expr0.print env.Env.env env.Env.cond strfmt expr;
	  Format.pp_print_string strfmt "; ") 
      (BddapronUtil.simplify_equs env.Env.env env.Env.cond f a);
      let str = Format.flush_str_formatter () in
      Format.pp_print_string fmt (Util.string_compact str)

(******************************************************************************)
(** {2 Operations} *)
(******************************************************************************)

let get_ass_equs env arc =
  match arc with 
    |Normal(a,f)|Loop(a,f)|Accel(a,f)|BoolAccel(a,f)
      |BoolNaccAccel(a,f)
      |Bool(a,f)|BoolNacc(a,f)|Nonacc(a,f) -> (a,f)|Flow(a,f) -> (a,f)
    |Id -> (Bddapron.Expr0.Bool.dtrue env.Env.env env.Env.cond,
           BddapronUtil.get_id_equs_for env.Env.env env.Env.cond env.Env.s_vars)
    |Apron(a,(fn,fb)) -> (a,fb)

let simplify env arc phi =
  match arc with 
    |Normal(a,f) -> 
       let  f = BddapronUtil.simplify_equs env.Env.env env.Env.cond f phi in Normal (a,f)
    |Loop (a,f) -> 
       let  f = BddapronUtil.simplify_equs env.Env.env env.Env.cond f phi in Loop (a,f)
    |Accel(a,f) -> 
       let  f = BddapronUtil.simplify_equs env.Env.env env.Env.cond f phi in Accel (a,f)
    |BoolAccel(a,f) -> 
       let  f = BddapronUtil.simplify_equs env.Env.env env.Env.cond f phi in BoolAccel (a,f)
    |BoolNaccAccel(a,f) -> 
       let  f = BddapronUtil.simplify_equs env.Env.env env.Env.cond f phi in BoolNaccAccel (a,f)
    |Bool(a,f) -> 
       let  f = BddapronUtil.simplify_equs env.Env.env env.Env.cond f phi in Bool (a,f)
    |BoolNacc(a,f) -> 
       let  f = BddapronUtil.simplify_equs env.Env.env env.Env.cond f phi in BoolNacc (a,f)
    |Nonacc(a,f) -> 
       let  f = BddapronUtil.simplify_equs env.Env.env env.Env.cond f phi in Nonacc (a,f)
    |Flow(a,f) -> 
       let  f = BddapronUtil.simplify_equs env.Env.env env.Env.cond f phi in Flow (a,f)
    |Id -> arc
    |Apron(a,(fn,fb)) -> 
      let  fb = BddapronUtil.simplify_equs env.Env.env env.Env.cond fb phi in
      Apron(a,(fn,fb))

let refine_ass env arc phi =
  match arc with 
    |Normal(a,f) -> Normal (Cudd.Bdd.dand a phi,f)
    |Loop (a,f) -> Loop (Cudd.Bdd.dand a phi,f)
    |Accel(a,f) -> Accel (Cudd.Bdd.dand a phi,f)
    |BoolAccel(a,f) -> BoolAccel (Cudd.Bdd.dand a phi,f)
    |BoolNaccAccel(a,f) -> BoolNaccAccel (Cudd.Bdd.dand a phi,f)
    |Bool(a,f) -> Bool (Cudd.Bdd.dand a phi,f)
    |BoolNacc(a,f) -> BoolNacc (Cudd.Bdd.dand a phi,f)
    |Nonacc(a,f) -> Nonacc (Cudd.Bdd.dand a phi,f)
    |Flow(a,f) -> Flow (Cudd.Bdd.dand a phi,f)
    |Id -> arc
    |Apron(a,(fn,fb)) -> Apron (Cudd.Bdd.dand a phi,(fn,fb))

let is_id env arc sinv =
  match arc with
    |Normal(a,f) 
    |Loop (a,f)
    |Accel(a,f)  
    |BoolAccel(a,f)
    |BoolNaccAccel(a,f) 
    |Bool(a,f)
    |BoolNacc(a,f) 
    |Nonacc(a,f)
    |Flow(a,f) -> BddapronAnalysis.is_id_equs2 
      (BddapronUtil.get_primed_var env.Env.env)
      (BddapronUtil.get_unprimed_var env.Env.env)
      env.Env.env env.Env.cond a f sinv
    |Id -> true
    |Apron(a,(fn,fb)) -> 
       BddapronAnalysis.is_id_bequs2 
      (BddapronUtil.get_primed_var env.Env.env)
      (BddapronUtil.get_unprimed_var env.Env.env)
      env.Env.env env.Env.cond a fb sinv (* TODO apron-equs id check *)

let replace_ass_equs arc (a,f) =
  match arc with 
    |Normal(_,_) -> Normal (a,f)
    |Loop (_,_) -> Loop (a,f)
    |Accel(_,_) -> Accel (a,f)
    |BoolAccel(_,_) -> BoolAccel (a,f)
    |BoolNaccAccel(_,_) -> BoolNaccAccel (a,f)
    |Bool(_,_) -> Bool (a,f)
    |BoolNacc(_,_) -> BoolNacc (a,f)
    |Nonacc(_,_) -> Nonacc (a,f)
    |Flow(_,_) -> Flow (a,f)
    |Id -> arc
    |Apron(_,(fn,_)) -> Apron (a,(fn,f))
