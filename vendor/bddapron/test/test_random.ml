open Format
open Bdd.Cond
open Bdd.Env
open Bddapron
open Cond
open Env

exception Error of string

let error str = raise (Error str)

let print_abs = ref true

let gmp_random_state = ref (Gmp_random.init_default ())

let ratio_bool_nbool = ref 10
let mpz_size = ref 2
let nblinvar = ref 2
let depthcond = ref 5

let nbasssub = ref 3
let depthexpr = ref 3
let destasssub = ref true
let assign_relational = ref true

let nbiterations = ref 100


let multipick_set nb filter set =
  let list =
    PSette.fold
      (fun elt res ->
	if filter elt then elt :: res else res
      )
      set []
  in
  let tab = Array.of_list list in
  let length = ref (Array.length tab) in
  let res = ref [] in
  for i=0 to pred nb do
    if !length=0 then raise Not_found;
    let index = Random.int !length in
    let elt = tab.(index) in
    tab.(index) <- tab.(!length-1);
    res := elt :: !res;
    decr length;
  done;
  !res

let pick_set filter set=
  List.hd (multipick_set 1 filter set)

module Expr = struct

  let pick_typ env typ =
    let alltyps =
      PMappe.fold
	(begin fun var typ res ->
	  PSette.add typ res
	end)
	env.Bdd.Env.vartyp
	(PSette.empty Pervasives.compare)
    in
    match typ with
    | `Bool -> `Bool
    | `Bint ->
	pick_set
	  (function
	    | `Bint(sign,size) -> true
	    | _ -> false
	  )
	  alltyps
    | `Benum ->
	pick_set
	  (function
	    | `Benum name -> true
	    | _ -> false
	  )
	  alltyps
    | `Int -> `Int
    | `Real -> `Real

  let multipick_var nb env typ =
    let vars = Env.vars env in
    multipick_set nb (fun var -> (Env.typ_of_var env var) = typ) vars

  let pick_var env typ =
    List.hd (multipick_var 1 env typ)

  let coeff () =
    let num = Mpz.init () in
    let den = Mpz.init () in
    Gmp_random.Mpz.urandomb num !gmp_random_state (!mpz_size+1);
    Mpz.sub_ui num num (1 lsl !mpz_size);
    Gmp_random.Mpz.urandomb den !gmp_random_state (Pervasives.max 1 (!mpz_size-2));
    Mpz.add_ui den den 1;
    let mpqf = Mpqf.of_mpz2 num den in
    let coeff = Apron.Coeff.s_of_mpqf mpqf in
    coeff

  let rec bool ?(lin=true) env cond depth =
    if depth=0 then
      Expr1.Bool.of_bool env cond (Random.bool ())
    else if depth=1 then
      Expr1.Bool.var env cond (pick_var env `Bool)
    else begin
      if (Random.int !ratio_bool_nbool)=0 then begin
	begin match Random.int 3 with
	| 0 ->
	    let e1 = bool ~lin env cond (depth-1) in
	    let e2 = bool ~lin env cond (depth-1) in
	    let op = match Random.int 8 with
	      | 0 -> Expr1.Bool.dor
	      | _ -> Expr1.Bool.dand
	    in
	    op cond e1 e2
	| 1 ->
	    let typ = pick_typ env `Bint in
	    let e1 = bint ~lin env cond typ (depth-1) in
	    let e2 = bint ~lin env cond typ (depth-1) in
	    let op = match Random.int 5 with
	      | 0 -> Expr1.Bint.eq
	      | 1|2 -> Expr1.Bint.supeq
	      | 3|4 -> Expr1.Bint.sup
	      | _ -> failwith ""
	    in
	    op cond e1 e2
	| 2 ->
	    let typ = pick_typ env `Benum in
	    let e1 = benum ~lin env cond typ (depth-1) in
	    let e2 = benum ~lin env cond typ (depth-1) in
	    Expr1.Benum.eq cond e1 e2
	| _ -> failwith ""
	end
      end
      else begin
	let e = apron ~lin env cond (depth-1) in
	let op = match Random.int 9 with
	  | 0 -> Expr1.Apron.eq
	  | 1|2|3|4 -> Expr1.Apron.eq
	  | 5|6|7|8 -> Expr1.Apron.sup
	  | _ -> failwith ""
	in
	op cond e
      end
    end

  and bint ?(lin=true) env cond typ depth =
    let (sign,size) = match typ with
      | `Bint(sign,size) -> (sign,size)
      | _ -> failwith ""
    in
    if depth=0 then begin
      let res = Random.int (1 lsl size) in
      let res = if sign then res - (1 lsl (size-1)) else res in
      Expr1.Bint.of_int env cond (`Bint(sign,size)) res
    end
    else if depth=1 then
      Expr1.Bint.var env cond (pick_var env typ)
    else begin
      let e1 = bint ~lin env cond typ (depth-1) in
      begin match Random.int 3 with
      | 0 ->
	  begin match Random.int 5 with
	  | 0 -> Expr1.Bint.succ cond e1
	  | 1 -> Expr1.Bint.pred cond e1
	  | 2 ->
	      if size>0
	      then Expr1.Bint.shift_left cond (Random.int size) e1
	      else e1
	  | 3 ->
	      if size>0
	      then Expr1.Bint.shift_right cond (Random.int size) e1
	      else e1
	  | 4 ->
	      if size>0
	      then Expr1.Bint.scale cond (Random.int (1 lsl (size-1))) e1
	      else e1
	  | _ -> failwith ""
	  end
      | 1 ->
	  let e2 = bint ~lin env cond typ (depth-1) in
	  let op = match Random.int 2 with
	    | 0 -> Expr1.Bint.add
	    | 1 -> Expr1.Bint.sub
	    | 2 -> Expr1.Bint.mul
	    | _ -> failwith ""
	  in
	  op cond e1 e2
      | 2 ->
	  let e2 = bint ~lin env cond typ (depth-1) in
	  let e = bool ~lin env cond (depth-1) in
	  Expr1.Bint.ite cond e e1 e2
      | _ -> failwith ""
      end
    end

  and benum ?(lin=true) env cond typ depth =
    let name = match typ with
      |  `Benum name -> name
      | _ -> failwith ""
    in
    if depth=0 then begin
      let typdef = Env.typdef_of_typ env name in
      match typdef with
      | `Benum tlabel ->
	  let i = Random.int (Array.length tlabel) in
	  Expr1.Benum.var env cond tlabel.(i)
    end
    else if depth=1 then
      Expr1.Benum.var env cond (pick_var env typ)
    else begin
      let e1 = benum ~lin env cond typ (depth-1) in
      let e2 = benum ~lin env cond typ (depth-1) in
      let e = bool ~lin env cond (depth-1) in
      Expr1.Benum.ite cond e e1 e2
    end

  and apron ?(lin=true) env cond depth =
    if lin then begin
      if depth<=1 then begin
	let lvar = multipick_var
	  (if !nblinvar=1 then 1 else (1 + (Random.int (!nblinvar-1))))
	  env `Real in
	List.fold_left
	  (begin fun res var ->
	    let cst = Expr1.Apron.cst env cond (coeff ()) in
	    let term = Expr1.Apron.mul cond cst
	      (Expr1.Apron.var env cond var)
	    in
	    Expr1.Apron.add cond res term
	  end)
	  (Expr1.Apron.cst env cond (coeff ()))
	  lvar
      end
      else begin
	let e1 = apron ~lin env cond (depth-1) in
	let e2 = apron ~lin env cond (depth-1) in
	let e = bool ~lin env cond (depth-1) in
	Expr1.Apron.ite cond e e1 e2
      end
    end
    else begin
      if depth=0 then begin
	let coeff = coeff () in
	Expr1.Apron.cst env cond coeff
      end
      else if depth=1 then
	Expr1.Apron.var env cond (pick_var env `Real)
      else begin
	let e1 = apron ~lin env cond (depth-1) in
	begin match Random.int 7 with
	| 0 ->
	    let e2 = apron ~lin env cond (depth-1) in
	    let e = bool ~lin env cond (depth-1) in
	    Expr1.Apron.ite cond e e1 e2
	| 1|2|3 ->
	    let op = match Random.int 4 with
	      | 0 -> Expr1.Apron.sqrt ?typ:None ?round:None
	      | _ -> Expr1.Apron.negate
	    in
	    op cond e1
	| 4|5|6 ->
	    let e2 = apron ~lin env cond (depth-1) in
	    let op = match Random.int 7 with
	      | 0|1  -> Expr1.Apron.add
	      | 2|3 -> Expr1.Apron.sub
	      | 4 -> Expr1.Apron.mul
	      | 5 -> Expr1.Apron.div
	      | 6 -> Expr1.Apron.gmod
	      | _ -> failwith ""
	    in
	    op cond e1 e2
	| _ -> failwith ""
	end
      end
    end

  let expr ?(lin=true) env cond (typ:'a Bddapron.Env.typ) depth : 'a Expr1.t =
    match typ with
    | `Bool -> Expr1.Bool.to_expr (bool ~lin env cond depth)
    | `Bint _ -> Expr1.Bint.to_expr (bint ~lin env cond typ depth)
    | `Benum _ -> Expr1.Benum.to_expr (benum ~lin env cond typ depth)
    | `Real -> Expr1.Apron.to_expr (apron ~lin env cond depth)
    | `Int -> failwith ""
end


module D = struct

  let ofst = function
    | None -> None
    | Some x -> Some (fst x)
  let osnd = function
    | None -> None
    | Some x -> Some (snd x)

  type t = {
    f : 'a 'b 'c. ('a -> 'c Env.t -> 'b);
  }

  let print man fmt abs =
    fprintf fmt
      "@[<v>abs1 = %a;@ abs2 = %a;@]"
      (Domain1.print (fst man)) (fst abs)
      (Domain1.print (snd man)) (snd abs)

  let cons f1 f2 check man env
      =
    let res1 = f1 (fst man) env in
    let res2 = f2 (snd man) env in
    check man (res1,res2)

  let bottom check = cons Domain1.bottom Domain1.bottom check
  let top check = cons Domain1.top Domain1.top check
  let test1 f1 f2 man abs =
    let res1 = f1 (fst man) (fst abs) in
    let res2 = f2 (snd man) (snd abs) in
    if res1<>res2 then error "test1 (is_bottom or is_leq)";
    res1

  let is_bottom man = test1 Domain1.is_bottom Domain1.is_bottom man
  let is_top man = test1 Domain1.is_top Domain1.is_top man

  let test2 f1 f2 man abs1 abs2 =
    let res1 = f1 (fst man) (fst abs1) (fst abs2) in
    let res2 = f2 (snd man) (snd abs1) (snd abs2) in
    if res1<>res2 then error "test2 (is_leq or is_eq)";
    res1

  let is_leq man = test2 Domain1.is_leq Domain1.is_leq man
  let is_eq man = test2 Domain1.is_eq Domain1.is_eq man

  let op2 f1 f2 check man abs1 abs2 =
    let res1 = f1 (fst man) (fst abs1) (fst abs2) in
    let res2 = f2 (snd man) (snd abs1) (snd abs2) in
    check man (res1,res2)

  let meet check = op2 Domain1.meet Domain1.meet check
  let join check = op2 Domain1.join Domain1.join check
  let widening check = op2 Domain1.widening Domain1.widening check
  let unify check = op2 Domain1.unify Domain1.unify check

  let meet_condition check man cond abs condition =
    let res1 = Domain1.meet_condition (fst man) cond (fst abs) condition in
    let res2 = Domain1.meet_condition (snd man) cond (snd abs) condition in
    check man (res1,res2)

  let meet_condition2 check man abs condition =
    let res1 = Domain1.meet_condition2 (fst man) (fst abs) condition in
    let res2 = Domain1.meet_condition2 (snd man) (snd abs) condition in
    check man (res1,res2)

  let asssub f1 f2 check man cond org lvar lexpr odest =
    let res1 = f1 (fst man) cond (fst org) lvar lexpr (ofst odest) in
    let res2 = f2 (snd man) cond (snd org) lvar lexpr (osnd odest) in
    check man (res1,res2)

  let assign_lexpr ?relational ?nodependency check =
    asssub
      (Domain1.assign_lexpr ?relational ?nodependency)
      (Domain1.assign_lexpr ?relational ?nodependency) check
  let substitute_lexpr check =
    asssub Domain1.substitute_lexpr Domain1.substitute_lexpr check

  let asssub2 f1 f2 check man org lvar lexpr odest =
    let res1 = f1 (fst man) (fst org) lvar lexpr (ofst odest) in
    let res2 = f2 (snd man) (snd org) lvar lexpr (osnd odest) in
    check man (res1,res2)

  let assign_listexp2 ?relational ?nodependency check =
    asssub2
      (Domain1.assign_listexpr2 ?relational ?nodependency)
      (Domain1.assign_listexpr2 ?relational ?nodependency) check
  let substitute_listexpr2 check =
    asssub2 Domain1.substitute_listexpr2 Domain1.substitute_listexpr2 check

  let forget_list check man abs lvar =
    let res1 = Domain1.forget_list (fst man) (fst abs) lvar in
    let res2 = Domain1.forget_list (snd man) (snd abs) lvar in
    check man (res1,res2)

  let rename check man abs lvarvar =
    let res1 = Domain1.rename (fst man) (fst abs) lvarvar in
    let res2 = Domain1.rename (snd man) (snd abs) lvarvar in
    check man (res1,res2)

  let of_condition check man env cond condition =
    let top = top check man env in
    meet_condition check man cond top condition

  let of_condition2 check man env condition =
    let top = top check man env in
    meet_condition2 check man top condition

  let is_cst man abs =
    is_bottom man abs || is_top man abs

end

let check
    man
    abs
    =
  let (mbdd,bdd) = Domain1.to_bdd ((fst man),(fst abs)) in
  let (mmtbdd,mtbdd) = Domain1.to_mtbdd ((snd man),(snd abs)) in
  let mbdd = mbdd.Domain0.man in
  let mmtbdd = mmtbdd.Domain0.man in
  let env = bdd.env in
  Bdddomain1.canonicalize ~apron:true mbdd bdd;

  let lbddabs0 = Mtbdddomain0.to_bddapron mmtbdd mtbdd.val0 in
  let list =
    List.map
      (fun (bdd,abs) -> { Bddleaf.guard = bdd; Bddleaf.leaf = abs })
      lbddabs0
  in
  let guardnonbottom = Bddleaf.guard env.cudd list in
  let bdd2 = {
    Bdddomain0.list = list;
    Bdddomain0.bottom = {
      Bddleaf.guard = Cudd.Bdd.dnot guardnonbottom;
      Bddleaf.leaf = (Apron.Abstract1.bottom mbdd.Bdddomain0.apron env.ext.eapron).Apron.Abstract1.abstract0
    };
    unique = true;
    disjoint = true;
  }
  in
  let bdd2 = make_value env bdd2 in
  if not (Bdddomain1.is_eq mbdd bdd bdd2) then begin
    printf "@.abs = %a@.Problem: bdd and mtbdd are not equal !" (D.print man) abs;
    error "Problem: bdd and mtbdd are not equal !"
  end;
  abs

let condition man env =
  let cond = Cond.make ~bddindex0:0 ~bddsize:200 ~symbol:Env.string_symbol env.cudd in
  let condition = Expr.bool env cond !depthcond in
  let res =
    Expr2.Bool.of_expr1 ~normalize:true ~reduce:true ~careset:true
      cond condition
  in
  res

let abs_rand man env =
  let condition = condition man env in
  try
    D.of_condition2 check man env condition
  with _ as exn ->
    printf "@.condition = %a@." Expr2.Bool.print condition;
    raise exn

let test_meet man env =
  printf "Testing meet@.  @[<v>";
  let i = ref 0 in
  while !i < !nbiterations do
    if false then printf "%2i " !i;
    let abs1 = abs_rand man env in
    let abs2 = abs_rand man env in
    let abs =
      try
	D.meet check man abs1 abs2
      with _ as exn ->
	printf "@.abs1 = %a;@.abs2 = %a;@."
	  (D.print man) abs1
	  (D.print man) abs2
	;
	raise exn
    in
    assert(D.is_leq man abs abs1);
    assert(D.is_leq man abs abs2);
    if not (
      D.is_cst man abs1 ||
	D.is_cst man abs2 ||
	D.is_cst man abs
    )
    then begin
      if false then printf "@ abs1 = %a;@ abs2 = %a;@ abs = %a;@ "
	(D.print man) abs1
	(D.print man) abs2
	(D.print man) abs
      ;
      incr i
    end
  done;
  printf "done@]@.";
  ()

let test_join man env =
  printf "Testing join@.  @[<v>";
  let i = ref 0 in
  while !i < !nbiterations do
    if false then printf "%2i " !i;
    let abs1 = abs_rand man env in
    let abs2 = abs_rand man env in
    let abs =
      try D.join check man abs1 abs2
      with _ as exn ->
	printf "@.abs1 = %a;@.abs2 = %a;@."
	  (D.print man) abs1
	  (D.print man) abs2
	;
	raise exn
    in
    assert(D.is_leq man abs1 abs);
    assert(D.is_leq man abs2 abs);
    if not (
      D.is_cst man abs1 ||
	D.is_cst man abs2 ||
	D.is_cst man abs
    )
    then begin
      if false then printf "@ abs1 = %a;@ abs2 = %a;@ abs = %a;@ "
	(D.print man) abs1
	(D.print man) abs2
	(D.print man) abs
      ;
      incr i
    end
  done;
  printf "  @ done@.";
  ()

let test_widening man env =
  printf "Testing widening@.  @[<v>";
  let i = ref 0 in
  while !i < !nbiterations do
    if false then printf "%2i " !i;
    let abs1 = abs_rand man env in
    let abs2 = abs_rand man env in
    let abs2 = D.join check man abs1 abs2 in
    let abs =
      try D.widening check man abs1 abs2
      with _ as exn ->
	printf "@.abs1 = %a;@.abs2 = %a;@."
	  (D.print man) abs1
	  (D.print man) abs2
	;
	raise exn
    in
    assert(D.is_leq man abs1 abs);
    assert(D.is_leq man abs2 abs);
    if not (
      D.is_cst man abs1 ||
	D.is_cst man abs2 ||
	D.is_cst man abs
    )
    then begin
      if false then printf "@ abs1 = %a;@ abs2 = %a;@ abs = %a;@ "
	(D.print man) abs1
	(D.print man) abs2
	(D.print man) abs
      ;
      incr i
    end
  done;
  printf "  @ done@.";
  ()

let test_asssub asssub man env =
  begin match asssub with
  | `Assign -> printf "Testing assignement@.  @[<v>"
  | `Substitute ->printf  "Testing substitution@.  @[<v>"
  end;
  let vars = Env.vars env in
  let i = ref 0 in
  while !i < !nbiterations do
    if true then printf "%2i " !i;
    let absorg = abs_rand man env in
    let cond = Bddapron.Cond.make ~bddindex0:0 ~bddsize:200 ~symbol:string_symbol env.cudd in
    let lvar = multipick_set !nbasssub (fun _ -> true) vars in
    let ltyp = List.map (Env.typ_of_var env) lvar in
    let lexpr = List.map (fun typ -> Expr.expr ~lin:true env cond typ !depthexpr) ltyp in
    let oabsdest = if !destasssub then Some(abs_rand man env) else None in
    let abs =
      try
	begin match asssub with
	| `Assign -> D.assign_lexpr ~relational:!assign_relational ~nodependency:false
	| `Substitute -> D.substitute_lexpr
	end
	  check man cond absorg lvar lexpr oabsdest
      with _ as exn ->
(*
  Printexc.print_backtrace stdout;
*)
	printf "@.lvar = %a@.lexpr=%a@.absorg = %a;@.absdest = %a;@."
	  (Print.list pp_print_string) lvar
	  (Print.list (Expr1.print cond)) lexpr
	  (D.print man) absorg
	  (D.print man)
	  (match oabsdest with None -> D.top check man env | Some x -> x)
	;
	raise exn
    in
    if not (
      D.is_cst man absorg ||
	D.is_cst man abs
    )
    then begin
      if true then printf "lvar = %a@ lexpr=%a@ absorg = %a;@ absdest = %a;@ abs = %a;@ "
	(Print.list pp_print_string) lvar
	(Print.list (Expr1.print cond)) lexpr
	(D.print man) absorg
	(D.print man) (match oabsdest with None -> D.top check man env | Some x -> x)
	(D.print man) abs
      ;
      incr i
    end;
    begin match asssub with
    | `Assign ->
	let abs2 = D.substitute_lexpr check man cond abs lvar lexpr None in
	if false then printf "abs2 = %a@." (D.print man) abs2;
	assert(if oabsdest=None then D.is_leq man absorg abs2 else true);
	let abs2 = D.substitute_lexpr check man cond abs lvar lexpr (Some absorg) in
	assert(if oabsdest=None then D.is_eq man absorg abs2 else true);
    | `Substitute ->
	let abs2 = D.assign_lexpr check man cond abs lvar lexpr (Some absorg) in
	if false then printf "@.lvar = %a@.lexpr=%a@.absorg = %a;@.absdest = %a;@.abs = %a@.abs2=%a@."
	  (Print.list pp_print_string) lvar
	  (Print.list (Expr1.print cond)) lexpr
	  (D.print man) absorg
	  (D.print man) (match oabsdest with None -> D.top check man env | Some x -> x)
	  (D.print man) abs
	  (D.print man) abs2;
	assert(D.is_leq man abs2 absorg);
    end;
  done;
  printf "  @ done@.";
  ()

let test_assign man env = test_asssub `Assign man env
let test_substitute man env = test_asssub `Substitute man env

let test_forget man env =
  let vars = Env.vars env in
  let i = ref 0 in
  while !i < !nbiterations do
    if false then printf "%2i " !i;
    let absorg = abs_rand man env in
    let lvar = multipick_set !nbasssub (fun _ -> true) vars in
    let abs =
      try D.forget_list check man absorg lvar
      with _ as exn ->
	printf "@.absorg = %a@.lvar=%a@."
	  (D.print man) absorg
	  (Print.list pp_print_string) lvar
	;
	raise exn
    in
    assert(D.is_leq man absorg abs);
    if not (
      D.is_cst man absorg ||
	D.is_cst man abs
    )
    then begin
      if false then printf "@.absorg = %a;@.lvar=%a@.abs = %a;@."
	(D.print man) absorg
	(Print.list pp_print_string) lvar
	(D.print man) abs
      ;
      incr i
    end
  done;
  printf "  @ done@.";
  ()

let make_man apron =
  let bdd = Domain1.make_bdd apron in
  let mtbdd = Domain1.make_mtbdd ~global:true apron  in
  (bdd,mtbdd)

let test apron env =
  let man = make_man apron in
(*
  test_forget man env;
  test_meet man env;
  test_join man env;
  test_widening man env;
  test_assign man env;
*)
  test_substitute man env;
  ()

let make_env ~bool ~bint ~real cudd =
  let env = Env.make ~bddindex0:200 ~bddsize:100 ~relational:!assign_relational ~symbol:string_symbol cudd in
  Env.add_typ_with env "e0" (`Benum [| "e0_0" |]);
  Env.add_typ_with env "e1" (`Benum [| "e1_0"; "e1_1"; "e1_2" |]);
(*
  Env.add_typ_with env "e2" (`Benum [| "e2_0"; "e2_1"; "e2_2";  "e2_3";  "e2_4"|]);
*)
  for i=0 to pred bool do
    Env.add_vars_with env [ (Print.sprintf "b%i" i),`Bool ]
  done;
  for i=0 to pred bint do
    Env.add_vars_with env [ (Print.sprintf "i%i" i),`Bint(false,2) ]
  done;
  Env.add_vars_with env [ (Print.sprintf "e%i" 0),`Benum "e0"  ];
  Env.add_vars_with env [ (Print.sprintf "e%i" 1),`Benum "e1"  ];
  for i=0 to pred real do
      Env.add_vars_with env [ (Print.sprintf "x%i" i),`Real  ]
    done;
  env

let case_loop cudd apron =
  let man = make_man apron in
  let env = Env.make ~relational:!assign_relational ~symbol:string_symbol cudd in
  Env.add_vars_with env
    [
      ("mode",`Bool);
      ("b",`Int);
      ("n",`Int);
      ("eps",`Int);
      ("m",`Int);
      ("iter",`Int);
      ("i",`Int);
      ("ITERMAX",`Int);
    ]
  ;
  let cond = Cond.make ~symbol:string_symbol env.cudd in
  let condition =
    Expr1.Bool.of_expr
      (Parser.expr1_of_string env cond
	"b==n and eps==m+1 and iter==0 and
((mode and 900<=b and b<=1000) or (not mode and 899>=b))")
  in
  printf "cond=%a@." (Expr1.Bool.print cond) condition;
  let l20 = D.of_condition check man env cond condition in
  printf "l20=%a@." (D.print man) l20;
  let l23 = D.meet_condition check man cond l20
    (Expr1.Bool.of_expr
      (Parser.expr1_of_string env cond
	"ITERMAX-iter-1>=0 and eps-m>=0"))
  in
  printf "l23=%a@." (D.print man) l23;
  let l23 = D.assign_lexpr check man cond l23 ["i"] [Parser.expr1_of_string env cond "0"] None in
  printf "l23=%a@." (D.print man) l23;
  let contrib = D.meet_condition check man cond l23
    (Expr1.Bool.of_expr
      (Parser.expr1_of_string env cond
	"i-n>=0"))
  in
  printf "contrib=%a@." (D.print man) contrib;
  let contrib = D.assign_lexpr check man cond contrib ["iter"] [Parser.expr1_of_string env cond "iter+1"] None in
  printf "contrib=%a@." (D.print man) contrib;
  let res = D.join check man l23 contrib in
  printf "res=%a@." (D.print man) res;
  ()


let main () =
  let cudd = Cudd.Man.make_v () in
  Cudd.Man.print_limit := 50;
  let env = make_env ~bool:5 ~bint:1 ~real:4 cudd in
  let apron = Polka.manager_alloc_loose () in
(*
  case_loop cudd apron;
*)
  test apron env;
  ()




let _ =
  main()
