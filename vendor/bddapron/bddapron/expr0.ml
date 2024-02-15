(** Finite-type and arithmetical expressions *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Cond
open Cond
open Bdd.Env
open Env

type 'a t = [
  | Cudd.Man.v Bdd.Expr0.t
  | `Apron of 'a ApronexprDD.t
]
type 'a expr = 'a t

(*  ********************************************************************** *)
(** {3 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

  let print_bdd env cond fmt bdd
      =
    Bdd.Expr0.O.print_bdd
      ~print_external_idcondb:begin fun fmt idb ->
	let condition = Bdd.Cond.cond_of_idb cond idb in
	Cond.print_cond env fmt condition
      end
      env fmt bdd

  let typ_of_expr (env:('a,'b,'c,'d) Env.O.t) (expr:[< 'a t]) =
    let typ = match expr with
    | `Bool _ -> `Bool
    | `Bint(x) -> `Bint(x.Bdd.Int.signed, Array.length x.Bdd.Int.reg)
    | `Benum(x) -> `Benum(env.symbol.unmarshal x.Bdd.Enum.typ)
    | `Apron _ -> `Real
    in
    (typ :> 'a Env.typ)

  (*  ==================================================================== *)
  (** {4 General expressions} *)
  (*  ==================================================================== *)

  let check_typ2 env e2 e3 =
    let t2 = typ_of_expr env e2 in
    let t3 = typ_of_expr env e3 in
    if t2 <> t3 then
      failwith
	(Print.sprintf "Expr.ite: 2 branches have different types %a and %a"
	  (Env.print_typ env.symbol.print) t2
	  (Env.print_typ env.symbol.print) t3)
    else
      t2

  let cofactor (e:'a t) (bdd:Cudd.Bdd.vt) : 'a t
      =
    let (res:'a t) =
      match e with
      | #Bdd.Expr0.t as x -> ((Bdd.Expr0.cofactor x bdd):>'a t)
      | `Apron x -> `Apron (Cudd.Mtbdd.cofactor x bdd)
    in
    res

  let restrict (e:'a t) (bdd:Cudd.Man.v Cudd.Bdd.t) : 'a t
      =
    match e with
    | #Bdd.Expr0.t as x -> ((Bdd.Expr0.restrict x bdd):>'a t)
    | `Apron x -> `Apron (Cudd.Mtbdd.restrict x bdd)

  let tdrestrict (e:'a t) (bdd:Cudd.Man.v Cudd.Bdd.t) : 'a t
      =
    match e with
    | #Bdd.Expr0.t as x -> ((Bdd.Expr0.tdrestrict x bdd):>'a t)
    | `Apron x -> `Apron (Cudd.Mtbdd.tdrestrict x bdd)

  let permute ?memo (e:'a t) (tab:int array) : 'a t
      =
    match e with
    | #Bdd.Expr0.t as x -> ((Bdd.Expr0.O.permute ?memo x tab):>'a t)
    | `Apron x -> `Apron (Cudd.Mtbdd.permute ?memo x tab)

  let permute_list ?memo le tab
      =
    match memo with
    | Some memo ->
	List.map (fun x -> permute ~memo x tab) le
    | None ->
	let hash = Cudd.Hash.create 1 in
	let memo = Cudd.Memo.Hash hash in
	let res = List.map (fun x -> permute ~memo x tab) le in
	Cudd.Hash.clear hash;
	res

  let varmap (e:'a t) : 'a t
      =
    match e with
    | #Bdd.Expr0.t as x -> ((Bdd.Expr0.O.varmap x):>'a t)
    | `Apron x -> `Apron (Cudd.Mtbdd.varmap x)

  let print
      env cond
      (fmt:Format.formatter) (expr:[<'a t])
      =
    match expr with
    | `Bool x -> print_bdd env cond fmt x
    | `Bint x -> Bdd.Int.print_minterm (print_bdd env cond) fmt x
    | `Benum x -> Bdd.Enum.print_minterm (print_bdd env cond) env fmt x
    | `Apron x -> ApronexprDD.print (print_bdd env cond) env.symbol fmt x

  let conditions_support' { cudd } cond le =
    List.fold_left begin fun conds e ->
      let esupp = match e with
        | #Bdd.Expr0.t as e -> Bdd.Expr0.O.support_cond cudd e
        | `Apron a -> Cudd.Mtbdd.support a
      in
      Cudd.Bdd.support_union conds esupp
    end (Cudd.Bdd.dtrue cudd) le |> Cudd.Bdd.support_inter cond.supp

  let conditions_support env cond e = conditions_support' env cond [e]

  let fold_filter_conditions env cond le nsub handle_cond acc =
    List.fold_left begin fun acc condid ->
      let `Apron c = PDMappe.x_of_y (condid, true) cond.condidb in
      let csupp = Cond.support_cond env (`Apron c) in
      let csub = PMappe.interset nsub csupp in
      if PMappe.is_empty csub then acc else handle_cond condid c csub acc
    end acc (conditions_support' env cond le |> Cudd.Bdd.list_of_support)

  let compose_of_lvarexpr
      env cond (le: 'a t list)
      (substitution:('a * 'a t) list)
      :
      Cudd.Bdd.vt array option * ('a, 'a t) PMappe.t
      =
    if false then printf "compose_of_lvarexpr@   %a@."
      (Print.list
	(fun fmt (var,expr) ->
	  fprintf fmt "%a <- %a@ "
	    env.Bdd.Env.symbol.Bdd.Env.print var
	    (print env cond) expr))
      substitution;
    (* we first look at Boolean variables/conditions *)
    let (bsub,osub) =
      List.fold_left
	(begin fun (bsub,osub) (var,expr) ->
	  begin match expr with
	  | #Bdd.Expr0.t as x ->
	      ((var,x)::bsub,osub)
	  | _ ->
	      (bsub, PMappe.add var expr osub)
	  end
	end)
	([],(PMappe.empty env.symbol.compare))
	substitution
    in
    let mk_tab () = Bdd.Expr0.O.composition_of_lvarexpr env bsub in
    let otab = if bsub<>[] then Some (mk_tab ()) else None in
    (* we initialize the substitution table *)
    let otab =
      if PMappe.is_empty osub then otab else
        (* we now take care of other conditions *)
        fold_filter_conditions env cond le osub
          (fun condid c csub otab ->
            let r = ApronexprDD.Condition.substitute env cond c csub in
            let tab = match otab with None -> mk_tab () | Some tab -> tab in
            tab.(condid) <- r;
            Some tab)
          otab
    in
    (otab, osub)

  let substitute_list
      ?memo
      env cond
      (le:'a t list) (substitution:('a * 'a t) list) : 'a t list
      =
    if substitution = [] || le=[] then
      le
    else begin
      let (otab,sub) = compose_of_lvarexpr env cond le substitution in
      let (ohash,memo) =
	if otab<>None && memo=None then
	  let hash = Cudd.Hash.create 1 in
	  let memo = Cudd.Memo.Hash hash in
	  (Some hash, Some memo)
	else
	  (None,memo)
      in
      let res =
	if PMappe.is_empty sub then begin
	  let tab = match otab with
	    | None -> assert false
	    | Some(tab) -> tab
	  in
	  List.map
	    (function
	      | #Bdd.Expr0.t as x ->
		  ((Bdd.Expr0.O.compose ?memo x tab):>'a t)
	      | `Apron mtbdd ->
		  `Apron (Cudd.Mtbdd.vectorcompose ?memo tab mtbdd)
	    )
	    le
	end
	else begin
	  List.map
	    (begin fun e -> match e with
	    | #Bdd.Expr0.t as x ->
		begin match otab with
		| None -> e
		| Some(tab) -> ((Bdd.Expr0.O.compose ?memo x tab):>'a t)
		end
	    | `Apron expr ->
		let cudd = env.cudd in
		let default = Cudd.Mtbdd.cst_u cudd (Cudd.Mtbdd.pick_leaf_u expr) in
                let res' = Cudd.Mtbdd.fold_guardleaves (fun guard apronexpr res ->
		  let nguard = match otab with
		    | None -> guard
		    | Some(tab) -> Cudd.Bdd.vectorcompose ?memo tab guard
		  in
		  let nexpr = ApronexprDD.substitute env apronexpr sub in
		  Cudd.Mtbdd.ite nguard nexpr res)
                  expr
                  default
                in
		`Apron res'
	    end)
	    le
	end
      in
      begin match ohash with
      | Some(hash) -> Cudd.Hash.clear hash
      | None -> ()
      end;
      res
    end

  let substitute
      ?memo
      env cond
      (e:'a t) (substitution:('a * 'a t) list) : 'a t
      =
    List.hd (substitute_list ?memo env cond [e] substitution)

  let var
      env cond
      (var:'a) : 'a t
      =
    let typ = Env.typ_of_var env var in
    match typ with
    | #Bdd.Env.typ ->
	((Bdd.Expr0.O.var env var):>'a t)
    | `Int | `Real ->
	`Apron (ApronexprDD.var env var)
    | _ -> raise Not_found

  let substitute_by_var_list
      ?memo
      ({ cudd; symbol } as env) cond
      le (substitution:('a * 'a) list)
      =

    (* Handle finite and numerical variables differently *)
    let bsub, nsub = List.partition
      (fun (v, _) ->
        match Env.typ_of_var env v with #Bdd.Env.typ -> true | _ -> false)
      substitution
    in

    (* Substituting finite variables only entails permutations *)
    let mk_perms () = Bdd.Expr0.O.permutation_of_rename env bsub in
    let operms = if bsub <> [] then Some (mk_perms ()) else None in

    (* Basic bookkeeping for numericals... *)
    let nsub = List.fold_left (fun nsub (v, v') -> PMappe.add v v' nsub)
      (PMappe.empty symbol.compare) nsub in
    let num_sub = not (PMappe.is_empty nsub) in

    let operms = if not num_sub then operms else
        (* substitute in conditions and update permutations in `perms'
           accordingly *)
        fold_filter_conditions env cond le nsub
          (fun condid (typ, e) csub operms ->
            let c' = typ, Apronexpr.substitute_by_var symbol e csub in
            let dd = ApronexprDD.Condition.of_apronexpr env cond c' in
            let perms = match operms with None -> mk_perms () | Some p -> p in
            perms.(condid) <- Cudd.Bdd.topvar dd;
            Some perms)
          operms
    in

    (* Actually perform the substitutions *)
    List.map begin function
      | #Bdd.Expr0.t as e ->
          ((match operms with
            | None -> e
            | Some perms -> Bdd.Expr0.O.permute ?memo e perms) :> 'a t)
      | `Apron a ->
          let open Cudd.Mtbdd in
          let a = match operms with
            | None -> a
            | Some perms -> permute ?memo a perms in
          `Apron
            (if not num_sub then a else fold_guardleaves
                (fun g n -> let n' = Apronexpr.substitute_by_var symbol n nsub in
                         ite g (ApronexprDD.of_apronexpr env n'))
                a (cst_u cudd (pick_leaf_u a)))
    end le

  let substitute_by_var
      ?memo
      env cond
      e (substitution:('a * 'a) list)
      =
    List.hd (substitute_by_var_list ?memo env cond [e] substitution)

  let ddsubstitute = substitute
  let ddsubstitute_by_var = substitute_by_var

  (*  ==================================================================== *)
  (** {4 Boolean expressions} *)
  (*  ==================================================================== *)

  module Bool = struct
    type 'a t = Cudd.Man.v Bdd.Expr0.O.Bool.t
    let of_expr = Bdd.Expr0.O.Bool.of_expr
    let to_expr = Bdd.Expr0.O.Bool.to_expr

    let dtrue env cond = Bdd.Expr0.O.Bool.dtrue env
    let dfalse env cond = Bdd.Expr0.O.Bool.dfalse env
    let of_bool env cond = Bdd.Expr0.O.Bool.of_bool env
    let var env cond = Bdd.Expr0.O.Bool.var env

    let dnot env cond = Bdd.Expr0.O.Bool.dnot env
    let dand env cond = Bdd.Expr0.O.Bool.dand env
    let dor env cond = Bdd.Expr0.O.Bool.dor env

    let xor env cond = Bdd.Expr0.O.Bool.xor env
    let nand env cond = Bdd.Expr0.O.Bool.nand env
    let nor env cond = Bdd.Expr0.O.Bool.nor env
    let nxor env cond = Bdd.Expr0.O.Bool.nxor env
    (** Exclusive or, not and, nor or and not xor *)

    let leq env cond = Bdd.Expr0.O.Bool.leq env
    let eq env cond = Bdd.Expr0.O.Bool.eq env
    let ite env cond = Bdd.Expr0.O.Bool.ite env

    let is_true env cond = Bdd.Expr0.O.Bool.is_true env
    let is_false env cond = Bdd.Expr0.O.Bool.is_false env
    let is_cst env cond = Bdd.Expr0.O.Bool.is_cst env
    let is_eq env cond = Bdd.Expr0.O.Bool.is_eq env
    let is_leq env cond = Bdd.Expr0.O.Bool.is_leq env
    let is_and_false env cond = Bdd.Expr0.O.Bool.is_and_false env

    let exist env cond = Bdd.Expr0.O.Bool.exist env
    let forall env cond = Bdd.Expr0.O.Bool.forall env

    let cofactor = Bdd.Expr0.O.Bool.cofactor
    let restrict = Bdd.Expr0.O.Bool.restrict
    let tdrestrict = Bdd.Expr0.O.Bool.tdrestrict
    let permute = Bdd.Expr0.O.Bool.permute
    let varmap = Bdd.Expr0.O.Bool.varmap

    let substitute_by_var ?memo env cond (e:'a t) sub = of_expr (ddsubstitute_by_var ?memo env cond (to_expr e) sub)
    let substitute ?memo env cond (e:'a t) sub = of_expr (ddsubstitute ?memo env cond (to_expr e) sub)

    let print = print_bdd
  end

  (*  ==================================================================== *)
  (** {4 Bounded integer expressions} *)
  (*  ==================================================================== *)

  module Bint = struct
    type 'a t = Cudd.Man.v Bdd.Expr0.O.Bint.t
    let of_expr = Bdd.Expr0.O.Bint.of_expr
    let to_expr = Bdd.Expr0.O.Bint.to_expr

    let of_int env cond = Bdd.Expr0.O.Bint.of_int env
    let var env cond = Bdd.Expr0.O.Bint.var env
    let ite env cond = Bdd.Expr0.O.Bint.ite env

    let neg env cond = Bdd.Expr0.O.Bint.neg env
    let succ env cond = Bdd.Expr0.O.Bint.succ env
    let pred env cond = Bdd.Expr0.O.Bint.pred env
    let add env cond = Bdd.Expr0.O.Bint.add env
    let sub env cond = Bdd.Expr0.O.Bint.sub env
    let mul env cond = Bdd.Expr0.O.Bint.mul env
    let shift_left env cond = Bdd.Expr0.O.Bint.shift_left env
    let shift_right env cond = Bdd.Expr0.O.Bint.shift_right env
    let scale env cond = Bdd.Expr0.O.Bint.scale env
    let zero env cond = Bdd.Expr0.O.Bint.zero env
    let eq env cond = Bdd.Expr0.O.Bint.eq env
    let eq_int env cond = Bdd.Expr0.O.Bint.eq_int env
    let supeq env cond = Bdd.Expr0.O.Bint.supeq env
    let supeq_int env cond = Bdd.Expr0.O.Bint.supeq_int env
    let sup env cond = Bdd.Expr0.O.Bint.sup env
    let sup_int env cond = Bdd.Expr0.O.Bint.sup_int env

    let cofactor = Bdd.Expr0.O.Bint.cofactor
    let restrict = Bdd.Expr0.O.Bint.restrict
    let tdrestrict = Bdd.Expr0.O.Bint.tdrestrict
    let permute = Bdd.Expr0.O.Bint.permute
    let varmap = Bdd.Expr0.O.Bint.varmap

    let substitute_by_var ?memo env cond (e:'a t) sub = of_expr (ddsubstitute_by_var ?memo env cond (to_expr e) sub)
    let substitute ?memo env cond (e:'a t) sub = of_expr (ddsubstitute ?memo env cond (to_expr e) sub)
    let guard_of_int env cond = Bdd.Expr0.O.Bint.guard_of_int env
    let guardints env cond = Bdd.Expr0.O.Bint.guardints env

    let print env cond fmt x = Bdd.Int.print_minterm (print_bdd env cond) fmt x
  end

  (*  ==================================================================== *)
  (** {4 Enumerated type expressions} *)
  (*  ==================================================================== *)

  module Benum = struct
    type 'a t = Cudd.Man.v Bdd.Expr0.O.Benum.t
    let of_expr = Bdd.Expr0.O.Benum.of_expr
    let to_expr = Bdd.Expr0.O.Benum.to_expr
    let var env cond = Bdd.Expr0.O.Benum.var env
    let ite env cond = Bdd.Expr0.O.Benum.ite env
    let eq env cond = Bdd.Expr0.O.Benum.eq env
    let eq_label env cond = Bdd.Expr0.O.Benum.eq_label env
    let cofactor = Bdd.Expr0.O.Benum.cofactor
    let restrict = Bdd.Expr0.O.Benum.restrict
    let tdrestrict = Bdd.Expr0.O.Benum.tdrestrict
    let permute = Bdd.Expr0.O.Benum.permute
    let varmap = Bdd.Expr0.O.Benum.varmap
    let substitute_by_var ?memo env cond (e:'a t) sub = of_expr (ddsubstitute_by_var ?memo env cond (to_expr e) sub)
    let substitute ?memo env cond (e:'a t) sub = of_expr (ddsubstitute ?memo env cond (to_expr e) sub)
    let guard_of_label env cond = Bdd.Expr0.O.Benum.guard_of_label env
    let guardlabels env cond = Bdd.Expr0.O.Benum.guardlabels env
    let print env cond fmt x = Bdd.Enum.print_minterm (print_bdd env cond) env fmt x
  end

  (*  ==================================================================== *)
  (** {4 Apronexprmetic expressions} *)
  (*  ==================================================================== *)

  module Apron = struct
    type 'a t = 'a ApronexprDD.t

    let of_expr = ApronexprDD.of_expr
    let to_expr = ApronexprDD.to_expr

    let print env cond fmt x =
      ApronexprDD.print (print_bdd env cond) env.symbol fmt x

    let cst env cond c = ApronexprDD.cst env c
    let var env cond name = ApronexprDD.var env name
    let add env cond = ApronexprDD.add env
    let sub env cond = ApronexprDD.sub env
    let mul env cond = ApronexprDD.mul env
    let div env cond = ApronexprDD.div env
    let gmod env cond = ApronexprDD.gmod env
    let negate env cond = ApronexprDD.negate env
    let cast env cond = ApronexprDD.cast env
    let sqrt env cond = ApronexprDD.sqrt env

    let supeq env cond = ApronexprDD.Condition.supeq env cond
    let sup env cond = ApronexprDD.Condition.sup env cond
    let eq env cond = ApronexprDD.Condition.eq env cond

    let ite env cond b (t1:'a t) (t2:'a t) : 'a t =
      Cudd.Mtbdd.ite b t1 t2

    let cofactor = Cudd.Mtbdd.cofactor
    let restrict = Cudd.Mtbdd.restrict
    let tdrestrict = Cudd.Mtbdd.tdrestrict
    let permute = Cudd.Mtbdd.permute
    let varmap = Cudd.Mtbdd.varmap
    let support = Cudd.Mtbdd.support
    let support_leaf = ApronexprDD.support_leaf

    let substitute_by_var ?memo env cond (e:'a t) sub = of_expr (ddsubstitute_by_var ?memo env cond (to_expr e) sub)
    let substitute ?memo env cond (e:'a t) sub = of_expr (ddsubstitute ?memo env cond (to_expr e) sub)
  end

  (*  ==================================================================== *)
  (** {4 General expressions} *)
  (*  ==================================================================== *)

  let eq env cond (e1:'a t) (e2:'a t) : 'a Bool.t
      =
    let t = check_typ2 env e1 e2 in
    match t with
    | `Bool ->
	Bool.eq env cond (Bool.of_expr e1) (Bool.of_expr e2)
    | `Bint _ ->
	Bint.eq env cond (Bint.of_expr e1) (Bint.of_expr e2)
    | `Benum _ ->
	Benum.eq env cond (Benum.of_expr e1) (Benum.of_expr e2)
    | `Real ->
	let diff = Apron.sub env cond
	  (Apron.of_expr e1)
	  (Apron.of_expr e2)
	in
	Apron.eq env cond diff
    | _ -> failwith ""

  let ite env cond (condition:'a Bool.t) (e1:'a t) (e2:'a t) : 'a t
      =
    let t = check_typ2 env e1 e2 in
    match t with
    | `Bool ->
	`Bool (Bool.ite env cond condition (Bool.of_expr e1) (Bool.of_expr e2))
    | `Bint _ ->
	`Bint (Bint.ite env cond condition (Bint.of_expr e1) (Bint.of_expr e2))
    | `Benum _ ->
	`Benum (Benum.ite env cond condition (Benum.of_expr e1) (Benum.of_expr e2))
    | `Real ->
	Apron.to_expr (Apron.ite env cond condition (Apron.of_expr e1) (Apron.of_expr e2))
    | _ -> failwith ""

  let support_cond
      cudd
      (expr:'a t) : Cudd.Bdd.vt
      =
    match expr with
    | #Bdd.Expr0.t as x ->
	Bdd.Expr0.O.support_cond cudd (x:> Cudd.Man.v Bdd.Expr0.t)
    | `Apron x ->
	Apron.support x

  let support
      env cond
      (expr:'a t) : 'a PSette.t
      =
    let supp = support_cond env.cudd expr in
    let list = Cudd.Bdd.list_of_support supp in
    let set = ref (PSette.empty env.symbol.compare) in
    List.iter
      (begin fun id ->
	try
	  let condition = Bdd.Cond.cond_of_idb cond (id,true) in
	  let supp = Cond.support_cond env condition in
	  set := PSette.union supp !set
	with Not_found ->
	  let var = PMappe.find id env.idcondvar in
	  set := PSette.add var !set
      end)
      list
    ;
    begin match expr with
    | #Bdd.Expr0.t -> ()
    | `Apron expr ->
	set := PSette.union (Apron.support_leaf env expr) !set
    end;
    !set

  let normalize
      ?(reduce=false) ?(careset=false)
      ((cond:('a,'b) Cond.O.t),lexpr)
      =
    let ncond = Bdd.Cond.copy cond in
    let support lexpr =
      List.fold_left
	(begin fun supp expr ->
	  Cudd.Bdd.support_union supp
	    (Cudd.Bdd.support_inter
	      ncond.supp
	      (support_cond ncond.Bdd.Cond.cudd expr))
	end)
	(Cudd.Bdd.dtrue cond.Bdd.Cond.cudd)
	lexpr
    in
    if reduce then begin
      let supp = support lexpr in
      Bdd.Cond.reduce_with ncond supp;
    end;
    let perm = Bdd.Cond.normalize_with ncond in
    let lexpr = permute_list lexpr perm in
    let lexpr =
      if careset then begin
	Bdd.Cond.compute_careset ncond ~normalized:true;
	let careset = ncond.careset in
	let lexpr = List.map (fun e -> tdrestrict e careset) lexpr in
	if reduce then begin
	  let supp = support lexpr in
	  if not (Cudd.Bdd.is_equal supp ncond.supp) then begin
	    Bdd.Cond.reduce_with ncond supp;
	    let perm = Bdd.Cond.normalize_with ncond in
	    let lexpr = permute_list lexpr perm in
	    lexpr
	  end
	  else lexpr
	end
	else lexpr
      end
      else lexpr
    in
    (ncond,lexpr)

  let print
      env cond
      (fmt:Format.formatter) (expr:[<'a t])
      =
    match expr with
    | `Bool x -> Bool.print env cond fmt x
    | `Bint x -> Bint.print env cond fmt x
    | `Benum x -> Benum.print env cond fmt x
    | `Apron x -> Apron.print env cond fmt x

  (* NB: let apply_change e change = *)

end

(*  ********************************************************************** *)
(** Closed signature *)
(*  ********************************************************************** *)

let typ_of_expr= O.typ_of_expr
let var = O.var
let ite = O.ite
let cofactor = O.cofactor
let restrict = O.restrict
let tdrestrict = O.tdrestrict
let permute = O.permute
let varmap = O.varmap
let substitute_by_var = O.substitute_by_var
let substitute = O.substitute
let substitute_by_var_list = O.substitute_by_var_list
let substitute_list = O.substitute_list
let support = O.support
let eq = O.eq
let support_cond = O.support_cond
let conditions_support' = O.conditions_support'
let conditions_support = O.conditions_support
let print = O.print
let normalize = O.normalize

module Bool = struct
  include O.Bool
end
module Bint = struct
  include O.Bint
end
module Benum = struct
  include O.Benum
end
type apron_coeff = Apron.Coeff.t
type apron_typ = Apron.Texpr1.typ
type apron_round = Apron.Texpr1.round
module Apron = struct
  type 'a t = 'a ApronexprDD.t
  let of_expr = O.Apron.of_expr
  let to_expr = O.Apron.to_expr
  let cst = O.Apron.cst
  let var = O.Apron.var
  let add = O.Apron.add
  let sub = O.Apron.sub
  let mul = O.Apron.mul
  let div = O.Apron.div
  let gmod = O.Apron.gmod
  let negate = O.Apron.negate
  let cast = O.Apron.cast
  let sqrt = O.Apron.sqrt
  let supeq = O.Apron.supeq
  let sup = O.Apron.sup
  let eq = O.Apron.eq
  let ite = O.Apron.ite
  let cofactor = O.Apron.cofactor
  let restrict = O.Apron.restrict
  let tdrestrict = O.Apron.tdrestrict
  let permute = O.Apron.permute
  let varmap = O.Apron.varmap
  let substitute_by_var = O.Apron.substitute_by_var
  let substitute = O.Apron.substitute
  let print = O.Apron.print
end
