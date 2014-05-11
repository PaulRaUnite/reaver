open Numutil.GmpRatio
open Numutil
open List
open Tools
open Qsopt_ex

let split l = 
  let rec split_rev xs ys = function 
    []       -> (xs,ys)
  | (x,y)::l -> split_rev (x::xs) (y::ys) l
  in
  let split_rev l = 
    split_rev [] [] l
  in
  split_rev (List.rev l)

exception Todo
exception Unsup

module type X = sig 
  type x 
  val string_of_x : x -> string 
end

module type LP = 
  sig
    type x (* = X.x *)
    type expr = Const of GmpRatio.num | Var of x | Mul of GmpRatio.num * expr | Sum of expr list
    type obj = Min of expr | Max of expr
    type constr = Eq of expr * expr | Le of expr * expr
    type constrs = constr list

    type normal_lin_expr = (num * x) list
    type normal_expr = num * normal_lin_expr 
    type normal_constr = 
      NEq of normal_lin_expr * num 
    | NLe of normal_lin_expr * num

    type lp = {prob_name : string; obj : obj; constrs : constr list}
    type sol = InfGmpRatio.num * (x -> GmpRatio.num)

    val transform_into_max_obj : obj -> obj
    val contains_var : expr -> bool
    val new_prob_name : unit -> string

    val ( ++. ) : expr -> expr -> expr
    val ( **. ) : GmpRatio.num -> expr -> expr 
    val expr_neg : expr -> expr 
    val ( --. ) : expr -> expr -> expr
    val ( ==. ) : expr -> expr -> constr
    val ( <=. ) : expr -> expr -> constr
    val ( >=. ) : expr -> expr -> constr

    val normal_expr : expr -> normal_expr
    val normal_constr : constr -> normal_constr

    val string_of_x : x -> string
    val string_of_normal_constr : normal_constr -> string
    val string_of_expr : expr -> string
    val string_of_obj : obj -> string
    val string_of_constr : constr -> string
    val string_of_constrs : constr list -> string
    val string_of_lp : lp -> string
    val solve : lp -> sol 
    val solve' : string -> constrs -> obj -> sol
  end

module Make(X : X) = struct
  include X

  type expr = Const of num | Var of x | Mul of num * expr | Sum of expr list
  type obj = Min of expr | Max of expr
  type constr = Eq of expr * expr | Le of expr * expr
  type constrs = constr list
  type lp = {prob_name : string; obj : obj; constrs : constr list}
  type sol = InfGmpRatio.num * (x -> num)

  exception Var_not_found of x

  let prob_id = ref 0
  let new_prob_name () =
    let id = !prob_id in
    let _ = prob_id := id + 1 in
    "Prob " ^ string_of_int id

  let ( ++. ) e1 e2 = Sum([e1;e2])
  let ( **. ) c e = Mul(c,e)
  let expr_neg e = Mul(from_int (-1), e)
  let ( --. ) e1 e2 = e1 ++. (expr_neg e2)
  let ( ==. ) e1 e2 = Eq(e1,e2)
  let ( <=. ) e1 e2 = Le(e1,e2)
  let ( >=. ) e1 e2 = e2 <=. e1

  let rec vars_of_expr l = function
    Const _  -> l
  | Var x    -> x :: l
  | Mul(_,e) -> vars_of_expr l e
  | Sum(es)  -> fold_left vars_of_expr l es

  let vars_of_expr = vars_of_expr []

  let rec contains_var = function
    Const _  -> false
  | Var _    -> true
  | Mul(_,e) -> contains_var e
  | Sum(es)  -> exists contains_var es
  
  let rec string_of_expr = function 
    Const n  -> to_string n
  | Var x    -> string_of_x x
  | Mul(c,e) -> to_string c ^ " * " ^ string_of_expr e
  | Sum(es)  -> "(" ^ String.concat " + " (map string_of_expr es) ^ ")"

  let string_of_obj = function
    Min e -> "min " ^ string_of_expr e
  | Max e -> "max " ^ string_of_expr e

  let string_of_constr = function
    Eq(e1,e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Le(e1,e2) -> string_of_expr e1 ^ " <= " ^ string_of_expr e2

  let string_of_constrs constrs = 
    String.concat "\n" (map string_of_constr constrs)

  let string_of_lp lp =
    string_of_obj lp.obj ^ "\ns.t.\n" ^ string_of_constrs lp.constrs

  let transform_into_max_obj = function 
    Max _ as obj -> obj
  | Min obj -> Max (Mul(minus_one,obj))

  let transform_into_max lp = {prob_name = lp.prob_name; obj = transform_into_max_obj lp.obj; constrs = lp.constrs}

  let rec normal_expr factor (const,lin) = function
    Const c  -> (const +. (factor *. c), lin)
  | Var x    -> (const, (factor,x) :: lin)
  | Mul(c,e) -> normal_expr (c *. factor) (const,lin) e
  | Sum(es)  -> fold_left (normal_expr factor) (const,lin) es
  let normal_expr = normal_expr one (zero,[])
  let normal_expr e =
    let (c,l) = normal_expr e in
    let l = sort (fun (_,x) (_,y) -> compare x y) l in
    let rec doit = function
    [] -> []
    | [_] as x -> x
    | (c1,x1)::(c2,x2)::l when x1 = x2 -> doit ((c1 +. c2, x1)::l)
    | (c1,x1)::(c2,x2)::l -> (c1,x1) :: doit ((c2,x2)::l)
    in
    let l = doit l in
    let l = filter (fun (c,_) -> not (c =. zero)) l in
    (c,l)

  type normal_lin_expr = (num * x) list
  type normal_expr = num * normal_lin_expr 
  type normal_constr = 
  NEq of normal_lin_expr * num 
  | NLe of normal_lin_expr * num

  let string_of_normal_constr = function
    NEq(lhs,b) -> String.concat " + " (map (fun (c,x) -> to_string c ^ " * " ^ string_of_x x) lhs) ^ " = " ^ to_string b
  | NLe(lhs,b) -> String.concat " + " (map (fun (c,x) -> to_string c ^ " * " ^ string_of_x x) lhs) ^ " <= " ^ to_string b

  let normal_constr = function
    Eq(e1,e2) -> 
      let e = e1 --. e2 in 
      let (b,lhs) = normal_expr e in
      let b = minus_one *. b in
      NEq(lhs,b)
  | Le(e1,e2) ->
      let e = e1 --. e2 in 
      let (b,lhs) = normal_expr e in
      let b = minus_one *. b in
      NLe(lhs,b)

  let is_bound = function NEq(lhs,b) | NLe(lhs,b) -> length lhs = 1

  let is_non_trivial = function
    NEq([],b) when GmpRatio.( =.  ) GmpRatio.zero b -> false
  | NLe([],b) when GmpRatio.( <=. ) GmpRatio.zero b -> false
  | _                                                       -> true

  let is_trivially_unsat = function
    NEq([],b) when not(GmpRatio.( =.  ) GmpRatio.zero b) -> true
  | NLe([],b) when not(GmpRatio.( <=. ) GmpRatio.zero b) -> true
  | _                                                            -> false

  let ( ++ ) f g x = g (f x)

  let is_min_lp lp = match lp.obj with Min _ -> true | Max _ -> false 
  let is_max_lp = is_min_lp ++ not
  
  let solve lp =
    let ht = Hashtbl.create 100 in 
    let next_id = ref 0 in
    let get_var_id x = 
      match Hashtbl.find_all ht x with
        [] -> 
          let id = !next_id in
          Hashtbl.replace ht x id;
          next_id := id + 1;
          id
      | [id] -> id
      | _ -> assert false
    in
    let init_variables_nexpr ne = 
      let (_,vars) = split ne in
      iter (fun x -> let _ = get_var_id x in ()) vars
    in
    let init_variables_nconstr = function NEq(ne,_) | NLe(ne,_) -> init_variables_nexpr ne in
    let neg_sol_if_nes = if is_max_lp lp then (fun x -> x) else (fun x ->InfGmpRatio.( *. ) (InfGmpRatio.from_int (-1)) x) in  
    let lp = transform_into_max lp in
    (*
    print_endline "****************";
    print_endline (string_of_lp lp);
    *)
    let Max obj = lp.obj in
    let (add,c) = normal_expr obj in
    let add = InfGmpRatio.Num add in
    let constrs = map normal_constr lp.constrs in
    if exists is_trivially_unsat constrs then
      (InfGmpRatio.Neginfty, fun _ -> raise Unsup)
    else
      let constrs = filter is_non_trivial constrs in
      assert(for_all (function NEq(_::_,_) | NLe(_::_,_) -> true | _ -> false) constrs);
      (* let _ = init_variables_nexpr c in *)
      let _ = iter init_variables_nconstr constrs in
      (* Die naechste Zeile ist nur mal fuer den Moment, sonst probleme *)
      let is_bound _ = false in
      let bounds, constraints = partition is_bound constrs in
      let number_of_variables = !next_id in 
      let number_of_constraints = length constraints in
      let number_of_values = 
        fold_left 
          ( + ) 
          0 
          (map (function NEq(lhs,_) | NLe(lhs,_) -> length lhs) constraints) 
      in
      let is_unconstrained x =
        match Hashtbl.find_all ht x with
          [] -> true
        | [id] -> false
        | _ -> assert false
      in
      if exists is_unconstrained (let _,vars = split c in vars) then
        (* There exists a unconstrained variable that occurrs in the objective function *)
        begin
          if contains_var obj then
            (InfGmpRatio.Infty, fun _ -> raise Unsup)
          else
            let rec eval = function
              Const c -> c
            | Var x -> raise Unsup
            | Mul(c,e) -> c *. (eval e)
            | Sum(es) -> fold_left ( +. ) zero (map eval es)
            in
            (InfGmpRatio.Num (eval obj), fun _ -> raise Unsup)
        end
      else
        begin
          (* All variables that occur in the objective function are constrained *)
          (* Building up the Qsopt_ex lp *)
          init 128;
          let lp = 
            create_lp 
              lp.prob_name
              number_of_variables 
              number_of_constraints 
              number_of_values
          in
          Hashtbl.iter (fun x i -> set_colname lp i (string_of_x x)) ht;
          let cmat = Array.create number_of_variables [] in
          iteri 
            (
              fun i -> function NEq(lhs,_) | NLe(lhs,_) ->
                iter 
                  (
                    fun (c,x) -> 
                      let var_id = get_var_id x in
                      cmat.(var_id) <- (i,c) :: cmat.(var_id) 
                  ) 
                  lhs
            )
            constraints;
          Array.iteri (fun i l -> cmat.(i) <- rev l) cmat;
          let cmatcnt = Array.map length cmat in
          Array.iteri (set_cmatcnt lp) cmatcnt;
          let cmatbeg = Array.fold_left (function (b::_) as bs -> fun l -> (b + l) :: bs ) [0] cmatcnt in
          let _ :: cmatbeg = cmatbeg in
          let cmatbeg = rev cmatbeg in
          iteri (set_cmatbeg lp) cmatbeg;
          let (cmatind,cmatval) = split (flatten (Array.to_list cmat)) in
          iteri (set_cmatind lp) cmatind;
          iteri 
            (set_sense lp) 
            (map (function NEq _ -> Comp_eq | NLe _ -> Comp_le) constraints);
          iteri (set_cmatval lp) cmatval;
          let obj = Array.create number_of_variables zero in
          iteri (fun i (c,x) -> let id = get_var_id x in obj.(id) <- obj.(id) +. c) c; 
          Array.iteri (set_obj lp) obj;
          iteri (set_rhs lp) (map (function NEq(_,b) | NLe(_,b) -> b) constraints);
          let lower = Hashtbl.create 100 in
          let upper = Hashtbl.create 100 in
          let update_lower x b =
            match Hashtbl.find_all lower x with
              []  -> Hashtbl.replace lower x b
            | [l] -> if GmpRatio.( <=. ) b l then () else Hashtbl.replace lower x b
            | _ -> assert false
           in
          let update_upper x b =
            match Hashtbl.find_all upper x with
              []  -> Hashtbl.replace upper x b
            | [l] -> if GmpRatio.( <=. ) l b then () else Hashtbl.replace upper x b
            | _ -> assert false
          in
          iter 
            (
              function 
                NLe([c,x],b) -> 
                  if GmpRatio.( <=. ) zero c then 
                    update_upper (get_var_id x) (b /. c)
                  else
                    update_lower (get_var_id x) (b /. c)
              | NEq([c,x],b) -> 
                  update_upper (get_var_id x) (b /. c);
                  update_lower (get_var_id x) (b /. c)
            )
            bounds;
          Hashtbl.iter (set_lower lp) lower;
          Hashtbl.iter (set_upper lp) upper;
          let sol = solve lp in
          let obj_value = neg_sol_if_nes (InfGmpRatio.( +. ) add (get_value_from_sol sol)) in
          (*
          print_endline ("Optimal value = " ^ to_string obj_value);
          *)
          let x =
            match obj_value with
              InfGmpRatio.Neginfty | InfGmpRatio.Infty -> 
                fun _ -> raise Unsup
            | InfGmpRatio.Num _    -> 
                let x = Array.init number_of_variables (get_x_from_sol sol) in 
                let _ = free_sol sol in
                fun var -> 
                  match Hashtbl.find_all ht var with
                    [] -> raise (Var_not_found var)
                  | [i] -> x.(i)
          in
          free_lp lp;
          (* free_sol sol; *)
          (obj_value, x)
        end

 (* ------------- *)

  let solve' prob_name constrs =
    (* let _ = print_endline "enter solve'"; flush stdout in *)
    let ht = Hashtbl.create 100 in 
    let next_id = ref 0 in
    let get_var_id x = 
      match Hashtbl.find_all ht x with
        [] -> 
          let id = !next_id in
          Hashtbl.replace ht x id;
          next_id := id + 1;
          id
      | [id] -> id
      | _ -> assert false
    in
    let init_variables_nexpr ne = 
      let (_,vars) = split ne in
      iter (fun x -> let _ = get_var_id x in ()) vars
    in
    let init_variables_nconstr = function NEq(ne,_) | NLe(ne,_) -> init_variables_nexpr ne in
    let constrs = map normal_constr constrs in
    if exists is_trivially_unsat constrs then
      fun obj -> (InfGmpRatio.Neginfty, fun _ -> raise Unsup)
    else
      let constrs = filter is_non_trivial constrs in
      assert(for_all (function NEq(_::_,_) | NLe(_::_,_) -> true | _ -> false) constrs);
      let _ = iter init_variables_nconstr constrs in
      (* Die naechste Zeile ist nur mal fuer den Moment, sonst probleme *)
      let is_bound _ = false in
      let bounds, constraints = partition is_bound constrs in
      let number_of_variables = !next_id in 
      let number_of_constraints = length constraints in
      let number_of_values = 
        fold_left 
          ( + ) 
          0 
          (map (function NEq(lhs,_) | NLe(lhs,_) -> length lhs) constraints) 
      in
      (* All variables that occur in the objective function are constrained *)
      (* Building up the Qsopt_ex lp *)
      init 128;
      let lp = create_lp prob_name number_of_variables number_of_constraints number_of_values in
      Hashtbl.iter (fun x i -> set_colname lp i (string_of_x x)) ht;
      let cmat = Array.create number_of_variables [] in
      iteri 
        (
          fun i -> function NEq(lhs,_) | NLe(lhs,_) ->
            iter 
              (
                fun (c,x) -> 
                  let var_id = get_var_id x in
                  cmat.(var_id) <- (i,c) :: cmat.(var_id) 
              ) 
              lhs
        )
        constraints;
      Array.iteri (fun i l -> cmat.(i) <- rev l) cmat;
      let cmatcnt = Array.map length cmat in
      Array.iteri (set_cmatcnt lp) cmatcnt;
      let cmatbeg = Array.fold_left (function (b::_) as bs -> fun l -> (b + l) :: bs ) [0] cmatcnt in
      let _ :: cmatbeg = cmatbeg in
      let cmatbeg = rev cmatbeg in
      iteri (set_cmatbeg lp) cmatbeg;
      let (cmatind,cmatval) = split (flatten (Array.to_list cmat)) in
      iteri (set_cmatind lp) cmatind;
      iteri 
        (set_sense lp) 
        (map (function NEq _ -> Comp_eq | NLe _ -> Comp_le) constraints);
      iteri (set_cmatval lp) cmatval;
      let lower = Hashtbl.create 100 in
      let upper = Hashtbl.create 100 in
      let update_lower x b =
        match Hashtbl.find_all lower x with
          []  -> Hashtbl.replace lower x b
        | [l] -> if GmpRatio.( <=. ) b l then () else Hashtbl.replace lower x b
        | _ -> assert false
      in
      let update_upper x b =
        match Hashtbl.find_all upper x with
          []  -> Hashtbl.replace upper x b
        | [l] -> if GmpRatio.( <=. ) l b then () else Hashtbl.replace upper x b
        | _ -> assert false
      in
      iter 
        (
          function 
            NLe([c,x],b) -> 
              if GmpRatio.( <=. ) zero c then 
                update_upper (get_var_id x) (b /. c)
              else
                update_lower (get_var_id x) (b /. c)
          | NEq([c,x],b) -> 
              update_upper (get_var_id x) (b /. c);
              update_lower (get_var_id x) (b /. c)
        )
        bounds;
      Hashtbl.iter (set_lower lp) lower;
      Hashtbl.iter (set_upper lp) upper;
      (* let _ = print_endline "JUHU"; flush stdout in *)
      (* Jetzt kommt die Zielfunktion ins Spiel *)
      fun obj -> 
        let neg_sol_if_nes =
          match obj with 
            Max _ -> fun x -> x
          | Min _ -> fun x -> InfGmpRatio.( *. ) (InfGmpRatio.from_int (-1)) x
        in
        let obj = transform_into_max_obj obj in
        let Max obj = obj in
        let (add,c) = normal_expr obj in
        let add = InfGmpRatio.Num add in
        let is_unconstrained x =
          match Hashtbl.find_all ht x with
            [] -> true
          | [id] -> false
          | _ -> assert false
        in
        if exists is_unconstrained (let _,vars = split c in vars) then
          (* There exists an unconstrained variable that occurrs in the objective function *)
          begin
            if contains_var obj then
              (InfGmpRatio.Infty, fun _ -> raise Unsup)
            else
              let rec eval = function
                Const c -> c
              | Var x -> raise Unsup
              | Mul(c,e) -> c *. (eval e)
              | Sum(es) -> fold_left ( +. ) zero (map eval es)
              in
              (InfGmpRatio.Num (eval obj), fun _ -> raise Unsup)
          end
        else
          (* All variables of the objective function are constrained *)
          begin
            let obj = Array.create number_of_variables zero in
            iteri (fun i (c,x) -> let id = get_var_id x in obj.(id) <- obj.(id) +. c) c; 
            Array.iteri (set_obj lp) obj;
            iteri (set_rhs lp) (map (function NEq(_,b) | NLe(_,b) -> b) constraints);
            let sol = Qsopt_ex.solve lp in
            let obj_value = neg_sol_if_nes (InfGmpRatio.( +. ) add (get_value_from_sol sol)) in
            (*
            print_endline ("Optimal value = " ^ to_string obj_value);
            *)
            let x =
              match obj_value with
                InfGmpRatio.Neginfty | InfGmpRatio.Infty -> 
                  fun _ -> raise Unsup
              | InfGmpRatio.Num _    -> 
                  let x = Array.init number_of_variables (get_x_from_sol sol) in 
                  let _ = free_sol sol in
                  fun var -> 
                    match Hashtbl.find_all ht var with
                      [] -> raise Not_found
                    | [i] -> x.(i)
                    | _ -> assert false
            in
            (* free_lp lp; *)
            (* free_sol sol; *)
            (* let _ = print_endline "exit solve'"; flush stdout in *)
            (obj_value, x)
          end
end

(* ------------- *)

module StringX = struct
  type x = string
  let string_of_x x = x
end

module Dualizer(PrimalLP : LP) = struct
  module DualX = struct
    type x = PrimalLP.constr
    let string_of_x x = "DVar(" ^ PrimalLP.string_of_constr x ^ ")" 
  end
  
  module DualLP = Make(DualX)

  (* Functions for generating the objective function of the dual LP *)
  let dualize_for_constr (con,ncon) =
    let c = match ncon with
               PrimalLP.NEq(e1, b) -> b
             | PrimalLP.NLe(e1, b) -> b
    in DualLP.Mul(c, DualLP.Var(con))    

  let dualize_obj_expr constrs =
    DualLP.Sum(map dualize_for_constr constrs)
  
  let create_dual_obj constrs obj = 
    match obj with
      PrimalLP.Max e -> DualLP.Min (dualize_obj_expr constrs)
    | PrimalLP.Min e -> DualLP.Max (dualize_obj_expr constrs)

  let rec partition compare acc part = function
    [] -> 
      begin 
        match part with
          [] -> acc
        | _  -> part::acc
      end
  | x::l -> 
      begin
        match part with
          []                        -> partition compare acc [x] l 
        | y::_ when compare x y = 0 -> partition compare acc (x::part) l
        | y::_                      -> partition compare (part::acc) [x] l
      end 

  let partition compare l =
    let l = sort compare l in 
    partition compare [] [] l 

  (* Functions for generating the constraints of the dual LP *)
  let create_dual_constrs constrs (c,lin_obj) =
    let handle_constr (constr_acc, slack_acc) pconstr = 
      let (constr,nconstr) = pconstr in
      match nconstr with
        PrimalLP.NLe(lhs,_) ->
          (((rev_map (fun (c,x) -> (x,pconstr,c)) lhs)::constr_acc), ((DualLP.Le(DualLP.Const(from_string "0"), DualLP.Var(constr)))::slack_acc ) )
      | PrimalLP.NEq(lhs,_) -> 
          (((rev_map (fun (c,x) -> (x,pconstr,c)) lhs)::constr_acc), slack_acc)
    in
    let (lconstrs, slacks) = fold_left handle_constr ([],[]) constrs in
    let lconstrs = flatten lconstrs in
    let lconstrs = partition (fun (x,_,_) (y,_,_) -> compare x y) lconstrs in
    let lin_obj  = partition (fun (_,x) (_,y) -> compare x y) lin_obj in 
    let lconstrs_and_lin_obj,_ = 
      fold_left 
        (
          fun (acc,lin_obj) lconstr -> 
            let (x,_,_)::_ = lconstr in 
            match lin_obj with
                        []                                      -> ((lconstr,zero)::acc,[])
                      | [(c,x')]::lin_obj when compare x' x = 0 -> ((lconstr,c)::acc,lin_obj)  
                      | [(c,x')]::_                             -> ((lconstr,zero)::acc,lin_obj)  
        ) 
        ([],lin_obj) 
        lconstrs 
    in
    let create_constr (lhs,rhs) =
      let lhs = DualLP.Sum (map (fun (_,(constr,_),c) -> DualLP.Mul(c,DualLP.Var(constr))) lhs) in
      let rhs = DualLP.Const rhs in
      DualLP.Eq(lhs,rhs)
    in
    rev_append (map create_constr lconstrs_and_lin_obj) slacks

  let dualize (lp : PrimalLP.lp) = 
    let obj = PrimalLP.transform_into_max_obj lp.PrimalLP.obj in
    (* We just have to deal with maximization problems from now on *)
    let PrimalLP.Max obj = obj in
    let obj = PrimalLP.normal_expr obj in
    let constrs = lp.PrimalLP.constrs in
    let constrs = map (fun constr -> (constr, PrimalLP.normal_constr constr)) constrs in 
    let is_non_trivial = function 
      PrimalLP.NEq([],c) when GmpRatio.( =. ) GmpRatio.zero c  -> false
    | PrimalLP.NLe([],c) when GmpRatio.( <=. ) GmpRatio.zero c -> false
    | _                                                        -> true 
    in
    let is_non_trivial (_,nconstr) = is_non_trivial nconstr in
    let constrs = List.filter is_non_trivial constrs in
    let dual_prob_name = "Dual of " ^ lp.PrimalLP.prob_name in 
    let dual_obj = create_dual_obj constrs lp.PrimalLP.obj in
    let dual_constrs = create_dual_constrs constrs obj in
    {
      DualLP.prob_name = dual_prob_name;
      DualLP.obj = dual_obj;
      DualLP.constrs = dual_constrs 
    }

  let dual_solve_no_test lp =
    let dual_lp = dualize lp in
    DualLP.solve dual_lp

  let dual_solve lp =
    let tstobj = PrimalLP.Max(PrimalLP.Const zero) in
    let tstlp = 
    {
      PrimalLP.prob_name = "test_lp";
      PrimalLP.obj = tstobj;
      PrimalLP.constrs = lp.PrimalLP.constrs
    } 
    in
    let tstsol = PrimalLP.solve tstlp in
    match tstsol with 
    (InfGmpRatio.Neginfty, _ ) -> 
      (InfGmpRatio.Neginfty,  
                  fun _ -> raise Unsup)
    | _ ->
      let dual_lp = dualize lp in
        DualLP.solve dual_lp

  let dual_solve_no_test' prob_name constrs  = fun obj ->
    let lp' = 
    {
      PrimalLP.prob_name =  prob_name;
      PrimalLP.obj = obj;
      PrimalLP.constrs = constrs
    } in dual_solve_no_test lp'


  let dual_solve' prob_name constrs  = fun obj ->
    let lp' = 
    {
      PrimalLP.prob_name =  prob_name;
      PrimalLP.obj = obj;
      PrimalLP.constrs = constrs
    } in dual_solve lp'

  let dual_solve_without_feasibility_check lp =
    let dual_lp = dualize lp in
    DualLP.solve dual_lp

  let dual_solve' prob_name constrs  = fun obj ->
    let lp' = 
    {
      PrimalLP.prob_name =  prob_name;
      PrimalLP.obj = obj;
      PrimalLP.constrs = constrs
    } in dual_solve lp'

end
