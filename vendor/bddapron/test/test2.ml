(*
ocaml

#use "topfind";;
#require "bddapron.bddapron";;
#require "apron.polkaMPQ";;
*)
open Format;;
open Bddapron;;

let apron = Polka.manager_alloc_loose ();;
let man = Mtbdddomain1.make_man apron;;
let cudd = Cudd.Man.make_v ();;
let env = Env.make ~bddindex0:0 ~bddsize:10 ~symbol:Env.string_symbol cudd;;
let cond = Cond.make ~bddindex0:10 ~bddsize:10 ~symbol:Env.string_symbol cudd;;
(*
#install_printer Apron.Abstract1.print;;
#install_printer Cudd.Bdd.print__minterm;;
let print fmt x = Apron.Environment.print fmt x;;
#install_printer print;;
let print fmt x = Apronexpr.print Env.string_symbol fmt x;;
#install_printer print;;
let print fmt x = Cudd.Weakke.print print fmt x;;
#install_printer print;;
let print fmt x = Cudd.Weakke.print Apron.Abstract1.print fmt x ;;
#install_printer print;;
let print fmt x = Mtbdddomain1.print fmt x;;
#install_printer print;;
#install_printer Env.print;;
#install_printer Expr2.Bool.print;;
let p = Cond.print env;;
#install_printer p;;
let p = Expr1.print cond;;
#install_printer p;;
let p = Expr1.Bool.print cond;;
#install_printer p;;
let p = Expr1.Apron.print cond;;
#install_printer p;;
let p x = Expr0.print env cond x;;
#install_printer p;;
let p = Expr0.Bool.print env cond;;
#install_printer p;;
let p = Expr0.Apron.print env cond;;
#install_printer p;;
let p fmt x = Mtbdddomain0.print env fmt x;;
#install_printer p;;
let p fmt (x: Polka.loose Polka.t ApronDD.table) = Cudd.Mtbdd.print_table (fun x -> Apron.Abstract0.print string_of_int x) fmt x;;
#install_printer p;;
*)


Env.add_typ_with env
  "typ" (`Benum [|"a";"b";"c"|]);;

Env.add_vars_with env [
  ("e0",`Benum "typ");
  ("b0",`Bool);
  ("b1",`Bool);
  ("b2",`Bool);
  ("x0",`Real);
  ("x1",`Real);
  ("x2",`Real);
];;


let _ =
  let domain = Bdddomain0.make_man apron in
  let s = Bdddomain0.top domain env in
  let c = Expr0.Bool.of_expr
	    (Parser.expr0_of_string env cond "x0>=0") in
  let res = Bdddomain0.meet_condition domain env cond s c in
  exit 0;
  ()

(* ********************************************************************** *)

let condition = Expr1.Bool.of_expr (Parser.expr1_of_string env cond "b0 and x0>=0");;

let top = Mtbdddomain1.top man env;;
let abs = Mtbdddomain1.meet_condition man cond top condition;;

(* ********************************************************************** *)

let expr1 = Expr1.Bool.of_expr (Parser.expr1_of_string env cond "e0 in { a, b}");;
let expr2 = Expr1.Bool.of_expr (Parser.expr1_of_string env cond "not (e0==c)");;

Expr1.Bool.is_eq cond expr1 expr2;;

printf "expr1=%a@.expr2=%a@."
Cudd.Bdd.print__minterm expr1.Env.val0
Cudd.Bdd.print__minterm expr2.Env.val0
;;
let expr = Parser.expr1_of_string env cond "if (b0==b1) and b2 then x0+2 else if not (b0==b1) then x0+1 else x0";;

let nexpr = Expr1.substitute_by_var cond expr [("b0","b2")];;

(* ********************************************************************** *)

let string_of_dim i = "x"^(string_of_int i);;
let print_table fmt table =
  Cudd.Mtbddc.print_table (fun x -> Apron.Abstract0.print string_of_dim x)
    fmt table
;;

let top = Mtbdddomain0.top man env;;
printf "table = %a@." print_table man.ApronDD.table;;
Gc.major();;
printf "table = %a@." print_table man.ApronDD.table;;

let bexpr0 = Parser.boolexpr2_of_string env cond
  "e0 == a";;



let bexpr1 = Parser.boolexpr2_of_string env cond
  "x0>=0";;
let bexpr1 = bexpr1.Cond.val1.Env.val0;;
let abs1 = Mtbdddomain0.meet_condition man env cond top bexpr1;;
printf "table = %a@." print_table man.ApronDD.table;;
Gc.major();;
printf "table = %a@." print_table man.ApronDD.table;;
Gc.major();;


let bexpr1 = Parser.boolexpr2_of_string env cond
  "x0+2*x1>=0 and x0+x1<=4 and x2>=0 and x2<=10";;
let bexpr1 = bexpr1.Cond.val1.Env.val0;;
let bexpr2 = Parser.boolexpr2_of_string env cond
  "x0+2*x1>=2 and x0+x1<=6 and x2>=3 and x2<=11";;
let bexpr2 = bexpr2.Cond.val1.Env.val0;;

let abs1 = Mtbdddomain0.meet_condition man env cond top bexpr1;;
printf "table = %a@." print_table man.ApronDD.table;;
let abs2 = Mtbdddomain0.meet_condition man env cond top bexpr2;;
printf "table = %a@." print_table man.ApronDD.table;;
let abs = Mtbdddomain0.join man abs1 abs2;;
printf "table = %a@." print_table man.ApronDD.table;;
Gc.compact();;
