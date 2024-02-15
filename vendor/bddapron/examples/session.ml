(* essai BddApronDomain *)

(*
if you have dynamic libraries:

ocaml -I $CAMLLIB_INSTALL/lib -I $MLCUDDIDL_INSTALL/lib -I $BDDAPRON_INSTALL/lib 

#load "cudd.cma";;
#load "camllib.cma";;
#load "bdd.cma";;

Otherwise:

bddtop -I $CAMLLIB_INSTALL/lib -I $MLCUDDIDL_INSTALL/lib -I $BDDAPRON_INSTALL/lib

#install_printer Bdd.Expr1.print;;
#install_printer Bdd.Expr1.Bool.print;;
#install_printer Bdd.Expr1.Bint.print;;
#install_printer Bdd.Expr1.Benum.print;;
#install_printer Bdd.Domain1.print;;
#install_printer Bdd.Env.print;;
*)

open Format;;
open Bdd

let cudd = Cudd.Man.make_d ();;
Cudd.Man.print_limit := 200;;
Cudd.Man.set_gc 10000
  (begin fun () -> printf "@.CUDD GC@." end)
  (begin fun () -> printf "@.CUDD REORDER@." end)
;;
let env = Bdd.Env.make cudd;;
Env.add_typ_with env "enum2" (`Benum [|"l1"; "l2"; "l3"|]);;
env;;
Env.add_vars_with env [
  ("q1",`Bint(false,3));
  ("q2",`Bint(false,3));
  ("e",`Benum("enum2"));
  ("b0",`Bool);
  ("b1",`Bool);
  ("b2",`Bool);
  ("b3",`Bool);
];;
env;;

let q1 = Bdd.Expr1.Bint.var env "q1";;
let q2 = Bdd.Expr1.Bint.var env "q2";;
let e = Bdd.Expr1.Benum.var env "e";;
let b0 = Bdd.Expr1.Bool.var env "b0";;
let b1 = Bdd.Expr1.Bool.var env "b1";;
let b2 = Bdd.Expr1.Bool.var env "b2";;
let b3 = Bdd.Expr1.Bool.var env "b3";;

let expra =  Bdd.Expr1.Bint.ite b0
  (Bdd.Expr1.Bint.of_int env (`Bint (true,4)) (-3))
  (Bdd.Expr1.Bint.of_int env (`Bint (true,4)) 2)
;;
let exprb =  Bdd.Expr1.Bint.ite b1
  (Bdd.Expr1.Bint.of_int env (`Bint (true,4)) 1)
  (Bdd.Expr1.Bint.of_int env (`Bint (true,4)) (-2))
;;
let exprc = Bdd.Expr1.Bint.mul expra exprb;;

let expr0 =  (Bdd.Expr1.Benum.eq_label e "l2")
;;
let expr1 = 
  (Bdd.Expr1.Bint.supeq (Bdd.Expr1.Bint.scale 2 q1) q2)
;;

let expr2 = Bdd.Expr1.Bool.eq expr0 expr1;;

let disj = Bdd.Expr0.O.Expr.disjunction_of_bdd expr2.Bdd.Env.env expr2.Bdd.Env.val0;;

Format.printf "%a@." (Bdd.Expr0.O.Expr.print_disjunction expr2.Bdd.Env.env) disj;;

let expr2 = Bdd.Expr1.eq (Bdd.Expr1.Bool.to_expr expr0) (Bdd.Expr1.Bool.to_expr expr1);;

let expr0 = Bdd.Expr1.Bool.ite b0

  (Bdd.Expr1.Bint.supeq q1 q2) 
  (Bdd.Expr1.Benum.eq_label e "l2")
;;
let expr1 = Bdd.Expr1.Bool.ite b1
  (Bdd.Expr1.Bint.supeq (Bdd.Expr1.Bint.scale 3 q1) q2)
  (Bdd.Expr1.Benum.eq_label e "l1")
;;

let expr2 = Bdd.Expr1.eq (Bdd.Expr1.Bool.to_expr expr0) (Bdd.Expr1.Bool.to_expr expr1);;

let expr3 = Bdd.Expr1.Bint.ite 
  (Bdd.Expr1.Bint.supeq (Bdd.Expr1.Bint.of_int env (`Bint (false,3)) 3) q1) 
  (Bdd.Expr1.Bint.of_int env (`Bint(false,3)) 4)
  (Bdd.Expr1.Bint.of_int env (`Bint(false,3)) 5)
;;
let expr4 = Bdd.Expr1.Bint.ite 
  (Bdd.Expr1.Bint.supeq (Bdd.Expr1.Bint.of_int env (`Bint (false,3)) 3) q1) 
  q1 
  (Bdd.Expr1.Bint.of_int env (`Bint(false,3)) 7);;
  

let expr3 = Bdd.Expr1.Bint.to_expr expr3;;
let expr4 = Bdd.Expr1.Bint.to_expr expr4;;

let abs = Bdd.Domain1.top env;;
let abs2 = Bdd.Domain1.assign_lexpr abs ["q1"] [expr3];;
let abs2 = Bdd.Domain1.assign_lexpr abs ["q2"] [expr4];;
let abs2 = Bdd.Domain1.assign_lexpr abs ["q1"; "q2"] [expr3;expr4];;
