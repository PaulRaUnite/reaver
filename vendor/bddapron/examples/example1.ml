(*
bddtop -I $CAMLLIB_INSTALL/lib -I $MLCUDDIDL_INSTALL/lib

open Bdd;;
#install_printer Env.print;;
#install_printer Expr1.Bool.print;;
#install_printer Expr1.print;;
#install_printer Domain1.print;;
*)

open Format;;
open Bdd;;

let cudd = Cudd.Man.make_d ();;
Cudd.Man.print_limit := 200;;
Cudd.Man.set_gc 10000
  (begin fun () -> printf "@.CUDD GC@." end)
  (begin fun () -> printf "@.CUDD REORDER@." end)
;;
let env = Env.make ~symbol:Env.string_symbol cudd;;
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

let expr = Expr1.Bool.var env "b0";;
printf "%a@." Expr1.Bool.print expr;;
