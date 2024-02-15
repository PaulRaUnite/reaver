(*
bddaprontop -I $CAMLLIB_INSTALL/lib -I $MLCUDDIDL_INSTALL/lib -I $APRON_INSTALL/lib

open Bddapron;;
#install_printer Env.print;;
#install_printer Expr2.Bool.print;;
#install_printer Expr2.print;;
#install_printer Mtbdddomain1.print;;
#install_printer Bdddomain1.print;;
*)

open Format;;
open Bddapron


let cudd = Cudd.Man.make_v ();;
Cudd.Man.print_limit := 200;;
Cudd.Man.set_gc 10000
  (begin fun () -> printf "@.CUDD GC@." end)
  (begin fun () -> printf "@.CUDD REORDER@." end)
;;

let apron = Polka.manager_alloc_loose();;

let env = Env.make ~symbol:Env.string_symbol cudd;;
let cond = Cond.make ~symbol:Env.string_symbol cudd;;

let env = Env.add_vars env 
  [
    ("counter",`Int);
    ("x",`Real);
    ("bcounter",(`Bint(false,3)))
  ];;

let counter = Expr1.var env cond "counter";;
printf "counter = %a@." (Expr1.print cond) counter;;
let x = Expr1.var env cond "x";;
printf "x = %a@." (Expr1.print cond) counter;;

let bcounter = Expr1.var env cond "bcounter";;
printf "bcounter = %a@." (Expr1.print cond) bcounter;;

let cst_0 =
  Expr1.Apron.to_expr
    (Expr1.Apron.cst env cond (Apron.Coeff.s_of_int 0));;
printf "cst_0 = %a@." (Expr1.print cond) cst_0;;
let cst_2 =
  Expr1.Apron.to_expr
    (Expr1.Apron.cst env cond (Apron.Coeff.s_of_int 2));;
printf "cst_2 = %a@." (Expr1.print cond) cst_2;;

let bcst_2 =
  Expr1.Bint.to_expr
    (Expr1.Bint.of_int env cond (`Bint(false,3)) 2);;
printf "bcst_2 = %a@." (Expr1.print cond) bcst_2;;

let bcst_3 =
  Expr1.Bint.to_expr
    (Expr1.Bint.of_int env cond (`Bint(false,3)) 3);;
printf "bcst_3 = %a@." (Expr1.print cond) bcst_3;;

let expr0 = Expr1.eq cond counter cst_2;;
printf "expr0 = %a@." (Expr1.Bool.print cond) expr0;;
(*
let expr0 = 
  Expr1.Bool.dand cond expr0 
    (Expr1.Bint.supeq cond 
      (Expr1.Bint.of_expr bcounter) (Expr1.Bint.of_expr bcst_3));;
printf "expr0 = %a@." (Expr1.Bool.print cond) expr0;;
*)
let expr1 = 
  Expr1.Apron.ite cond 
    (Expr1.eq cond bcounter bcst_3)
    (Expr1.Apron.of_expr x)
    (Expr1.Apron.add cond 
      (Expr1.Apron.of_expr x)
      (Expr1.Apron.of_expr cst_2))
;;
printf "expr1 = %a@." (Expr1.Apron.print cond) expr1;;

let cons1 = Expr1.Apron.supeq cond expr1;;

let expr2 = Expr1.eq cond bcounter bcst_2;;
printf "expr2 = %a@." (Expr1.Bool.print cond) expr2;;

let expr3 = Expr1.ite cond expr2 bcst_3 bcounter;;
printf "expr3 = %a@." (Expr1.print cond) expr3;;

let man1 = Mtbdddomain1.make_man apron;;
let top1 = Mtbdddomain1.top man1 env;;
let abs1 = Mtbdddomain1.meet_condition man1 cond top1 expr0;;
let abs1 = Mtbdddomain1.meet_condition man1 cond abs1 cons1;;

printf "abs1 = %a@." (fun fmt x -> Mtbdddomain1.print fmt x) abs1;;

let man2 = Bdddomain1.make_man apron;;
let top2 = Bdddomain1.top man2 env;;
let abs2 = Bdddomain1.meet_condition man2 cond top2 expr0;;
let abs2 = Bdddomain1.meet_condition man2 cond abs2 cons1;;

printf "abs2 = %a@." (fun fmt x -> Bdddomain1.print fmt x) abs2;;
Bdddomain1.canonicalize man2 abs2;;
printf "abs2 = %a@." (fun fmt x -> Bdddomain1.print fmt x) abs2;;

let manbdd = Domain1.make_bdd apron;;
let topbdd = Domain1.top manbdd env;;
let absbdd = Domain1.meet_condition manbdd cond topbdd expr0;;
let absbdd = Domain1.meet_condition manbdd cond absbdd cons1;;

let manmtbdd = Domain1.make_mtbdd apron;;
let topmtbdd = Domain1.top manmtbdd env;;
let absmtbdd = Domain1.meet_condition manmtbdd cond topmtbdd expr0;;
let absmtbdd = Domain1.meet_condition manmtbdd cond absmtbdd cons1;;
