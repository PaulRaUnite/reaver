(* essai BddApronDomain *)

(*
if you have dynamic libraries:

ocaml -I $CAMLLIB_INSTALL/lib -I $MLCUDDIDL_INSTALL/lib -I $BDDAPRON_INSTALL/lib -I $MLGMPIDL_INSTALL/lib -I $APRON_INSTALL/lib

#load "cudd.cma";;
#load "bigarray.cma";;
#load "gmp.cma";;
#load "apron.cma";;
#load "polkaMPQ.cma";;
#load "camllib.cma";;
#load "bddapron.cma";;


Otherwise:
bddaprontop -I $CAMLLIB_INSTALL/lib -I $MLCUDDIDL_INSTALL/lib -I $BDDAPRON_INSTALL/lib -I $MLGMPIDL_INSTALL/lib -I $APRON_INSTALL/lib
*)

(*
open Format;;
open Bddapron;;
#install_printer Apron.Abstract1.print;;
#install_printer Cudd.Bdd.print__minterm;;
let print fmt x = Apron.Environment.print fmt x;;
#install_printer print;;
let print fmt x = Bddapron.Apronexpr.print Env.string_symbol fmt x;;
#install_printer print;;
let print fmt x = Cudd.Weakke.print (Bddapron.Apronexpr.print Env.string_symbol) fmt x;;
#install_printer print;;
let print fmt x = Cudd.Weakke.print Apron.Abstract1.print fmt x ;;
#install_printer print;;
#install_printer Env.print;;
*)

open Format;;
open Bddapron;;

let cudd = Cudd.Man.make_v ();;
Cudd.Man.print_limit := 200;;
Cudd.Man.set_gc 10000
  (begin fun () -> printf "@.CUDD GC@." end)
  (begin fun () -> printf "@.CUDD REORDER@." end)
;;

let env = Env.make ~symbol:Env.string_symbol cudd;;
Env.add_typ_with env "enum2" (`Benum [|"l1"; "l2"; "l3"|]);;
env;;
Env.add_vars_with env [
  ("q",`Bint(false,3));
  ("e",`Benum("enum2"));
  ("b0",`Bool);
  ("b1",`Bool);
  ("b2",`Bool);
  ("b3",`Bool);
  ("x0",`Real);
  ("x1",`Real);
  ("x2",`Real);
  ("x3",`Real);
];;
env;;

let cond = Bddapron.Cond.make ~symbol:Env.string_symbol cudd;;

let apron = Polka.manager_alloc_loose ();;
let bddapron = Domain1.make_mtbdd apron;;

(*
let p = Cond.print env;;
#install_printer p;;
let p = Expr1.print cond;;
#install_printer p;;
let p = Expr1.Bool.print cond;;
#install_printer p;;
let p = Expr1.Bint.print cond;;
#install_printer p;;
let p = Expr1.Benum.print cond;;
#install_printer p;;
let p = Expr1.Apron.print cond;;
#install_printer p;;
let p = Bdddomain1.print;;
#install_printer p;;
let p = Mtbdddomain1.print;;
#install_printer p;;
let p = Domain1.print bddapron;;
#install_printer p;;
*)



let expr0 = Parser.expr1_of_string env cond "if b0 then x0 else 2*x1";;

let expr = Expr1.Bool.of_expr (Parser.expr1_of_string env cond "x0>=10 or x1>=10");;

let top = Domain1.top bddapron env;;
let abs0 = Domain1.meet_condition bddapron cond top expr;;

let expr0 = Expr1.Apron.ite cond
  (Expr1.Bool.var env cond "b0")
  (Expr1.Apron.var env cond "x0")
  (Expr1.Apron.mul cond
    (Expr1.Apron.cst env cond (Apron.Coeff.s_of_int 2))
    (Expr1.Apron.var env cond "x1"))
;;
Cudd.Man.garbage_collect cudd;;

printf "%a@." (Expr1.Apron.print cond) expr0;;
let expr0 = expr0.Env.val0;;
let leaves = Cudd.Mtbdd.leaves expr0;;
let leaves_u = Cudd.Mtbdd.leaves_u expr0;;
Cudd.Mtbdd.guard_of_leaf_u expr0 leaves_u.(1);;


printf "%a@."
  (Cudd.Mtbdd.print__minterm
    Apronexpr.print)
    expr0
;;
printf "%a@."
  (Cudd.Mtbdd.print_minterm
    pp_print_int
    Apronexpr.print)
    expr0
;;



let expr0 = Parser.expr1_of_string env cond "if b0 then x0 else 2*x1";;
let expr0 = Expr1.Apron.of_expr expr0;;
printf "%a@." (Expr1.print cond) expr0;;
let expr1 = Expr1.Apron.ite cond
  (Expr1.Bool.var env cond "b1")
  (Expr1.Apron.var env cond "x1")
  (Expr1.Apron.mul cond
    (Expr1.Apron.cst env cond (Apron.Coeff.s_of_int 2))
    (Expr1.Apron.var env cond "x2"))
;;

let expr2 = Expr1.Apron.mul cond expr0 expr1;;
let expr3 = Expr1.Apron.add cond expr1 expr2;;
let expr4 = Expr1.Apron.add cond expr3 expr3;;

let res = ref expr2;;
for i=0 to 9 do
  res := Expr1.Apron.add cond !res expr2
done;;

let f i =
  let q = Expr1.Bint.var env cond "q" in
  let qi = Expr1.Bint.eq_int cond q i in
  let (i,pol) = (i/2, ((i mod 2)=0)) in
  let b = Expr1.Bool.var env cond (sprintf "b%i" i) in
  let x = Expr1.Apron.var env cond (sprintf "x%i" i) in
  if pol then
    Expr1.Bool.dand cond
      (Expr1.Bool.dand cond qi b)
      (Expr1.Apron.supeq cond
	(Expr1.Apron.sub cond
	  x
	  (Expr1.Apron.cst env cond (Apron.Coeff.s_of_int 1))))
  else
    Expr1.Bool.dand cond
      (Expr1.Bool.dand cond qi (Expr1.Bool.dnot cond b))
      (Expr1.Bool.dand cond
	(Expr1.Apron.supeq cond
	  (Expr1.Apron.negate cond x))
	(Expr1.Apron.supeq cond
	  (Expr1.Apron.add cond
	    x
	    (Expr1.Apron.cst env cond (Apron.Coeff.s_of_int 3)))))
;;
let tcond = Array.init 8 f;;
let top = Domain1.top bddapron env;;
printf "After top@.";;

let cond2 = Expr1.Bool.dand cond
  (Expr1.Bool.var env cond "b0")
  (Expr1.Apron.supeq cond (Expr1.Apron.var env cond "x0"));;

let abs = Domain1.meet_condition bddapron cond top cond2;;
printf "After meet_condition@.";;

let tabs =
  Array.map (fun cond2 -> Domain1.meet_condition bddapron cond top cond2) tcond
;;

let abs = Domain1.join bddapron tabs.(1) tabs.(2);;
Domain1.forget_list bddapron abs ["b0"];;
Domain1.assign_lexpr bddapron cond abs ["b1"] [Expr1.Bool.to_expr (Expr1.Bool.dtrue env cond)] None;;
Domain1.assign_lexpr bddapron cond abs ["b0"] [Expr1.Bool.to_expr (Expr1.Bool.dtrue env cond)] None ;;
Domain1.assign_lexpr bddapron cond abs ["e"] [Expr1.Benum.to_expr (Expr1.Benum.var env cond "l2")] None;;
let abs =
  Array.fold_left (Domain1.join bddapron) tabs.(0) tabs
;;
printf "After assign@.";;

let nabs = Domain1.assign_lexpr bddapron cond abs ["x0";"x3"]
  [
    Expr1.Apron.to_expr (Expr1.Apron.add cond (Expr1.Apron.var env cond "x1") (Expr1.Apron.var env cond "x1"));
    Expr1.ite cond
    (Expr1.Bool.var env cond "b2")
    (Expr1.Apron.to_expr (Expr1.Apron.add cond (Expr1.Apron.var env cond "x1") (Expr1.Apron.var env cond "x2")))
    (Expr1.Apron.to_expr (Expr1.Apron.sub cond (Expr1.Apron.var env cond "x1") (Expr1.Apron.var env cond "x2")))
  ]
  None;;
printf "After nabs@.";;
printf "abs=%a@." (Domain1.print bddapron) abs;;
Gc.full_major();;
printf "nabs=%a@." (Domain1.print bddapron) nabs;;
printf "env=%a@." Env.print nabs.Env.env;;
let env = nabs.Env.env;;
let env2 = Env.rename_vars env [("x1","y"); ("b0","c");("q","r")];;
let nabs2 = Domain1.rename bddapron nabs [("x1","y"); ("b0","c");("q","r")];;
printf "After nabs2@.";;
printf "nabs2=%a@." (Domain1.print bddapron) nabs2;;
printf "env=%a@." Env.print nabs2.Env.env;;


(*
module Leaf = struct
  type t = { min:int; max:int }
  let make a b = {min=a; max=b}
  let cst n = make n n
  let add t1 t2 =
    {
      min = t1.min + t2.min;
    max = t1.max + t2.max
    }
  let print fmt t =
    fprintf fmt "[%i,%i]" t.min t.max
  let background = make 1 (-1)
  let hash t = 11*t.min + 13*t.max
  let equal = (=)
end;;

let print_bdd fmt (bdd:Cudd.Bdd.vt) = Cudd.Bdd.print_minterm pp_print_int fmt bdd;;
let print_mtbdd = Cudd.Mtbdd.print print_bdd Leaf.print;;
let print_table = Cudd.Mtbdd.print_table Leaf.print;;

(*
#install_printer print_bdd;;
#install_printer print_mtbdd;;
#install_printer print_table;;
*)



let cudd = Cudd.Man.make_v();;

let table = Cudd.Mtbdd.make_table
  ~hash:Leaf.hash
  ~equal:Leaf.equal
;;

let b = Array.init 5 (fun i -> Cudd.Bdd.ithvar cudd i);;
let i = Array.init 5 (fun i -> Cudd.Mtbdd.cst cudd table (Leaf.cst i));;

let add t1 t2 =
  Cudd.Mtbdd.mapleaf2
    (fun x y -> Gc.compact(); Cudd.Mtbdd.unique table (Leaf.add (Cudd.Mtbdd.get x) (Cudd.Mtbdd.get y))) t1 t2;;

let proc1 () =
  let res = ref i.(0) in
  for j=2 to 4 do
    res := Cudd.Mtbdd.ite b.(j) i.(j) !res
  done;
  !res
;;
let proc2 () =
  let f = proc1 () in
  let g = add f f in
  printf "g=%a@." print_mtbdd g;;
  ()
;;

proc2();;
*)
