
open Ocamlqsopt_ex.Numutil
open GmpRatio
open Ocamlqsopt_ex.Lp
module StringLP = Make(StringX)
module MyDualizer = Dualizer(StringLP)
open MyDualizer
open StringLP

let lp = 
  {
    prob_name = "test_01";
    obj = Max(Sum([from_int 3 **. Var("x"); from_int 2 **. Var("y"); from_int 4 **. Var("z")]));
    constrs = 
      [
        Var("x") >=. Const(from_int 2);
        Var("z") >=. Const(from_int 1);
        Var("z") <=. Const(from_int 10);
        Sum([from_int 3 **. Var("x"); from_int 2 **. Var("y"); from_int 1 **. Var("z")]) <=. Const(from_int 12);
        Sum([from_int 5 **. Var("x"); from_int 1 **. Var("y")]) ==. Const(from_int 10);
      ]
  }
(*
let lp = 
  {
    prob_name = "test_01";
    obj = Max(from_int 1 **. Var("x"));
    constrs = 
      [
        Var("x") >=. Const(from_int 2);
      ]
  }


let _ = print_endline "\n*** The Primal LP:"
let _ = print_endline (string_of_lp lp)
let (objective_value,optimal_solution) = solve lp
let _ = print_endline ("Objective Value = " ^ Ocamlqsopt_ex.Numutil.InfGmpRatio.to_string objective_value) 

let _ = print_endline ("Value for x = " ^ to_string (optimal_solution "x")) 
let _ = print_endline ("Value for y = " ^ to_string (optimal_solution "y")) 
let _ = print_endline ("Value for z = " ^ to_string (optimal_solution "z")) 

*)

let dual_lp = dualize lp
let _ = print_endline "\n*** The Dual LP:"
let _ = print_endline (DualLP.string_of_lp dual_lp)

let (objective_value,optimal_solution) = MyDualizer.DualLP.solve dual_lp

let _ = print_endline ("Objective Value = " ^ Ocamlqsopt_ex.Numutil.InfGmpRatio.to_string objective_value) 


(*
let lp = 
  {
    prob_name = "test_01";
    obj = Max(from_int 0 **. Var("x"));
    constrs = 
      [
        Var("x") >=. Const(from_int 2);
      ]
  }
let lp = 
  {
    prob_name = "test_01";
    obj = Min(from_int (0) **. Var("x"));
    constrs = 
      [
        from_int (-1) **. Var("x") ==. Const(from_int 1);
        Var("x") >=. Const(from_int 0)
      ]
  }
let lp' = 
  {
    prob_name = "test_02";
    obj = Max(Var("x"));
    constrs = 
      [
        Var("x") <=. Var("y");
        Var("x") <=. Const(from_int (-0));
        Var("x") >=. Const(from_int (-2));
      ]
  }
*)

