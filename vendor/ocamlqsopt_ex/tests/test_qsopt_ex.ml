open List
open Ocamlqsopt_ex.Numutil.InfGmpRatio
open Ocamlqsopt_ex.Qsopt_ex

let _ = init 128;
let lp = create_lp "test_prob" 3 2 5 in 
iteri (set_cmatcnt lp) [2;2;1];
iteri (set_cmatbeg lp) [0;2;4];
iteri (set_cmatind lp) [0;1;0;1;0];
iteri (set_sense lp)   [Comp_le; Comp_eq] ;
iteri (set_cmatval lp) (map Ocamlqsopt_ex.Numutil.GmpRatio.from_int [3;5;2;1;1]);
iteri (set_obj lp) (map Ocamlqsopt_ex.Numutil.GmpRatio.from_int [3;2;4]);
iteri (set_rhs lp) (map Ocamlqsopt_ex.Numutil.GmpRatio.from_int [12;10]);
set_lower lp 0 (Ocamlqsopt_ex.Numutil.GmpRatio.from_int 2);
  (*let _ = set_upper lp 1 (from_int 100)*)
set_lower lp 2 (Ocamlqsopt_ex.Numutil.GmpRatio.from_int 1);
set_upper lp 2 (Ocamlqsopt_ex.Numutil.GmpRatio.from_int 10);
let value =  solve lp in 
print_endline (to_string (get_value_from_sol value));
print_string "\n\n\n"

