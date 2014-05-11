open List
open Ocamlqsopt_ex.Numutil.Ratio
open Qsopt_ex 
open Tools

let _ = init 128
let lp = create_lp 3 2 5
let _ = iteri (set_cmatcnt lp) [2;2;1]
let _ = iteri (set_cmatbeg lp) [0;2;4]
let _ = iteri (set_cmatind lp) [0;1;0;1;0]
let _ = iteri (set_sense lp)   [Comp_le; Comp_eq]
let _ = iteri (set_cmatval lp) (map from_int [3;5;2;1;1])
let _ = iteri (set_obj lp) (map from_int [3;2;4])
let _ = iteri (set_rhs lp) (map from_int [12;10])
let _ = set_lower lp 0 (from_int 2)
  (*let _ = set_upper lp 1 (from_int 100)*)
let _ = set_lower lp 2 (from_int 1)
let _ = set_upper lp 2 (from_int 10)
let value = solve lp
let _ = print_endline (to_string value)
let _ = print_string "\n\n\n"

