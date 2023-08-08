(* relating physical and logical time *)

let node cnt () = n where rec n = 0 fby n+1

let hybrid main () = x where
  rec der t = 1.0 init 0.0 reset 0.0 every (up (10.0 -. t))
  and x = (cnt ()) every (init or (up (10.0 -. t)))
