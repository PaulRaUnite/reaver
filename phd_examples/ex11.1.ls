(* thermostat *)

let node main (xi,eps) = (assert,ok) where
  rec assert = 0.0 <= xi && xi <= 30.0 && 
           -. 0.1 <= eps && eps <= 0.1 
  and ok = true
  and der x = if onn then xi -. x +. 22 else xi -. x init xi
  and onn = true every (up(18.0 -. x +. eps))
         | false every (up(x -. 20.0)) init xi <= 19.0 and
  and n = (last n)+1 every (up(18.0 -. x +. eps)) init 0
  and stop = true every (up(n-10)) init false