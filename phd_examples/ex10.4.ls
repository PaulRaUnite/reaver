(* bouncing ball *)

let hybrid bouncingball (x0,v0,start,eps) = x where
rec der x = v init x0
and der v = -9.81 init 0.0 
                  reset v0 every start
                  | -0.6 *. (last v) every (up(-. x))
                  |    0.0 every ((up(-. x)) on (-. v < eps))
            
