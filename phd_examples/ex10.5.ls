(* bouncing ball with automaton *)

let hybrid bouncingball (x0,v0,start,eps) = x where
  rec init x = x0
  and automaton
    | Wait -> do der v = 0.0 until start then Bounce(v0) done
    | Bounce(v00) ->
        local z,v in
        do der v = -. 9.81 init v00
           and der x = v
           and z = up(-. x)
        until (z on (-. v < eps)) then Wait
            | z then Bounce(-. 0.6 *. v)
        done
    end