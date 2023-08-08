(* The Hybrid Synchronous Language compiler, version 0.1
  (Sat Aug 11 18:07:22 CEST 2012) *)
type ('a) bouncingball =
  { mutable init_30 : 'a }

let bouncingball () =
  { init_30 = (true: bool) }

let bouncingball_step self (z_24, z_22) (lx_19, lv_16) d_1 x0_5 v0_4 
                           start_3 eps_2 =
  let lx_20 = if self.init_30 then x0_5 else lx_19 in
  let lv_17 = if self.init_30 then 0. else lv_16 in
  let v_12 =
  if start_3
  then v0_4
  else
    if z_22
    then (-0.6) *. lv_17
    else
      if on z_24 ((<) ((~-.) v_12) eps_2)
      then 0.
      else lv_17 in
  let dx_21 = if d_1 then lx_20 else v_12 in
  let dv_18 = if d_1 then v_12 else (-9.81) in
  let upz_23 = (~-.) lx_20 in
  let upz_25 = (~-.) lx_20 in
  self.init_30 <- false;
  ((upz_25, upz_23), (dx_21, dv_18), (false, lx_20))

let bouncingball_reset self =
  self.init_30 <- true;
  ()

