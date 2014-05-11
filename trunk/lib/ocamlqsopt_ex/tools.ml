let rec iteri i f = function
  []    -> ()
| x::xs -> f i x; iteri (i+1) f xs
let iteri f l = iteri 0 f l

