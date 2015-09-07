
type matgen = Apron.Generator0.typ * Matrix.Mpqf.t

val print_matgen :
  Format.formatter -> Apron.Generator0.typ * Matrix.Mpqf.t -> unit

val ltemplate : nbcoeffs:int -> int -> Bound.expr list

val assigngenx_of_polyjordany :
  trans:Trans.trans ->
  apron:'a Polka.t Apron.Manager.t ->
  'a Polka.t Apron.Abstract0.t ->
  Trans.assigngen

val abstrans_of_ltemplate :
  guardtrans:Trans.guardtrans ->
  apron:'a Polka.t Apron.Manager.t -> Bound.expr list -> 'a Trans.abstrans

val refine_abstrans_with_poly_guard :
  apron:'a Polka.t Apron.Manager.t ->
  abstrans:'a Trans.abstrans ->
  poly:'a Polka.t Apron.Abstract0.t ->
  unit
