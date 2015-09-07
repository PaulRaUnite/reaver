type trans = {
  jordan : Jordan.real Jordan.t;
  s_xy : Matrix.Mpqf.t;
  sinv_yx : Matrix.Mpqf.t;
  nbcoeffs : int;
  dmap_coeff_dim : (Jordan.coeff, int) DMappe.t;
  map_dim_lsij : (int, (bool * int * int) list) Mappe.t;
  map_ij_sdim : (int * int, bool * int) Mappe.t;
  tdim : Apron.Dim.t array;
  tlinexpr0: Apron.Linexpr0.t array;
}

type guard = Matrix.Mpqf2.t
type uguard = guard list

type guardtrans = {
  trans: trans;
  tlinconsx: Apron.Lincons0.t array;
  guardy: guard;
}
type assigngen = {
  lvertex: Apron.Linexpr0.t array list;
  lray: Matrix.Mpqf.t list;
  lline: Matrix.Mpqf.t list
}
type 'a abstrans = {
  guardtrans : guardtrans;
  ltemplate : Bound.expr list;
  polyjordan: 'a Polka.t Apron.Abstract0.t;
  assigngen: assigngen;
  mutable cassigngen: assigngen;
}

val assigngen_bottom : assigngen
val abstrans_copy : 'a abstrans -> 'a abstrans
val print_trans : Format.formatter -> trans -> unit
val print_guard : Format.formatter -> guard -> unit
val print_uguard : Format.formatter -> uguard -> unit
val print_guardtrans : Format.formatter -> guardtrans -> unit
val print_assigngen : Format.formatter -> assigngen -> unit
val print_abstrans : Format.formatter -> 'a abstrans -> unit

val trans_of_jordan :
  tdim: int array ->
  tlinexpr0: Apron.Linexpr0.t array ->
  s_xy:Matrix.Mpqf.t ->
  jordan:Jordan.real Jordan.t ->
  sinv_yx:Matrix.Mpqf.t ->
  trans
val guardtrans_of_trans_tlincons0 :
  trans -> Apron.Lincons0.t array ->
  guardtrans

val guard_negate : guard -> uguard
val uguard_or : uguard -> uguard -> uguard
val uguard_negate : uguard -> uguard
val assigngen_apply :
  trans:trans ->
  'a Polka.t Apron.Manager.t ->
  'a Polka.t Apron.Abstract0.t ->
  assigngen -> 'a Polka.t Apron.Abstract0.t
val guardassigngen_apply :
  trans:trans ->
  'a Polka.t Apron.Manager.t ->
  'a Polka.t Apron.Abstract0.t ->
   tlincons:Apron.Lincons0.t array -> assigngen:assigngen ->
  'a Polka.t Apron.Abstract0.t
val abstrans_apply :
  'a Polka.t Apron.Manager.t ->
  'a Polka.t Apron.Abstract0.t ->
  'b abstrans -> 'a Polka.t Apron.Abstract0.t
