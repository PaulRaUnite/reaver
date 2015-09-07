type 'a block = {
  eigenvalue: 'a;
  lk : int list;
  sumk: int (** should be the sum of the list *)
}
type 'a t = {
  lblock: 'a block list;
  dim: int;
    (** should be the sum of the size of the blocks *)
}

type real = [
  | `Real of float
  | `Complex of Complex.t * float * float (* (positive norm,angle) *)
]

type coeff = [
  | `Real of float
  | `Sin of float * float * int (** (positive norm,angle,r) with r=0 or 1 (cos) *)
] * int

val print_real : Format.formatter -> [< real ] -> unit
val print_coeff : Format.formatter -> coeff -> unit
val print_block :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a block -> unit
val print_cblock : Format.formatter -> Complex.t block -> unit
val print_rblock : Format.formatter -> real block -> unit
val print :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val printc : Format.formatter -> Complex.t t -> unit
val printr : Format.formatter -> real t -> unit

val real_one : real
val coeff_one : coeff

val mat_F_of_C : Matrix.C.t -> Matrix.F.t
val mat_C_of_F : Matrix.F.t -> Matrix.C.t
val conjugate2 : Matrix.C.t -> Matrix.C.t

val nbcoeffs : real t -> int

val block_of_cblock2 :
  Complex.t block -> Complex.t block ->
  real block * Matrix.C.t

val real_of_complex :
  sinv_yx:Matrix.C.t ->
  jordany:Complex.t t ->
  s_xy:Matrix.C.t ->
  Matrix.F.t * real t * Matrix.F.t
val real_of_complexsage :
  sinv_yx:Matrix.F.t ->
  jordany:Complex.t t ->
  s_xy:Matrix.F.t ->
  real t

val dmap_coeff_dim : real t -> (coeff, int) DMappe.t
val maps_dim_ij :
  real t -> (coeff, int) DMappe.t ->
  (int, (bool * int * int) list) Mappe.t *
  (int*int, bool*int) Mappe.t

val list_max : int list -> int

val mat_C_of_cjordan : Complex.t t -> Matrix.C.t
val mat_F_of_rjordan : real t -> Matrix.F.t
