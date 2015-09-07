
type weight = { mu1 : float; mu2 : float; s : float; }
type uexpr = [`Unary of int]
type bexpr = [`Binary of weight * int * int]
type expr = [ uexpr | bexpr ]

val print_weight : Format.formatter -> weight -> unit
val print_expr : Format.formatter -> [< expr] -> unit
val print_pair : Format.formatter -> float * int -> unit
val print_gk : Format.formatter -> float * int -> unit
val print_lk : Format.formatter -> float * int -> unit
val print_lgk : Format.formatter -> float * float * int -> unit
val print_ltrk : Format.formatter -> float * float * int * int -> unit
val print_gtrk : Format.formatter -> float * float * int * int -> unit

val round_float: ?nb:int -> [`D | `U] -> float -> float

val binom : int -> int -> int
val binom_float : float -> int -> float
val add_ratio_dpoly_poly : gamma:float -> float -> int -> float
val fact : int -> int

val inf : float
val neginf : float
val pi : float
val pi2 : float
val pi4 : float
val piinv : float
val pi2inv : float
val sin_atan : float -> float


module Int :
  sig
    exception Bottom
    type t = float * float
    val bottom : t
    val zero : t
    val join : t -> t -> t
    val meet : t -> t -> t
    val is_leq : eps:float -> t -> t -> bool
    val add : t -> t -> t
    val neg : t -> t
    val sub : t -> t -> t
    val scale : float -> t -> t
    val add_scalar : t -> float -> t
    val div_scalar : t -> float -> t
    val mul : t -> t -> t
    val atan : t -> t
    val sin_atan : t -> t
    val pow : float -> t -> t
    val binom_float : t -> int -> t
    val round : ?nb:int -> t -> t
    val print : Format.formatter -> t -> unit
  end

module Real : sig
  val single_int : lk:float * int -> int -> float
  val n_single : lk:float * int -> int
  val n_two :
      weight -> lk1:float * int -> lk2:float * int -> m:float -> int
end
module Complex : sig
  val mu_varphi_of_sum : weight -> float*int*int -> float*float
  val single_int : ltrk:float * float * int * int -> int -> float
end
val bound_fun_range : nmin:int -> nmax:int -> (int -> float) -> Int.t
val bound_single_coeff :
  Jordan.coeff ->
  nmin:int -> nmax:int -> Int.t
val bound_two_coeff :
  weight ->
  Jordan.coeff -> Jordan.coeff ->
  nmin:int -> nmax:int -> Int.t
val bound_expr :
  dmap_coeff_dim:(Jordan.coeff, int) DMappe.t ->
  nmin:int -> nmax:int -> expr -> Int.t
val bound_lexpr :
  dmap_coeff_dim:(Jordan.coeff, int) DMappe.t ->
  nmin:int -> nmax:int -> expr list -> (expr * Int.t) list
