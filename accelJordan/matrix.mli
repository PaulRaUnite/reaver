module type Field = sig
  type t
  val print : Format.formatter -> t -> unit
  val print_float : Format.formatter -> t -> unit
  val of_int : int -> t
  val of_float : float -> t
  val of_mpqf : Mpqf.t -> t
  val of_mpfrf : Mpfrf.t -> t
  val of_scalar : Apron.Scalar.t -> t
  val of_interval : Apron.Interval.t -> t
  val of_coeff : Apron.Coeff.t -> t
  val to_float : t -> float
  val to_coeff : t -> Apron.Coeff.t
  val neg : t -> t
  val inv : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val is_zero : t -> bool
  val is_scalar : t -> bool
end
type 'a tGen = 'a array array
module type S = sig
  module F : Field
  type t = F.t tGen

  val print_float : Format.formatter -> t -> unit
  val print_F : Format.formatter -> t -> unit
  val print_default : (Format.formatter -> t -> unit) ref
  val print : Format.formatter -> t -> unit
  val make0 : int -> int -> t
  val identity : int -> t
  val diag : t list -> t
  val scale : F.t -> t -> t
  val gemm : t -> t -> t
  val gemmv : t -> F.t array -> F.t array
  val vgemm : F.t array -> t -> F.t array
  val row_set_linexpr_affine: F.t array -> Apron.Linexpr0.t -> unit
  val row_of_linexpr_affine: nbdims:int -> Apron.Linexpr0.t -> F.t array
  val of_tlincons_affine : nbdims:int -> Apron.Lincons0.t array -> t
  val row_to_linexpr_affine: F.t array -> Apron.Linexpr0.t
  val to_tlinexpr_affine : t -> Apron.Linexpr0.t array
  val to_tlincons_affine : t -> Apron.Lincons0.t array
(*
  val row_to_linexpr_linear : F.t array -> Apron.Linexpr0.t
  val to_tlinexpr_linear : t -> Apron.Linexpr0.t array
  val to_tlincons_linear : t -> Apron.Lincons0.t array
*)
end

module FF : (Field with type t = float)
module FC : (Field with type t = Complex.t)
module FMpqf : (Field with type t = Mpqf.t)
module FMpqf2 : (Field with type t = Mpqf.t * Mpqf.t)

val dim1 : 'a tGen -> int
val dim2 : 'a tGen -> int
val map : ('a -> 'b) -> 'a tGen -> 'b tGen
val mapij : (int -> int -> 'a -> 'b) -> 'a tGen -> 'b tGen
val maptranspose :  ('a -> 'b) -> 'a tGen -> 'b tGen
val mpqf_of_d : float tGen -> Mpqf.t tGen
val interval_of_scalar : 'a tGen -> ('a * 'a) tGen


module Make : functor (F : Field) -> (S with module F=F)
module F : (S with module F=FF)
module C : (S with module F=FC)
module Mpqf : (S with module F=FMpqf)
module Mpqf2 : (S with module F=FMpqf2)
