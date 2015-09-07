val string_of_dimx : int -> string
val string_of_dimy : int -> string
val string_of_dimj : int -> string
val string_of_dim_default : int -> string
val scalar_to_float : Apron.Scalar.t -> float
val coeff_to_float : Apron.Coeff.union_5 -> float
val interval_to_float2 : Apron.Interval.t -> float * float
val print_scalar : Format.formatter -> Apron.Scalar.t -> unit
val print_interval : Format.formatter -> Apron.Interval.t -> unit
val print : Format.formatter -> Apron.Coeff.union_5 -> unit
val row_normalize : float array -> unit
val linexpr0_normalize : Apron.Linexpr0.t -> Apron.Linexpr0.t
val print_linexpr0 :
  ?string_of_dim:(Apron.Dim.t -> string) ->
  Format.formatter -> Apron.Linexpr0.t -> unit
val print_lincons0 :
  ?string_of_dim:(Apron.Dim.t -> string) ->
  Format.formatter -> Apron.Lincons0.t -> unit
val print_generator0 :
  ?string_of_dim:(Apron.Dim.t -> string) ->
  Format.formatter -> Apron.Generator0.t -> unit
val print_abstract0 :
  ?string_of_dim:(Apron.Dim.t -> string) ->
  Format.formatter -> 'a Polka.t Apron.Abstract0.t -> unit
val print_abstract0_gen :
  ?string_of_dim:(Apron.Dim.t -> string) ->
  Format.formatter -> 'a Polka.t Apron.Abstract0.t -> unit
(*
val meet_lincons_array :
  'a Apron.Manager.t ->
  'a Apron.Abstract0.t ->
  Apron.Lincons0.t array ->
  'a Apron.Abstract0.t
*)
