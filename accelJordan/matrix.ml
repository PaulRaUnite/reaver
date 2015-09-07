

open Format

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
  val row_to_linexpr_affine: F.t array -> Apron.Linexpr0.t
  val to_tlinexpr_affine : t -> Apron.Linexpr0.t array
  val to_tlincons_affine : t -> Apron.Lincons0.t array
(*
  val row_to_linexpr_linear : F.t array -> Apron.Linexpr0.t
  val to_tlinexpr_linear : t -> Apron.Linexpr0.t array
  val to_tlincons_linear : t -> Apron.Lincons0.t array
*)
end

module FMpqf = struct
  include Mpqf

  let is_inf x =
    let d = get_den x in
    (Mpzf.sgn d) = 0

  let mpzzero = Mpz.init ()
  let mpzone = let res = Mpz.init () in Mpz.set_si res 1; res
  let inf =
    let res = Mpq.init () in
    Mpq.set_num res mpzone;
    Mpq.set_den res mpzzero;
    _mpqf res
  let neginf =
    let res = Mpq.init () in
    Mpq.set_num res mpzone;
    Mpq.set_den res mpzzero;
    Mpq.neg res res;
    _mpqf res
  let of_float x =
    if x=1./.0. then inf
    else if x= -1./.0. then neginf
    else of_float x

  let to_float x =
    if is_inf x then begin
      let s = sgn x in
      assert(s<>0);
      (float_of_int s) /. 0.
    end
    else
      to_float x

  let print_float fmt x =
    let x = to_float x in
    let s = sprintf "%.3G@?" x in
    let w = String.make (max 0 (4-(String.length s))) ' ' in
    pp_print_string fmt s;
    pp_print_string fmt w;
    ()
  let of_mpqf x = x
  let of_mpfrf x = Mpfrf.to_mpqf x
  let of_scalar (y:Apron.Scalar.t) : t =
    match y with
    | Apron.Scalar.Float x -> of_float x
    | Apron.Scalar.Mpqf x -> x
    | Apron.Scalar.Mpfrf x -> Mpfrf.to_mpqf x
  let of_interval x = failwith "1"
  let of_coeff x =
     Apron.(Coeff.(match x with
    | Scalar x -> of_scalar x
    | Interval x -> of_interval x
     ))
  let to_coeff x = Apron.Coeff.Scalar(Apron.Scalar.Mpqf x)
  let is_zero x = (Mpqf.sgn x) = 0
  let is_scalar x = true
end
module FMpqf2 = struct
  type t = FMpqf.t * FMpqf.t
  let print fmt (y1,y2) =
    Format.fprintf fmt "[%a, %a]" FMpqf.print y1 FMpqf.print y2;
    ()
  let print_float fmt (y1,y2) =
    Format.fprintf fmt "[%a, %a]" FMpqf.print_float y1 FMpqf.print_float y2;
    ()
  let of_int x = let y = FMpqf.of_int x in (y,y)
  let of_float x = let y = FMpqf.of_float x in (y,y)
  let of_mpqf x = let y = FMpqf.of_mpqf x in (y,y)
  let of_mpfrf x = let y = FMpqf.of_mpfrf x in (y,y)
  let of_scalar x = let y = FMpqf.of_scalar x in (y,y)
  let of_interval x =
    Apron.(Interval.(
      let y1 = FMpqf.of_scalar x.inf in
      let y2 = FMpqf.of_scalar x.sup in
      (y1,y2)
    ))
  let of_coeff x =
     Apron.(Coeff.(match x with
    | Scalar x -> of_scalar x
    | Interval x -> of_interval x
     ))
  let to_float (x1,x2) = failwith "2"
  let to_coeff (x1,x2) =
    Apron.(Coeff.(Interval.(
      if Mpqf.cmp x1 x2 = 0 then
	Scalar(Scalar.Mpqf x1)
      else
	Interval({inf = Scalar.Mpqf x1; sup = Scalar.Mpqf x2})
    )))

  let sign (x1,x2) =
    if Mpqf.sgn x2 <0 then -1
    else if Mpqf.sgn x1 >=0 then 1
    else 0

  let neg (y1,y2) = (FMpqf.neg y2, FMpqf.neg y1)
  let inv ((x1,x2) as x) =
    let s = sign x in
    if s<>0 then (FMpqf.inv x2, FMpqf.inv x1)
    else failwith "FMpqf2.inv"

  let myadd y z =
    if FMpqf.is_inf y then
      if FMpqf.is_inf z then
	if (FMpqf.sgn y) = (FMpqf.sgn z) then y else failwith "3"
      else
	y
    else if FMpqf.is_inf z then
      z
    else
      FMpqf.add y z

  let add (y1,y2) (z1,z2) =
    (myadd y1 z1, myadd y2 z2)

  let mysub y z =
    if FMpqf.is_inf y then
      if FMpqf.is_inf z then
	if (FMpqf.sgn y) <> (FMpqf.sgn z) then y else failwith "4"
      else
	y
    else if FMpqf.is_inf z then
      z
    else
      FMpqf.sub y z

  let sub (y1,y2) (z1,z2) =
    (mysub y1 z2, myadd y2 z1)

  let mymul y z =
    if FMpqf.is_inf y || FMpqf.is_inf z then begin
      let s1 = FMpqf.sgn y in
      let s2 = FMpqf.sgn z in
      let s=s1*s2 in
      if s>0 then FMpqf.inf else if s<0 then FMpqf.neginf else failwith "5"
    end else
      FMpqf.mul y z
  let scale y (z1,z2) =
    if (Mpqf.sgn y)<0 then
      (mymul y z2, mymul y z1)
    else
      (mymul y z1, mymul y z2)

  let mycmp y z =
    if FMpqf.is_inf y then
      if FMpqf.is_inf z then
	(FMpqf.sgn y) - (FMpqf.sgn z)
      else
	FMpqf.sgn y
    else if FMpqf.is_inf z then
      - (FMpqf.sgn z)
    else
      FMpqf.cmp y z
  let join (y1,y2) (z1,z2) =
    ((if (mycmp y1 z1) <= 0 then y1 else z1),
     (if (mycmp y2 z2) >=0 then y2 else z2))

  let mul ((y1,y2) as y) ((z1,z2) as z) =
    let sy = sign y and sz = sign z in
    if sy=0 then
      join (scale y1 z) (scale y2 z)
    else if sz=0 then
      join (scale z1 y) (scale z2 y)
    else
      if sy>0 then
	if sz>0 then
	  (mymul y1 z1, mymul y2 z2)
	else
	  (
	    assert (sz<0);
	    (mymul y2 z1, mymul y1 z2)
	  )
      else (
	assert (sy<0);
	if sz>0 then
	  (mymul y1 z2, mymul y2 z1)
	else
	  (assert (sz<0);
	   (mymul y2 z2, mymul y1 z1))
      )

  let is_zero (x1,x2) = FMpqf.is_zero x1 && FMpqf.is_zero x2
  let is_scalar (x1,x2) = Mpqf.equal x1 x2
end
module FF = struct
  type t = float
  let print fmt el =
    let s = sprintf "%.3G" el in
    let w = String.make (max 0 (4-(String.length s))) ' ' in
    pp_print_string fmt s;
    pp_print_string fmt w
  let print_float = print
  let of_int = float_of_int
  let of_float x = x
  let of_mpqf x = FMpqf.to_float x
  let of_mpfrf x = Mpfrf.to_float x
  let of_scalar x =
    Apron.(Scalar.(match x with
    | Float x -> x
    | Mpqf x -> FMpqf.to_float x
    | Mpfrf x -> Mpfrf.to_float x
    ))
  let of_interval x = failwith "6"
  let of_coeff x =
    Apron.(Coeff.(match x with
    | Scalar x -> of_scalar x
    | Interval x -> of_interval x
    ))
  let to_float x = x
  let to_coeff x = Apron.Coeff.Scalar(Apron.Scalar.Float x)
  let neg x = -. x
  let inv x = 1./.x
  let add = (+.)
  let sub = (-.)
  let mul x y = x*.y
  let is_zero x = x=0.
  let is_scalar x = true
end
module FC = struct
  include Complex
  let print fmt el =
    let s =
      if el.im = 0.0 then
	sprintf "%.3G" el.re
      else if el.re = 0.0 then
	sprintf "%.3Gi" el.im
      else if el.im > 0. then
	sprintf "%.3G+%.3Gi" el.re el.im
      else
	sprintf "%.3G-%.3Gi" el.re (-.el.im)
    in
    let w = String.make (max 0 (7-(String.length s))) ' ' in
    pp_print_string fmt s;
    pp_print_string fmt w
  let print_float = print
  let of_int x = { re = float_of_int x; im=0. }
  let of_float x = { re = x;  im=0. }
  let of_mpqf x = { re=FMpqf.to_float x; im=0. }
  let of_mpfrf x = { re=Mpfrf.to_float x; im=0. }
  let of_scalar x =
    { re=FF.of_scalar x; im=0. }
  let of_interval x = failwith "7"
  let of_coeff x = { re=FF.of_coeff x; im=0. }
  let to_float x = failwith "8"
  let to_coeff x = failwith "9"
  let is_zero x = x=zero
  let is_scalar x = true
end


let dim1 mat = Array.length mat
let dim2 mat =
  let length = Array.length mat in
  if length=0 then failwith "" else Array.length mat.(0)

let mapij f t =
  Array.mapi
    (begin fun i row ->
      Array.mapi
	 (begin fun j el -> f i j el end)
	 row
     end)
    t

let map f t =
  Array.map
    (begin fun row ->
      Array.map f row
     end)
    t

let maptranspose f t =
  let dim1 = dim1 t and dim2 = dim2 t in
  if dim1=0 or dim2=0 then [||]
  else
    let el = f t.(0).(0) in
    let res = Array.init dim2 (fun i -> Array.make dim1 el) in
    for i1=0 to dim1-1 do
      for i2=0 to dim2-1 do
	res.(i2).(i1) <- f t.(i1).(i2)
      done;
    done;
    res

let mpqf_of_d mat = map FMpqf.of_float mat

let interval_of_scalar mat =
  map (fun x -> (x,x)) mat

module Make(F:Field) = struct
  module F=F
  type t = F.t tGen

  let make0 x y =
    let el = F.of_int 0 in
    Array.init
      x
      (fun i -> Array.make y el)

  let identity n =
    let mat = make0 n n in
    let one = F.of_int 1 in
    for i=0 to pred n do
      mat.(i).(i) <- one
    done;
    mat

  let diag lmat =
    let glength =
      List.fold_left
	(begin fun res mat -> res + dim1 mat end)
	0 lmat
    in
    let rmat = make0 glength glength in
    let offset = ref 0 in
    List.iter
      (begin fun mat ->
	let length = dim1 mat in
	for i=0 to length-1 do
	  for j=0 to length-1 do
	    rmat.(!offset+i).(!offset+j) <- mat.(i).(j)
	  done;
	done;
	offset := !offset + length
       end)
      lmat
    ;
    assert (!offset = glength);
    rmat

  let scale fact mat =
    let res = map (fun x -> F.mul fact x) mat in
    res

(*
  let of_mat_D
      mat
      =
    Bigarray.(
      let dim1 = Array2.dim1 mat in
      let dim2 = Array2.dim2 mat in
      let nmat = make0 dim1 dim2 in
      for i=0 to pred dim1 do
	for j=0 to pred dim2 do
	  nmat.(i).(j) <- F.of_float (Array2.get mat (i+1) (j+1))
	done;
      done;
      nmat
    )

  let to_mat_D
      mat
      =
    Bigarray.(
      let dim1 = dim1 mat in
      let dim2 = dim2 mat in
      let nmat = Array2.create float64 fortran_layout dim1 dim2 in
      for i=0 to pred dim1 do
	for j=0 to pred dim2 do
	  Array2.set nmat (i+1) (j+1) (F.to_float mat.(i).(j))
	done
      done;
      nmat
    )
*)
  let print_float fmt mat =
    Print.array ~first:"[@[<v>" ~last:"@]]"
      (Print.array ~first:"[@[<hov 8>" ~last:"@]]"
	F.print_float)
      fmt
      mat

  let print_F fmt mat =
    Print.array ~first:"[@[<v>" ~last:"@]]"
      (Print.array ~first:"[@[<hov 8>" ~last:"@]]"
	 F.print)
      fmt
      mat
  let print_default = ref print_F
  let print fmt mat = !print_default fmt mat

  let gemm matxy matyz =
    let dimx = dim1 matxy in
    let dimy = dim1 matyz in
    let dimz = dim2 matyz in
    assert(dim2 matxy = dimy);
    let matxz = make0 dimx dimz in
    for i=0 to pred dimx do
      for k=0 to pred dimz do
	let res = ref (F.of_int 0) in
	for j=0 to pred dimy do
	  let elxy = matxy.(i).(j) in
	  let elyz = matyz.(j).(k) in
	  if (not (F.is_zero elxy)) && (not (F.is_zero elyz)) then
	    res := F.add !res (F.mul elxy elyz)
	done;
	matxz.(i).(k) <- !res
      done;
    done;
    matxz

  let gemmv matxy vecy =
    let dimy = Array.length vecy in
    assert(dim2 matxy = dimy);
    Array.map
      (begin fun rowxy ->
	let res = ref (F.of_int 0) in
	for i=0 to pred dimy do
	  if (not (F.is_zero rowxy.(i))) && (not (F.is_zero vecy.(i))) then
	    res := F.add !res (F.mul rowxy.(i) vecy.(i))
	done;
	!res
       end)
      matxy

  let vgemm vecx matxy =
    let dimx = dim1 matxy in
    let dimy = dim2 matxy in
    assert(Array.length vecx = dimx);
    let resy = Array.make (dim2 matxy) (F.of_int 0) in
    for j=0 to dimy-1 do
      for i=0 to dimx-1 do
	if (not (F.is_zero vecx.(i))) && (not (F.is_zero matxy.(i).(j))) then
	  resy.(j) <- F.add resy.(j) (F.mul vecx.(i) matxy.(i).(j))
      done;
    done;
    resy

  let row_set_linexpr_affine row linexpr0
      =
    let nbdims = (Array.length row) in
    Array.fill row 0 nbdims (F.of_int 0);
    Apron.Linexpr0.iter
      (begin fun coeff dim ->
	assert(dim<nbdims-1);
	row.(dim) <- F.of_coeff coeff
       end)
      linexpr0
    ;
    row.(nbdims-1) <- F.of_coeff (Apron.Linexpr0.get_cst linexpr0);
    if false then
      printf "nbdims=%i linexpr0=%a row=%a@ "
	nbdims
	(Apronaux.print_linexpr0 ?string_of_dim:None) linexpr0
	(Print.array F.print) row;
    ()

  let row_of_linexpr_affine ~nbdims linexpr0
      =
    let row = Array.make nbdims (F.of_int 0) in
    row_set_linexpr_affine row linexpr0;
    row

  let of_tlincons_affine ~nbdims tlincons0
      =
    Apron.(Lincons0.(
      let length = ref 0 in
      Array.iter
	(begin fun lincons ->
	  length := !length +
	    (if lincons.typ=EQ then 2 else 1)
	 end)
	tlincons0
      ;
      let mat = make0 !length nbdims in
      let i = ref 0 in
      Array.iter
	(begin fun lincons0 ->
	  row_set_linexpr_affine mat.(!i) lincons0.linexpr0;
	  incr i;
	  if lincons0.typ=EQ then begin
	    for j=0 to nbdims-1 do
	      mat.(!i).(j) <- F.neg mat.(!i-1).(j)
	    done;
	    incr i;
	  end
	 end)
	tlincons0
      ;
      mat
    ))

  let row_to_linexpr_affine row =
    let nbdims = dim1 row in
    let linexpr0 = Apron.Linexpr0.make None in
    Apron.Linexpr0.set_cst linexpr0 (F.to_coeff row.(nbdims-1));
    for dim=nbdims-2 downto 0 do
      let el = row.(dim) in
      if not (F.is_zero el) then
	Apron.Linexpr0.set_coeff linexpr0 dim (F.to_coeff el)
    done;
    if false then
      printf "nbdims=%i row=%a linexpr0=%a@ "
	nbdims
	(Print.array F.print) row
	(Apronaux.print_linexpr0 ?string_of_dim:None) linexpr0
    ;
    linexpr0

  let row_to_linexpr_linear row =
    let nbdims = dim1 row in
    let linexpr0 = Apron.Linexpr0.make None in
    for dim=nbdims-1 downto 0 do
      let el = row.(dim) in
      if not (F.is_zero el) then
	Apron.Linexpr0.set_coeff linexpr0 dim (F.to_coeff el)
    done;
    if false then
      printf "nbdims=%i row=%a linexpr0=%a@ "
	nbdims
	(Print.array F.print) row
	(Apronaux.print_linexpr0 ?string_of_dim:None) linexpr0
    ;
    linexpr0

  let to_tlinexpr_affine mat =
    Array.map row_to_linexpr_affine mat
  let to_tlinexpr_linear mat =
    Array.map row_to_linexpr_linear mat

  let to_tlincons_affine mat =
    Array.map
      (fun row ->
	let linexpr0 = row_to_linexpr_affine row in
	Apron.Lincons0.({ typ=SUPEQ; linexpr0 })
      )
      mat
  let to_tlincons_linear mat =
    Array.map
      (fun row ->
	let linexpr0 = row_to_linexpr_linear row in
	Apron.Lincons0.({ typ=SUPEQ; linexpr0 })
      )
      mat

end


module Mpqf = Make(FMpqf)
module Mpqf2 = Make(FMpqf2)
module F = Make(FF)
module C = Make(FC)
