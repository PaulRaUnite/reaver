
open Format
open Apron

let inf = 1./.0.

let string_of_dimx i = sprintf "x%i" i
let string_of_dimy i = sprintf "y%i" i
let string_of_dimj i = sprintf "j%i" i
let string_of_dim_default = string_of_dimx

let scalar_to_float x =
  Scalar.(match x with
  | Mpqf x ->
      if (Mpzf.sgn (Mpqf.get_den x))=0 then
	(float_of_int (Mpqf.sgn x)) *. inf
      else
	Mpqf.to_float x
  | Mpfrf x -> Mpfrf.to_float x
  | Float x -> x
  )

let coeff_to_float x =
  Coeff.(match x with
  | Scalar x -> scalar_to_float x
  | Interval x -> failwith "1"
  )

let interval_to_float2 x =
  Interval.(
    (scalar_to_float x.inf,
     scalar_to_float x.sup)
  )



let print_scalar fmt x =
  fprintf fmt "%.4G" (scalar_to_float x)

let print_interval fmt itv = Interval.(
  Format.fprintf fmt "[@[<hv>%a;@ %a@]]"
    print_scalar itv.inf print_scalar itv.sup
)
let print fmt c = Coeff.(match c with
| Scalar s -> print_scalar fmt s
| Interval i -> print_interval fmt i
)

let row_normalize row =
  let nbdims = Array.length row in
  let min = ref (1./.0.) in
  for i=0 to nbdims-2 do
    let el = abs_float row.(i) in
    if el > 0. && el < !min then min := el
  done;
  Array.iteri (begin fun i el -> row.(i) <- el /. !min end) row;
  ()

let linexpr0_normalize
    linexpr0
    =
  let maxdim = ref 0 in
  Linexpr0.iter
    (fun coeff dim -> maxdim := max !maxdim dim)
    linexpr0
  ;
  if false then printf "maxdim=%i@." !maxdim;
  let nbdims = !maxdim+2 in
  if false then printf "nbdims=%i@." nbdims;
  let row = Array.make nbdims 0. in
  Linexpr0.iter
    (begin fun coeff dim ->
      if false then printf "dim=%i@." dim;
      row.(dim) <- (coeff_to_float coeff);
     end)
    linexpr0
  ;
  row.(nbdims-1) <- (coeff_to_float (Linexpr0.get_cst linexpr0));
  if false then printf "Here1@.";
  row_normalize row;
 if false then printf "Here2@.";
  let linexpr0 = Linexpr0.make None in
  for i=nbdims-2 downto 0 do
    if row.(i)<>0. then
      Linexpr0.set_coeff linexpr0 i (Coeff.s_of_float row.(i))
  done;
  Linexpr0.set_cst linexpr0 (Coeff.s_of_float row.(nbdims-1));
  if false then printf "Here3@.";
  linexpr0

let print_linexpr0 ?(string_of_dim=string_of_dim_default) fmt expr =
  Linexpr0.(
    Format.fprintf fmt "@[<hov>";
    let first = ref true in
    iter
      (begin fun coeff dim ->
	let coeff = Coeff.reduce coeff in
	let sgn = match coeff with
	| Coeff.Scalar x -> Scalar.sgn x
	| Coeff.Interval i ->
	    if Interval.is_zero i then 0 else 1
	in
	if sgn <> 0 then begin
	  if not !first then Format.fprintf fmt "@,";
	  if sgn>0 then begin
	    if not !first then Format.pp_print_string fmt "+";
	  end;
	  begin match coeff with
	  | Coeff.Scalar scalar ->
	      if Scalar.equal_int scalar (-1) then
		Format.pp_print_string fmt "-"
	      else if not (Scalar.equal_int scalar 1) then
		print_scalar fmt scalar;
	  | Coeff.Interval i ->
	      print_interval fmt i
	  end;
	  Format.pp_print_string fmt (string_of_dim dim);
	  first := false;
	end;
       end)
      expr;
    begin match get_cst expr with
    | Coeff.Scalar scalar ->
	let sgn = Scalar.sgn scalar in
	if sgn <> 0 then begin
	  if not !first then Format.fprintf fmt "@,";
	  if sgn>0 && not !first then Format.pp_print_char fmt '+';
	  print_scalar fmt scalar;
	end
	else if !first then
	  Format.pp_print_char fmt '0';
    | Coeff.Interval i ->
	if not (Interval.is_zero i) then begin
	  if not !first then Format.pp_print_char fmt '+';
	  print_interval fmt i
	end
	else if !first then
	  Format.pp_print_char fmt '0';
    end;
    Format.fprintf fmt "@]";
    ()
  )

let print_lincons0 ?(string_of_dim=string_of_dim_default) fmt cons = Lincons0.(
  print_linexpr0 ~string_of_dim fmt
    (match cons.typ with
    | EQMOD _ -> cons.linexpr0
    | _ -> linexpr0_normalize cons.linexpr0
    )
  ;
  Format.fprintf fmt "%s0" (string_of_typ cons.typ);
  begin match cons.typ with
  | EQMOD x -> Format.fprintf fmt " mod %a" Scalar.print x;
  | _ -> ()
  end;
  ()
)

let print_generator0 ?(string_of_dim=string_of_dim_default) fmt gen = Generator0.(
  Format.fprintf fmt "%s:" (string_of_typ gen.typ);
  print_linexpr0 ~string_of_dim fmt
    (match gen.typ with
    | VERTEX | RAYMOD | LINEMOD -> gen.linexpr0
    | _ -> linexpr0_normalize gen.linexpr0
    )
  ;
  ()
)

let print_abstract0 ?(string_of_dim=string_of_dim_default) fmt poly =
  let man = Apron.Abstract0.manager poly in
  if Apron.Abstract0.is_bottom man poly then
    pp_print_string fmt "bottom"
  else if Apron.Abstract0.is_top man poly then
    pp_print_string fmt "top"
  else
    let dim = Apron.Abstract0.dimension man poly in
    let tlincons0 = Apron.Abstract0.to_lincons_array man poly in
    let tlingen0 = Apron.Abstract0.to_generator_array man poly in
    fprintf fmt "@[<v>";
    let first = ref true in
    if false then begin
      if (Array.length tlingen0)<=48 then begin
	first:=false;
	fprintf fmt "%a@ "
	  (Print.array (print_generator0 ~string_of_dim)) tlingen0
      end;
    end;
    begin match !Option.print with
    | Option.Print_poly | Option.Print_boxpoly ->
      if (Array.length tlincons0)<=48 then begin
	if not !first then fprintf fmt "@ " else first := false;
	Print.array (print_lincons0 ~string_of_dim) fmt tlincons0;
      end;
    | _ -> ()
    end;
    begin match !Option.print with
    | Option.Print_box | Option.Print_boxpoly ->
      if not !first then fprintf fmt "@ " else first:=false;
      let box = Apron.Abstract0.to_box man poly in
      let box = Array.mapi (fun i x -> (i, interval_to_float2 x)) box in
      Print.array
	(fun fmt (dim,(l,u)) ->
	  let name = string_of_dim dim in
	  if l=u then
	    fprintf fmt "%s=%.4G" name l
	  else if not (l= -1./.0. && u=1./.0.) then
	    fprintf fmt "%s in [%.4G , %.4G]" name l u
	)
	fmt
	box
    | _ -> ()
    end;
    if not !first then fprintf fmt "@ " else first:=false;
    fprintf fmt "%i constraints, %i generators"
      (Array.length tlincons0) (Array.length tlingen0)
    ;
    fprintf fmt "@]"

let print_abstract0_gen ?(string_of_dim=string_of_dim_default) fmt poly =
  let man = Apron.Abstract0.manager poly in
  if Apron.Abstract0.is_bottom man poly then
    pp_print_string fmt "bottom"
  else if Apron.Abstract0.is_top man poly then
    pp_print_string fmt "top"
  else
    let dim = Apron.Abstract0.dimension man poly in
    let tlingen0 = Apron.Abstract0.to_generator_array man poly in
    fprintf fmt "(%i,%i):" dim.Apron.Dim.intd dim.Apron.Dim.reald;
    if (Array.length tlingen0)<=48 then begin
      fprintf fmt "%a@ "
	(Print.array (print_generator0 ~string_of_dim)) tlingen0
    end;
    fprintf fmt "%i generators" (Array.length tlingen0)

(*
let meet_lincons_array apron abs tlincons0 =
  if false then
    printf "meet_lincons_array %a@? %a@? ...@?"
      (Apron.Abstract0.print (fun i -> sprintf "x%i" i)) abs
      (Print.array (Apron.Lincons0.print (fun i -> sprintf "x%i" i))) tlincons0
  ;
  let res = Apron.Abstract0.meet_lincons_array apron abs tlincons0 in
  printf "done@.";
  res
*)
