
open Format

type trans = {
  jordan : Jordan.real Jordan.t;
  s_xy : Matrix.Mpqf.t;
  sinv_yx : Matrix.Mpqf.t;
  nbcoeffs : int;
  dmap_coeff_dim : (Jordan.coeff,int) DMappe.t;
  map_dim_lsij : (int,(bool*int*int) list) Mappe.t;
  map_ij_sdim : (int*int,bool*int) Mappe.t;
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

let print_list ?first ?sep ?last print fmt list =
  let l = List.length list in
  if l >= 6 then pp_print_int fmt l
  else Print.list ?first ?sep ?last print fmt list

let assigngen_bottom = { lvertex=[]; lray=[]; lline=[] }
let abstrans_copy a = { a with cassigngen = a.cassigngen }

let print_trans fmt
    {
      jordan;
      s_xy;
      sinv_yx;
      dmap_coeff_dim;
      map_dim_lsij;
      map_ij_sdim;
      tdim;
      tlinexpr0
    }
    =
  let string_of_dim i = sprintf "x%i" i in
  Format.fprintf fmt
    "{@   @[<v>jordan = %a@ s_xy = %a@ sinv_yx = %a@ dmap_coeff_dim = %a@ map_dim_lsij=%a@ map_ij_sdim=%a tdim=%a@ tlinexpr0=%a@]@ }"
    Jordan.printr jordan
    Matrix.Mpqf.print s_xy
    Matrix.Mpqf.print sinv_yx
    (DMappe.print Jordan.print_coeff pp_print_int)
    dmap_coeff_dim
    (Mappe.print pp_print_int (Print.list (fun fmt (s,i,j) -> fprintf fmt "(%b,%i,%i)" s i j)))
    map_dim_lsij
    (Mappe.print
       (Print.pair pp_print_int pp_print_int)
       (Print.pair pp_print_bool pp_print_int))
    map_ij_sdim
    (Print.array pp_print_int) tdim
    (Print.array (Apron.Linexpr0.print string_of_dim)) tlinexpr0

let print_guard fmt guard =
  Matrix.Mpqf2.print
    fmt
    guard

let print_uguard fmt uguard =
  Print.list
    ~sep:"@ or "
    print_guard
    fmt
    uguard

let print_guardtrans fmt
    {
      trans;
      tlinconsx;
      guardy
    }
    =
  let string_of_dim i = sprintf "x%i" i in
  Format.fprintf fmt
    "{@   @[<v>trans = %a@ tlinconsx = %a@ guardy = %a@]@ }"
    print_trans trans
    (Print.array (Apron.Lincons0.print string_of_dim)) tlinconsx
    print_guard guardy

let print_assigngen fmt { lvertex; lray; lline } =
  Format.fprintf fmt
    "{@   @[<v>lvertex = %a@ lray = %a@ lline = %a@]@ }"
    (print_list (Print.array (Apronaux.print_linexpr0 ~string_of_dim:Apronaux.string_of_dimx))) lvertex
    (print_list Matrix.Mpqf.print_float) lray
    (print_list Matrix.Mpqf.print_float) lline
let print_abstrans fmt { assigngen; cassigngen = guardassigngen } =
  Format.fprintf fmt
    "{@   @[<v>assigngen = %a;@ guardassigngen = %a@]@ }"
    print_assigngen assigngen
    print_assigngen guardassigngen

let poly_approximate ?(bdigits=0) (poly:'a Polka.t Apron.Abstract0.t)
    :
    'a Polka.t Apron.Abstract0.t
    =
  if bdigits>=1 then begin
    let man = Apron.Abstract0.manager poly in
    let intern = Polka.manager_get_internal man in
    Polka.set_approximate_max_coeff_size intern bdigits;
    let res = Apron.Abstract0.copy man poly in
    Apron.Abstract0.approximate man res !Option.approximate;
    if false then printf "approximate@.<= %a@.=> %a@."
      (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx) poly
      (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx) res;
    res
  end else begin
    poly
  end

let trans_of_jordan ~tdim ~tlinexpr0 ~s_xy ~(jordan:Jordan.real Jordan.t) ~sinv_yx
    :
    trans
    =
  let nbcoeffs = Jordan.nbcoeffs jordan in
  let dmap_coeff_dim = Jordan.dmap_coeff_dim jordan in
  let (map_dim_lsij,map_ij_sdim) = Jordan.maps_dim_ij jordan dmap_coeff_dim in
  {
    jordan;
    s_xy;
    sinv_yx;
    nbcoeffs;
    dmap_coeff_dim;
    map_dim_lsij;
    map_ij_sdim;
    tdim;
    tlinexpr0
  }

let guardtrans_of_trans_tlincons0
    (trans:trans)
    (tlinconsx:Apron.Lincons0.t array)
    :
    guardtrans
    =
  let nbdims = trans.jordan.Jordan.dim in
  let guardx = Matrix.Mpqf.of_tlincons_affine ~nbdims tlinconsx in
  (* guardy = guardx . sinv_yx *)
  let guardy = if guardx<>[||] then Matrix.Mpqf.gemm guardx trans.sinv_yx else [||] in
  let guardy = Matrix.interval_of_scalar guardy in
  { trans; tlinconsx; guardy }

let guard_negate (guard:guard) : uguard =
  let res = ref [] in
  Array.iter
    (begin fun row ->
      let nrow = Array.map Matrix.FMpqf2.neg row in
      res := [|nrow|] :: !res
     end)
    guard
  ;
  !res

let uguard_or (uguard1:uguard) (uguard2:uguard) : uguard =
  let rec walk res uguard2 =
    match uguard2 with
    | [] -> res
    | guard2::uguard2 ->
	let guard = List.rev_map (fun guard1 -> Array.append guard1 guard2) uguard1 in
	walk (List.rev_append guard res) uguard2
  in
  walk [] uguard2

let uguard_negate (uguard:uguard) : uguard =
  let cucons =
    List.rev_map
      guard_negate
      uguard
  in
  List.fold_left
    uguard_or
    (List.hd cucons) (List.tl cucons)

let assigngen_apply
    ~(trans:trans)
    man
    poly
    (assigngen:assigngen)
    =
  if true then printf "assigngen_apply poly:%a %a@ @?"
    (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx) poly
    print_assigngen assigngen
  ;
  if assigngen=assigngen_bottom then
    Apron.(
      let dim = Abstract0.dimension man poly in
      Abstract0.bottom man dim.Dim.intd dim.Dim.reald
    )
  else begin
    let { lvertex; lray; lline } = assigngen in
    let nbdims = trans.jordan.Jordan.dim in
    if true then printf "nbdims=%i@ @?" nbdims;
    let tdim = Array.init (nbdims-1) (fun i ->  i) in
    (* 1. compute image by vertices *)
    if true then printf "computing images by vertices: @?";
    let limages =
      List.rev_map
	(begin fun vertex ->
	  let res = Apron.Abstract0.assign_linexpr_array man poly tdim vertex None in
	  let res = poly_approximate ~bdigits:(!Option.nbbits) res in
	  if true then printf ".@?";
	  res
	 end)
	lvertex
    in
    if true then printf "done@.";
    let respoly = ref (List.hd limages) in
    if true then
      printf "limages=%a@ @?"
	(print_list (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx)) limages
    ;
    (* 2. add to respoly lines and rays *)
    if lline<>[] || lray<>[] then begin
      if true then printf "adding lines and rays@.";
      (* Extract the generators of polyx *)
      let tlingen0 = Apron.Abstract0.to_generator_array man poly in
      if false then
	printf "poly generators=%a@ @?"
	  (Print.array (Apron.Generator0.print (fun i -> sprintf "x%i" i))) tlingen0
      ;
      (* Apply to them line and ray matrices *)
      let res = ref [] in
      Apron.Generator0.(
	Array.iter
	  (begin fun lingen0 ->
	    let row = Matrix.Mpqf.row_of_linexpr_affine nbdims lingen0.linexpr0 in
	    row.(nbdims-1) <- Matrix.FMpqf.of_int 1;
	    if false then
	      printf "row=%a@ @?"
		(Print.array Matrix.FMpqf.print) row
	    ;
	    List.iter
	      (begin fun mat ->
		let nrow = Matrix.Mpqf.gemmv mat row in
		if false then
		  printf "line mat=%a@ nrow=%a@ @?"
		    Matrix.Mpqf.print_float mat
		    (Print.array Matrix.FMpqf.print) nrow
		;
		let linexpr0 = Matrix.Mpqf.row_to_linexpr_affine nrow in
		let typ = LINE in
		res := { typ; linexpr0 } :: !res
	       end)
	      lline
	    ;
	    List.iter
	      (begin fun mat ->
		let nrow = Matrix.Mpqf.gemmv mat row in
		if false then
		  printf "raymat=%a@ nrow=%a@ @?"
		    Matrix.Mpqf.print_float mat
		    (Print.array Matrix.FMpqf.print) nrow
		;
		let linexpr0 = Matrix.Mpqf.row_to_linexpr_affine nrow in
		let typ = if lingen0.typ = LINE then LINE else RAY in
		res := { typ; linexpr0 } :: !res
	       end)
	      lray
	   end)
	  tlingen0
      );
      if true then
	printf "generators to add=%a@ @?"
	  (print_list (Apronaux.print_generator0 ~string_of_dim:Apronaux.string_of_dimx))
	  !res
      ;
      (* Add transformed generators to respoly *)
      respoly := Apron.Abstract0.add_ray_array man !respoly (Array.of_list !res);
      if true then printf "done@.";
    end;
    (* 3. convex hull with images by other vertices *)
    if true then printf "computing convex hull @?";
    respoly := Apron.Abstract0.join_array man (Array.of_list (!respoly :: (List.tl limages)));
    if true then printf "done => %a@."
      (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx) !respoly;
    respoly := poly_approximate ~bdigits:(!Option.nbbitsu) !respoly;
    if true then printf "approx => %a@."
      (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx) !respoly;

    if true then
      printf "assigngen_apply => @."
    ;
    !respoly
  end

let guardassigngen_apply
    ~(trans:trans)
    man
    poly
    ~tlincons
    ~assigngen
    =
  let poly2 = Apron.Abstract0.meet_lincons_array man poly tlincons in
  let res = assigngen_apply ~trans man poly2 assigngen in
  res

let abstrans_apply
    man
    poly
    (abstrans:'a abstrans)
    =
  let trans = abstrans.guardtrans.trans in
  let tlincons = abstrans.guardtrans.tlinconsx in
(*
  let odest =
    if abstrans.cassigngen = abstrans.assigngen then
      None
    else
      let res =
	guardassigngen_apply ~trans man poly ~tlincons ~assigngen:abstrans.assigngen in
      Some res
  in
*)
  let res = guardassigngen_apply ~trans man poly ~tlincons ~assigngen:abstrans.cassigngen in
  if true then
    printf "assigngen_apply => %a@ @?"
      (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx) res
  ;
  let res2 = Apron.Abstract0.meet_lincons_array man res tlincons in
  if true then
    printf "assigngen_apply inter guard => %a, %a@ @?"
      (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx) res
      (Apronaux.print_abstract0_gen ~string_of_dim:Apronaux.string_of_dimx) res
  ;
  let res2 = Apron.Abstract0.assign_linexpr_array man res2 trans.tdim trans.tlinexpr0 None in
  if true then
    printf "apply_linexpr => %a, %a@ @?"
      (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx) res2
      (Apronaux.print_abstract0_gen ~string_of_dim:Apronaux.string_of_dimx) res2
  ;
  Apron.Abstract0.join man poly res2
