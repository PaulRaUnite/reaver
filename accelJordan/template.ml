
open Format
open Bigarray
open Jordan
open Trans

type matgen = Apron.Generator0.typ * Matrix.Mpqf.t

let print_matgen fmt (typ,mat) =
  fprintf fmt "%s: %a"
    (Apron.Generator0.string_of_typ typ)
    Matrix.Mpqf.print mat

(*  ********************************************************************** *)
(** {2 Generating list of template expressions} *)
(*  ********************************************************************** *)

let product_lmu_ls lmu ls =
  let res = ref [] in
  List.iter
    (begin fun mu ->
      List.iter
	(begin fun s ->
	  res := Bound.({ mu1=mu; mu2=1.-.mu; s })::(!res)
	 end)
	ls
     end)
    lmu
  ;
  !res

let lmu n =
  let nb = 1 lsl n in
  let res = ref [] in
  for i=nb-1 downto 1 do
    let mu = ldexp (float_of_int i) (-n) in
    res := mu :: !res
  done;
  !res

let lweight n =
  product_lmu_ls (lmu n) [-1.;1.]

let binary_make (coeff1,dim1) (coeff2,dim2) =
  `Binary Bound.(
    {
      mu1=abs_float coeff1;
      mu2=abs_float coeff2;
      s=if coeff1*.coeff2>0. then 1. else -1.
    },
    dim1,dim2
  )

let lbinary_of_lweight ~nbcoeffs (lweight:Bound.weight list) =
  let res = ref [] in
  for i2=nbcoeffs-2 downto 1 do
    for i1=i2-1 downto 0 do
      List.iter
	(fun t -> res := `Binary(t,i1,i2) :: !res)
	lweight
    done;
  done;
  !res

let lunary_make ~nbcoeffs =
  let res = ref [] in
  for i=nbcoeffs-2 downto 0 do
    res := `Unary(i):: !res
  done;
  !res

let ltemplate ~nbcoeffs n : Bound.expr list =
  let lweight = lweight n in
  let lunary = lunary_make ~nbcoeffs in
  let lbinaryj = lbinary_of_lweight ~nbcoeffs lweight in
  let res = List.rev_append lunary lbinaryj in
  if true then printf "%i coeffs, %i template constraints@." nbcoeffs (List.length res);
  res

(*  ********************************************************************** *)
(** {2 Converting template constraints to APRON constraints} *)
(*  ********************************************************************** *)

let lincons0_of_exprbound expr typ bound =
  Apron.(Lincons0.(
    let linexpr0_of_expr_l expr l =
      let linexpr0 = Linexpr0.make None in
      begin match expr with
	| `Unary(dim) ->
	    Linexpr0.set_list linexpr0
	      [((Coeff.s_of_int 1),dim)]
	      (Some (Coeff.s_of_float (-.l)))
	| `Binary(t,dim1,dim2) ->
	    Linexpr0.set_list linexpr0
	      [
		((Coeff.s_of_float t.Bound.mu1),dim1);
		((Coeff.s_of_float (t.Bound.s*.t.Bound.mu2)),dim2);
	      ]
	      (Some (Coeff.s_of_float (-.l)))
	end;
	linexpr0
    in
    let linexpr0_of_expr_u expr u =
      let linexpr0 = Linexpr0.make None in
      begin match expr with
      | `Unary(dim) ->
	  Linexpr0.set_list linexpr0
	    [((Coeff.s_of_int (-1)),dim)]
	    (Some (Coeff.s_of_float u))
      | `Binary(t,dim1,dim2) ->
	  Linexpr0.set_list linexpr0
	    [
	      ((Coeff.s_of_float (-.t.Bound.mu1)),dim1);
	      ((Coeff.s_of_float (-.t.Bound.s*.t.Bound.mu2)),dim2);
	    ]
	    (Some (Coeff.s_of_float u))
      end;
      linexpr0
    in
    match typ with
    | `E ->
	{ linexpr0 = linexpr0_of_expr_l expr bound;
	  typ = EQ }
    | `L ->
	{ linexpr0 = linexpr0_of_expr_l expr bound;
	  typ = SUPEQ }
    | `U ->
	{ linexpr0 = linexpr0_of_expr_u expr bound;
	  typ = SUPEQ }
  ))

let tlincons0_of_lexprbound lexprbound =
  let llincons0 = ref [] in
  List.iter
    (begin fun (expr,(l,u)) ->
      if l=u then begin
	llincons0 := (lincons0_of_exprbound expr `E l) :: !llincons0
      end else begin
	let (l,u) = Bound.Int.round (l,u) in
	if l<>Bound.neginf then begin
	  llincons0 := (lincons0_of_exprbound expr `L l) :: !llincons0
	end;
	if u<>Bound.inf then begin
	  llincons0 := (lincons0_of_exprbound expr `U u) :: !llincons0
	end
      end
     end)
    lexprbound
  ;
  Array.of_list !llincons0

(*  ********************************************************************** *)
(** {2 Constraining polyhedron on Jordan coefficients with guard(s)} *)
(*  ********************************************************************** *)

(* We assume M=S.jordan.Sinv, or jordan=Sinv.M.S *)
let matlincons_of_poly_guardtrans
    ~guardtrans
    ~apron
    ~poly
    :
    Matrix.Mpqf2.t
    =
    Apron.(
      let { trans; guardy } = guardtrans in
      let { jordan; nbcoeffs; map_ij_sdim; s_xy; _ } = trans in
      let nbdims = jordan.Jordan.dim in
      (* polyy = s_xy. polyx *)
      let polyx =
	Abstract0.add_dimensions apron poly
	  Dim.({ dim=[|nbdims-1|]; intdim=0; realdim=1 }) true
      in
      if false then printf "polyx=%a@ " (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx) polyx;
      let tdim = Array.init nbdims (fun i -> i) in
      let tlinexpr0 = Array.map Matrix.Mpqf.row_to_linexpr_affine s_xy in
      if false then
	printf "tdim=%a@ tlinexpr0=%a@ "
	  (Print.array pp_print_int) tdim
	  (Print.array ((Apronaux.print_linexpr0 ~string_of_dim:Apronaux.string_of_dimx)))
	  tlinexpr0
      ;
      let polyy = Abstract0.assign_linexpr_array apron polyx tdim tlinexpr0 None in
      if false then
	printf "polyy=%a@ "
	  (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimy) polyy;
      let boxy = Abstract0.to_box apron polyy in
      let boxy = Array.map Matrix.FMpqf2.of_interval boxy in
      if false then
	printf "boxy=%a@ " (Print.array Matrix.FMpqf2.print_float) boxy ;
      (* we need to generate the constraints uguardy . J . [minY, maxY] >= 0 *)
      (* 1. we generate J . [minY, maxY] >= 0 *)
      let mat = Matrix.Mpqf2.make0 nbdims nbcoeffs in
      for i=0 to pred nbdims do
	for j=0 to pred nbdims do
	  try
	    let (s,dim) = Mappe.find (i,j) map_ij_sdim in
	    mat.(i).(dim) <- if s then boxy.(j) else Matrix.FMpqf2.neg boxy.(j)
	  with Not_found ->
	    ()
	done;
      done;
      if false then printf "J.minmaxY=%a@ "
	Matrix.Mpqf2.print_float mat
      ;
      (* 2. We multiply with uguardy *)
      if false then printf "guardy=%a@ "
	Matrix.Mpqf2.print_float guardy
      ;
      let mat = if guardy<>[||] then Matrix.Mpqf2.gemm guardy mat else [||] in
      if false then printf "mat=guardy.J.minmaxY=%a@ "
	Matrix.Mpqf2.print_float mat
      ;
      mat
    )


let polyjordan_meet_with_matlincons
    ~apron
    ~polyjordan
    ~mat
    =
  Apron.(
    let tlincons0 = Matrix.Mpqf2.to_tlincons_affine mat in
    if false then printf "tlincons0=%a@ "
    (* (Print.array (Apronaux.print_lincons0 ~string_of_dim:Apronaux.string_of_dimj)) tlincons0*)
      (Print.array (Lincons0.print Apronaux.string_of_dimj)) tlincons0
    ;
    let npolyjordan = Abstract0.meet_lincons_array apron polyjordan tlincons0 in
    if false then
      printf "npolyjordan=%a@ "
	(Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimj) npolyjordan
    ;
    npolyjordan
  )

let lincons0_of_dim_sign dim sign =
  Apron.(
    let linexpr0 = Linexpr0.make None in
    Linexpr0.set_coeff linexpr0 dim (Coeff.s_of_int (if sign then 1 else -1));
    Lincons0.make linexpr0 Lincons0.SUPEQ
  )

exception Identity

let polyjordan_meet_with_matlincons_quadrant
    ~apron
    ~polyjordan
    ~mat
    =
  Apron.(
    let nbrows = Matrix.dim1 mat in
    let nbdims = pred (Matrix.dim2 mat) in
    let identity = ref false in
    let i = ref 0 and halt = ref false in
    let rec parcours res dim polyjordan2 mat =
      assert (not (Abstract0.is_bottom apron polyjordan));
      if false then begin
	printf "parcours dim=%i polyjordan2=%a@ "
	  dim
	  (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimj) polyjordan2
      end;
      if dim=nbdims then begin
	(* terminal case *)
	let npolyjordan = polyjordan_meet_with_matlincons ~apron ~polyjordan:polyjordan2 ~mat in
	if false then begin
	  printf "npolyjordan=%a@ "
	    (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimj) npolyjordan
	  ;
	end;
	if Abstract0.is_bottom apron npolyjordan then
	  res
	else if Abstract0.is_eq apron npolyjordan polyjordan then begin
	  raise Identity (* identity := true; res *)
	end
	else
	  npolyjordan::res
      end
      else begin
	i := 0; halt := false;
	while not !halt && !i < nbrows do
	  if Matrix.FMpqf2.is_scalar mat.(!i).(dim) then incr i else halt:=true;
	done;
	if not !halt then
	  parcours res (dim+1) polyjordan2 mat
	else begin
	  let res =
	    let polyjordanp =
	      Abstract0.meet_lincons_array apron polyjordan2 [|lincons0_of_dim_sign dim true|]
	    in
	    if Abstract0.is_bottom apron polyjordanp then res
	    else
	      let matp = Matrix.mapij
		(fun i1 dim1 ((x,y) as coeff) ->
		  if dim1=dim && i1>= !i && Matrix.FMpqf.to_float y < 1./.0. then (y,y) else coeff)
		mat
	      in
	      parcours res (dim+1) polyjordanp matp
	  in
	  let res =
	    let polyjordann =
	      Abstract0.meet_lincons_array apron polyjordan2 [|lincons0_of_dim_sign dim false|]
	    in
	    if Abstract0.is_bottom apron polyjordann then res
	    else
	      let matn = Matrix.mapij
		(fun i1 dim1 ((x,y) as coeff) ->
		  if dim1=dim && i1>= !i && Matrix.FMpqf.to_float x > -1./.0. then (x,x) else coeff)
		mat
	      in
	      parcours res (dim+1) polyjordann matn
	  in
	  res
	end
      end
    in
    let res =
      try
	let res = parcours [] 0 polyjordan mat in
	if res=[] then None else Some res
      with Identity ->
	Some[]
    in
    res
  )

let polyjordan_meet_with_poly_guardtrans
    ~guardtrans
    ~apron
    ~polyjordan
    ~poly
    :
    'a Apron.Abstract0.t list option
    =
  if false then
    printf "polyjordan=%a@ "
      (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimj) polyjordan
  ;
  let mat = matlincons_of_poly_guardtrans ~guardtrans ~apron ~poly in
  let res =
    if !Option.quadrant then
      polyjordan_meet_with_matlincons_quadrant ~apron ~polyjordan ~mat
    else
      let res = polyjordan_meet_with_matlincons ~apron ~polyjordan ~mat in
      if Apron.Abstract0.is_bottom apron res then None
      else if Apron.Abstract0.is_eq apron polyjordan res then Some []
      else Some [res]
  in
  if false then begin
    printf "res=%a@ "
      (Print.option (Print.list (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimj)))
      res
  end;
  res

(*  ********************************************************************** *)
(** {2 Computing polyhedron on Jordan coefficients} *)
(*  ********************************************************************** *)

(*  ********************************************************************** *)
(** {2 Computing generator matrices} *)
(*  ********************************************************************** *)

let matgen_of_lingeny ~nbdims ~map_dim_lsij lingen0 : matgen =
  Apron.(Generator0.(
    if false then printf "matgen_of_lingeny nbdims=%i lingen0=%a@ "
      nbdims (Apronaux.print_generator0 ~string_of_dim:Apronaux.string_of_dimj) lingen0
    ;
    let mat =
      if lingen0.typ=VERTEX
      then Matrix.Mpqf.identity nbdims
      else Matrix.Mpqf.make0 nbdims nbdims
    in
    Linexpr0.iter
      (begin fun coeff dim ->
	let coeff = Matrix.FMpqf.of_coeff coeff in
	let lsij = Mappe.find dim map_dim_lsij in
	List.iter
	  (begin fun (s,i,j) ->
	    mat.(i).(j) <- if s then coeff else Matrix.FMpqf.neg coeff
	   end)
	  lsij
       end)
      lingen0.linexpr0;
    if false then printf "matgen=%a@ "
      Matrix.Mpqf.print mat
    ;
    (lingen0.typ,mat)
  ))

(* We assume M=S.jordan.Sinv, or jordan=Sinv.M.S *)
let assigngenx_of_polyjordany
    ~(trans:Trans.trans)
    ~apron
    poly
    :
    Trans.assigngen
    =
  let { jordan; nbcoeffs; map_dim_lsij; s_xy; sinv_yx; _ } = trans in
  let nbdims = jordan.Jordan.dim in
  if false then printf "jordany = %a@ %a@ "
    (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimj) poly
    (Apronaux.print_abstract0_gen ~string_of_dim:Apronaux.string_of_dimj) poly
  ;
  let tlingen0 = Apron.Abstract0.to_generator_array apron poly in
  let vertex = ref [] and ray = ref [] and line = ref [] in
  Apron.Generator0.(
    Array.iter
      (begin fun lingen0 ->
	let (typ,mat) = matgen_of_lingeny ~nbdims ~map_dim_lsij lingen0 in
	let mat = Matrix.Mpqf.gemm sinv_yx (Matrix.Mpqf.gemm mat s_xy) in
	match typ with
	| VERTEX ->
	    let mat = Array.sub mat 0 (nbdims-1) in
	    vertex := (Matrix.Mpqf.to_tlinexpr_affine mat) :: !vertex
	| RAY -> ray := mat :: !ray
	| LINE -> line := mat :: !line
	| _ -> failwith ""
       end)
      tlingen0
  )
  ;
  let res = { lvertex = !vertex; lray = !ray; lline = !line } in
  if false then
    printf "assigngen = %a@ " Trans.print_assigngen res
  ;
  res



(*  ********************************************************************** *)
(** {2 Abstracting with a polyhedron consecutive powers of a Jordan matrix} *)
(*  ********************************************************************** *)

let abstract
    ~(apron:'a Apron.Manager.t)
    ~(trans:trans)
    ~nmin ~nmax
    ltemplate
    :
    'a Apron.Abstract0.t
    =
  if true then printf "Template.abstract <= nmin=%i, nmax=%i@." nmin nmax;
  let { nbcoeffs; dmap_coeff_dim; sinv_yx; _ } = trans in
  let lexprbound = Bound.bound_lexpr ~dmap_coeff_dim ~nmin ~nmax ltemplate in
  if true then
    printf "lexprbound = %a@ "
      (Print.list (Print.pair Bound.print_expr Bound.Int.print))
      lexprbound
  ;
  let tlincons0 = tlincons0_of_lexprbound lexprbound in
  let polyjordan = Apron.Abstract0.of_lincons_array apron 0 (nbcoeffs-1) tlincons0 in
  if true then
    printf "polyjordan=%a@ " (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimj) polyjordan
  ;
  if true then printf "Template.abstract =>@.";
  polyjordan

let abstrans_of_ltemplate
    ~guardtrans
    ~apron
    ltemplate
    =
  let polyjordan = abstract ~apron ~trans:guardtrans.trans ~nmin:0 ~nmax:max_int ltemplate in
  let assigngen = assigngenx_of_polyjordany ~trans:guardtrans.Trans.trans ~apron polyjordan in
  let guardassigngen = assigngen in
  { guardtrans; ltemplate; polyjordan; assigngen; cassigngen = guardassigngen }

let refine_abstrans_with_poly_guard
    ~apron
    ~abstrans
    ~poly
    :
    unit
    =
  if true then printf "Template.refine_with_poly_guard <= poly=%a@."
    (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimj) poly
  ;
  let guardtrans = abstrans.guardtrans in
  if guardtrans.tlinconsx=[||] then
    abstrans.cassigngen <- abstrans.assigngen
  else begin
    let olpolyjordan2 =
      polyjordan_meet_with_poly_guardtrans
	~guardtrans ~apron ~polyjordan:abstrans.polyjordan ~poly
    in
    match olpolyjordan2 with
    | None ->
      abstrans.cassigngen <- assigngen_bottom
    | Some [] ->
      abstrans.cassigngen <- abstrans.assigngen
    | Some lpolyjordan2 ->
      if true then printf "computing n@.";
      let n =
	begin match lpolyjordan2 with
	| [polyjordan2] ->
	  Counting.main ~apron ~trans:guardtrans.trans ~polyjordan:polyjordan2 ~first:0
	    abstrans.ltemplate
	| _ ->
	  let map_ipolyjordan2 = ref Mappe.empty in
	  List.fold_left
	    (begin fun i x ->
	      map_ipolyjordan2 := Mappe.add i x !map_ipolyjordan2;
	      i+1
	     end)
	    0 lpolyjordan2
	  ;
	  let first = ref (-1) and n = ref 0 in
	  while !first < !n && !n < max_int do
	    first := !n;
	    if true then printf "first=%i@ " !first;
	    if !map_ipolyjordan2=Mappe.empty then
	      n := max_int
	    else begin
	      let (n2,i2) =
		Mappe.fold
		  (begin fun i polyjordan2 (n2,i2)->
		    if false then printf "count i=%i polyjordan2=%a@."
		      i
		      (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimj) polyjordan2
		    ;
		    let npoly =
		      Counting.main ~apron ~trans:guardtrans.trans ~polyjordan:polyjordan2 ~first:(!first)
			abstrans.ltemplate
		    in
		    if false then printf "npoly=%i@ " npoly;
		    if npoly > n2 then
		      (npoly,i)
		    else
		      (n2,i2)
		   end)
		  !map_ipolyjordan2 (-1,-1)
	      in
	      n := n2;
	      map_ipolyjordan2 := Mappe.remove i2 !map_ipolyjordan2;
	      if false then printf "n=%i, i2=%i@ " !n i2;
	    end;
	  done;
	  !n
	end
      in
      if true then
	printf "done, nfinal=%i@." n;
      if n=max_int then
	abstrans.cassigngen <- abstrans.assigngen
      else if n=0 then
	abstrans.cassigngen <- assigngen_bottom
      else begin
	assert (if n>=1 then true else (printf "n=%i@." n; false));
	if true then printf "reabstracting @?";
	let polyjordan =
	  abstract ~apron ~trans:guardtrans.trans ~nmin:0 ~nmax:(n-1) abstrans.ltemplate
	in
	if true then printf "done => polyjordan=%a@."
	  (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimj) polyjordan;
	if true then printf "joining lpolyjordan2(size %i) @?" (List.length lpolyjordan2);
	let upolyjordan2 = Apron.Abstract0.join_array apron (Array.of_list lpolyjordan2) in
	if true then printf "done => %a@.meet polyjordan upolyjordan2 @?"
	  (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimj) upolyjordan2;
	Apron.Abstract0.meet_with apron polyjordan upolyjordan2;
	if true then printf "done@.";
	abstrans.cassigngen <-
	  assigngenx_of_polyjordany ~trans:guardtrans.Trans.trans ~apron polyjordan
      end;
      if true then printf "Template.refine_with_poly_guard =>@.";
  end


(*
let lincons0_of_aproncons
    env
    aproncons
    =
  let symbol = env.Bdd.Env.symbol in
  let apronenv = Bddapron.Env.apron env in
  let apron0 = Bddapron.Apronexpr.Condition.to_apron0 symbol apronenv aproncons in
  begin match apron0 with
  | `Lin lincons0 -> lincons0
  | `Tree _ -> failwith ""
  end

let tlincons0_of_laproncons
    env
    laproncons
    =
  let llincons0 =
    Apron.(Lincons0.(
      List.fold_left
	(begin fun res aproncons ->
	  let lincons0 = lincons0_of_aproncons env aproncons in
	  match lincons0.constyp with
	  | SUPEQ
	  | SUP -> lincons0::res
	  | EQ ->
	      let nlinexpr0 = Linexpr0.make None in
	      Linexpr0.iter
		(begin fun (coeff,dim) ->
		  Linexpr0.set nlinexpr0 dim (Coeff.neg coeff)
		 end)
		lincons0.linexpr0
	      ;
	      { constyp=SUPEQ; linexpr0=nlinexpr0 } ::
		{ constyp = SUPEQ; linexpr0=lincons0.linexpr0 } ::
		!llincons0
	  | _ -> assert(false);
	 end)
	[] laproncons
    ))
  in
  Array.of_list llincons0
*)
