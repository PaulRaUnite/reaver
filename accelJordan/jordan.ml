
open Format
open Bigarray

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

let print_real fmt = function
  | `Real x -> fprintf fmt "%G" x
  | `Complex(c,x,y) -> fprintf fmt "Complex(%G,%Gi)" c.Complex.re c.Complex.im
let print_coeff fmt (coeff,k) =
  match coeff with
  | `Real x -> fprintf fmt "(Real(%G),%i)" x k
  | `Sin(x,y,r) -> fprintf fmt "(Sin(%G,%G,%i),%i)" x y r k

let print_block printa fmt { eigenvalue; lk; _ } =
  fprintf fmt "{ eigen=%a; lk=%a }"
    printa eigenvalue
    (Print.list pp_print_int) lk
let print printa fmt jordan =
  Print.list
    (print_block printa)
    fmt jordan.lblock
let print_cblock fmt x = print_block (fun fmt c -> fprintf fmt "(%G,%Gi)" c.Complex.re c.Complex.im) fmt x
let printc fmt x = print (fun fmt c -> fprintf fmt "(%G,%Gi)" c.Complex.re c.Complex.im) fmt x
let print_rblock fmt x = print_block print_real fmt x
let printr fmt x =  print print_real fmt x

let real_one = `Real 1.0
let coeff_one = (real_one,0)

let list_max l = List.fold_left max min_int l

let nbcoeffs jordan =
  List.fold_left
    (fun res block ->
      let maxk = list_max block.lk in
      match block.eigenvalue with
      | `Real _ -> res + maxk
      | `Complex _ -> res + 2*maxk
    )
    0
    jordan.lblock

let mat_F_of_C matZ =
  Matrix.map
    (fun c ->
      assert(true || c.Complex.im=0.0);
      c.Complex.re)
    matZ

let mat_C_of_F matD =
  Matrix.map
    (fun r ->
      { Complex.re = r; Complex.im = 0. })
    matD

let mat_C_of_cjordan jordan =
  let mat = Matrix.C.make0 jordan.dim jordan.dim in
  let i = ref 0 in
  List.iter
    (begin fun block ->
      List.iter
	(begin fun size ->
	  for j=0 to size-1 do
	    mat.(!i).(!i) <- block.eigenvalue;
	    if j<size-1 then
	      mat.(!i).(!i+1) <- Complex.one;
	    incr i;
	  done;
	 end)
	block.lk
     end)
    jordan.lblock;
  mat

let mat_F_of_rjordan jordan =
  let mat = Matrix.F.make0 jordan.dim jordan.dim in
  let i = ref 0 in
  List.iter
    (begin fun block ->
      match block.eigenvalue with
      | `Real x ->
	  List.iter
	    (begin fun size ->
	      for j=0 to size-1 do
		mat.(!i).(!i) <- x;
		if j<size-1 then
		  mat.(!i).(!i+1) <- 1.;
		incr i;
	      done;
	     end)
	    block.lk
      | `Complex(c,_,_) ->
	  List.iter
	    (begin fun size ->
	      for j=0 to size-1 do
		mat.(!i).(!i) <- c.Complex.re;
		mat.(!i+1).(!i+1) <- c.Complex.re;
		mat.(!i).(!i+1) <- (-.c.Complex.im);
		mat.(!i+1).(!i) <- c.Complex.im;
		if j<size-1 then begin
		  mat.(!i).(!i+2) <- 1.;
		  mat.(!i+1).(!i+3) <- 1.;
		end;
		i := !i + 2;
	      done;
	     end)
	    block.lk
     end)
    jordan.lblock;
  mat

let block_of_cblock2 cblock1 cblock2 =
  let open Complex in
      assert(cblock1.eigenvalue.im<>0.);
      assert(cblock1.eigenvalue = conj cblock2.eigenvalue);
      assert(cblock1.lk=cblock2.lk);
      let (cblock1,cblock2,s) =
	if cblock1.eigenvalue.im<0. then
	  (cblock1,cblock2,1.)
	else
	  (cblock2,cblock1,-1.)
      in
      let norm = Complex.norm cblock2.eigenvalue in
      let angle = Complex.arg cblock2.eigenvalue in
      let block = {
	cblock2 with
	  eigenvalue = `Complex(cblock2.eigenvalue,norm,angle)
      }
      in
      let nbdims = block.sumk in
      (* Fills t with the matrix
	 (
	  (1 i ...    )
	  (... 1 i ...)
	  ...
	  (1 -i ....    )
	  (.... 1 -i    )
	  ...            )
      *)
      let t = Matrix.C.make0 (2*nbdims) (2*nbdims) in
      for p=0 to nbdims-1 do
	t.(p).(2*p+1) <- { re=0.0; im= -.s };
	t.(p).(2*p) <- Complex.one;
      done;
      for p=0 to nbdims-1 do
	t.(nbdims+p).(2*p+1) <- { re=0.; im= s };
	t.(nbdims+p).(2*p) <- Complex.one
      done;
      (block,t)

let conjugate2 mat =
  Matrix.maptranspose
    (fun x -> Complex.({ re = x.re/.2.; im = -.x.im/.2. }))
    mat

let real_of_complex
    ~(sinv_yx:Matrix.C.t)
    ~(jordany:Complex.t t)
    ~(s_xy:Matrix.C.t)
    :
    Matrix.F.t * real t * Matrix.F.t
    =
  let rec parcours (ltinv_yx,lblock,lt_xy) = function
    | [] -> (ltinv_yx,lblock,lt_xy)
    | [cblock] ->
	assert(cblock.eigenvalue.Complex.im=0.);
	let m = Matrix.C.identity cblock.sumk in
	(
	  m::ltinv_yx,
	  {cblock with eigenvalue = `Real cblock.eigenvalue.Complex.re }::lblock,
	  m::lt_xy
	)
    | cblock1::((cblock2::l2) as l1) ->
	if (cblock1.eigenvalue.Complex.im<>0.) then
	  if cblock1.eigenvalue = Complex.conj cblock2.eigenvalue then
	    let (block,tinv_yx) = block_of_cblock2 cblock1 cblock2 in
	    let t_xy = conjugate2 tinv_yx in
	    let nres = (tinv_yx::ltinv_yx, block::lblock, t_xy::lt_xy) in
	    parcours nres l2
	  else
	    assert false
	else
	  let m = Matrix.C.identity cblock1.sumk in
	  let nres = (
	    m::ltinv_yx,
	    {cblock1 with eigenvalue = `Real cblock1.eigenvalue.Complex.re }::lblock,
	    m::lt_xy
	  )
	  in
	  parcours nres l1
  in
  let (ltinv_yx,lblock,lt_xy) = parcours ([],[],[]) jordany.lblock in
  let ltinv_yx = List.rev ltinv_yx in
  let lblock = List.rev lblock in
  let lt_xy = List.rev lt_xy in
  if true then begin
    printf "sinv_yx=%a@ s_xy=%a@ "
      Matrix.C.print sinv_yx
      Matrix.C.print s_xy
    ;
    printf "jordany.lblock=%a@ lblock=%a@ lt_xy=%a@ ltinv_yx=%a@ "
      (Print.list print_cblock) jordany.lblock
      (Print.list print_rblock) lblock
      (Print.list Matrix.C.print) lt_xy
      (Print.list Matrix.C.print) ltinv_yx
  end;
  let njordany = { jordany with lblock = lblock } in
  let tinv_yx = Matrix.C.diag ltinv_yx in
  let t_xy = Matrix.C.diag lt_xy in
  if true then
    printf "tinv_yx=%a@ t_xy=%a@ "
      Matrix.C.print tinv_yx
      Matrix.C.print t_xy
  ;
  let nsinv_yx = Matrix.C.gemm sinv_yx tinv_yx in
  let ns_xy = Matrix.C.gemm t_xy s_xy in
  if true then begin
    let j=mat_C_of_cjordan jordany in
    let jr=Matrix.C.gemm t_xy (Matrix.C.gemm j tinv_yx) in
    printf "j=%a@ "
      Matrix.C.print j
    ;
    printf "(sinv_yx j s_xy)=%a@ "
      Matrix.C.print (Matrix.C.gemm sinv_yx (Matrix.C.gemm j s_xy))
    ;
    printf "jr:%a@ = (t_xy j tinv_yx):%a@ "
      Matrix.C.print jr
      Matrix.C.print (Matrix.C.gemm t_xy (Matrix.C.gemm j tinv_yx))
    ;
    printf "(sinv_yx tinv_yx jr t_xy s_xy)=%a@ "
      Matrix.C.print (Matrix.C.gemm (Matrix.C.gemm sinv_yx tinv_yx) (Matrix.C.gemm jr (Matrix.C.gemm t_xy s_xy)));
    printf "nsinv_yx=%a@ ns_xy=%a@ "
      Matrix.C.print nsinv_yx
      Matrix.C.print ns_xy
    ;
    printf "(nsinv_yx jr s_xy)=%a@ "
      Matrix.C.print (Matrix.C.gemm nsinv_yx (Matrix.C.gemm jr ns_xy));
  end;

  let nsinv_yx = mat_F_of_C nsinv_yx in
  let ns_xy = mat_F_of_C ns_xy in
  (nsinv_yx, njordany, ns_xy)

let block_of_cblock2_sage cblock1 cblock2 =
  let open Complex in
      assert(cblock1.eigenvalue.im<>0.);
      assert(cblock1.eigenvalue = conj cblock2.eigenvalue);
      assert(cblock1.lk=cblock2.lk);
      assert(cblock1.eigenvalue.im<0.);
      let norm = Complex.norm cblock2.eigenvalue in
      let angle = Complex.arg cblock2.eigenvalue in
      { cblock2 with
	eigenvalue = `Complex(cblock2.eigenvalue,norm,angle)
      }

let real_of_complexsage
    ~(sinv_yx:Matrix.F.t)
    ~(jordany:Complex.t t)
    ~(s_xy:Matrix.F.t)
    :
    real t
    =
  let rec parcours lblock = function
  | [] -> lblock
  | [cblock] ->
    assert(cblock.eigenvalue.Complex.im=0.);
    {cblock with eigenvalue = `Real cblock.eigenvalue.Complex.re }::lblock
  | cblock1::((cblock2::l2) as l1) ->
    if (cblock1.eigenvalue.Complex.im<>0.) then
      if cblock1.eigenvalue = Complex.conj cblock2.eigenvalue then
	let block = block_of_cblock2_sage cblock1 cblock2 in
	parcours (block::lblock) l2
      else
	assert false
    else
      parcours ({cblock1 with eigenvalue = `Real cblock1.eigenvalue.Complex.re }::lblock) l1
  in
  let lblock = parcours [] jordany.lblock in
  let lblock = List.rev lblock in
  if true then begin
    printf "sinv_yx=%a@ s_xy=%a@ "
      Matrix.F.print sinv_yx
      Matrix.F.print s_xy
    ;
    printf "jordany.lblock=%a@ lblock=%a@ "
      (Print.list print_cblock) jordany.lblock
      (Print.list print_rblock) lblock
  end;
  let njordany = { jordany with lblock = lblock } in
  njordany

let dmap_coeff_dim (jordan:real t) : (coeff,int) DMappe.t =
  let nbcoeffs = nbcoeffs jordan in
  let dmap = ref (DMappe.add coeff_one (nbcoeffs-1) DMappe.empty) in
  let dim = ref 0 in
  List.iter
    (begin fun block ->
      let maxk = list_max block.lk in
      match block.eigenvalue with
      | `Real _ as lambda ->
	  for k=0 to maxk-1 do
	    if not (k=0 && lambda=real_one) then begin
	      dmap := DMappe.add (lambda,k) !dim !dmap;
	      incr dim;
	    end
	  done
      | `Complex(c,norm,angle) ->
	  for k=0 to maxk-1 do
	    dmap := DMappe.add (`Sin(norm,angle,1),k) !dim !dmap;
	    incr dim;
	    dmap := DMappe.add (`Sin(norm,angle,0),k) !dim !dmap;
	    incr dim;
	  done
     end)
    jordan.lblock
  ;
  if false then
    printf "dmap_coeff_dim=%a@ "
      (DMappe.print print_coeff pp_print_int) !dmap
  ;
  !dmap

let maps_dim_ij (jordan:real t) (dmap_coeff_dim:(coeff,int) DMappe.t) =
  let map_dim_lsij = ref Mappe.empty in
  let map_ij_sdim = ref Mappe.empty in
  let imat = ref 0 in
  List.iter
    (begin fun block ->
      let maxk = list_max block.lk in
      match block.eigenvalue with
      | `Real _ as lambda ->
	  for k=0 to maxk-1 do
	    let dim = DMappe.y_of_x (lambda,k) dmap_coeff_dim in
	    let rlist = ref [] in
	    let iblock = ref 0 in
	    List.iter
	      (begin fun order ->
		if k<order then begin
		  for i=0 to order-1-k do
		    let row = !imat + !iblock + i in
		    rlist := (true,row,row+k)::(!rlist);
		    map_ij_sdim := Mappe.add (row,row+k) (true,dim) !map_ij_sdim;
		  done;
		end;
		iblock := !iblock + order
	       end)
	      block.lk
	    ;
	    map_dim_lsij := Mappe.add dim !rlist !map_dim_lsij
	  done;
	  imat := !imat + block.sumk
      | `Complex(c,norm,angle) ->
	  for k=0 to maxk-1 do
	    let dimcos = DMappe.y_of_x (`Sin(norm,angle,1),k) dmap_coeff_dim in
	    let dimsin = DMappe.y_of_x (`Sin(norm,angle,0),k) dmap_coeff_dim in
	    let rlistcos = ref [] and rlistsin = ref [] in
	    let iblock = ref 0 in
	    List.iter
	      (begin fun order ->
		if k<order then begin
		  for i=0 to order-1-k do
		    let row = !imat + !iblock + 2*i in
		    rlistcos := (true,row,row+2*k)::(!rlistcos);
		    rlistcos := (true,row+1,row+2*k+1)::(!rlistcos);
		    rlistsin := (false,row,row+2*k+1)::(!rlistsin);
		    rlistsin := (true,row+1,row+2*k)::(!rlistsin);
		    map_ij_sdim := Mappe.add (row,row+2*k) (true,dimcos) !map_ij_sdim;
		    map_ij_sdim := Mappe.add (row+1,row+2*k+1) (true,dimcos) !map_ij_sdim;
		    map_ij_sdim := Mappe.add (row,row+2*k+1) (false,dimsin) !map_ij_sdim;
		    map_ij_sdim := Mappe.add (row+1,row+2*k) (true,dimsin) !map_ij_sdim;
		  done;
		end;
		iblock := !iblock + 2*order
	       end)
	      block.lk
	    ;
	    map_dim_lsij := Mappe.add dimcos !rlistcos !map_dim_lsij;
	    map_dim_lsij := Mappe.add dimsin !rlistsin !map_dim_lsij;
	  done;
	  imat := !imat + 2*block.sumk
     end)
    jordan.lblock
  ;
  (!map_dim_lsij,!map_ij_sdim)
