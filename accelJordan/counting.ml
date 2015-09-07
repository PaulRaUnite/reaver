open Format
open Bound
open Trans

let newton ~epsilon ~t0 ~(f:float -> float) ~(fp:float -> float) ~(v:float) =
  let t = ref t0 in
  let ft = ref ((f t0) -. v) in
  let aeps =
    let av = abs_float v in
    if av <= ldexp 1. (-40) then
      epsilon
    else
      epsilon *. av
  in
  if false then printf "newton epsilon=%.5G, t=%.5G, ft=%.5G, fpt=%.5G@." epsilon !t !ft (fp t0);
  while (abs_float !ft) >= aeps do
    t := !t -. ((f !t) -. v) /. (fp !t);
    ft := (f !t) -. v;
    if false then printf "t=%.5G, ft=%.5G, fpt=%.5G@." !t !ft (fp !t);
  done;
  if false then printf "newton t=%.5G@." !t;
  !t

let eval_jordan_ith ~trans i =
  if false then printf "Counting.eval_jordan_ith %i @." i;
  let nbdims = trans.nbcoeffs-1 in
  let vertex = Array.make nbdims 0. in
  DMappe.iter
    (begin fun coeff dim ->
      if false then printf "coeff=%a dim=%i @?"
	Jordan.print_coeff coeff dim
      ;
      if dim<nbdims then
	let (c,k) = coeff in
	let v =
	  match c with
	  | `Real lambda ->
	      Bound.Real.single_int ~lk:(lambda,k) i
	  | `Sin(lambda,theta,r) ->
	      Bound.Complex.single_int ~ltrk:(lambda,theta,r,k) i
	in
	if false then printf "%.5G@." v;
	vertex.(dim) <- v;
     end)
    trans.dmap_coeff_dim
  ;
  if false then printf "@.Counting.eval_jordan_ith %i => %a@."
    i (Print.array (fun fmt x -> fprintf fmt "%.5G" x)) vertex
  ;
  vertex

let poly_of_vertex ~apron vertex =
  Apron.(
    let tlincons0 =
      Array.mapi
	(begin fun dim v ->
	  let linexpr0 = Linexpr0.make None in
	  Linexpr0.set_coeff linexpr0 dim (Coeff.s_of_int (-1));
	  Linexpr0.set_cst linexpr0 (Coeff.s_of_float v);
	  Lincons0.make linexpr0 Lincons0.EQ
	 end)
	vertex
    in
    Apron.Abstract0.of_lincons_array apron 0 (Array.length vertex) tlincons0
  )

let counting_jordanpoly_preamble
    ~apron
    ~first
    ~start
    ~trans
    ~polyjordan
    lexpr
    =
  if false then printf "Counting.counting_jordan_preamble first=%i start=%i@." first start;
  assert(first<=start);
  let n = ref (first-1) in
  let res =
    try
      while !n < start do
	incr n;
	let vertex = eval_jordan_ith ~trans !n in
	let poly = poly_of_vertex ~apron vertex in
	if not (Apron.Abstract0.is_leq apron poly polyjordan) then
	  raise Exit;
      done;
      max_int
    with Exit ->
      if false then begin
	List.iter
	  (begin fun e ->
	    let intv = Bound.bound_expr ~dmap_coeff_dim:trans.dmap_coeff_dim ~nmin:!n ~nmax:!n e in
	    let refintv = Bound.bound_expr ~dmap_coeff_dim:trans.dmap_coeff_dim ~nmin:0 ~nmax:max_int e in
	    if not (Bound.Int.is_leq ~eps:(ldexp 1. 16) intv refintv) then
	      printf "expr=%a intv=%a refintv=%a@."
		Bound.print_expr e
		Bound.Int.print intv
		Bound.Int.print refintv
	   end)
	  (List.rev lexpr)
      end;
      !n
  in
  if false then
    printf "res=%i@." res;
  res

let start_for_counting_jordanpoly
    ~apron
    ~trans
    ~polyjordan
    ~first
    (lexpr: Bound.expr list)
    =
  List.fold_left
    (begin fun start expr ->
      let starte = match expr with
      | `Unary dim ->
	  let (coeff,k) = DMappe.x_of_y dim trans.dmap_coeff_dim in
	  begin match coeff with
	  | `Real lambda ->
	      Bound.Real.n_single ~lk:(lambda,k)
	  | _ ->
	      0
	  end
      | `Binary(t,dim1,dim2) ->
	  let (coeff1,k1) = DMappe.x_of_y dim1 trans.dmap_coeff_dim in
	  let (coeff2,k2) = DMappe.x_of_y dim2 trans.dmap_coeff_dim in
	  begin match coeff1,coeff2 with
	  | (`Real lambda1),(`Real lambda2) ->
	      Bound.Real.n_two t ~lk1:(lambda1,k1) ~lk2:(lambda2,k2) ~m:1.0675
	  | _ ->
	      min_int
	  end
      in
      max start starte
     end)
     first lexpr

let binary_of_linexpr0 linexpr0 =
  if false then printf "binary_of_linexpr0 %a@.%a@."
    (Apronaux.print_linexpr0 ~string_of_dim:Apronaux.string_of_dimj) linexpr0
    (Apron.Linexpr0.print Apronaux.string_of_dimj) linexpr0
  ;
  try
    let nbcoeff = ref 0 in
    let mudim = Array.make 2 (0.,0) in
    Apron.(
      Linexpr0.iter
	(begin fun coeff dim ->
	  if not (Coeff.is_zero coeff) then begin
	    let coeff = Matrix.FF.of_coeff coeff in
	    if coeff=nan || coeff=infinity || coeff=neg_infinity then raise Exit;
	    mudim.(!nbcoeff) <- (coeff, dim);
	    incr nbcoeff;
	  end;
	 end)
	linexpr0
    );
    if false then printf "mudim=%a@." (Print.array (fun fmt (coeff,dim) -> fprintf fmt "(%.5G,%i)" coeff dim)) mudim;
    if !nbcoeff=2 then begin
      let mudim =
	if (fst mudim.(0))<0.
	then Array.map (fun (mu,dim) -> (-.mu,dim)) mudim
	  else mudim
	in
	let (mu1,dim1) = mudim.(0)
	and (mu2,dim2) = mudim.(1) in
	let sqr x = x*.x in
	let norm = sqrt((sqr mu1) +. (sqr mu2)) in
	let norm =
	  if norm=1./.0. then
	    if abs_float mu1 >= abs_float mu2 then mu1*.sqrt(1. +. sqr (mu2/.mu1)) else (abs_float mu2)*.sqrt(1.+.sqr(mu1/.mu2))
	  else
	    norm
	in
	if norm=infinity then raise Exit;
	if false then printf "norm=%.5G@." norm;
	let w = Bound.({
	  mu1= mu1/.norm;
	  mu2 = (abs_float mu2)/.norm;
	  s = if mu2>0. then 1. else -1.;
	})
	in
	`Binary(w,dim1,dim2)
    end
    else
      `None
  with Exit -> `None

let select_leqn_for_counting_jordanpoly
    ~apron
    ~trans
    ~polyjordan
    lexpr
    =
  let tlincons0 = Apron.Abstract0.to_lincons_array apron polyjordan in
  let lexpr =
    Array.fold_left
      (begin fun res lincons0 ->
	let linexpr0 = lincons0.Apron.Lincons0.linexpr0 in
	match binary_of_linexpr0 linexpr0 with
	| (`Binary _) as expr -> expr::res
	| `None -> res
       end)
      lexpr
      tlincons0
  in
  if false then printf "lexpr=%a@."
    (Print.list Bound.print_expr) lexpr
  ;
  List.fold_left
    (begin fun res expr ->
      begin match expr with
      | `Unary dim ->
	  let (coeff,k) = DMappe.x_of_y dim trans.dmap_coeff_dim in
	  begin match coeff with
	  | `Real lambda when lambda>0. && (lambda,k)<>(1.,0) ->
	      let interval = Apron.Abstract0.bound_dimension apron polyjordan dim in
	      let (l,u) = Bound.Int.round (Apronaux.interval_to_float2 interval) in
	      if lambda>=1. && u<infinity then
		(`Unary (lambda,k,u))::res
	      else if lambda<1. && l>0. then
		(`Unary (lambda,k,l))::res
	      else
		res
	  | `Sin(lambda,theta,r) when k=0 ->
	      let interval = Apron.Abstract0.bound_dimension apron polyjordan dim in
	      let (l,u) = Bound.Int.round (Apronaux.interval_to_float2 interval) in
	      let varphi = if r=0 then 0. else Bound.pi2 in
	      let res =
		if u<infinity
		then (`SinUpper(1.,lambda,theta,varphi,u))::res
		else res
	      in
	      let res =
		if l>neg_infinity
		then (`SinLower(1.,lambda,theta,varphi,l))::res
		else res
	      in
	      res
	  | _ -> res
	  end
      | `Binary(t,dim1,dim2) ->
	  let (coeff1,k1) = DMappe.x_of_y dim1 trans.dmap_coeff_dim in
	  let (coeff2,k2) = DMappe.x_of_y dim2 trans.dmap_coeff_dim in
	  begin match coeff1,coeff2 with
	  | (`Real lambda1),(`Real lambda2) when
	      (lambda1,k1)>(1.,0) || (lambda2,k2)>(1.,0) ->
	      let linexpr0 = Apron.Linexpr0.make None in
	      Apron.Linexpr0.set_list linexpr0
		[
		  (Apron.Coeff.s_of_float t.mu1,dim1);
		  (Apron.Coeff.s_of_float (t.s*.t.mu2), dim2)
		]
		None
	      ;
	      let interval = Apron.Abstract0.bound_linexpr apron polyjordan linexpr0 in
	      let (l,u) = Bound.Int.round (Apronaux.interval_to_float2 interval) in
	      if u<infinity &&
		  (t.s=1. || (lambda1,k1)>=(lambda2,k2))
	      then
		(`Binary(t.mu1,lambda1,k1,(t.s*.t.mu2),lambda2,k2,u))::res
	      else if l > neg_infinity
		   && t.s= -1. && (lambda2,k2)>=(lambda1,k1)
	      then
		(`Binary(t.mu2,lambda2,k2,(t.s*.t.mu1),lambda1,k1,-.l))::res
	      else
		res
	  | (`Sin(lambda1,theta1,r1),`Sin(lambda2,theta2,r2)) when k1=0 && k1=k2 && lambda1=lambda2 && theta1=theta2 ->
	      assert(r1=(1-r2));
	      let linexpr0 = Apron.Linexpr0.make None in
	      Apron.Linexpr0.set_list linexpr0
		[
		  (Apron.Coeff.s_of_float t.mu1,dim1);
		  (Apron.Coeff.s_of_float (t.s*.t.mu2), dim2)
		]
		None
	      ;
	      let interval = Apron.Abstract0.bound_linexpr apron polyjordan linexpr0 in
	      let (l,u) = Bound.Int.round (Apronaux.interval_to_float2 interval) in
	      let (mu,varphi) = Bound.Complex.mu_varphi_of_sum t (theta1,r1,k1) in
	      if false then
		printf "w=%a, lambda1=%.5G, theta1=%.5G, r1=%i, [l,u]=[%.5G,%.5G], mu=%.5G, varphi=%.5G@."
		  Bound.print_weight t lambda1 theta1 r1 l u mu varphi
	      ;
	      let res =
		if u<infinity
		then (`SinUpper(mu,lambda1,theta1,varphi,u))::res
		else res
	      in
	      let res =
		if l>neg_infinity
		then (`SinLower(mu,lambda1,theta1,varphi,l))::res
		else res
	      in
	      res
	  | _ ->
	      res
	  end
      end
     end)
    [] lexpr

let print_eqn fmt = function
  | `Unary(lambda,k,sup) ->
      fprintf fmt "Unary(lambda=%.5G,k=%i,sup=%.5G)"
	lambda k sup
  | `Binary(mu1,lambda1,k1,mu2,lambda2,k2,sup) ->
      fprintf fmt "Binary(mu1=%2G,lambda1=%.5G,k1=%i,mu2=%2G,lambda2=%.5G,k2=%i,sup=%.5G)"
	mu1 lambda1 k1 mu2 lambda2 k2 sup
  | `SinUpper(mu,lambda,theta,varphi,u) ->
      fprintf fmt "SinUpper(mu=%.5G,lambda=%.5G,theta=%.5G,varphi=%.5G,sup=%.5G)"
	mu lambda theta varphi u
  | `SinLower(mu,lambda,theta,varphi,l) ->
      fprintf fmt "SinLower(mu=%.5G,lambda=%.5G,theta=%.5G,varphi=%.5G,inf=%.5G)"
	mu lambda theta varphi l

exception Val of float

let solve_for_counting_jordanpoly_one ~start eqn =
  let epsilon = ldexp 1. (-16) in
  let tstart = float_of_int start in
  if false then
    printf "solve_for_counting <= %i %a@."
      start print_eqn eqn
  ;
  let t =
    match eqn with
    | `Unary(lambda,k,sup) ->
	let gamma = log lambda in
	let fk = float_of_int k in
	let f t = (Bound.binom_float t k) *. lambda**(t-.fk) in
	let fp t = (f t) *. (Bound.add_ratio_dpoly_poly ~gamma t k) in
	let t = newton ~epsilon ~t0:tstart ~f ~fp ~v:sup in
	let ct = ceil t in
	if t=ct then ct+.1. else ct
    | `Binary(mu1,lambda1,k1,mu2,lambda2,k2,sup) ->
	let gamma1 = log lambda1 in
	let gamma2 = log lambda2 in
	let fk1 = float_of_int k1 in
	let fk2 = float_of_int k2 in
	let f t =
	  mu1 *. (Bound.binom_float t k1) *. lambda1**(t-.fk1) +.
	    mu2 *. (Bound.binom_float t k2) *. lambda2**(t-.fk2)
	in
	let fp t =
	  mu1 *. (Bound.binom_float t k1) *. lambda1**(t-.fk1) *.
	    (Bound.add_ratio_dpoly_poly ~gamma:gamma1 t k1) +.
	    mu2 *. (Bound.binom_float t k2) *. lambda2**(t-.fk2) *.
	    (Bound.add_ratio_dpoly_poly ~gamma:gamma2 t k2)
	in
	let t = newton ~epsilon ~t0:tstart ~f ~fp ~v:sup in
	let ct = ceil t in
	if t=ct then ct+.1. else ct
    | `SinUpper(mu,lambda,theta,varphi,u) ->
	assert(theta>0. && theta < Bound.pi);
	let gamma = log lambda in
	let f t = mu *. lambda**t *. sin(theta*.t+.varphi) in
	let fp t = mu *. lambda**t *. (gamma *. sin(theta*.t+.varphi) +. theta *. cos(theta*.t+.varphi)) in
	(* compute first t0>=tstart such that (f t0)=0 and (fp t0)>0, which means
	   theta.t0+varphi= 2pi.k >= theta.tstart+varphi *)
	let fk0 = ceil ((theta *. tstart +. varphi)/.(2.*.Bound.pi)) in
	let t0 = (2.*.Bound.pi*.fk0 -. varphi) /. theta in
	assert (fk0>=0. && t0>=0.);
	if u<=0. then begin
	  let fstart = f tstart in
	  if fstart > u then neg_infinity
	  else begin
	    (* compute the first infimum before t0 *)
	    let tinf = t0 -. Bound.pi2/.theta in
	    let t = newton ~epsilon ~t0:t0 ~f ~fp ~v:u in
	    assert(t>=tinf && t<=t0);
	    let ct = ceil t in
	    let ct1 = if t=ct then ct +. 1. else ct in
	    assert(ct1-.t0<Bound.pi/.theta && (f ct1) > u);
	    ct1
	  end
	end
	else begin (* u > 0. *)
	  try
	    if lambda>1. then begin
	      let tenveloppe = (log (u/.mu)) /. gamma in
	      (* compute first t0>=tstart, t0>=tenveloppe such that (f t0)=0 and (fp t0)>0:
		 theta.t0+varphi= 2pi.k0 >= theta.(max tstart tenveloppe)+varphi
		 (overapproximation in case (f tenveloppe)=u) *)
	      let tmax = max tenveloppe tstart in
	      let fk0 = ceil ((theta *. tmax +. varphi)/.(2.*.Bound.pi)) in
	      for i=0 to 10 do
		let fk = fk0 +. (float_of_int i) in
		let t0 = (2.*.Bound.pi*.fk -. varphi)/.theta in
		let t = newton ~epsilon ~t0:t0 ~f ~fp ~v:u in
		assert(t>=t0);
		let ct = ceil t in
		let ct1 = if t=ct then ct +. 1. else ct in
		if ct1<=t0+.Bound.pi2/.theta && (f ct1)>u then
		  raise (Val ct1)
	      done;
	      infinity
	    end else begin (* lambda<=1. *)
	      for i=0 to 10 do
		let fk = fk0 +. (float_of_int i) in
		let t0 = (2.*.Bound.pi*.fk -. varphi)/.theta in
		if (f (t0 +. Bound.pi2/.theta))<u then raise (Val infinity);
		let t = newton ~epsilon ~t0:t0 ~f ~fp ~v:u in
		assert(t>=t0);
		let ct = ceil t in
		let ct1 = if t=ct then ct +. 1. else ct in
		assert(ct1-.t0<Bound.pi/.theta);
		if (f ct1) > u then raise (Val ct1);
	      done;
	      infinity
	    end
	  with Val t ->
	    t
	end
    | `SinLower(mu,lambda,theta,varphi,l) ->
	assert(theta>0. && theta < Bound.pi);
	let gamma = log lambda in
	let f t = mu *. lambda**t *. sin(theta*.t+.varphi) in
	let fp t = mu *. lambda**t *. (gamma *. sin(theta*.t+.varphi) +. theta *. cos(theta*.t+.varphi)) in
	(* compute first t0>=tstart such that (f t0)=0 and (fp t0)<0, which means
	   theta.t0+varphi= pi+2pi.k >= theta.tstart+varphi *)
	let fk0 = ceil ((theta *. tstart +. varphi -. Bound.pi)/.(2.*.Bound.pi)) in
	let t0 = (2.*.Bound.pi*.fk0 +. Bound.pi -. varphi) /. theta in
	if false then printf "fk0=%.5G t0=%.5G@." fk0 t0;
	assert (fk0>=0. && t0>=0.);
	if l>=0. then begin
	  let fstart = f tstart in
	  if fstart < l then neg_infinity
	  else begin
	    (* compute the first supremum before t0 *)
	    let tinf = t0 -. Bound.pi2/.theta in
	    let t = newton ~epsilon ~t0:t0 ~f ~fp ~v:l in
	    assert(t>=tinf && t<=t0);
	    let ct = ceil t in
	    let ct1 = if t=ct then ct +. 1. else ct in
	    assert(ct1-.t0<Bound.pi/.theta && (f ct1) < l);
	    ct1
	  end
	end
	else begin (* l < 0. *)
	  try
	    if lambda>1. then begin
	      let tenveloppe = (log (-.l/.mu)) /. gamma in
	      (* compute first t0>=tstart, t0>=tenveloppe such that (f t0)=0 and (fp t0)<0:
		 theta.t0+varphi= pi+2pi.k0 >= theta.(max tstart tenveloppe)+varphi
		 (overapproximation in case (f tenveloppe)=u) *)
	      let tmax = max tenveloppe tstart in
	      let fk0 = ceil ((theta *. tmax +. varphi -. Bound.pi)/.(2.*.Bound.pi)) in
	      for i=0 to 10 do
		let fk = fk0 +. (float_of_int i) in
		let t0 = (2.*.Bound.pi*.fk +. Bound.pi -. varphi)/.theta in
		let t = newton ~epsilon ~t0:t0 ~f ~fp ~v:l in
		assert(t>=t0);
		let ct = ceil t in
		let ct1 = if t=ct then ct +. 1. else ct in
		if ct1<=t0+.Bound.pi2/.theta && (f ct1)<l then
		  raise (Val ct1)
	      done;
	      infinity
	    end else begin (* lambda<=1. *)
	      for i=0 to 10 do
		let fk = fk0 +. (float_of_int i) in
		let t0 = (2.*.Bound.pi*.fk +. Bound.pi -. varphi)/.theta in
		if (f (t0 +. Bound.pi2/.theta))>l then raise (Val infinity);
		let t = newton ~epsilon ~t0:t0 ~f ~fp ~v:l in
		assert(t>=t0);
		let ct = ceil t in
		let ct1 = if t=ct then ct +. 1. else ct in
		assert(ct1-.t0<Bound.pi/.theta);
		if (f ct1) < l then raise (Val ct1);
	      done;
	      infinity
	    end
	  with Val t ->
	    t
	end
  in
  let res =
    if t >= float_of_int (max_int-1) then
      max_int
    else if t=neg_infinity then
      min_int
    else
      int_of_float t
  in
  if false then
    printf "solve_for_counting => res=%i@."
      res
  ;
  res

let solve_for_counting_jordanpoly ~start leqn =
  if false then printf "solve_for_counting_jordanpol <= start=%i@." start;
  let res =
    List.fold_left
      (begin fun res eqn ->
	let n = solve_for_counting_jordanpoly_one ~start eqn in
	min n res
       end)
      max_int
      leqn
  in
  if false then printf "solve_for_counting_jordanpol => res = %i@." res;
  res

let main ~apron ~trans ~polyjordan ~first (lexpr:Bound.expr list) =
  if false then printf "Counting.main <= first=%i@." first;
  let start = start_for_counting_jordanpoly ~apron ~trans ~polyjordan ~first lexpr in
  if false then printf "start = %i@ " start;
  let n = counting_jordanpoly_preamble ~apron ~first ~start ~trans ~polyjordan lexpr in
  if n<max_int then begin
    if false then printf "res_by_preamble = %i@." n;
    n
  end
  else begin
    let leqn =
      select_leqn_for_counting_jordanpoly ~apron ~trans ~polyjordan lexpr
    in
    if false then printf "selected eqn = %a@." (Print.list print_eqn) leqn;
    let n = solve_for_counting_jordanpoly ~start leqn in
    if false then printf "res = %i@." n;
    n
  end
