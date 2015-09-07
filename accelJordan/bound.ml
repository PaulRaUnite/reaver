
open Format

type weight = {
  mu1: float; (* A strictly positive real *)
  mu2: float; (* A strictly positive real *)
  s: float (* 1. or -1. *)
}
type uexpr = [`Unary of int]
type bexpr = [`Binary of weight * int * int]
type expr = [ uexpr | bexpr ]

let print_weight fmt { mu1; mu2; s } =
  fprintf fmt "{ mu1=%.5G; s.mu2=%.5G }"
    mu1 (s*.mu2)

let print_expr fmt = function
  | `Unary(dim) -> fprintf fmt "Unary(%i)" dim
  | `Binary(t,dim1,dim2) ->
      fprintf fmt "Binary(%a,%i,%i)"
	print_weight t
	dim1 dim2

let print_pair fmt (f,k) =
  fprintf fmt "(%.5G,%i)" f k

let print_gk = print_pair
let print_lk = print_pair
let print_lgk fmt (f,g,k) =
  fprintf fmt "(%.5G,%.5G,%i)" f g k
let print_ltrk fmt (f,g,k,h) =
  fprintf fmt "(%.5G,%.5G,%i,%i)" f g k h
let print_gtrk = print_ltrk

let round_float ?(nb=16) dir x =
  if x=infinity || x=neg_infinity then x
  else begin
    let (man,exponent) = frexp x in
    let man2 = ldexp man nb in
    match dir with
    | `U -> ldexp (ceil man2) (exponent-nb)
    | `D -> ldexp (floor man2) (exponent-nb)
  end

(* In practice k small (1,2,3,4), so no optimization *)
let binom n k =
  if n<k then
    0
  else
    let res = ref 1 in
    for l=0 to k-1 do
      res := !res * (n-l);
    done;
    for l=2 to k do
      res := !res/l;
    done;
    !res

(* In practice k small (1,2,3,4), so no optimization *)
let binom_float (t:float) (k:int) : float =
  let fk = float_of_int k in
  if t<fk then
    0.
  else
    let res = ref 1. in
    for l=0 to k-1 do
      res := !res *. (t -. (float_of_int l))
    done;
    for l=2 to k do
      res := !res/.(float_of_int l);
    done;
    !res

let add_ratio_dpoly_poly ~(gamma:float) (t:float) (k:int) : float =
  if (abs_float gamma) < 1./.t then begin
    let res = ref gamma in
    for l=0 to k-1 do
      res := !res +. 1./.(t -. (float_of_int l))
    done;
    !res
  end else begin
    let res = ref 0. in
    for l=0 to k-1 do
      res := !res +. 1./.(t -. (float_of_int l))
    done;
    !res +. gamma
  end
let sin_atan x = x/.(sqrt (1.+.x*.x))

let fact n =
  assert(n>=0);
  let rec f res n =
    if n<=1 then res else f (res*n) (n-1)
  in
  f 1 n

let inf = 1./.0.
let neginf = -. inf
let pi = 3.14159265358979323846264338327950288
let pi2 = 1.57079632679489661923132169163975144
let pi4 = 0.785398163397448309615660845819875721
let piinv = 0.318309886183790671537767526745028724
let pi2inv = 0.636619772367581343075535053490057448

module Int = struct
  exception Bottom
  type t = float*float
  let print fmt (l,u) =
    fprintf fmt "[%.5G, %.5G]"
      l u
  let is_bottom (l,u) = l>u
  let bottom = (inf,neginf)
  let zero = (0.,0.)
  let join (a1,b1) (a2,b2) =
    (min a1 a2, max b1 b2)
  let meet (a1,b1) (a2,b2) =
    let a=max a1 a2 and b=min b1 b2 in
    if false then
     printf "meet %a %a = %a@."
       print (a1,b1) print (a2,b2) print (a,b)
;
    if a>b then raise Bottom;
    (a,b)
  let is_leq ~eps (a1,b1) (a2,b2) =
    a1-.eps>=a2 && b1+.eps<=b2
  let add (a1,b1) (a2,b2) =
    (a1+.a2,b1+.b2)
  let neg (a,b) =
    (-.b, -. a)
  let sub (a1,b1) (a2,b2) =
    add (a1,b1) (neg (a2,b2))
  let scale mu (a,b) =
    if mu>=0.
    then (mu*.a, mu*.b)
    else (mu*.b, mu*.a)
  let add_scalar (a,b) mu =
    (a+.mu,b+.mu)
  let div_scalar (a,b) mu =
    if mu>0.
    then (a/.mu, b/.mu)
    else (b/.mu, a/.mu)
  let rec mul (a1,b1) (a2,b2) =
    assert(a1>=0.);
    if a2>=0. then
      (a1*.a2, b1*.b2)
    else if b2<=0. then
      (b1*.a2,a1*.b2)
    else
      scale b1 (a2,b2)
  let round ?nb (l,u) =
    (round_float ?nb `D l,
     round_float ?nb `U u)
  let atan (l,u) =
    (atan l, atan u)
  let sin_atan (l,u) =
    (sin_atan l, sin_atan u)

  let pow lambda (l,u) =
    if lambda<1. then
      (lambda**u, lambda**l)
    else if lambda>1. then
      (lambda**l, lambda**u)
    else
      (1.,1.)

  let binom_float (l,u) k =
    (binom_float l k, binom_float u k)
end

let bound_fun_range ~nmin ~nmax (f:int -> float) =
  assert (nmin<=nmax);
  let l = ref (f nmin) in
  let u = ref (!l) in
  let nmin = nmin+1 in
  begin (* even arguments *)
    for i=(nmin+1)/2 to nmax/2 do
      let v = f (2*i) in
      if v < !l then l:=v;
      if v > !u then u:=v;
    done;
  end;
  begin (* odd arguments *)
    for i=nmin/2 to (nmax-1)/2 do
      let v = f (2*i+1) in
      if v < !l then l:=v;
      if v > !u then u:=v;
    done;
  end;
  (!l,!u)

let bound_fun ~nmin ~nmax ~nmonotone (f:int -> float) =
  if nmin = nmax then begin
    (* a single value in the range *)
    let v = f nmin in
    (v,v)
  end
  else if nmax<=nmonotone then
    bound_fun_range ~nmin ~nmax f
  else (* nmonotone < nmax *)
    let lu_max =
      let v0 = f nmax and v1 = f (nmax-1) in
      if v0<=v1 then (v0,v1) else (v1,v0)
    in
    if nmonotone <= nmin then
      let lu_min =
	let v0 = f nmin and v1 = f (nmin+1) in
	if v0<=v1 then (v0,v1) else (v1,v0)
      in
      Int.join lu_min lu_max
    else
      let lu0 = bound_fun_range ~nmin ~nmax:nmonotone f in
      Int.join lu0 lu_max

let bound_fun_lt f lt =
  assert(lt<>[]);
  let t0 = List.hd lt in
  let l = ref (f t0) in
  let u = ref (!l) in
  List.iter
    (begin fun t ->
      let v = f t in
      if v < !l then l:=v
      else if v > !u then u:=v;
     end)
    (List.tl lt)
  ;
  (!l,!u)

module Real = struct
  let single_int ~lk:((lambda,k) as lk) (n:int) =
    let sign = if lambda >= 0. || (n-k mod 2=0) then 1. else -1. in
    let lambda = abs_float lambda in
    let res =
      if n>=max_int-1 then begin
	if lambda<1. then 0.
	else if lambda=1. && k=0 then 1.
	else inf
      end
      else begin
	if n<k then
	  0.0
	else
	  let binom = binom n k in
	  (float_of_int binom) *. (lambda**(float_of_int (n-k)))
      end
    in
    let res = sign *. res in
    if false then printf "single_int lk:%a n:%i = %.5G@." print_lk lk n res;
    res

  let two_int t ~lk1:((lambda1,k1) as lk1) ~lk2:((lambda2,k2) as lk2) (n:int) =
    assert(lk1<>lk2);
    let t1 = t.mu1 *. (single_int ~lk:lk1 n) in
    let t2 = t.s *. t.mu2 *. (single_int ~lk:lk2 n) in
    let res =
      if n>=max_int-1 && (lk1>(1.,0) || lk2>(1.,0)) then
	if lk1>lk2 then t1 else t2
      else
	t1 +. t2
    in
    if false then printf "two_int n:%i => (t1,t2)=(%5G,%5G) => %5G@."
      n t1 t2 res
    ;
    res

  let n_gamma_k ~gk:(gamma,k) =
    if gamma>=0. then
      k
    else
      let ratio = ceil ((float_of_int k) /. (-.gamma)) in
      if ratio >= float_of_int (max_int-1-k) then failwith "Overflow";
      k+(int_of_float ratio)

  let n_single ~lk:(lambda,k) =
    let lambda = abs_float lambda in
    if lambda=0.then
      k+1
    else if lambda >= 1. then
      k
    else
      n_gamma_k ~gk:(log lambda, k)

  let n0_two ~gk1:((gamma1,k1) as gk1) ~gk2:((gamma2,k2) as gk2) =
    max
      (2*(max k1 k2)+1)
      (max
	 (n_gamma_k gk1)
	 (n_gamma_k gk2)
      )

  let r_two w ~lk1:((lambda1,k1) as lk1) ~lk2:((lambda2,k2) as lk2) =
    let res =
      (w.mu1/.w.mu2) *.
	((float_of_int (fact k2)) /. (float_of_int (fact k1))) *.
	(lambda2**(float_of_int k2) /. lambda1**(float_of_int k1))
    in
    if false then
      printf "r_two w:%a lk1:%a lk2:%a = %.5G@."
	print_weight w print_lk lk1 print_lk lk2 res
    ;
    res

  let l_two ~gk1:((gamma1,k1) as gk1) ~gk2:((gamma2,k2) as gk2) ~n =
    if gamma1>=gamma2 && gamma2>=0. && gamma1>0.0 then
      gamma1/.(gamma2 +. (float_of_int k2)/.(float_of_int (n-k2+1)))
    else if gamma1=gamma2 && gamma1=0.0 then
      (float_of_int k1) /. (2.0*.(float_of_int k2))
    else if gamma1>=gamma2 && gamma1<0.0 then
      (-.gamma1 -. (float_of_int k1)/.(float_of_int (n-k1+1))) /. (-.gamma2)
    else if gamma1>0.0 && gamma2<0.0 then
      gamma1 /. (-.gamma2)
    else
      failwith (Print.sprintf "l_two gk1:%a gk2:%a n:%i"
		  print_gk gk1 print_gk gk2 n)

  let t_two (m:float) ~lgk:((lambda,gamma,k) as lgk) =
    if false then printf "t_two ~m:%5G ~lgk:%a@." m print_lgk lgk;
    assert(gamma>= 0.);
    let fk = float_of_int k in
    if gamma=0. then begin
      assert(k>0);
      m**(1./.fk)
    end
    else
      let p = 4. in
      let t0 =
	if k>=0 then
	  3.
	else begin
	  let mpfk = -.p*.fk in
	  let mpfk_div_gamma = mpfk/.gamma in
	  let t0 = max mpfk_div_gamma 4. in
	  t0
	end
      in
      let rho = t0**fk *. (exp (gamma *. t0)) in
      let diff = m -. rho in
      if diff<=0. then
	t0
      else
	let rhop = rho *. (gamma +. fk /. t0) in
	t0 +. diff /. rhop

  let n_two_pos w ~lgk1:((lambda1,gamma1,k1) as lgk1) ~lgk2:((lambda2,gamma2,k2) as lgk2) ~n ~(m:float) =
    if false then
      printf "n_two_pos w:%a lgk1:%a lgk2:%a ~n:%i m:%G@."
	print_weight w print_lgk lgk1 print_lgk lgk2 n m
    ;
    assert (n>=(n0_two ~gk1:(gamma1,k1) ~gk2:(gamma2,k2)));
    assert ((lambda1,k1)>(lambda2,k2));
    let r_two = r_two w ~lk1:(lambda1,k1) ~lk2:(lambda2,k2) in
    let twok1MR = 2.0**(float_of_int k1) *. m /. r_two in
    let t_two =
      if gamma1=0.0 && gamma2<0.0 then
	t_two
	  (twok1MR *. (-.gamma2) /. (float_of_int k1))
	  ~lgk:(
	    (1./.lambda2),
	    (-. gamma2),
	    (k1-k2-1)
	  )
      else begin
	let l_two = l_two ~gk1:(gamma1,k1) ~gk2:(gamma2,k2) ~n in
	if false then
	  printf "r_two=%.5G, l_two=%.5G factor=%.5G@." r_two l_two  (twok1MR /. l_two)
	;
	t_two
	  (twok1MR /. l_two)
	  ~lgk:(
	    (lambda1 /. lambda2),
	    (gamma1 -. gamma2),
	    (k1-k2)
	  )
      end
    in
    if false then printf "t_two => %.5G@." t_two;
    let fn = float_of_int n in
    let n_two =
      if t_two < fn then
	n
      else
	let t_two = ceil t_two in
	if t_two >= float_of_int (max_int-1) then failwith "Overflow";
	int_of_float t_two
    in
    if false then printf "n_two_pos => %i@." n_two;
    n_two

  let n_two w ~lk1:((lambda1,k1) as lk1) ~lk2:((lambda2,k2) as lk2) ~(m:float) =
    if false then
      printf "n_two w:%a lk1:%a lk2:%a m:%G@."
	print_weight w print_lk lk1 print_lk lk2 m
    ;
    let res =
      if lk1=lk2 then
	n_single ~lk:lk1
      else begin
	let lambda1 = abs_float lambda1 in
	let lambda2 = abs_float lambda2 in
	if lambda1=0. || lambda1=1. && k1=0 then begin
	  let n2 = n_single ~lk:lk2 in
	  max (k1+1) n2
	end
	else if lambda2=0. || lambda2=1. && k2=0 then begin
	  let n1 = n_single ~lk:lk1 in
	  max n1 (k2+1)
	end
	else
	  let lk1 = (lambda1,k1) and lk2 = (lambda2,k2) in
	  let (sign,w,(lambda1,k1),(lambda2,k2)) =
	    if lk1>lk2
	    then (1.,w,lk1,lk2)
	    else (w.s, { w with mu1=w.mu2; mu2=w.mu1 }, lk2,lk1)
	  in
	  let gamma1 = log lambda1 in
	  let gamma2 = log lambda2 in
	  n_two_pos w
	    ~lgk1:(lambda1,gamma1,k1) ~lgk2:(lambda2,gamma2,k2)
	    ~n:(n0_two ~gk1:(gamma1,k1) ~gk2:(gamma2,k2))
	    ~m
      end
    in
    if false then printf "n_two => %i@." res;
    res

  let bound_single ~lk:((lambda,k) as lk) ~(nmin:int) ~(nmax:int) =
    if false then
      printf "Real.bound_single %a nmin:%i nmax:%i@."
	print_lk lk nmin nmax
    ;
    let nmonotone = n_single ~lk in
    let res = bound_fun ~nmin ~nmax ~nmonotone (single_int ~lk) in
    if false then printf "res=%a@." Int.print res;
    res

  let bound_two
      w
      ~lk1:lk1
      ~lk2:lk2
      ~nmin ~nmax
      =
    if false then
      printf "Real.bound_two %a %a %a nmin:%i nmax:%i@."
	print_weight w print_lk lk1 print_lk lk2 nmin nmax
    ;
    let nmonotone = n_two w ~lk1 ~lk2 ~m:1.0675 in
    let res = bound_fun ~nmin ~nmax ~nmonotone (two_int w ~lk1 ~lk2) in
    if false then printf "res=%a@." Int.print res;
    res

end

module Complex = struct
  let single_int ~ltrk:(lambda,theta,r,k) (n:int) =
    assert (n<max_int-1);
    let angle = theta*.(float_of_int (n-k)) in
    (Real.single_int ~lk:(lambda,k) n) *.
      (if r=0 then sin angle else cos angle)

  let two_int w ~ltrk1:((lambda1,theta1,r1,k1) as ltrk1) ~ltrk2:((lambda2,theta2,r2,k2) as ltrk2) (n:int) =
    let t1 = w.mu1 *. (single_int ~ltrk:ltrk1 n) in
    let t2 = w.s *. w.mu2 *. (single_int ~ltrk:ltrk2 n) in
    t1 +. t2

  let mu_varphi_of_sum w (theta,r1,k) =
    if false then printf "mu_varphi_of w:%a theta:%.9G r:%i k:%i@." print_weight w theta r1 k;
    let mu = sqrt (w.mu1*.w.mu1 +. w.mu2*.w.mu2) in
    let varphi =
      let fk = float_of_int k in
      let tk = theta*.fk in
      let costk = cos tk and sintk = sin tk in
      if r1=0 then
	let num = -. w.mu1 *. sintk +. w.s *. w.mu2 *. costk in
	let den = w.mu1 *. costk +. w.s *. w.mu2 *. sintk in
	atan(num/.den) +. (if den>=0. then 0. else pi)
      else
	let num = w.mu1 *. costk -. w.s *. w.mu2 *. sintk in
	let den = w.mu1 *. sintk +. w.s *. w.mu2 *. costk in
	atan(num/.den) +. (if den>=0. then 0. else pi)
    in
    if false then printf "=> mu=%.9G varphi=%.9G@." mu varphi;
    (mu,varphi)

  let l_phi_of_T ~t =
    let l = ceil (t /. pi -. 0.5) in
    let phi = t -. l*.pi in
    (l,phi)

  (* List of argument on which $e^\gamma t sin(\theta t +
     \varphi)$ may reach a global extremum in the interval
     [nmin,nmax] *)
  let textrema_of_egamma_sin ~gamma ~theta ~varphi ~nmin ~nmax =
    if true then
      printf "textrema_of_egamma_sin gamma=%5G theta=%5G varphi=%5G nmin:%i nmax:%i@."
	gamma theta varphi nmin nmax
    ;
    if nmax=max_int && gamma>0. then
      []
    else begin
      let res =
	let tmin = float_of_int nmin in
	let theta_div_gamma = theta /. gamma in
	let atan_theta_div_gamma = atan theta_div_gamma in
	let (lmin,phimin) = l_phi_of_T ~t:(theta*.tmin+.varphi) in
	let l0 = if phimin <= -.atan_theta_div_gamma then lmin else lmin+.1. in
	let t0 = (-.atan_theta_div_gamma -. varphi +.l0 *. pi) /. theta in
	let t1 = t0+. pi /. theta in
	if true then
	  printf "theta_div_gamma=%5G atan=%5G lmin=%5G,phimin=%5G,l0=%5G,t0=%5G,t1=%5G@."
	    theta_div_gamma atan_theta_div_gamma lmin phimin l0 t0 t1
	;
	if nmax=max_int then begin
	  [ tmin; t0; t1 ]
	end else begin
	  let tmax = float_of_int nmax in
	  let (lmax,phimax) = l_phi_of_T ~t:(theta*.tmax+.varphi) in
	  let l2 = if phimax <= -.atan_theta_div_gamma then lmax-.1. else lmax in
	  let t2 = (-.atan_theta_div_gamma -. varphi +. l2 *. pi) /. theta in
	  let t3 = t2 -. pi /. theta in
	  if true then
	    printf "lmax=%5G,phimax=%5G,l2=%5G,t2=%5G,t3=%5G@."
	      lmax phimax l2 t2 t3
	  ;
	  if t1 <= tmax then begin
	    assert(if t3>=tmin then true else (printf "t3=%g, tmin=%g@." t3 tmin; false));
	    [ tmin; t0; t1; t2; t3; tmax ]
	  end
	  else if t0 <= tmax then begin
	    assert(t3<tmin);
	    assert(t2>=tmin);
	    [ tmin; t0; tmax ];
	  end
	  else begin
	    assert (t2<tmin);
	    [ tmin; tmax ]
	  end
	end
      in
      if false then
	printf "textrema_of_egamma_sin => %a@." (Print.list (fun fmt x -> fprintf fmt "%.5G" x)) res
      ;
      res
    end

  (* Interval bounding $e^{\gamma t} sin(\theta t + \varphi)$ on the
     interval [nmin,nmax] *)
  let vextrema_of_egamma_sin ~gamma ~theta ~varphi ~nmin ~nmax =
    let lt = textrema_of_egamma_sin ~gamma ~theta ~varphi ~nmin ~nmax in
    if lt=[] then (neginf,inf)
    else begin
      let foo t = exp(gamma*.t) *. sin (theta*.t +. varphi) in
      if false then printf "@.vextrema_of_egamma_sin@ varphi=%.7G@ lt=%a@."
	varphi
	(Print.list (fun fmt x -> fprintf fmt "%.7G => %.7G" x (foo x))) lt
      ;
      let res =
	bound_fun_lt
	  (fun t -> exp(gamma*.t) *. sin (theta*.t +. varphi))
	  lt
      in
      if false then printf "vextrema_of_egamma_sin -> %a@."
	Int.print res
      ;
      res
    end
  let rmin_rmax_gamma_zero ~theta ~k ~tmin ~tmax =
    let fk = float_of_int k in
    let rmin1 = theta/.fk*.(tmin-.fk+.1.) in
    let rmax1 = tmin +. 2.*.pi/.theta in
    let rmax1 = min rmax1 tmax in
    let rmax1 = theta/.fk*.rmax1 in
    if tmax=inf then
      ((rmin1,rmax1),None)
    else
      let rmin2 = max tmin (tmax-.2.*.pi/.theta) in
      let rmin2 = theta/.fk*.rmin2 in
      let rmax2 = theta/.fk*.tmax in
      ((rmin1,rmax1),Some(rmin2,rmax2))

  let rmin_rmax_gamma_notzero ~gamma ~theta ~fp =
    let rmax = theta/.gamma in
    let rmin =
      rmax *.
	(if gamma>0. then fp/.(fp+.1.) else fp/.(fp-.1.))
    in
    (rmin,rmax)

  let extrema_of_interval_min ~gamma ~theta ~varphi ~intr ~tmin =
    if false then printf "extrema_of_interval_min gamma=%5G theta=%5G varphi=%5G intr=[%5G,%5G] tmin=%5G@."
      gamma theta varphi (fst intr) (snd intr) tmin;
    let (l,phi) = l_phi_of_T ~t:(theta*.tmin+.varphi) in
    let atanR = Int.atan (Int.neg intr) in
    let invtheta = 1./.theta in
    let intt0 =
      try
	Int.scale
	  invtheta
	  (Int.add_scalar (Int.meet atanR (phi,pi2)) (-.varphi +. l*.pi))
      with Int.Bottom -> Int.bottom
    in
    let intt0p =
      try
	Int.scale
	  invtheta
	  (Int.add_scalar (Int.meet atanR (-.pi2,phi)) (-.varphi +. (l+.1.)*.pi))
      with Int.Bottom -> Int.bottom
    in
    let pi_div_theta = pi *. invtheta in
    let intt1 = Int.add_scalar intt0 pi_div_theta in
    let intt1p = Int.add_scalar intt0p pi_div_theta in
    (intt0,intt0p,intt1,intt1p)

  let extrema_of_interval_max ~gamma ~theta ~varphi ~intr ~tmax =
    let (l,phi) = l_phi_of_T ~t:(theta*.tmax+.varphi) in
    let atanR = Int.atan (Int.neg intr) in
    let invtheta = 1./.theta in
    let intt2 =
      try
	Int.scale
	  invtheta
	  (Int.add_scalar (Int.meet atanR (phi,pi2)) (-.varphi +. (l-.1.)*.pi))
      with Int.Bottom -> Int.bottom
    in
    let intt2p =
      try
	Int.scale
	  invtheta
	  (Int.add_scalar (Int.meet atanR (-.pi2,phi)) (-.varphi +. l*.pi))
      with Int.Bottom -> Int.bottom
    in
    let pi_div_theta = pi *. invtheta in
    let intt3 = Int.add_scalar intt2 (-.pi_div_theta) in
    let intt3p = Int.add_scalar intt2p (-.pi_div_theta) in
    (intt2,intt2p,intt3,intt3p)

  (* Returns start and list of argument on which $e^{\gamma t} Pk(t) sin(\theta t +
     \varphi)$ may reach a global extremum in the interval
     [start,nmax] *)
  let textrema_of_egamma_pk_sin ~(gamma:float) ~theta ~(k:int) ~varphi ~fp ~(nmin:int) ~(nmax:int)
      : int * float list
      =
    assert(k>0);
    if false then printf "textrema_of_egamma_pk_sin gamma=%5G theta=%5G k=%i varphi=%5G fp=%5G nmin=%i nmax=%i@."
      gamma theta k varphi fp nmin nmax;
    if nmax=max_int && gamma>0. then
      (-1,[])
    else begin
      let (nmin:int) =
	max
	  nmin
	  (if gamma=0.0 then k else Real.n_gamma_k ~gk:(gamma/.fp,k))
      in
      if false then printf "nmin=%i@." nmin;
      if nmin>=nmax then
	(nmax,[])
      else begin
	let tmin = float_of_int nmin in
	let tmax = if nmax=max_int then inf else float_of_int nmax in
	let (intr1,ointr2) =
	  if gamma=0.
	  then rmin_rmax_gamma_zero ~theta ~k ~tmin ~tmax
	  else (rmin_rmax_gamma_notzero ~gamma ~theta ~fp, None)
	in
	if false then printf "intr1=[%5G,%5G]@." (fst intr1) (snd intr1);
	let tint =
	  let (intt0,intt0p,intt1,intt1p) =
	    extrema_of_interval_min ~gamma ~theta ~varphi ~intr:intr1 ~tmin
	  in
	  if tmax=inf then
	    [|intt0;intt0p;intt1;intt1p|]
	  else
	    let intr2 = match ointr2 with
	    | Some x -> x
	    | None -> intr1
	    in
	    let (intt2,intt2p,intt3,intt3p) =
	      extrema_of_interval_max ~gamma ~theta ~varphi ~intr:intr2 ~tmax
	    in
	    [|intt0;intt0p;intt1;intt1p;intt2;intt2p;intt3;intt3p|]
	in
	let lint =
	  Array.fold_right
	    (begin fun interval res ->
	      try
		if Int.is_bottom interval then res
		else
		  let (l,u) = Int.meet interval (tmin,tmax) in
		  l::u::res
	      with Int.Bottom ->
		res
	     end)
	    tint
	    [tmin;tmax]
	in
	(nmin,lint)
      end
    end

  (* Returns an interval bounding $e^{\gamma t} Pk(t) sin(\theta t
     + \varphi)$ in the interval [start,nmax] *)
  let vextrema_of_egamma_pk_sin ~gamma ~theta ~k ~varphi ~fp ~nmin ~nmax =
    assert(k>0);
    if false then printf "vextrema_of_egamma_pk_sin gamma=%5G theta=%5G k=%i varphi=%5G fp=%5G nmin=%i nmax=%i@."
      gamma theta k varphi fp nmin nmax;
    let (start,lt) = textrema_of_egamma_pk_sin ~gamma ~theta ~k ~varphi ~fp ~nmin ~nmax in
    if start= -1 then
      (neginf,inf)
    else begin
      assert(nmin<=start);
      let f t =
	exp(gamma*.t) *. (binom_float t k) *. sin (theta*.t +. varphi)
      in
      let int1 =
	if nmin<start then
	  Some (bound_fun_range ~nmin ~nmax:(start-1) (fun t -> f (float_of_int t)))
	else
	  None
      in
      let int2 =
	if lt=[] then
	  None
	else
	  Some(bound_fun_lt f lt)
      in
      match int1,int2 with
      | Some x,None | None,Some x -> x
      | Some x1, Some x2 -> Int.join x1 x2
      | None,None -> failwith ""
    end

  let bound_single ~ltrk:((lambda,theta,r,k) as ltrk) ~(fp:float) ~nmin ~nmax =
    assert(theta>0. && theta<pi && lambda>0.);
    if false then
      printf "Complex.bound_single ltrk=%a fp:%5G %i %i@."
	print_ltrk ltrk fp nmin nmax
    ;
    let fr = float_of_int r in
    let fk = float_of_int k in
    let varphi = -.theta*.fk +. fr*.pi2 in
    let gamma = log lambda in
    let res =
      if k=0 then
	vextrema_of_egamma_sin ~gamma ~theta ~varphi ~nmin ~nmax
      else
	Int.scale
	  (exp(-.gamma*.fk)/.(float_of_int (fact k)))
	  (vextrema_of_egamma_pk_sin ~gamma ~theta ~k ~varphi ~fp ~nmin ~nmax)
    in
    if false then
      printf "res=%a@." Int.print res
    ;
    res

  let bound_two
      w
      ~ltrk1:(lambda1,theta1,r1,k1)
      ~ltrk2:(lambda2,theta2,r2,k2)
      ~(fp:float)
      ~nmin
      ~nmax
      :
      Int.t
      =
    let res =
      if nmax < k1 && nmax < k2 then
	(0.,0.)
      else if theta1=theta2 && r1=r2 && k1=k2 then
	  let int1 = Real.bound_two w ~lk1:(lambda1,k1) ~lk2:(lambda2,k2) ~nmin ~nmax in
	  let fr1 = float_of_int r1 in
	  let fk1 = float_of_int k1 in
	  let varphi = -.theta1*.fk1 +. fr1*.pi2 in
	  let int2 =
	    vextrema_of_egamma_sin ~gamma:0. ~theta:theta1 ~varphi ~nmin:k1 ~nmax
	  in
	  Int.mul int1 int2
      else if lambda1=lambda2 && theta1=theta2 && k1=k2 then
	let gamma1 = log lambda1 in
	let fk1 = float_of_int k1 in
	let (mu,varphi) = mu_varphi_of_sum w (theta1,r1,k1) in
	if false then printf "bound_two gamma=%.7G mu=%.7G@ varphi=%.7G@."
	  gamma1 mu varphi
	;
	let interval =
	  if k1=0 then
	    vextrema_of_egamma_sin ~gamma:gamma1 ~theta:theta1 ~varphi ~nmin:k1 ~nmax
	  else
	    vextrema_of_egamma_pk_sin ~gamma:gamma1 ~theta:theta1 ~k:k1 ~varphi ~fp ~nmin:k1 ~nmax
	in
	Int.scale
	  (mu *. exp(-.gamma1*.fk1)/.(float_of_int (fact k1)))
	  interval
      else
	(neginf,inf)
    in
    res
end

let bound_single_coeff (dim,k) ~nmin ~nmax =
  match dim with
  | `Real lambda ->
    Real.bound_single ~lk:(lambda,k) ~nmin ~nmax
  | `Sin(lambda,theta,r) ->
    Complex.bound_single ~ltrk:(lambda,theta,r,k) ~fp:5. ~nmin ~nmax

let bound_two_coeff w (dim1,k1) (dim2,k2) ~nmin ~nmax =
  if false then
    printf "bound_two_coeff w:%a coeff1:%a coeff2:%a nmin:%i nmax:%i@."
      print_weight w
      Jordan.print_coeff (dim1, k1)
      Jordan.print_coeff (dim2, k2)
      nmin nmax
  ;
  let res =
    match (dim1,dim2) with
  | (`Real lambda1),(`Real lambda2) ->
      Real.bound_two w ~lk1:(lambda1,k1) ~lk2:(lambda2,k2) ~nmin ~nmax
  | (`Sin(lambda1,theta1,r1)),(`Sin(lambda2,theta2,r2)) ->
      Complex.bound_two
	w
	~ltrk1:(lambda1,theta1,r1,k1)
	~ltrk2:(lambda2,theta2,r2,k2)
	~fp:5.
	~nmin ~nmax
  | (`Real lambda1),(`Sin(lambda2,theta2,r2)) ->
      let (l1,u1) = Real.bound_two { w with s = 1. } ~lk1:(lambda1,k1) ~lk2:(lambda2,k2) ~nmin ~nmax in
      let (l2,u2) = Real.bound_two { w with s = -1. } ~lk1:(lambda1,k1) ~lk2:(lambda2,k2) ~nmin ~nmax in
      (min l1 l2,max u1 u2)
  | (`Sin(lambda1,theta1,r1)),(`Real lambda2) ->
      let nw = { s=1.; mu1 = w.mu2; mu2=w.mu1 } in
      let (l1,u1) = Real.bound_two nw ~lk1:(lambda2,k2) ~lk2:(lambda1,k1) ~nmin ~nmax in
      let (l2,u2) = Real.bound_two { nw with s= -1. } ~lk1:(lambda2,k2) ~lk2:(lambda1,k1) ~nmin ~nmax in
      Int.scale w.s (min l1 l2,max u1 u2)
  in
  if false then printf "bound_two_coeff => %a@." Int.print res;
  res

let bound_expr ~(dmap_coeff_dim:(Jordan.coeff,int) DMappe.t) ~nmin ~nmax (expr:expr) = match expr with
  | `Unary(dim) ->
      let coeff = DMappe.x_of_y dim dmap_coeff_dim in
      bound_single_coeff coeff ~nmin ~nmax
  | `Binary(t,dim1,dim2) ->
      let coeff1 = DMappe.x_of_y dim1 dmap_coeff_dim in
      let coeff2 = DMappe.x_of_y dim2 dmap_coeff_dim in
      bound_two_coeff t coeff1 coeff2 ~nmin ~nmax

let bound_lexpr ~dmap_coeff_dim ~nmin ~nmax (lexpr:expr list) =
  List.rev_map
    (fun expr -> (expr,bound_expr ~dmap_coeff_dim ~nmin ~nmax expr))
    lexpr
