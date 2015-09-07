
type 'number degree = {
  gamma : 'number;
  d : int;
}
type 'number gt = ('number degree, 'number) PMappe.t
type ('id,'number) git = ('id, 'number gt) PMappe.t


module Make(F:Field) = struct
    type t = Field.t gpolynom

    let degree_zero ={ gamma=F.zero; d=0 }
    let degree_cmp d1 d2 =
      let res = Field.compare d1.gamma d2.gamma in
      if res=0 then d1.d-d2.d else res
    let degree_sub d1 d2 =
      { gamma = Field.(d1.gamma - d2.gamma);
        d = d1.d - d2.d }


    let zero = PMappe.empty degree_cmp
    let cst cst = PMappe.singleton degree_cmp degree_zero cst
    let one = cst Field.one

    let of_degree degree = PMappe.singleton degree_cmp degree F.one

    let of_monom ?(coeff=Field.one) ~gamma ~d =
      PMappe.singleton degree_cmp { gamma; d } coeff

    let add p1 p2 =
      PMappe.combine None None
        (fun d1 c1 -> Some(c1))
        (fun d2 c2 -> Some(c2))
        (fun d c1 c2 ->
          let c = Field.add c1 c2 in
          if Field.is_zero c then None else Some c)
        p1 p2

    let sub p1 p2 =
      PMappe.combine None None
        (fun d1 c1 -> Some(c1))
        (fun d2 c2 -> Some(Field.neg c2))
        (fun d c1 c2 ->
          let c = Field.sub c1 c2 in
          if Field.is_zero c then None else Some c)
        p1 p2

    let scale factor p =
      if Field.is_zero factor
      then zero
      else PMappe.map (fun d c -> Field.mul factor c) p

    let normalize p =
      let maxdegree,maxcoeff = PMappe.max_binding p in
      if F.equal maxcoeff F.one then (Field.one,p)
      else
        let p = PMappe.map
          (fun _ coeff -> Field.(coeff / maxcoeff))
          p
        in
        (maxcoeff,p)

    let deriv p =
      PMappe.fold
        (fun d c acc ->
          if Field.is_zero d.gamma then
            if d.d = 0
            then acc
            else PMappe.add { d with d=d.d-1 } (Field.mul_int c d.d) acc
          else
            let acc = PMappe.add d (Field.mul c d.gamma) acc in
            if d.d=0
            then acc
            else PMappe.add { d with d=d.d-1 } (Field.mul_int c d.d) acc
        )
        p

    let degree_eval d t =
      Field.((exp (d.gamma * t))*pow_int t d.d)

    let eval p t =
      PMappe.fold
        (fun degree coeff acc ->
          Field.(coeff*(degree_eval degree t) + acc))
        p Field.zero

    let degree_minT_greater_or_equal ?(ratio=Field.two) degree bound =
      assert(degree_cmp degree degree_zero > 0);
      let sign =  Field.sign d.gamma in
      if sign>0 then
        let t0 = if degree.d>=0
          then ratio
          else Field.(ratio * (int_div (-degree.d) degree.gamma))
        in
        Field.(t0 +
                 (max zero (bound - degree_eval degree t0)) /
                 (eval (deriv @@ of_degree degree) t0))
      else
        Field.(pow bound (1/(of_int degree.d)))

    let minT_cst_sign ?(ratio=Field.two) p =
      let maxdegree,maxcoeff = PMappe.max_binding p in
      let p = PMappe.remove maxdegree p in
      let cardinal = PMappe.cardinal p in
      let maxcoeff = Field.abs maxcoeff in
      Mappe.fold
          (begin degree coeff t ->
            let coeff = Field.abs coeff in
            Field.(max t
                     (degree_minT ~ratio
                        (degree_sub maxdegree degree)
                        (div_int ratio cardinal)))
           end)
          p Field.zero

    let bound_by_enumeration (low,high) p =
      if low>high then F.(infty,neg_infty)
      else begin
        let minv = ref (eval p (F.of_int low))  in
        let maxv = ref !minv in
        for i=low+1 to high do
          let v = eval p (F.of_int i) in
          if F.compare v !minv < 0 then minv := v
          else if F.compare v !maxv > 0 then maxv :=v
        done;
        (!minv, !maxv)
      end

    let bound_on_integer_range ?ratio (low,ohigh) p =
      let n0 = F.to_int @@ F.round `UP (minT_cst_sign ?ratio p) in
      let pmin = eval p low in
      let pmax = eval p (match ohigh with
        | Some n -> F.of_int n
        | None -> F.infinity)
      in
      let (pmin2,pmax2) = bound_by_enumeration 
        (low+1,
         (min n0 (match ohigh with Some n -> n-1 | None -> max_int)))
        p
      in
      F.(
        min pmin2 (min pmin pmax),
        max pax2 (max pmin pmax)
      )

end
