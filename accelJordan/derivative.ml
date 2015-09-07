
open Format

type t = {
  tlinconsx: Apron.Lincons0.t array;
  tdim: Apron.Dim.t array;
  tlinexpr0: Apron.Linexpr0.t array;
  tlingen0: Apron.Generator0.t array;
}

let print fmt
    {
      tlinconsx;
      tdim;
      tlinexpr0;
      tlingen0
    }
    =
  let string_of_dim i = sprintf "x%i" i in
  fprintf fmt
    "{@   @[<v>tlinconsx=%a@ tdim=%a@ tlinexpr0=%a@ tlingen0=%a@]@ }"
    (Print.array (Apron.Lincons0.print string_of_dim)) tlinconsx
    (Print.array pp_print_int) tdim
    (Print.array (Apron.Linexpr0.print string_of_dim)) tlinexpr0
    (Print.array (Apron.Generator0.print string_of_dim)) tlingen0

let of_jordan ~apron abstrans =
  let tlinconsx = Trans.(abstrans.guardtrans.tlinconsx) in
  let tdim = Trans.(abstrans.guardtrans.trans.tdim) in
  let tlinexpr0 = Trans.(abstrans.guardtrans.trans.tlinexpr0) in
  let tlingen0 =
    Apron.(
      let nbdims = Array.length tdim in
      let mat = Array.map (Matrix.Mpqf.row_of_linexpr_affine ~nbdims:(nbdims+1)) tlinexpr0 in
      Array.iteri
	(begin fun i row ->
	  row.(i) <- Matrix.FMpqf.sub row.(i) (Matrix.FMpqf.of_int 1)
	 end)
	mat;
      let tlinexpr0 = Matrix.Mpqf.to_tlinexpr_affine mat in
      let top = Abstract0.top apron 0 nbdims in
      let image = Abstract0.assign_linexpr_array apron top tdim tlinexpr0 None in
      let tlingen0 = Abstract0.to_generator_array apron image in
      let tlingen0 =
	Array.map
	  Generator0.(begin fun lingen0 ->
	    match lingen0.typ with
	    | VERTEX -> { lingen0 with typ=RAY }
	    | _ -> lingen0
	  end)
	  tlingen0
      in
      let tlincons0 =
	Array.init nbdims
	  (fun dim ->
	    let linexpr0 = Linexpr0.make None in
	    Linexpr0.set_coeff linexpr0 dim (Coeff.s_of_int 1);
	    Lincons0.make linexpr0 Lincons0.EQ
	  )
      in
      let poly = Abstract0.meet_lincons_array apron top tlincons0 in
      let poly = Abstract0.add_ray_array apron poly tlingen0 in
      let tlingen0 = Abstract0.to_generator_array apron poly in
      Array.of_list
	(List.filter
	   Generator0.(fun lingen0 -> lingen0.typ<>VERTEX)
	   (Array.to_list tlingen0)
	)
    )
  in
  { tlinconsx; tdim; tlinexpr0; tlingen0 }

let apply man poly0 { tlinconsx; tdim; tlinexpr0; tlingen0 } =
  Apron.(
    let poly1 = Abstract0.meet_lincons_array   man poly0 tlinconsx in
    let poly2 = Abstract0.add_ray_array        man poly1 tlingen0 in
    let poly3 = Abstract0.meet_lincons_array   man poly2 tlinconsx in
    let poly4 = Abstract0.assign_linexpr_array man poly3 tdim tlinexpr0 None in
    Abstract0.join man poly0 poly4
  )
