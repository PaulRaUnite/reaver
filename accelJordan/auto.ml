open Format
open Syntax
open Apron

type assign = Apron.Lincons0.t array * Apron.Dim.t array * Apron.Linexpr0.t array
type jordan = Polka.loose Trans.abstrans

type vertex = string
type hedge = int
type attr = unit
type arc = [
  | `Assign of assign
  | `Jordan of jordan
]
type allarc = [
| `Assign of assign
| `Derivative of Derivative.t
| `Jordan of jordan
]

type info = {
  nbdims: int;
  env: Apron.Environment.t;
  init: vertex;
  id_hedge: int ref;
}
type 'a t = (vertex,hedge,attr,'a,info) PSHGraph.t

(*  ********************************************************************** *)
(** {2 Conversion from Syntax} *)
(*  ********************************************************************** *)

let linexpr0_of_linexpr ~env linexpr =
  if false then printf "linexpr0_of_linexpr@.%a@."
    (Print.list (fun fmt (name,coeff) -> fprintf fmt "(%a,%s)" Apron.Coeff.print coeff name))
    linexpr;
  let linexpr0 = Linexpr0.make None in
  List.iter
    (begin fun (var,coeff) ->
      if var="" then
	Linexpr0.set_cst linexpr0 coeff
      else begin
	let var = Var.of_string var in
	let dim = Environment.dim_of_var env var in
	Linexpr0.set_coeff linexpr0 dim coeff
      end
     end)
    linexpr
  ;
  linexpr0

let lincons0_of_lincons ~env (typ,linexpr) =
  Lincons0.make (linexpr0_of_linexpr ~env linexpr) typ

let arc_of_syntax ~env ~logtemplate ~(apron:Polka.loose Polka.t Apron.Manager.t) (t:Syntax.guardedtransformation) : arc
    =
  let (guard,(lvarexpr,jordan)) = t in
  let tlincons0 = Array.map (lincons0_of_lincons ~env) guard in
  let ldimlinexpr0 =
    List.rev_map
      (begin fun (var,expr) ->
	let var = Apron.Var.of_string var in
	let dim = Environment.dim_of_var env var in
	let linexpr0 = linexpr0_of_linexpr ~env expr in
	(dim,linexpr0)
       end)
      lvarexpr
  in
  let ldimlinexpr0  = List.sort (fun (dim1,_) (dim2,_) -> dim2-dim1) ldimlinexpr0 in
  let (ldim,llinexpr0) =
    List.fold_left
      (begin fun (ldim,llinexpr0) (dim,linexpr0) ->
	(dim::ldim, linexpr0::llinexpr0)
       end)
      ([],[]) ldimlinexpr0
  in
  let tdim = Array.of_list ldim and tlinexpr0 = Array.of_list llinexpr0 in
  let ntrans = match jordan with
  | `None ->
    `Assign (tlincons0, tdim, tlinexpr0)
  | `Jordan(sinv_yx,cjordany,s_xy) ->
    let (sinv_yx,rjordany,s_xy) = Jordan.real_of_complex ~sinv_yx ~jordany:cjordany ~s_xy in
    let sinv_yx = Matrix.mpqf_of_d sinv_yx in
    let s_xy = Matrix.mpqf_of_d s_xy in
    let trans = Trans.trans_of_jordan ~tdim ~tlinexpr0 ~s_xy ~sinv_yx ~jordan:rjordany in
    let guardtrans = Trans.guardtrans_of_trans_tlincons0 trans tlincons0 in
    let ltemplate = Template.ltemplate ~nbcoeffs:(Jordan.nbcoeffs rjordany) logtemplate in
    let abstrans = Template.abstrans_of_ltemplate ~guardtrans ~apron ltemplate in
    `Jordan abstrans
  | `Jordansage(sinv_yx,cjordany,s_xy) ->
    let rjordany = Jordan.real_of_complexsage ~sinv_yx ~jordany:cjordany ~s_xy in
    let sinv_yx = Matrix.mpqf_of_d sinv_yx in
    let s_xy = Matrix.mpqf_of_d s_xy in
    let trans = Trans.trans_of_jordan ~tdim ~tlinexpr0 ~s_xy ~sinv_yx ~jordan:rjordany in
    let guardtrans = Trans.guardtrans_of_trans_tlincons0 trans tlincons0 in
    let ltemplate = Template.ltemplate ~nbcoeffs:(Jordan.nbcoeffs rjordany) logtemplate in
    let abstrans = Template.abstrans_of_ltemplate ~guardtrans ~apron ltemplate in
    `Jordan abstrans
  in
  ntrans

let graph_of_syntax ~logtemplate ~(apron:Polka.loose Polka.t Apron.Manager.t) ~info (ltrans:Syntax.transition list) : arc t =
  let graph = PSHGraph.create PSHGraph.stdcompare 23 info in
  let id = info.id_hedge in
  List.iter
    (begin fun (org,dest,transf) ->
      if not (PSHGraph.is_vertex graph org) then
	PSHGraph.add_vertex graph org ();
      if not (PSHGraph.is_vertex graph dest) then
	PSHGraph.add_vertex graph dest ();
      let arc = arc_of_syntax ~env:info.env ~logtemplate ~apron transf in
      begin match arc with
      | `Jordan _ ->
	if org<>dest then failwith "Error: Jordan spec allowed only on self-loops";
      | _ ->
	()
      end;
      PSHGraph.add_hedge graph !id arc ~pred:[|org|] ~succ:[|dest|];
      incr id
     end)
    ltrans
  ;
  graph

let info_of_syntax (sys:Syntax.t) : info =
  let (lvars,init,ltransitions) = sys in
  let tvars = Array.of_list lvars in
  let tvars = Array.map Var.of_string tvars in
  let nbdims = Array.length tvars in
  let env = Environment.make [||] tvars in
  { nbdims; env; init; id_hedge = ref 0 }

let of_syntax ~logtemplate ~(apron:Polka.loose Polka.t Apron.Manager.t) (sys:Syntax.t) : arc t =
  let info = info_of_syntax sys in
  let (_,_,ltransitions) = sys in
  graph_of_syntax ~logtemplate ~apron ~info ltransitions

(*  ********************************************************************** *)
(** {2 Graph transformation} *)
(*  ********************************************************************** *)

let sgraph_of_graph (graph:arc t) : [`Assign of assign] t =
  PSHGraph.map graph
    (begin fun vertex attr -> attr end)
    (begin fun hedge arc ->
      match arc with
      | (`Assign _) as x -> x
      | `Jordan abstrans->
	  let tlinconsx = Trans.(abstrans.guardtrans.tlinconsx) in
	  let tdim = Trans.(abstrans.guardtrans.trans.tdim) in
	  let tlinexpr0 = Trans.(abstrans.guardtrans.trans.tlinexpr0) in
	  `Assign(tlinconsx,tdim,tlinexpr0)
     end)
    (fun x -> x)

let accgraph_of_graph (map_jordan:jordan -> [<allarc]) (graph:arc t) =
  let info = PSHGraph.info graph in
  let ngraph = PSHGraph.create graph.PSHGraph.compare 23 info in
  let id_hedge = info.id_hedge in
  (* We copy vertices *)
  PSHGraph.iter_vertex graph
    (fun vertex attr ~pred ~succ -> PSHGraph.add_vertex ngraph vertex attr)
  ;
  (* We iterate on hedges, with the exception of self-loops *)
  let id = ref min_int in
  PSHGraph.iter_hedge graph
    (begin fun hedge arc ~pred ~succ ->
      assert((Array.length pred) = 1);
      assert((Array.length succ) = 1);
      id := max !id hedge;
      if pred.(0)<>succ.(0) then begin
	begin match arc with
	| (`Assign _) as x -> PSHGraph.add_hedge ngraph hedge x ~pred ~succ
	| _ -> assert false
	end
      end
     end);
  (* We look for vertices with self-loops *)
  PSHGraph.iter_vertex graph
    (begin fun vertex attr ~pred ~succ ->
      let setloop = PSette.inter pred succ in
      let incoming = PSette.diff pred setloop in
      let outgoing = PSette.diff succ setloop in
      let cardinal = PSette.cardinal setloop in
      if cardinal=1 then begin (* single loop *)
	let hedge = PSette.choose setloop in
	let pred = PSHGraph.predvertex graph hedge in
	let succ = PSHGraph.succvertex graph hedge in
	assert((Array.length pred) = 1);
	assert((Array.length succ) = 1);
	assert(pred.(0)=vertex);
	assert(succ.(0)=vertex);
	let arc = PSHGraph.attrhedge graph hedge in
	match arc with
	| (`Assign _) as x -> PSHGraph.add_hedge ngraph hedge x ~pred ~succ
	| `Jordan jordan ->
	  (* duplicate vertex *)
	  let svtx = vertex^"_start" in
	  PSHGraph.add_vertex ngraph svtx ();
	      (* redirect incoming edges from vtx to svtx *)
	  let predhedges = PSHGraph.predhedge ngraph vertex in
	  PSette.iter
	    (begin fun hedge ->
	      let arc = PSHGraph.attrhedge ngraph hedge in
	      let pred = PSHGraph.predvertex ngraph hedge in
	      PSHGraph.remove_hedge ngraph hedge;
	      PSHGraph.add_hedge ngraph hedge arc ~pred ~succ:[|svtx|]
	     end)
	    predhedges
	  ;
	  (* add transition from svtx to vtx *)
	  PSHGraph.add_hedge ngraph hedge ~pred:[|svtx|] ~succ (map_jordan jordan)
      end
      else if cardinal>1 then begin (* several loops *)
	PSHGraph.remove_vertex ngraph vertex;
	let tvtx = Array.init cardinal
	  (fun i ->
	    let nvtx = vertex^"_l"^(string_of_int i) in
	    PSHGraph.add_vertex ngraph nvtx ();
	    nvtx)
	in
	Array.iter
	  (fun vtx ->
	    PSette.iter
	      (begin fun hedge ->
		let pred = PSHGraph.predvertex graph hedge in
		let arc = PSHGraph.attrhedge graph hedge in
		match arc with
		| (`Assign _) as x -> PSHGraph.add_hedge ngraph (incr id_hedge; !id_hedge) x ~pred ~succ:[|vtx|]
		| _ -> assert false
	       end)
	      incoming;
	    PSette.iter
	      (begin fun hedge ->
		let succ = PSHGraph.succvertex graph hedge in
		let arc = PSHGraph.attrhedge graph hedge in
		match arc with
		| (`Assign _) as x -> PSHGraph.add_hedge ngraph (incr id_hedge; !id_hedge) x ~pred:[|vtx|] ~succ
		| _ -> assert false
	       end)
	      outgoing;
	  )
	  tvtx
	;
	let i = ref (-1) in
	PSette.iter
	  (fun hedge ->
	    incr i;
	    let arc = PSHGraph.attrhedge graph hedge in
	    for j=0 to cardinal-1 do
	      if j <> !i then
		match arc with
		| `Jordan jordan ->
		  PSHGraph.add_hedge ngraph (incr id_hedge; !id_hedge) (map_jordan jordan) ~pred:[|tvtx.(j)|] ~succ:[|tvtx.(!i)|]
		| _ -> assert false
	    done)
	  setloop;
      end
     end)
  ;
  ngraph

let jgraph_of_graph graph
    : [`Assign of assign | `Jordan of jordan] t
    =
  accgraph_of_graph (fun x -> `Jordan (Trans.abstrans_copy x)) graph

let dgraph_of_graph ~apron graph
    : [`Assign of assign | `Derivative of Derivative.t] t
    =
  accgraph_of_graph (fun x -> `Derivative(Derivative.of_jordan ~apron x)) graph



(*  ********************************************************************** *)
(** {2 Analysis} *)
(*  ********************************************************************** *)

let string_of_dim env dim =
  Var.to_string (Environment.var_of_dim env dim)

let print_tdimtlinexpr0 env fmt (tdim,tlinexpr) =
  let tab = Array.init (Array.length tdim) (fun i -> (tdim.(i),tlinexpr.(i))) in
  Print.array ~first:"@[<hv>" ~sep:";@ " ~last:"@]"
    (fun fmt (dim,linexpr) ->
      fprintf fmt "%s := %a"
	(string_of_dim env dim)
	(Linexpr0.print (string_of_dim env)) linexpr)
    fmt
    tab

let make_fixpoint ~apron (graph: ([<`Assign of assign | `Jordan of jordan | `Derivative of Derivative.t] as 'a) t) =
  let info = PSHGraph.info graph in
  FixpointType.(
    {
      bottom = begin fun vertex -> Abstract0.bottom apron 0 info.nbdims end;
      canonical = begin fun vertex abs -> () end;
      is_bottom = begin fun vertex abs -> Abstract0.is_bottom apron abs end;
      is_leq = begin fun vertex abs1 abs2 -> Abstract0.is_leq apron abs1 abs2 end;
      join = begin fun vertex abs1 abs2 -> Abstract0.join apron abs1 abs2 end;
      join_list = begin fun vertex labs -> Abstract0.join_array apron (Array.of_list labs) end;
      widening = begin fun vertex abs1 abs2 -> Abstract0.widening apron abs1 abs2 end;
      odiff = None;
      (* Some begin fun vertex abs1 abs2 ->
	 if Abstract0.is_leq apron abs1 abs2 then
	 Abstract0.bottom apron 0 info.nbdims
	 else
	 abs1
	 end;
      *)
      abstract_init = begin fun vertex -> Abstract0.top apron 0 info.nbdims end;
      arc_init = begin fun hedge -> () end;
      apply = begin fun hedge tabs ->
	let (arc:'a) = PSHGraph.attrhedge graph hedge in
	let abs = tabs.(0) in
	let res = match arc with
	| `Assign (assign:assign) ->
	    let (tlincons0,tdim,tlinexpr0) = (assign:assign) in
	    if false then begin
	      Apron.Abstract0.canonicalize apron abs;
	      let string_of_dim = Apronaux.string_of_dimx in
	      printf "Assign @[<v>abs:%a@ tlincons0:%a@ tdim:%a@ tlinexpr0:%a@ "
		(Apronaux.print_abstract0 ~string_of_dim) abs
		(Print.array (Apronaux.print_lincons0 ~string_of_dim)) tlincons0
		(Print.array pp_print_int) tdim
		(Print.array (Apronaux.print_linexpr0 ~string_of_dim)) tlinexpr0;
	    end;
	    if false then begin
	      let intern = Polka.manager_get_internal (Obj.magic apron) in
	      Polka.set_approximate_max_coeff_size intern 2;
	      let abs = Apron.Abstract0.copy apron abs in
	      Apron.Abstract0.approximate apron abs 10;
	      let string_of_dim = Apronaux.string_of_dimx in
	      printf "abs2=%a@ "
		(Apronaux.print_abstract0 ~string_of_dim) abs
	      ;
	      let res =
		if tlincons0<>[||]
		then Apron.Abstract0.meet_lincons_array apron abs tlincons0
		else abs
	      in
	      if false then
		printf "res1=%a@ "
		  (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx) res
	      ;
	      let res =
		if tdim<>[||]
		then Abstract0.assign_linexpr_array apron res tdim tlinexpr0 None
		else res
	      in
	      if false then
		printf "res2=%a@]@ "
		  (Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx) res
	      ;
	    end;
	    let res =
	      if tlincons0<>[||]
	      then Apron.Abstract0.meet_lincons_array apron abs tlincons0
	      else abs
	    in
	    if false then
	      printf "res1=%a@ "
		(Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx) res
	    ;
	    let res =
	      if tdim<>[||]
	      then Abstract0.assign_linexpr_array apron res tdim tlinexpr0 None
	      else res
	    in
	    if false then
	      printf "res2=%a@]@ "
		(Apronaux.print_abstract0 ~string_of_dim:Apronaux.string_of_dimx) res
	    ;
	    res
	| `Jordan (abstrans:jordan) ->
	    Template.refine_abstrans_with_poly_guard
	      ~apron
	      ~abstrans
	      ~poly:abs;
	    Trans.abstrans_apply apron abs abstrans
	| `Derivative (t:Derivative.t) ->
	    Derivative.apply apron abs t
	in
	((),res)
      end;
      print_vertex = pp_print_string;
      print_hedge = pp_print_int;
      print_abstract = begin fun fmt abs ->
	Apronaux.print_abstract0 ~string_of_dim:(string_of_dim info.env) fmt abs
      end;
      print_arc = begin fun fmt _ -> pp_print_string fmt "()" end;
      accumulate = true;
      print_fmt = std_formatter;
      print_analysis = !Option.debug >= 1;
      print_component = !Option.debug >= 2;
      print_step = !Option.debug >= 3;
      print_state = !Option.debug >= 4;
      print_postpre = !Option.debug >= 5;
      print_workingsets = !Option.debug >= 6;
      dot_fmt = !Option.dot_fmt;
      dot_vertex = pp_print_string;
      dot_hedge = pp_print_int;
      dot_attrvertex = pp_print_string;
      dot_attrhedge = begin fun fmt hedge ->
	let (arc:'a) = PSHGraph.attrhedge graph hedge in
	fprintf fmt "@[<hv>";
	begin match arc with
	| `Assign ((tlincons0,tdim,tlinexpr0):assign) ->
	    if tlincons0<>[||] then
	      fprintf fmt "%a ?@ "
		(Print.array ~first:"@[<hov>" ~sep:" and@ " ~last:"@]"  (Lincons0.print (string_of_dim info.env)))
		tlincons0
	    ;
	    fprintf fmt "%a@]" (print_tdimtlinexpr0 info.env) (tdim,tlinexpr0)
	| `Jordan (abstrans:jordan) ->
	    let (tlincons0,tdimtlinexpr0) = Trans.(
	      let gt = abstrans.guardtrans in
	      (gt.tlinconsx, (gt.trans.tdim, gt.trans.tlinexpr0))
	    )
	    in
	    if tlincons0<>[||] then
	      fprintf fmt "(%a ?@ "
		(Print.array ~first:"@[<hov>" ~sep:" and@ " ~last:"@]"  (Lincons0.print (string_of_dim info.env)))
		tlincons0
	    ;
	    fprintf fmt "%a)*@]" (print_tdimtlinexpr0 info.env) tdimtlinexpr0
	| `Derivative (t:Derivative.t) ->
	    Derivative.(
	      let { tlinconsx; tdim; tlinexpr0; _ } = t in
	      if tlinconsx<>[||] then
		fprintf fmt "(%a ?@ "
		  (Print.array ~first:"@[<hov>" ~sep:" and@ " ~last:"@]"  (Lincons0.print (string_of_dim info.env)))
		  tlinconsx
	      ;
	      fprintf fmt "%a)*@]" (print_tdimtlinexpr0 info.env) (tdim,tlinexpr0)
	    )
	end
      end;
    }
  )

let analysis ~apron graph =
  let info = PSHGraph.info graph in
  let sinit = PSette.singleton PSHGraph.stdcompare.PSHGraph.comparev info.init in
  let fixpoint = make_fixpoint ~apron graph in
  let time = ref 0.0 in
  let output =
    Time.wrap_duration time
      (begin fun () ->
	if !Option.iteration_guided then
	  Fixpoint.analysis_guided
	    fixpoint graph sinit
	    (fun filter ->
	      Fixpoint.make_strategy_default
		~depth:!Option.iteration_depth
		~widening_start:!Option.widening_start
		~widening_descend:!Option.widening_descend
		~priority:(PSHGraph.Filter filter)
		~vertex_dummy:""
		~hedge_dummy:min_int
		graph sinit
	    )
	else
	  Fixpoint.analysis_std
	    fixpoint graph sinit
	    (Fixpoint.make_strategy_default
	       ~depth:!Option.iteration_depth
	       ~widening_start:!Option.widening_start
	       ~widening_descend:!Option.widening_descend
	       ~vertex_dummy:""
	       ~hedge_dummy:min_int
	       graph sinit
	    )
       end)
  in
  if !Option.debug>=1 then printf "Time = %f@." !time;
  output
