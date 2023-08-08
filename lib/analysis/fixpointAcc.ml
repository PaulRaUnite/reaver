(******************************************************************************)
(* FixpointAcc *)
(* extension of fixpoint library for acceleration *)
(* author: Peter Schrammel *)
(* version: 0.9.3 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

(* strategy flattening dependent on parent component *)
let flattenp ~(depth:int)
    ~(flatten:(parent:'a -> ('a,'b) Ilist.t -> bool))
    (ilist:('a,'b) Ilist.t) : ('a,'b) Ilist.t
  =
  let rec rev_flatten (res:('a,'b) Ilist.el list) (cdepth:int) (parent:'a) 
     (ilist:('a,'b) Ilist.el list) : ('a,'b) Ilist.el list = 
    match ilist with
    | [] -> res
    | x::l ->
	let nres = begin match x with
	| Ilist.Atome(_) -> x::res
	| Ilist.List(b,l2) ->
	    if (depth<=cdepth) && (flatten ~parent (b,l2)) then
	      rev_flatten res (cdepth+1) b l2
	    else
	      let nres2 = rev_flatten [] (cdepth+1) b l2 in
	      (Ilist.List(b,nres2))::res
	end
	in
	rev_flatten nres cdepth parent l
  in
  let (b,l) = ilist in 
  let res = rev_flatten [] 1 b l in
  Ilist.rev (b,res)

(* copied from FixpointType because not made public there *)
let make_strategy_vertex
    ?priority
    (graph:('vertex,'hedge,'a,'b,'c) PSHGraph.t)
    (widen:bool)
    (vertex:'vertex)
    :
    ('vertex,'hedge) FixpointType.strategy_vertex
    =
  let spredhedges = PSHGraph.predhedge graph vertex in
  let hedges =
    PSette.fold
      (begin fun hedge res ->
	let takeit = match priority with
	  | None -> true
	  | Some(PSHGraph.Filter filter) -> filter hedge
	  | Some(PSHGraph.Priority p) -> (p hedge)>=0
	in
	if takeit then hedge::res else res
      end)
      spredhedges []
  in
  let strategy_vertex = {
    FixpointType.vertex = vertex;
    FixpointType.hedges = hedges;
    FixpointType.widen = widen
  }
  in
  strategy_vertex

(* patch to camllib Ilist.map  implementation *)
let ilist_map 
    (fb: bool -> ('b,'a) Ilist.t -> 'c) 
    (fa:bool -> 'a -> 'd) 
    (ilist:('b,'a) Ilist.t) 
    : 
    ('c,'d) Ilist.t =
  let rec parcours flag = function
    | [] -> []
    | Ilist.Atome(a)::l ->
	Ilist.Atome(fa flag a)::(parcours false l)
    | Ilist.List(b,l2)::l ->
        let l3 = parcours true l2 in
        let b3 = fb false (b,l2) in
	Ilist.List(b3,l3)::(parcours false l)
  in
  let (b,l) = ilist in 
  (fb true (b,l),parcours false l)

(* strategy with widening starts dependent on whether the component is accelerable or not *)
let make_strategy_widening_start
    ?(depth=2)
    ?priority
    ~(widening_start:((unit,'vertex) Ilist.t -> int))
    ~(widening_descend:int)
    ~(flatten:(parent:FixpointType.strategy_iteration -> 
                        ('vertex, 'hedge) Fixpoint.strategy -> bool))
    ~(vertex_dummy:'vertex)
    ~(hedge_dummy:'hedge)
    (graph:('vertex,'hedge,'a,'b,'c) PSHGraph.t)
    (sinit:'vertex PSette.t)
    :
    ('vertex,'hedge) Fixpoint.strategy
    =
  let scfc =
    PSHGraph.scfc_multi
      vertex_dummy hedge_dummy
      ?priority graph sinit
  in
  let scfc : ('vertex,'hedge) Fixpoint.strategy = ilist_map
    (fun level0 l -> if level0 then FixpointType.make_strategy_iteration
              ~widening_start:0 ()
       else FixpointType.make_strategy_iteration 
              ~widening_start:(widening_start l) ())
    (fun flag vertex -> make_strategy_vertex graph flag vertex)
    scfc
  in
  flattenp ~depth ~flatten scfc
