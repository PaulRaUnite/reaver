val make_strategy_widening_start :
  ?depth:int ->
  ?priority:'hedge PSHGraph.priority ->
  widening_start:((unit,'vertex) Ilist.t -> int) ->
  widening_descend:int ->
  flatten:(parent:FixpointType.strategy_iteration -> 
              ('vertex, 'hedge) Fixpoint.strategy -> bool) ->
  vertex_dummy:'vertex ->
  hedge_dummy:'hedge ->
  ('vertex, 'hedge, 'e, 'f, 'g) PSHGraph.t -> 'vertex PSette.t ->
  ('vertex, 'hedge) Fixpoint.strategy
     (**
  
	Build a strategy which allows to define the widening_start value
        for each strongly connected component (SCC) individually.
        
        - depth: as in [make_strategy_default]
        - priority: as in [make_strategy_default]

        - widening_start: assigns widening_start to each SCC
        - flatten: SCCs are only flattened if they are below level depth and if the predicate flatten evaluates to true. The parameter parent is the value of the predicate evaluated over the englobing SCC.
     *)
