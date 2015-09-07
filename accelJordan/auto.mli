type assign = Apron.Lincons0.t array * int array * Apron.Linexpr0.t array
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

val of_syntax :
  logtemplate:int -> apron:Polka.loose Polka.t Apron.Manager.t -> Syntax.t -> arc t
val sgraph_of_graph : arc t -> [ `Assign of assign ] t
val jgraph_of_graph : arc t -> [ `Assign of assign | `Jordan of jordan ] t
val dgraph_of_graph :
  apron:Polka.loose Polka.t Apron.Manager.t -> arc t -> [ `Assign of assign | `Derivative of Derivative.t ] t
val analysis :
  apron:Polka.loose Polka.t Apron.Manager.t ->
  [< allarc] t ->
  (vertex, hedge, Polka.loose Polka.t Apron.Abstract0.t, unit) Fixpoint.output
