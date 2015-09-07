
type t = {
  tlinconsx: Apron.Lincons0.t array;
  tdim: Apron.Dim.t array;
  tlinexpr0: Apron.Linexpr0.t array;
  tlingen0: Apron.Generator0.t array;
}

val print : Format.formatter -> t -> unit
val of_jordan : apron:'a Apron.Manager.t -> 'b Trans.abstrans -> t
val apply : 'a Apron.Manager.t -> 'a Apron.Abstract0.t -> t -> 'a Apron.Abstract0.t
