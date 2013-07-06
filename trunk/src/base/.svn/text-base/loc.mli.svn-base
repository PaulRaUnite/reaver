(** framework base: locations of control flow graphs *)

type t = Env.boolexpr_t

(** {2 Printing} *)

(** print location *)
val print : Env.t -> Format.formatter -> t -> unit

(** {2 Operations} *)

(** create location with the given location definition (invariant) *)
val make_inv : Env.boolexpr_t -> t

(** get location definition (invariant) *)
val get_inv : t -> Env.boolexpr_t

(** refine location definition by the given expression *)
val refine_inv : Env.t -> t -> Env.boolexpr_t -> t

