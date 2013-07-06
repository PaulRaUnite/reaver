(** transformation implementation: transformations for abstract acceleration *)

(** decoupling mode: no decoupling, decoupling of Boolean and
    accelerable equations, decoupling of Boolean/non-accelerable and
    accelerable equations *)
type decoupling_mode_t = DecoupleNone | DecoupleBool | DecoupleBoolNacc

(** remove Boolean inputs and classify accelerable self-loops *)
val remove_bool_inputs : decoupling_mode_t -> VerifUtil.trans_t

(** split accelerable self-loops into convex guards *)
val split_arcs : VerifUtil.trans_t

(** decouple accelerable self-loops *)
val decouple_equs : VerifUtil.trans_t

(** flatten (resp. expand multiple) accelerable self-loops *)
val flatten_loops : VerifUtil.trans_t

(** inputize Boolean decoupled self-loops *)
val inputize : VerifUtil.trans_t
