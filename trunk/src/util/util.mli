(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** utilities *)

(** {2 Camllib conversions} *)

val mappe2keylist : ('a, 'b) Mappe.t -> 'a list
val mappe2list : ('a, 'b) Mappe.t -> ('a * 'b) list
val mappe2lists : ('a, 'b) Mappe.t -> 'a list * 'b list

val pmappe2keylist : ('a, 'b) PMappe.t -> 'a list
val pmappe2valuelist : ('a, 'b) PMappe.t -> 'b list

val list2pmappe : ('a -> 'a -> int) -> ('a * 'b) list -> ('a, 'b) PMappe.t
val list2mappe : ('a * 'b) list -> ('a, 'b) Mappe.t
val list2psette : ('b -> 'b -> int) -> 'b list -> 'b PSette.t
val list2sette : 'b list -> 'b Sette.t

val array2psette : ('b -> 'b -> int) -> 'b array -> 'b PSette.t
val array2sette : 'b array -> 'b Sette.t

val psette_map : ('a -> 'b) -> ('b ->'b -> int) -> 'a PSette.t -> 'b PSette.t 
val psette2list : 'a PSette.t -> 'a list

(** computes the set of equivalence classes induced by the given equivalence
    relation *)
val sette2eqclasses : 'a Sette.t -> ('a -> 'a -> bool) -> 'a Sette.t Sette.t

(** computes the set of equivalence classes induced by the given equivalence
    relation *)
val psette2eqclasses : 'a PSette.t  -> ('a -> 'a -> int) -> ('a -> 'a -> bool) -> 'a PSette.t PSette.t

(** {2 List utilities} *)

val list_is_empty : 'a list -> bool
val list_compress : 'a list -> 'a list
val list_getminmax : ('a -> 'a -> int) -> 'a list -> 'a * 'a
val list_diff : 'a list -> 'a list -> 'a list
val list_inter : 'a list -> 'a list -> 'a list
val list_equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val list_product : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val list_filter2 :  ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list * 'b list
val list_iteri2 : (int -> 'a -> 'b -> 'c) -> 'a list -> 'b list -> unit
val list_fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val list_print : ?csep:string -> ?copen:string -> ?cclose:string ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val assoclist_find_key : ('a * 'b) list -> 'b -> 'a


(** {2 Array utilities} *)

val array_split : ('a * 'b) array -> 'a array * 'b array
val array_mem : 'a -> 'a array -> bool
val array_diff : 'a array -> 'a array -> 'a array
val array_map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
val array_iter2 : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit
val array_exists : ('a -> bool) -> 'a array -> bool
val array_exists2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
val array_print : ?csep:string -> ?copen:string -> ?cclose:string ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a array -> unit
val array_union :  'a array -> 'a array -> 'a array
val array_inter :  'a array -> 'a array -> 'a array
val array_get_index_of :  'a -> 'a array -> int
val array_fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a

(** {2 Hashtbl utilities} *)

val hashtbl_to_list : ('a,'b) Hashtbl.t -> ('a * 'b) list
val hashtbl_to_array : ('a * 'b) -> ('a,'b) Hashtbl.t -> ('a * 'b) array
val hashtbl_print : ?csep:string -> ?copen:string -> ?cmapto:string -> ?cclose:string -> (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) -> Format.formatter -> ('a,'b) Hashtbl.t -> unit

(** {2 String utilities} *)

(** removes the occurrences of a given string from another string *)
val string_remove : string -> string -> string

(** removes whitespaces *)
val string_remove_whitespaces : string -> string

(** replaces multiple whitespaces and newlines by a single space *)
val string_compact : string -> string

(** {2 Printing utilities} *)

(** prints a string such that linebreaks happen at whitespaces *)
val print_breakable : Format.formatter -> string -> unit

(** prints a string such of a fixed length 
    (truncates or fills up with whitespaces *)
val print_fixed : Format.formatter -> int -> string -> unit



