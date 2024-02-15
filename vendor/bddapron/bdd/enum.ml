(** Enumerated expressions with BDDs *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Env

(*  ********************************************************************** *)
(** {3 Types} *)
(*  ********************************************************************** *)

type 'a typ = [
  `Benum of 'a
]
  (** A type is just a name *)

type 'a typdef = [
  `Benum of 'a array
]
  (** An enumerated type is defined by its (ordered) set of labels *)

(** {4 Datatype representing a BDD register of enumerated type} *)

type 'a t = {
  typ: string;
    (** Type of the value (refers to the database, see below) *)
  reg: 'a Reg.t
    (** Value itself *)
}

type dt = Cudd.Man.d t
type vt = Cudd.Man.v t

(*  ====================================================================== *)
(** {4 Associations} *)
(*  ====================================================================== *)

let labels_of_typ (env:('a,'b,'c,'d,'e) Env.O.t) typ : 'a array
  =
  let typdef = PMappe.find typ env.typdef in
  begin match typdef with
  | `Benum tlabel -> tlabel
  | _ ->
      failwith (Print.sprintf "Bddenum.labels_of_typ: type %a not defined as an enumerated type" env.symbol.print typ)
  end

let size_of_typ env (typ:'b) : int
  =
  let labels = labels_of_typ env typ in
  let nb = Array.length labels in
  Reg.min_size (nb-1)

let maxcode_of_typ env (typ:'b) : int
  =
  let labels = labels_of_typ env typ in
  pred(Array.length labels)

let mem_typcode env (typ:'b) (code:int) : bool
  =
  code <= maxcode_of_typ env typ

let findcode (label:'a) (tlabels:'a array) : int
  =
  let i = ref 0 in
  while !i < Array.length tlabels && tlabels.(!i) <> label do incr i done;
  if !i = Array.length tlabels then
    raise Not_found
  else
    !i

let typ_of_label (env:('a,'b,'c,'d,'e) Env.O.t) (label:'a) : 'a
  =
  let typ = PMappe.find label env.vartyp in
  match typ with
  | `Benum typ -> typ
  | _ -> failwith ""

let code_of_label (env:('a,'b,'c,'d,'e) Env.O.t) (label:'a) : int
  =
  let typ = typ_of_label env label in
  let labels = labels_of_typ env typ in
  let code = findcode label labels in
  code

let label_of_typcode (env:('a,'b,'c,'d,'e) Env.O.t) (typ:'a) (code:int) : 'a
  =
  let t = labels_of_typ env typ in
  if code < Array.length t then
    t.(code)
  else
    failwith (Print.sprintf "Bddenum.label_of_typcode: no label for typ=%a, code=%i"
      env.symbol.print typ code)


(*  *********************************************************************** *)
(** {3 Constants and Operation(s)} *)
(*  *********************************************************************** *)

let of_label (env:('a,'b,'c,'d,'e) Env.O.t) (label:'a) :'d t  =
  let typ = typ_of_label env label in
  let labels = labels_of_typ env typ in
  let size = size_of_typ env typ in
  let code = findcode label labels in
  let t = {
    typ = env.symbol.marshal typ;
    reg = Reg.of_int env.cudd size code
  } in
(*
  printf "Bddenum.of_typlabel typ=%a label=%a code=%i@.reg = %a@."
    pp_print_string typ pp_print_string label code
    (Reg.print string_of_int) t.reg;
*)
  t

let is_cst (x:'e t) :bool
  =
  Reg.is_cst x.reg

let to_code (x:'e t) : int
  =
  Reg.to_int ~signed:false x.reg

let to_label (env:('a,'b,'c,'d,'e) Env.O.t) (x:'d t) : 'a
  =
  let code = to_code x in
  let label = label_of_typcode env (env.symbol.unmarshal x.typ) code in
(*
  printf "to_label %a = %i,%a@."
    (Reg.print string_of_int) x.reg
    code pp_print_string label;
*)
  label

let equal_label (env:('a,'b,'c,'d,'e) Env.O.t) (x:'d t) (label:'a) : 'd Cudd.Bdd.t
  =
  Reg.equal_int env.cudd x.reg (code_of_label env label)

let equal (env:('a,'b,'c,'d,'e) Env.O.t) (x:'d t) (y:'d t) : 'd Cudd.Bdd.t
  =
  if x.typ<>y.typ then
    failwith (sprintf "Bddenum.equal: applied between different types %s and %s" x.typ y.typ)
  ;
  Reg.equal env.cudd x.reg y.reg

let ite bdd a b =
  if a.typ <> b.typ then
    failwith (Format.sprintf "Bddenum.ite: types %s and %s are different" a.typ b.typ)
  ;
  { typ = a.typ;
    reg = Reg.ite bdd a.reg b.reg }

(*  *********************************************************************** *)
(** {3 Decomposition in guarded form} *)
(*  *********************************************************************** *)

module Minterm = struct

  let iter (env:('a,'b,'c,'d,'e) Env.O.t) (typ:'a) (f:'a -> unit) (minterm:Reg.Minterm.t) : unit =
    let maxcode = maxcode_of_typ env typ in
    Int.Minterm.iter ~signed:false
      (begin fun code ->
	if code<=maxcode then
	  f (label_of_typcode env typ code)
      end)
      minterm

  let map (env:('a,'b,'c,'d,'e) Env.O.t) (typ:'a) (f:'a -> 'f) (minterm:Reg.Minterm.t) : 'f list =
    let res = ref [] in
    let nf minterm = begin res := (f minterm) :: !res end in
    iter env typ nf minterm;
    List.rev !res

end

let guard_of_label (env:('a,'b,'c,'d,'e) Env.O.t) (x:'d t) (label:'a) : 'd Cudd.Bdd.t
  =
  Reg.guard_of_int env.cudd x.reg (code_of_label env label)

let guardlabels (env:('a,'b,'c,'d,'e) Env.O.t) (x:'d t) : ('d Cudd.Bdd.t * 'a) list
  =
  let typ = env.symbol.unmarshal x.typ in
  let lguardints = Reg.guardints env.cudd ~signed:false x.reg in
  let maxcode = maxcode_of_typ env typ in
  let res =
    List.fold_left
      (begin fun res (bdd,code) ->
	if code<=maxcode then
	  (bdd, label_of_typcode env typ code)::res
	else
	  res
      end)
      []
      lguardints
  in
  List.rev res

(*  ********************************************************************** *)
(** {3 Evaluation} *)
(*  ********************************************************************** *)

let cofactor x bdd = { x with reg = Reg.cofactor x.reg bdd }
let restrict x bdd = { x with reg = Reg.restrict x.reg bdd }
let tdrestrict x bdd = { x with reg = Reg.tdrestrict x.reg bdd }

(*  ********************************************************************** *)
(** {3 Printing} *)
(*  ********************************************************************** *)

open Format

let print f fmt t =
  fprintf fmt "{ @[<hv>typ=%s;@ reg=%a@] }"
    t.typ
    (Reg.print f) t.reg

let print_minterm
  (print_bdd: Format.formatter -> 'd Cudd.Bdd.t -> unit)
  env
  fmt
  (x:'d t)
  =
  if is_cst x then begin
    let label = to_label env x in
    fprintf fmt "{ %a }" env.symbol.print label
  end
  else begin
    let lguardlabels = guardlabels env x in
    Print.list ~first:"{ @[<v>" ~sep:"@; " ~last:"@] }"
      (fun fmt (guard,label) ->
	fprintf fmt "%a IF @[%a@]" env.symbol.print label print_bdd guard)
      fmt
      lguardlabels
  end

let permute ?memo x tab = { x with reg = Reg.permute ?memo x.reg tab }
let varmap x = { x with reg = Reg.varmap x.reg }
let vectorcompose ?memo tab x =
  { x with reg = Reg.vectorcompose ?memo tab x.reg }
