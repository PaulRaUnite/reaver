(** Normalized variable managers/environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Env

(** Type definitions *)
type 'a typdef = 'a Bdd.Env.typdef

(** Types *)
type 'a typ = [
  | 'a Bdd.Env.typ
  | Apronexpr.typ
]

(** Manager for manipulating symbols.

    DO NOT USE [Marshal.to_string] and [Marshal.from_string], as they
    generate strings with NULL character, which is not handled
    properly when converted to C strings.

    You may use instead {!marshal} and {!unmarshal}. *)
type 'a symbol = 'a Bdd.Env.symbol = {
  compare : 'a -> 'a -> int; (** Total order *)
  marshal : 'a -> string;    (** Conversion to string.  The
				 generated strings SHOULD NOT
				 contain NULL character, as they
				 may be converted to C strings. *)
  unmarshal : string -> 'a;  (** Conversion from string *)
  mutable print : Format.formatter -> 'a -> unit; (** Printing *)
}

(** Environment *)
type ('a,'b) ext = {
  mutable table : 'a Apronexpr.t Cudd.Mtbdd.table;
  mutable eapron : Apron.Environment.t;
  mutable aext : 'b;
}
type ('a,'b,'c,'d) t0 = ('a,'b,'c,Cudd.Man.v,('a,'d) ext) Bdd.Env.t0

let copy_ext ~copy_aext ext =
  { ext with
    aext = copy_aext ext.aext }

module O = struct
  type ('a,'b,'c,'d) t = ('a,'b,'c,'d) t0
  constraint 'b = [>'a typ]
  constraint 'c = [>'a typdef]

  let make
      ~symbol
      ~copy_aext
      ?bddindex0 ?bddsize ?relational cudd aext
      =
    let env =
      Bdd.Env.O.make
	~symbol
	~copy_ext:(copy_ext ~copy_aext)
	?bddindex0 ?bddsize ?relational cudd
	{
	  table = Cudd.Mtbdd.make_table ~hash:Hashtbl.hash ~equal:(=);
	  eapron = Apron.Environment.make [||] [||];
	  aext = aext;
	}
    in
    env.ext.table <-
      Cudd.Mtbdd.make_table
      ~hash:(Apronexpr.hash env.symbol)
      ~equal:(Apronexpr.equal env.symbol)
    ;
    env

  let print_ext print_aext fmt ext =
    Format.fprintf fmt "{@[<v>eapron = %a;@ aext = %a@]}"
      (fun fmt x -> Apron.Environment.print fmt x) ext.eapron
      print_aext ext.aext

  let print print_typ print_typdef print_aext fmt env =
    Bdd.Env.O.print print_typ print_typdef
      (print_ext print_aext)
      fmt env
end

type 'a t = ('a,'a typ,'a typdef,unit) O.t

(*  ********************************************************************** *)
(** {3 Printing} *)
(*  ********************************************************************** *)

let print_typ print_symbol fmt typ =
  match typ with
  | #Apronexpr.typ as x -> Apronexpr.print_typ fmt x
  | _ as x -> Bdd.Env.print_typ print_symbol fmt x

let print_typdef print_symbol fmt typdef = Bdd.Env.print_typdef print_symbol fmt typdef

let print fmt env =
  O.print (print_typ env.symbol.print) (print_typdef env.symbol.print) (fun fmt _ -> pp_print_string fmt "_") fmt env

let print_idcondb = Bdd.Env.print_idcondb
let print_order = Bdd.Env.print_order

(*  ********************************************************************** *)
(** {3 Constructors} *)
(*  ********************************************************************** *)

let marshal = Bdd.Env.marshal
let unmarshal = Bdd.Env.unmarshal
let make_symbol = Bdd.Env.make_symbol
let string_symbol = Bdd.Env.string_symbol

let make
    ~symbol
    ?bddindex0 ?bddsize ?relational cudd =
  O.make
    ~symbol
    ~copy_aext:(fun () -> ())
    ?bddindex0 ?bddsize ?relational cudd ()

let make_string ?bddindex0 ?bddsize ?relational cudd =
  make ~symbol:string_symbol ?bddindex0 ?bddsize ?relational cudd

let copy = Bdd.Env.copy

(*  ********************************************************************** *)
(** {3 Accessors} *)
(*  ********************************************************************** *)

let mem_typ = Bdd.Env.mem_typ
let mem_var = Bdd.Env.mem_var
let mem_label = Bdd.Env.mem_label
let typdef_of_typ = Bdd.Env.typdef_of_typ
let typ_of_var = Bdd.Env.typ_of_var
let vars env =
  let vars = PMappe.maptoset env.vartid in
  let (ivar,qvar) = Apron.Environment.vars env.ext.eapron in
  let add ap_var set = PSette.add (env.symbol.unmarshal (Apron.Var.to_string ap_var)) set in
  let vars = Array.fold_right add ivar vars in
  let vars = Array.fold_right add qvar vars in
  vars
let labels = Bdd.Env.labels
let apron env = env.ext.eapron

(*  ********************************************************************** *)
(** {3 Adding types and variables} *)
(*  ********************************************************************** *)

let add_typ_with = Bdd.Env.add_typ_with
let add_typ = Bdd.Env.add_typ

let add_vars_with env ?booking_factor ?packing lvartyp : int array option =
  let operm = Bdd.Env.add_vars_with ?booking_factor ?packing env lvartyp in
  let (integer,real) =
    List.fold_left
      (begin fun ((integer,real) as acc) (var,typ) ->
        let v var = Apron.Var.of_string (env.symbol.marshal var) in
	match typ with
	  | `Int  -> (v var::integer, real)
	  | `Real -> (integer, v var::real)
	  | _ -> acc
       end)
      ([],[]) lvartyp
  in
  if integer<>[] || real<>[] then begin
    env.ext.eapron <-
      (Apron.Environment.add env.ext.eapron
	(Array.of_list integer) (Array.of_list real))
  end;
  operm

let remove_vars_with env lvar : int array option =
  let arith =
    List.fold_left
      (begin fun acc var ->
	match typ_of_var env var with
	| `Int
	| `Real -> (Apron.Var.of_string (env.symbol.marshal var))::acc
	| _ -> acc
      end)
      [] lvar
  in
  let operm = Bdd.Env.remove_vars_with env lvar in
  if arith<>[] then begin
    env.ext.eapron <-
      (Apron.Environment.remove env.ext.eapron (Array.of_list arith))
  end;
  operm

let rename_vars_with env lvarvar
    :
    int array option * Apron.Dim.perm option
    =
  let (lvar1,lvar2) =
    List.fold_left
      (begin fun ((lvar1,lvar2) as acc) (var1,var2) ->
	match (typ_of_var env var1) with
	| `Int
	| `Real ->
	    ((Apron.Var.of_string (env.symbol.marshal var1))::lvar1,
	    (Apron.Var.of_string (env.symbol.marshal var2))::lvar2)
	| _ -> acc
      end)
      ([],[]) lvarvar
  in
  let operm = Bdd.Env.rename_vars_with env lvarvar in
  let oapronperm =
    if lvar1<>[] then begin
      let (n_eapron,perm) =
	Apron.Environment.rename_perm
	  env.ext.eapron
	  (Array.of_list lvar1) (Array.of_list lvar2)
      in
      env.ext.eapron <- n_eapron;
      Some perm
    end
    else
      None
  in
  (operm,oapronperm)

let add_vars env lvartyp =
  let nenv = copy env in
  ignore (add_vars_with nenv lvartyp);
  nenv
let remove_vars env lvars =
  let nenv = copy env in
  ignore (remove_vars_with nenv lvars);
  nenv
let rename_vars env lvarvar =
  let nenv = copy env in
  ignore (rename_vars_with nenv lvarvar);
  nenv

(* ********************************************************************** *)
(** {3 Operations} *)
(* ********************************************************************** *)

let is_leq = Bdd.Env.is_leq
let is_eq = Bdd.Env.is_eq

let lce env1 env2 =
  let env = Bdd.Env.lce env1 env2 in
  if not (env==env1 || env==env2) then
    env.ext.eapron <- Apron.Environment.lce env1.ext.eapron env2.ext.eapron
  ;
  env

(*  ********************************************************************** *)
(** {3 Precomputing change of environments} *)
(*  ********************************************************************** *)

type change = {
  cbdd : Cudd.Man.v Bdd.Env.change;
  capron : Apron.Dim.change2;
}

let compute_change env1 env2 =
  let cbdd = Bdd.Env.compute_change env1 env2 in
  let capron =
    Apron.Environment.dimchange2 env1.ext.eapron env2.ext.eapron
  in
  { cbdd = cbdd; capron = capron; }

(*  ********************************************************************** *)
(** {3 Utilities} *)
(*  ********************************************************************** *)

type ('a,'b) value = ('a,'b) Bdd.Env.value = {
  env : 'a;
  val0 : 'b
}

let make_value = Bdd.Env.make_value
let get_env = Bdd.Env.get_env
let get_val0 = Bdd.Env.get_val0
let check_var = Bdd.Env.check_var
let check_lvar = Bdd.Env.check_lvar
let check_value = Bdd.Env.check_value
let check_value2 = Bdd.Env.check_value2
let check_value3 = Bdd.Env.check_value3
let check_lvarvalue = Bdd.Env.check_lvarvalue
let check_lvalue = Bdd.Env.check_lvalue
let check_ovalue = Bdd.Env.check_ovalue
let mapunop = Bdd.Env.mapunop
let mapbinop = Bdd.Env.mapbinop
let mapbinope = Bdd.Env.mapbinope
let mapterop = Bdd.Env.mapterop

let var_of_aprondim (env:('a,'b,'c,'d) O.t) dim =
  let avar = Apron.Environment.var_of_dim env.ext.eapron dim in
  let strvar = Apron.Var.to_string avar in
  let var = env.symbol.unmarshal strvar in
  var

let aprondim_of_var (env:('a,'b,'c,'d) O.t) var =
  let varstr = env.symbol.marshal var in
  let avar = Apron.Var.of_string varstr in
  let dim = Apron.Environment.dim_of_var env.ext.eapron avar in
  dim

let string_of_aprondim (env:('a,'b,'c,'d) O.t) =
  Print.string_of_print
    (fun fmt dim ->
      env.symbol.print fmt (var_of_aprondim env dim))
