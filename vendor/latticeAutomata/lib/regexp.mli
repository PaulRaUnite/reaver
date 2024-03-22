(***********************************************************************)
(*                                                                     *)
(*                    The Lattice Automata Library                     *)
(*                                                                     *)
(*                Bertrand Jeannet and Tristan Le Gall                 *)
(*                                                                     *)
(*  Copyright 2008 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(** Regular expressions *)

(** Non-empty expressions *)
type 'a exp = 
| Epsilon
    (** Empty word *)
| Letter of 'a 
    (** Word with a single letter *)
| Union  of 'a exp list
    (** Union of expressions *)
| Concat of 'a exp list
    (** Concatenation of expressions *)
| Star   of 'a exp
    (** Kleene closure *)
| Plus   of 'a exp
    (** Strict Kleene closure *)

(** Regular expressions *)
type 'a t = 
  | Empty
      (** Denotes empty language (does not contain the empty word *)
  | Exp of 'a exp
      (** Regular expression *)

val map : ('a -> 'b) -> 'a t -> 'b t
      (** [map f exp] applies [f] to the letters of the expression *)

val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f exp] applies [f] to the letters of the
      expression. The order of application is not specified *)

val simplify  : ('a -> 'a -> bool) -> 'a t -> 'a t
  (** [simplify equal exp] simplifies the expression, using the
      equality function [equal] on letters *)

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
      (** Printing function *)
