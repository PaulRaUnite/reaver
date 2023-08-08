(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** ReaVer command line options *)

exception InvalidArgs (** invalid arguments *)
exception HelpAndExit (** print usage and exit *)

val strategy : string ref (** verification strategies *) 
val df2cf : string ref  (** DF to CF transformation *)
val cfg2dot_file : string ref (** print CFG to dot output file *)
val cfg2dot_arcs : bool ref (** print CFG to dot with arcs *)
val print2nbac_file : string ref (** print DF program to NBAC file*)
val inputformat : string ref (** input format *)
val inputfile : string ref (** input file *)
val input : string ref (** input program *)
val num_factor : int ref (** increase number of numerical 
                             constraints in the environment *)

val parse : unit -> unit (** parse options *)
val print_usage : unit -> unit (** print usage *)

val get_df2cf : string -> string (** get DF to CF transformation 
                                       with given default value*)
val get_strategy : string -> string (** get verification strategy 
                                          with given default strategy *)
