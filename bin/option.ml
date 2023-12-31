(******************************************************************************)
(* Option *)
(* Options for ReaVer *)
(* author: Peter Schrammel *)
(* version: 0.9.0 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

exception InvalidArgs
exception HelpAndExit

(** ********************************************************************** *)
(** {2 Variables storing options} *)
(** ********************************************************************** *)

let strategy = ref ""
let df2cf = ref ""
let cfg2dot_file = ref ""
let cfg2dot_arcs = ref true
let print2nbac_file = ref ""
let inputfile = ref ""
let inputformat = ref ""
let input = ref ""
let num_factor = ref 1

(** ********************************************************************** *)
(** {2 Parsing} *)
(** ********************************************************************** *)

let print_strategies () =
  let fmt = Format.std_formatter in
  Format.pp_print_string fmt "Available strategies: ";
  Format.pp_print_newline fmt ();
  Verif.print_strategies fmt ();
  raise HelpAndExit

let print_domains () =
  let fmt = Format.std_formatter in
  Format.pp_print_string fmt "Available domains: ";
  Format.pp_print_newline fmt ();
  Verif.print_domains fmt ();
  raise HelpAndExit

let print_df2cf () =
  let fmt = Format.std_formatter in
  Format.pp_print_string fmt "Available program types: ";
  Format.pp_print_newline fmt ();
  Df2cf.print_df2cf fmt ();
  raise HelpAndExit

let usage = "Usage: reaver <filename> [options]"
let options = 
    [
    ("-p",Arg.Set_string df2cf,"use the given program type for preprocessing");
    ("-p_help",Arg.Unit print_df2cf,"prints the available program types");
    ("-s",Arg.Set_string strategy,"use the given strategy");
    ("-s_help",Arg.Unit print_strategies,"prints the available strategies");
    ("-dom_help",Arg.Unit print_domains,"prints the available domains");
    ("-inv",Arg.Clear Verif.check_property,"inference only, do not check property");
    ("-print_overall",Arg.Set Verif.print_overall,"print overall invariant");
    ("-cudd_print_limit",Arg.Set_int Env.cudd_print_limit,
       "up to which BDD size formulas are printed");
    ("-env_num_factor",Arg.Set_int num_factor,
       "increase number of constraint variables in BDDs");
    ("-inputformat",Arg.Set_string inputformat,"specifies the input format (for command line and stdin)");
    ("-input",Arg.Set_string input,"the input program on the command line");
    ("-xml",Arg.Unit (fun () -> Log.print_format := Log.Xml),"log output in XML format");
    ("-dot",Arg.Set_string cfg2dot_file, "print CFG to dot file");
    ("-dot_noarcs",Arg.Clear cfg2dot_arcs,"do not print arc formulas to dot");
    ("-nbac",Arg.Set_string print2nbac_file, "print program in (Hybrid) NBAC format");
    ("-debug",Arg.String (fun l -> Log.globallevel:=Log.string2level l), 
     "debug level (ERROR|WARN|INFO|DEBUG)");
    ("-debug_force",Arg.String (fun l -> Log.globallevel_weaken:=
                                           Log.string2level l), 
     "debug level (ERROR|WARN|INFO|DEBUG)")
    ]

let print_usage () = Arg.usage options usage

let parse () =
  Arg.parse options (fun x -> inputfile := x) usage

let get_df2cf default_df2cf = 
  match !df2cf with
    |"" -> default_df2cf
    |_ -> !df2cf

let get_strategy default_strat = 
  match !strategy with
    |"" -> default_strat
    |_ -> !strategy
