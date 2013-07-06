(******************************************************************************)
(* main *)
(* ReaVer main *)
(* author: Peter Schrammel *)
(* version: 0.9.1 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

let version = "0.9.1"

let logger = {Log.fmt=Format.std_formatter; 
              Log.module_name="Main";
              Log.level=Log.Info}
let globallevel = Log.Info (* default log level *)
let globallevel_weaken = Log.Error (* default log level weakening all default log levels*)

(******************************************************************************)
(** {2 Printing} *)
(******************************************************************************)

let result_to_text result = 
  if result then "PROPERTY TRUE (final unreachable)"
  else "PROPERTY: DON'T KNOW (final reachable)"

let display_dfprog_info env dfprog =
  let bs_vars = Env.number_of_boolvars env env.Env.bs_vars in
  let ns_vars = List.length env.Env.ns_vars in
  Log.info logger ("variables(bool/num): state=("^
    (string_of_int bs_vars)^"/"^
    (string_of_int ns_vars)^"), input=("^
    (string_of_int ((Env.number_of_boolvars env env.Env.bi_vars)-bs_vars))^"/"^
    (string_of_int ((List.length env.Env.ni_vars)-ns_vars))^")");
  Log.debug_o logger (BddapronUtil.print_equations env.Env.env env.Env.cond) 
     "discrete equation system: " dfprog.Program.d_disc_equs;
  Log.debug_o logger (BddapronUtil.print_equations ~cont:true 
       env.Env.env env.Env.cond) 
     "continuous equation system: " dfprog.Program.d_cont_equs;
  Log.debug2_o logger Bddapron.Env.print 
     "environment: " env.Env.env;
  Log.debug2_o logger (Bddapron.Cond.print env.Env.env)
     "conditions: " env.Env.cond;
  Log.debug2_o logger (fun f e -> Bddapron.Env.print_order e f)
     "env-order: " env.Env.env;
  Log.info_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond) 
     "initial: " dfprog.Program.d_init;
  Log.info_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond) 
     "final: " dfprog.Program.d_final;
  Log.info_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond) 
     "assertion: " dfprog.Program.d_ass;
  Log.debug_o logger (Env.print_zero_defs env) 
     "zero_defs: " dfprog.Program.d_zero_defs;
  Log.debug_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond) 
    "careset = " env.Env.cond.Bdd.Cond.careset;
  Log.debug3_o logger (Format.pp_print_int) 
     "bool_size: " !Env.bool_size


(******************************************************************************)
(** {2 Parsing} *)
(******************************************************************************)

(** builds the environment and the semantics of the program 
    (retries if the BDD size for numerical constraints  was chosen too small) *)
let build_env_dfprog decl translate =
  let make_env num_factor = Env.make ~num_factor
        decl.Program.typdef decl.Program.state decl.Program.input 
  in
  let succeeded = ref(false) in
  let num_factor = ref(!Option.num_factor) in
  let env = ref(make_env !num_factor) in
  let dfprog = ref(Program.make_empty_dfprog !env) in
  while not !succeeded do
    dfprog := 
      try 
        let dfprog = translate !env in
        succeeded := true;
        dfprog
      with 
        Bdd.Env.Bddindex -> 
        begin
          num_factor := !num_factor+1;
          Log.warn_o logger (Format.pp_print_int) 
            "restarting with num_factor=" !num_factor;
          env := make_env !num_factor;
          !dfprog
        end
  done;
  (!env,!dfprog)

(******************************************************************************)
(** parses the input file: 
    add new frontends here *) 
let parse inputfile inputformat input =
  if inputformat="nbac" then
  begin
    if inputfile<>"" then
      NbacTrans.parse ~is_file:true inputfile
    else
      NbacTrans.parse ~is_file:false input
  end
  else if Filename.check_suffix inputfile ".nbac" then 
    NbacTrans.parse inputfile
  else 
    raise (Arg.Bad ("input file format of '"^inputfile^"' not recognized"))


(******************************************************************************)
(** {2 Main} *)
(******************************************************************************)

let main () =
  Option.parse ();

  (* parsing input to dfprog *)
  Log.info_o logger Format.pp_print_string
    "parsing inputfile " !Option.inputfile; 
  let (decl,translate,default_df2cf,default_strat) = 
    parse !Option.inputfile !Option.inputformat !Option.input in
  let (env,dfprog) = build_env_dfprog decl (translate) in

  Env.cudd_reorder env;
  Env.compute_careset env;

  display_dfprog_info env dfprog;

  (* dfprog to cfprog *)
  let (env,cfprog) = Df2cf.run env dfprog (Option.get_df2cf default_df2cf) in
(*
  let fexpr = BddapronUtil.get_fexpr (BddapronUtil.get_primed_var env.Env.env) env.Env.env env.Env.cond cfprog.Program.c_disc_equs in
  Env.cudd_reorder env;
  let fexpr = BddapronUtil.simplify_boolexpr env.Env.env env.Env.cond fexpr env.Env.cond.Bdd.Cond.careset in 
  Log.info_o logger (BddapronUtil.print_boolexpr env.Env.env env.Env.cond) "fexpr = " fexpr; 
*)

  (* run transformation/analysis strategy *)
  let strategy = Verif.str_to_strategy env
    (Util.string_remove_whitespaces (Option.get_strategy default_strat)) in
  let (result,_,_) = Verif.run env strategy cfprog in

   Log.info logger (result_to_text result);

  match !Option.cfg2dot_file with
    |"" -> ()
    |"stdout" -> 
    begin
      let out_channel = stdout in
      let dotfmt = Format.formatter_of_out_channel out_channel in
      Cfg.print_dot env dotfmt cfprog.Program.c_cfg !Option.cfg2dot_arcs;
      close_out out_channel;
    end
    |_ -> 
    begin
      let out_channel = open_out !Option.cfg2dot_file in
      let dotfmt = Format.formatter_of_out_channel out_channel in
      Cfg.print_dot env dotfmt cfprog.Program.c_cfg !Option.cfg2dot_arcs;
      close_out out_channel;
    end

let _ =
  try
   Log.globallevel := globallevel; Log.globallevel_weaken := globallevel_weaken;
   Log.info_o logger Format.pp_print_string
    ("ReaVer, version ") version; 
   (*test () *)
     main () 
  with
  | Option.InvalidArgs ->
    Log.error logger "invalid arguments";
    Option.print_usage ()
  | Option.HelpAndExit -> ()
  | Parse.Lex_error -> 
    Log.error logger "aborted."
  | Pervasives.Exit -> 
    Log.error logger "aborted."
  | Arg.Bad(s) -> 
    Log.error logger s
  | NbacExpr.NbacParseError(s) -> 
    Log.error logger s
  | Df2cf.InvalidDf2cf(s) ->
    Log.error logger ("invalid preprocessing method: "^s);
  | Verif.InvalidStrategy(s) ->
    Log.error logger ("invalid verification strategy: "^s);
  | Verif.InvalidStrategyOption(o) ->
    Log.error logger ("invalid verification strategy option: "^o);
  | Verif.InvalidStrategyOptionValue(v) ->
    Log.error logger ("invalid value in verification strategy option: "^v);
  | Df2cfHybrid.InvalidZeroSemantics(o) ->
    Log.error logger ("invalid zero-crossing semantics: "^o);
  | BddapronUtil.NotSupported(s) ->
    Log.error logger ("not supported: "^s);
  | Domain.NotSupported(s) ->
    Log.error logger ("not supported: "^s);
  | Bdd.Env.Bddindex ->
    Log.error logger ("insufficient environment size: try with increasing values for option \"-env_num_factor <n>\"");
  | Sys_error(s) ->
    Log.error logger s


