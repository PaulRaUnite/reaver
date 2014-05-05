(******************************************************************************)
(* Log *)
(* generic logging module *)
(* author: Peter Schrammel *)
(* version: 0.9.3 *)
(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)
(******************************************************************************)

type level_t = Error | Warn | Info | Debug | Debug2 | Debug3

type logger_t = 
  {fmt: Format.formatter; 
   module_name: string;
   level: level_t}

(* everything with less or equal verbosity will be logged, 
if enabled in individual loggers *)
let globallevel = ref (Info)

(* the log level of all modules with less verbosity will be weakened
to this value (i.e. made more verbose) *)
let globallevel_weaken = ref (Error)

let level2string level =
  match level with
    |Error -> "ERROR"
    |Warn -> "WARN"
    |Info -> "INFO"
    |Debug -> "DEBUG"
    |Debug2 -> "DEBUG2"
    |Debug3 -> "DEBUG3"

let string2level level =
  match level with
    |"WARN" -> Warn
    |"INFO" -> Info
    |"DEBUG" -> Debug
    |"DEBUG2" -> Debug2
    |"DEBUG3" -> Debug3
    |_ -> Error


let level2int level =
  match level with
    |Error -> 1
    |Warn -> 2
    |Info -> 3
    |Debug -> 4
    |Debug2 -> 5
    |Debug3 -> 6

let level_geq l1 l2 = (level2int l1)>=(level2int l2)

let check_level logger level =
  (level_geq logger.level level) && (level_geq !globallevel level) ||
  (level_geq !globallevel_weaken level)

let log logger level msg =
  if check_level logger level then
    begin
    Format.fprintf logger.fmt "[%.3f] "(Sys.time ());
    Format.pp_print_string logger.fmt (level2string level);
    Format.fprintf logger.fmt " [%s] " logger.module_name;
    Format.pp_print_string logger.fmt msg;
    Format.pp_print_newline logger.fmt ()
  end

let log_o logger level print_o msg obj  =
  if check_level logger level then
  begin
    Format.fprintf logger.fmt "[%.3f] "(Sys.time ());
    Format.pp_print_string logger.fmt (level2string level);
    Format.fprintf logger.fmt " [%s] " logger.module_name;
    Format.pp_print_string logger.fmt msg;
    print_o logger.fmt obj;
    Format.pp_print_newline logger.fmt ()
  end

let debug2 logger msg = log logger Debug2 msg
let debug2_o logger print_o msg obj = log_o logger Debug2 print_o msg obj
let debug3 logger msg = log logger Debug3 msg
let debug3_o logger print_o msg obj = log_o logger Debug3 print_o msg obj
let debug logger msg = log logger Debug msg
let debug_o logger print_o msg obj = log_o logger Debug print_o msg obj
let info logger msg = log logger Info msg
let info_o logger print_o msg obj = log_o logger Info print_o msg obj
let warn logger msg = log logger Warn msg
let warn_o logger print_o msg obj = log_o logger Warn print_o msg obj
let error logger msg = log logger Error msg
let error_o logger print_o msg obj = log_o logger Error print_o msg obj
