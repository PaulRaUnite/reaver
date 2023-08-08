(* This file is part of ReaVer released under the GNU GPL.  
   Please read the LICENSE file packaged in the distribution *)

(** logging *)

type level_t = Error | Warn | Info | Debug | Debug2 | Debug3 | Hidden

type print_format_t = Plain | Xml

(** logger: declare one logger per module *)
type logger_t = 
  {fmt: Format.formatter; 
   module_name: string;
   level: level_t}

(** {2 Global configuration} *)

(** everything with less or equal verbosity will be logged, 
if enabled in individual loggers *)
val globallevel : level_t ref

(** the log level of all modules with less verbosity will be weakened
to this value (i.e. made more verbose) *)
val globallevel_weaken : level_t ref

(** output format *)
val print_format : print_format_t ref


(** {2 Logging functions} *)

val debug2 : logger_t -> string -> unit
val debug2_o : logger_t -> (Format.formatter -> 'a -> unit) -> string -> 'a -> unit
val debug3 : logger_t -> string -> unit
val debug3_o : logger_t -> (Format.formatter -> 'a -> unit) -> string -> 'a -> unit
val debug : logger_t -> string -> unit
val debug_o : logger_t -> (Format.formatter -> 'a -> unit) -> string -> 'a -> unit
val info : logger_t -> string -> unit
val info_o : logger_t -> (Format.formatter -> 'a -> unit) -> string -> 'a -> unit
val warn : logger_t -> string -> unit
val warn_o : logger_t -> (Format.formatter -> 'a -> unit) -> string -> 'a -> unit
val error : logger_t -> string -> unit
val error_o : logger_t -> (Format.formatter -> 'a -> unit) -> string -> 'a -> unit

(** {2 Helpers} *)

(** parses a log level (upper case) from a string *)
val string2level : string -> level_t

(** returns true if the given level is activated in the given logger *)
val check_level : logger_t -> level_t -> bool
