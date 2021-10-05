type opt_fn =
    Apply_Unit of (unit -> unit)
  | Apply_String of (string -> unit)
  | Rest
type opt = { keys : string list; fn : opt_fn; help : string; }
val print_opts : opt list -> string
val is_switch : string -> bool
val parse_opts : string list -> opt list -> string list
