type action =
    Rest
  | Set_bool of bool ref
  | Inc_int of int ref
  | Append_string of string list ref
  | Append_string_of of string list ref * (string -> string)
type t = { keys : string list; action : action; help : string; }
val print : t list -> string
val parse : t list -> string list
