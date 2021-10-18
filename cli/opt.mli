type action =
    Rest
  | Set_bool of bool ref
  | Inc_int of int ref
  | Append_string of string list ref
type t = { keys : string list; action : action; help : string; }
val print : t list -> string
val parse : t list -> string list
