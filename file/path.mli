exception NoUp of string
exception MalformedPath of string
exception NonRelativePath of string
type t = Root | Rel | Here of t | Name of t * string | Up of t
val is_relative : t -> bool
val to_string_list : t -> string list -> string list
val to_string : t -> string
val up : t -> t
val up_opt : t -> t option
val normalize : t -> t
val normalize_partial : t -> t
val of_string_list : t -> string list -> t
val of_string : string -> t
val append : t -> t -> t
val base : t -> string