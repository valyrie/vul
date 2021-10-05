exception NoUp of string
type path = Root | Rel | Here of path | Name of path * string | Up of path
val is_relative : path -> bool
val to_string : path -> string
val up : path -> path
val up_opt : path -> path option
val normalize : path -> path
val normalize_partial : path -> path
