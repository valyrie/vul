type t = { bytes : bytes; }
val to_bytes : t -> bytes
val is_printable : t -> bool
val of_bytes : bytes -> t
val of_string : string -> t
val to_string : t -> string
val unescaped_of_bytes : bytes -> t
val escaped_bytes_of : t -> bytes
val unescaped_of_str : string -> t
val escaped_str_of : t -> string