type output_path = Stdout | Stderr | Path of Path.t
type t = { path : output_path; channel : out_channel; }
val stdout : t
val stderr : t
val open_path : Path.t -> t
val output_bytes : t -> bytes -> unit
val close : t -> unit
val destroy : t -> unit