type t = { path : Path.t; channel : out_channel; }
val open_path : Path.t -> t
val output_bytes : t -> bytes -> unit
val close : t -> unit
val destroy : t -> unit
