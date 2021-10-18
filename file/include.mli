type t = { path : Path.t; }
val make_from_path : Path.t -> t
val search : Path.t -> t -> Path.t option