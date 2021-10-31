type t = {
  path : Path.t;
  channel : in_channel;
  mutable buffer : bytes;
  min_load : int;
}
val path : t -> Path.t
val open_path : Path.t -> t
val load : t -> int -> int
val read_bytes : t -> int -> int -> bytes
val read_byte : t -> int -> char option
val close : t -> unit