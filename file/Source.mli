type t = {
  path : Path.t;
  channel : in_channel;
  mutable buffer : bytes;
  mutable offset : int;
  min_load : int;
}
val open_path : Path.t -> t
val load : t -> int -> int
val tell : t -> int -> int
val look : t -> int -> char option
val advance : t -> int -> int
val close : t -> unit