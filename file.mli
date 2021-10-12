val is_dir : Path.path -> bool
val path_exists : Path.path -> bool
val is_file : Path.path -> bool
val input_bytes : in_channel -> int -> bytes
exception ReadError of string
exception WriteError of string
exception FileNotFound of string
exception WrongFileOrDir of string
module Output :
  sig
    type t = { path : Path.path; channel : out_channel; }
    val open_path : Path.path -> t
    val output_bytes : t -> bytes -> unit
    val close : t -> unit
  end
module Include :
  sig
    type t = { path : Path.path; }
    val make_from_path : Path.path -> t
    val search : Path.path -> t -> Path.path option
  end
module Source :
  sig
    type t = {
      path : Path.path;
      channel : in_channel;
      mutable buffer : bytes;
      mutable offset : int;
      min_load : int;
    }
    val open_path : Path.path -> t
    val load : t -> int -> int
    val read : 'a -> 'b -> 'c -> unit
    val tell : t -> int -> int
    val look : t -> int -> char option
    val advance : t -> int -> int
    val close : t -> unit
  end
