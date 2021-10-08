(* filesystem interactions *)

module Source = struct
    type t = {path: Path.path; channel: in_channel; buffer: string; pos: int}
end

module Output = struct
  type t = {path: Path.path; channel: out_channel}
end

module Include = struct
  type t = {path: Path.path}
end

