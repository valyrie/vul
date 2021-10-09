(* filesystem interactions *)

(* fs helpers *)
let is_dir (path: Path.path): bool =
  try Sys.is_directory (Path.to_string path) with
      Sys_error _ -> false
let path_exists (path: Path.path): bool =
    Sys.file_exists (Path.to_string path)
let is_file (path: Path.path): bool =
    (path_exists path) && not (is_dir path)

(* file i/o types *)

exception ReadError of string
exception WriteError of string
exception FileNotFound of string
exception WrongFileOrDir of string

module Output = struct
  type t = {path: Path.path; channel: out_channel}
  let open_path p =
    (* TODO check for existence of parent directory *)
    try {path = p; channel = (open_out_bin (Path.to_string p))} with
      Sys_error _ -> raise (WriteError (String.concat "" [(Path.to_string p); ": Unable to open file for writing"]))
  let push_bytes b out =
    output_bytes out b
  let close out =
    try close_out out.channel with
      Sys_error _ -> raise (WriteError (String.concat "" [(Path.to_string out.path); ": Encountered write error while closing file"]))
end

module Include = struct
  type t = {path: Path.path}
  let make_from_path p =
    if not (path_exists p) then
      raise (FileNotFound (String.concat "" [(Path.to_string p); ": No such directory"]))
    else
      if not (is_dir p) then
        raise (WrongFileOrDir (String.concat "" [(Path.to_string p); ": Is not a directory"]))
      else
        {path = p}
  let search p inc =
    if not (path_exists (Path.append inc.path p)) then
      None
  else
      Some (Path.append inc.path p)
end

module Source = struct
  type t = {path: Path.path; channel: in_channel; pos: int}
  let open_path p =
    if not (path_exists p) then
      raise (FileNotFound (String.concat "" [(Path.to_string p); ": No such file"]))
    else
      if not (is_file p) then
        raise (WrongFileOrDir (String.concat "" [(Path.to_string p); ": Is not a file"]))
      else
        try {path = p; channel = open_in_bin (Path.to_string p); pos = 0} with
          Sys_error _ -> raise (ReadError (String.concat "" [(Path.to_string p); ": Unable to open file for reading"]));
  (* TODO src fns *)
end