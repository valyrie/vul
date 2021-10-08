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
  let open_path (p: Path.path) =
    if not (path_exists p) then
      raise (FileNotFound (String.concat "" [(Path.to_string p); ": No such file"]))
    else
      if not (is_file p) then
        raise (WrongFileOrDir (String.concat "" [(Path.to_string p); ": Is not a file"]))
      else
        try {path = p; channel = (open_out_bin (Path.to_string p))} with
          Sys_error _ -> raise (WriteError (String.concat "" [(Path.to_string p); ": Unable to open file for writing"]))
  (* TODO output fns *)
end

module Include = struct
  type t = {path: Path.path}
  let open_path (p: Path.path) =
    if not (path_exists p) then
      raise (FileNotFound (String.concat "" [(Path.to_string p); ": No such directory"]))
    else
      if not (is_dir p) then
        raise (WrongFileOrDir (String.concat "" [(Path.to_string p); ": Is not a directory"]))
      else
        {path = p}
  (* TODO include fns *)
end

module Source = struct
  type t = {path: Path.path; channel: in_channel; buffer: string; pos: int}
  let open_path (p: Path.path) =
    if not (path_exists p) then
      raise (FileNotFound (String.concat "" [(Path.to_string p); ": No such file"]))
    else
      if not (is_file p) then
        raise (WrongFileOrDir (String.concat "" [(Path.to_string p); ": Is not a file"]))
      else
        try {path = p; channel = open_in_bin (Path.to_string p); buffer = ""; pos = 0} with
          Sys_error _ -> raise (ReadError (String.concat "" [(Path.to_string p); ": Unable to open file for reading"]));
  (* TODO src fns *)
end