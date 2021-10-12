(* filesystem interactions *)

(* fs helpers *)
let is_dir (path: Path.path): bool =
  try Sys.is_directory (Path.to_string path) with
      Sys_error _ -> false
let path_exists (path: Path.path): bool =
    Sys.file_exists (Path.to_string path)
let is_file (path: Path.path): bool =
    (path_exists path) && not (is_dir path)

(* file i/o helpers *)
let rec input_bytes_prepend chan len prepend =
  if len > 0 then
    let buf = Bytes.create len in
      let readin = input chan buf 0 len in
        if readin > 0 then
          let readbuf = Bytes.sub buf 0 readin in
            input_bytes_prepend chan (len - readin) (Bytes.cat prepend readbuf)
        else
          prepend
  else
    prepend

let input_bytes chan len =
  input_bytes_prepend chan len Bytes.empty

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
  let output_bytes out b =
    output_bytes out.channel b
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
  type t = {path: Path.path; channel: in_channel; mutable buffer: bytes; mutable offset: int; min_load: int}
  let open_path p =
    if not (path_exists p) then
      raise (FileNotFound (String.concat "" [(Path.to_string p); ": No such file"]))
    else
      if not (is_file p) then
        raise (WrongFileOrDir (String.concat "" [(Path.to_string p); ": Is not a file"]))
      else
        let in_ch = try open_in_bin (Path.to_string p) with 
          Sys_error _ -> raise (ReadError (String.concat "" [(Path.to_string p); ": Unable to open file for reading"]))
        in {path = p;
          channel = in_ch;
          buffer = Bytes.empty;
          offset = 0;
          min_load = 0x10000}
  let load src len =
    let ahead_dist = Bytes.length src.buffer - src.offset in
      if ahead_dist >= len then
        ahead_dist
      else
        let to_load = max len src.min_load in
          let read_buf = input_bytes src.channel (len - to_load) in
            let read_buf_len = Bytes.length read_buf in
              src.buffer <- Bytes.cat src.buffer read_buf;
              ahead_dist + read_buf_len
  (* TODO arbitrary position read *)
  let tell src ahead =
    src.offset + ahead
  let look src ahead =
    if not (load src ahead < ahead) then
      Some (Bytes.get src.buffer (tell src ahead))
    else
      None
  let advance src ahead =
    let loaded = load src ahead in
      src.offset <- src.offset + loaded;
      loaded
  let close src =
    try close_in src.channel with
      Sys_error _ -> raise (ReadError (String.concat "" [(Path.to_string src.path); ": Encountered read error while closing file"]))
end