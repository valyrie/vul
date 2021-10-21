(* source file *)

type t = {path: Path.t; channel: in_channel; mutable buffer: bytes; mutable offset: int; min_load: int}
let open_path p =
  if not (Fs.path_exists p) then
    raise (Io.FileNotFound (String.concat "" [(Path.to_string p); ": No such file"]))
  else
    if not (Fs.is_file p) then
      raise (Io.WrongFileOrDir (String.concat "" [(Path.to_string p); ": Is not a file"]))
    else
      let in_ch = try open_in_bin (Path.to_string p) with 
        Sys_error _ -> raise (Io.ReadError (String.concat "" [(Path.to_string p); ": Unable to open file for reading"]))
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
        let read_buf = Io.input_bytes src.channel (len - to_load) in
          let read_buf_len = Bytes.length read_buf in
            src.buffer <- Bytes.cat src.buffer read_buf;
            ahead_dist + read_buf_len
let read_bytes src off ahead =
  let requested_load = off + ahead in
    let loaded = load src (max (requested_load - (Bytes.length src.buffer)) 0) in
      let available_bytes = Bytes.length src.buffer in
        if requested_load >= available_bytes then
          Bytes.sub src.buffer off ahead
        else
          if off < available_bytes then
            Bytes.sub src.buffer off (available_bytes - off)
          else
            Bytes.empty
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
    Sys_error _ -> raise (Io.ReadError (String.concat "" [(Path.to_string src.path); ": Encountered read error while closing file"]))