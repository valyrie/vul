(* source file *)

type t = {path: Path.t; channel: in_channel; mutable buffer: bytes; min_load: int}
let path s = s.path
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
        min_load = 0x10000}
let load src len =
  let loaded_len = Bytes.length src.buffer in
    if loaded_len >= len then
      loaded_len
    else
      let to_load = max (len - loaded_len) src.min_load in
        let read_buf = Io.input_bytes src.channel to_load in
          let read_buf_len = Bytes.length read_buf in
            src.buffer <- Bytes.cat src.buffer read_buf;
            loaded_len + read_buf_len
let read_bytes src off len =
  let requested_load = off + len in
    let _ = load src (max (requested_load - (Bytes.length src.buffer)) 0) in
      let available_bytes = Bytes.length src.buffer in
        if available_bytes >= requested_load then
          Bytes.sub src.buffer off len
        else
          if off < available_bytes then
            Bytes.sub src.buffer off (available_bytes - off)
          else
            Bytes.empty
let read_byte src off =
  if load src (off + 1) > off then
    Some (Bytes.get src.buffer off)
  else
    None
let close src =
  try close_in src.channel with
    Sys_error _ -> raise (Io.ReadError (String.concat "" [(Path.to_string src.path); ": Encountered read error while closing file"]))