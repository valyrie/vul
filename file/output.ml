(* output file *)

type t = {path: Path.t; channel: out_channel}
let open_path p =
  (* TODO check for existence of parent directory *)
  try {path = p; channel = (open_out_bin (Path.to_string p))} with
    Sys_error _ -> raise (Io.WriteError (String.concat "" [(Path.to_string p); ": Unable to open file for writing"]))
let output_bytes out b =
  output_bytes out.channel b
let close out =
  try close_out out.channel with
    Sys_error _ -> raise (Io.WriteError (String.concat "" [(Path.to_string out.path); ": Encountered write error while closing file"]))
let destroy out =
  close out;
  try Sys.remove (Path.to_string out.path) with
    Sys_error _ -> raise (Io.WriteError (String.concat "" [(Path.to_string out.path); ": Unable to remove file"]))