(* output file *)

type output_path =
  Stdout
  | Stderr
  | Path of Path.t
type t = {path: output_path; channel: out_channel}
let stdout = {path = Stdout; channel = stdout}
let stderr = {path = Stderr; channel = stderr}
let open_path p =
  try {path = Path p; channel = (open_out_bin (Path.to_string p))} with
    Sys_error _ -> raise (Io.WriteError (String.concat "" [(Path.to_string p); ": Unable to open file for writing"]))
let output_bytes out b =
  output_bytes out.channel b
let close out =
  match out.path with
  Stdout
  | Stderr -> ()
  | Path p -> try close_out out.channel with
    Sys_error _ -> raise (Io.WriteError (String.concat "" [(Path.to_string p); ": Encountered write error while closing file"]))
let destroy out =
  match out.path with
  Stdout
  | Stderr -> ()
  | Path p -> close out;
    try Sys.remove (Path.to_string p) with
      Sys_error _ -> raise (Io.WriteError (String.concat "" [(Path.to_string p); ": Unable to remove file"]))