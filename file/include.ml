(* include directory *)

type t = {path: Path.t}
let make_from_path p =
  if not (Fs.path_exists p) then
    raise (Io.FileNotFound (String.concat "" [(Path.to_string p); ": No such directory"]))
  else
    if not (Fs.is_dir p) then
      raise (Io.WrongFileOrDir (String.concat "" [(Path.to_string p); ": Is not a directory"]))
    else
      {path = p}
let search p inc =
  if not (Fs.path_exists (Path.append inc.path p)) then
    None
else
    Some (Path.append inc.path p)