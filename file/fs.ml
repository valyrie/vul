(* filesystem interactions *)
let is_dir p =
    try Sys.is_directory (Path.to_string p) with
        Sys_error _ -> false
  let path_exists p =
      Sys.file_exists (Path.to_string p)
  let is_file p =
      (path_exists p) && not (is_dir p)