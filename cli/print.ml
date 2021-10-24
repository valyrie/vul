(* cli printing facilities *)

let error_code = ref 0
let print s =
    Printf.fprintf stdout "%s\n" s;
    flush stdout

let error s =
    Printf.fprintf stderr "%s: error: %s\n" Env.basename s;
    flush stderr;
    error_code := 1