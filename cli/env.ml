(* cli environment *)

let basename =
    List.hd (
        List.rev (
            String.split_on_char '/' (
                String.map
                    (fun c: char -> if not (c = '\\') then c else '/')
                    (Array.get Sys.argv 0))))

let cwd = (File.Path.of_string (Sys.getcwd ()))

let readvar var =
    Sys.getenv_opt var

let rec readvar_fallback vars =
    match vars with
        [] -> None
        | hd :: tl -> let env = Sys.getenv_opt hd in
        match env with
            Some _ -> env
            | None -> readvar_fallback tl