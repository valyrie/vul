(* language, main module *)

(* parse options *)

type opt_fn =
    Apply_Unit of (unit -> unit)
    | Apply_String of (string -> unit)
    | Rest
type opt = {keys: string list; fn: opt_fn; help: string}
let opt_pad (padding: int): string =
    String.init padding (fun _ -> ' ')
let rec keys_length (keys: string list): int =
    match keys with
        [] -> 0
        | [k] -> String.length k
        | k :: tail -> (String.length k) + (String.length ", ") + (keys_length tail)
let rec opt_keys_length (opts: opt list): int =
    match opts with
        [] -> 0
        | opt :: tail -> max (keys_length opt.keys) (opt_keys_length tail)
let print_opt (klen: int) (opt: opt): string =
    String.concat "" [
        "  ";
        opt_pad (klen - (keys_length opt.keys));
        String.concat ", " opt.keys;
        "  ";
        opt.help]
let print_opts (opts: opt list): string =
    let klen = opt_keys_length opts in
        String.concat "\n" (List.map (fun opt -> print_opt klen opt) opts) 
let rec arg_in_keys (arg: string) (keys: string list): bool =
    match keys with
        [] -> false
        | k :: rest -> if arg = k then
            true
        else
            arg_in_keys arg rest
let rec match_opt (arg: string) (opts: opt list): (opt option) =
    match opts with
        [] -> None
        | opt :: rem_opts -> if not (arg_in_keys arg opt.keys) then
            match_opt arg rem_opts
        else
            Some opt
let is_switch (s: string): bool =
    (String.get s 0) = '-'
let rec parse_opts (argv: string list) (opts: opt list): (string list) =
    match argv with
        [] -> []
        | switch :: tail -> match (match_opt switch opts) with
            None -> argv
            | Some opt -> match opt.fn with
                Rest -> tail
                | Apply_Unit f -> f (); parse_opts tail opts
                | Apply_String f -> match tail with
                    [] -> argv
                    | arg :: rem_tail -> f arg; parse_opts rem_tail opts

let set_bool (b: bool ref) =
    b := true
let inc_int (i: int ref) =
    i := !i + 1
let append_to_list (l: 'a list ref) (x: 'a) =
    l := x :: !l

(* fs utils *)

let dir_exists (path: string): bool =
    try Sys.is_directory path with
        Sys_error _ -> false
let file_exists (path: string): bool =
    (Sys.file_exists path) && not (dir_exists path)
let append_path (names: string list): string =
    String.concat "/" names

(* TODO path substring matches *)

(* open output files *)

type output =
    Channel of out_channel
    | Error of string
let open_output (path: string): output =
    try Channel (open_out_bin path) with
        Sys_error s -> Error s

(* search include directories *)

(* TODO search include directory for file *)

(* open source files *)

type source =
    Channel of in_channel
    | Error of string
let open_source (path: string): source =
    try Channel (open_in_bin path) with
        Sys_error s -> Error s

(* commandline options *)

let print_help = ref false
let print_version = ref false
let print_usage = ref false
let print_license = ref false
let verbosity = ref 0
let (output_paths: string list ref) = ref []
let (include_paths: string list ref) = ref []
let (input_paths: string list ref) = ref []

let basename = 
    List.hd (
        List.rev (
            String.split_on_char '\\' (
                Array.get Sys.argv 0)))
let license = "Copyright 2021, Valyrie Autumn, All rights reserved."
let version = String.concat " " ["unnamed language"; "version 0a"]
let usage = String.concat " " [basename; "[-o OUTPATH...]"; "[-i INCDIR...]"; "[--]"; "FILE..."]
let opts = [
    {keys = ["-h"; "-?"; "--help"; "--?"]; fn = Apply_Unit (fun () -> set_bool print_help); help = "print this help and exit."};
    {keys = ["-V"; "--version"]; fn = Apply_Unit (fun () -> set_bool print_version); help = "display version and exit."};
    {keys = ["--usage"]; fn = Apply_Unit (fun () -> set_bool print_usage); help = "print usage and exit."};
    {keys = ["--license"]; fn = Apply_Unit (fun () -> set_bool print_license); help = "print license and exit."};
    {keys = ["-v"; "--verbose"]; fn = Apply_Unit (fun () -> inc_int verbosity); help = "increase verbosity; may be specified multiple times."};
    {keys = ["-o"; "--output"]; fn = Apply_String (append_to_list output_paths); help = "specify an output path; may be specified multiple times."};
    {keys = ["-I"; "--include"]; fn = Apply_String (append_to_list include_paths) ; help = "specify a directory to include; may be specified multiple times."};
    {keys = ["--"]; fn = Rest; help = "explicitly terminate options."}
]
let help = String.concat "\n" [
    usage;
    "";
    print_opts opts;
    "";
    version;
    license
]

let args = parse_opts (List.tl (Array.to_list Sys.argv)) opts

let () = if !print_help then
    print_endline help
else if !print_usage || !print_version then
    begin
        (if !print_usage then
            print_endline usage);
        (if !print_version then
            print_endline version);
        (if !print_license then
            print_endline license)
    end
else
    print_string (String.concat " " args)