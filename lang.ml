(* language, main module *)

(* fs paths *)

(*

type path =
    Stack of string Stack.t
(* TODO check if path is relative *)
(* TODO normalize path *)
(* TODO convert strings into paths *)
(* TODO string of path *)
let is_dir (path: path): bool =
    try Sys.is_directory (string_of_path path) with
        Sys_error _ -> false
let path_exists (path: path): bool =
    Sys.file_exists (string_of_path path)
let is_file (path: path): bool =
    (path_exists path) && not (is_dir path)
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

*)

(* commandline options *)

let set_bool (b: bool ref) =
    b := true
let inc_int (i: int ref) =
    i := !i + 1
let append_to_list (l: 'a list ref) (x: 'a) =
    l := x :: !l

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
let opts: Opts.opt list = [
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
    Opts.print_opts opts;
    "";
    version;
    license
]

let args = Opts.parse_opts (List.tl (Array.to_list Sys.argv)) opts

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