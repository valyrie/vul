(* unnamed language, main module *)

(* basename *)

let basename = 
    List.hd (
        List.rev (
            String.split_on_char '\\' (
                Array.get Sys.argv 0)))

(* print to stdout *)

let print_stdout s =
    Printf.fprintf stdout "%s\n" s;
    flush stdout

(* print errors to stderr *)

let error = ref 0

let print_stderr s =
    Printf.fprintf stderr "%s\n" s;
    flush stderr

let print_error s c =
    Printf.fprintf stderr "%s: error: %s\n" basename s;
    flush stderr;
    error := c

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

let license = "Copyright 2021, Valyrie Autumn, All rights reserved."
let version = String.concat " " ["unnamed language"; "version 0a"]
let usage = String.concat " " ["usage:" ;basename; "[-o OUTPATH...]"; "[-I INCDIR...]"; "[--]"; "FILE..."]
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

let open_output s =
    Path.of_string s
    |> Path.normalize_partial
    |> File.Output.open_path

let make_include s =
    Path.of_string s
    |> Path.normalize_partial
    |> File.Include.make_from_path

let open_source s =
    Path.of_string s
    |> Path.normalize_partial
    |> File.Source.open_path

let sources = List.map open_source args
let includes = List.map make_include !include_paths
let outputs = List.map open_output !output_paths

let () =
begin
    if Array.length Sys.argv <= 1 then
        print_stdout help
    else
        begin
            if List.length sources = 0 then
                begin
                    print_usage := true;
                    print_error "no source file(s) specified" 1
                end;
            if List.length outputs = 0; then
                begin
                    print_usage := true;
                    print_error "no output file(s) specified" 1
                end;
            if !print_help then
                print_stdout help
            else if !print_usage || !print_version || !print_license then
                begin
                    (if !print_usage then
                        print_stdout usage);
                    (if !print_version then
                        print_stdout version);
                    (if !print_license then
                        print_stdout license)
                end
            else
                if !error = 0 then
                    print_stdout (String.concat " " args)
        end
end;
    exit !error