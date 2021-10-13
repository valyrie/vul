(* unnamed language, main module *)

(* basename *)

let basename = 
    List.hd (
        List.rev (
            String.split_on_char '/' (
                String.map
                    (fun c: char -> if not (c = '\\') then c else '/')
                    (Array.get Sys.argv 0))))

(* print to stdout *)

let print_stdout s =
    Printf.fprintf stdout "%s\n" s;
    flush stdout

(* print errors to stderr *)

let error = ref 0

let print_stderr s =
    Printf.fprintf stderr "%s\n" s;
    flush stderr

let print_error s =
    Printf.fprintf stderr "%s: error: %s\n" basename s;
    flush stderr;
    error := 1

(* commandline options *)

let set_bool (b: bool ref) =
    b := true
let inc_int (i: int ref) =
    i := !i + 1
let append_to_list (l: 'a list ref) (x: 'a) =
    l := !l @ [x]

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

(* TODO wire verbosity in *)
let args = Opts.parse_opts (List.tl (Array.to_list Sys.argv)) opts

let open_output l s =
    try (Path.of_string s
    |> Path.normalize_partial
    |> File.Output.open_path) :: l with
        Path.MalformedPath _ -> print_error (String.concat "" [s; ": Malformed path"]); l
        | File.FileNotFound e -> print_error e; l
        | File.WrongFileOrDir e -> print_error e; l
        | File.WriteError e -> print_error e; l

let make_include l s =
    try (Path.of_string s
    |> Path.normalize_partial
    |> File.Include.make_from_path) :: l with
        Path.MalformedPath _ -> print_error (String.concat "" [s; ": Malformed path"]); l
        | File.FileNotFound e -> print_error e; l
        | File.WrongFileOrDir e -> print_error e; l

let open_source l s =
    try (Path.of_string s
    |> Path.normalize_partial
    |> File.Source.open_path) :: l with
        Path.MalformedPath _ -> print_error (String.concat "" [s; ": Malformed path"]); l
        | File.FileNotFound e -> print_error e; l
        | File.WrongFileOrDir e -> print_error e; l
        | File.ReadError e -> print_error e; l

let outputs = List.fold_left open_output [] !output_paths
let includes = List.fold_left make_include [] !include_paths
let sources = List.fold_left open_source [] args

let handle_exit () =
    if !error != 0 then
        (try List.iter File.Output.destroy outputs with
            File.WriteError s -> print_error s);
    (try List.iter File.Output.close outputs with
        File.WriteError s -> print_error s);
    (try List.iter File.Source.close sources with
        File.ReadError s -> print_error s);
    exit !error

let () =
try begin begin
    if Array.length Sys.argv <= 1 then
        print_stdout help
    else
        begin
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
                begin
                    if List.length args = 0 then
                        begin
                            print_error "no source file(s) specified"
                        end;
                    if List.length !output_paths = 0; then
                        begin
                            print_error "no output file(s) specified"
                        end;
                    if !error = 0 then
                        print_stdout (String.concat " " args)
                end
        end
end;
    handle_exit ()
end with
    x -> print_error (String.concat "" ["uncaught exception: "; (Printexc.to_string x)]); handle_exit ()