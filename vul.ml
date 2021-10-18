(* valyrie's unnamed language -- commandline tool *)

(* commandline options *)

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
let usage = String.concat " " ["usage:" ;Cli.Env.basename; "[-o OUTPATH...]"; "[-I INCDIR...]"; "[--]"; "FILE..."]
let opts: Cli.Opt.t list = [
    {keys = ["-h"; "-?"; "--help"; "--?"]; action = Set_bool print_help; help = "print this help and exit."};
    {keys = ["-V"; "--version"]; action = Set_bool print_version; help = "display version and exit."};
    {keys = ["--usage"]; action = Set_bool print_usage; help = "print usage and exit."};
    {keys = ["--license"]; action = Set_bool print_license; help = "print license and exit."};
    {keys = ["-v"; "--verbose"]; action = Inc_int verbosity; help = "increase verbosity; may be specified multiple times."};
    {keys = ["-o"; "--output"]; action = Append_string output_paths; help = "specify an output path; may be specified multiple times."};
    {keys = ["-I"; "--include"]; action = Append_string include_paths; help = "specify a directory to include; may be specified multiple times."};
    {keys = ["--"]; action = Rest; help = "explicitly terminate options."}
]
let help = String.concat "\n" [
    usage;
    "cli tool for val's unnamed language";
    "";
    Cli.Opt.print opts;
    "";
    version;
    license
]

(* TODO wire verbosity in *)
let source_paths = Cli.Opt.parse opts

let open_output l s =
    try (Path.of_string s
    |> Path.normalize_partial
    |> File.Output.open_path) :: l with
        Path.MalformedPath _ -> Cli.Print.error (String.concat "" [s; ": Malformed path"]); l
        | File.FileNotFound e -> Cli.Print.error e; l
        | File.WrongFileOrDir e -> Cli.Print.error e; l
        | File.WriteError e -> Cli.Print.error e; l

let make_include l s =
    try (Path.of_string s
    |> Path.normalize_partial
    |> File.Include.make_from_path) :: l with
        Path.MalformedPath _ -> Cli.Print.error (String.concat "" [s; ": Malformed path"]); l
        | File.FileNotFound e -> Cli.Print.error e; l
        | File.WrongFileOrDir e -> Cli.Print.error e; l

let open_source l s =
    try (Path.of_string s
    |> Path.normalize_partial
    |> File.Source.open_path) :: l with
        Path.MalformedPath _ -> Cli.Print.error (String.concat "" [s; ": Malformed path"]); l
        | File.FileNotFound e -> Cli.Print.error e; l
        | File.WrongFileOrDir e -> Cli.Print.error e; l
        | File.ReadError e -> Cli.Print.error e; l

let outputs = List.fold_left open_output [] !output_paths
let includes = List.fold_left make_include [] !include_paths
let sources = List.fold_left open_source [] source_paths

let handle_exit () =
    if !Cli.Print.error_code != 0 then
        (try List.iter File.Output.destroy outputs with
            File.WriteError s -> Cli.Print.error s);
    (try List.iter File.Output.close outputs with
        File.WriteError s -> Cli.Print.error s);
    (try List.iter File.Source.close sources with
        File.ReadError s -> Cli.Print.error s);
    exit !Cli.Print.error_code

let () =
try begin begin
    if Array.length Sys.argv <= 1 then
        Cli.Print.print help
    else
        begin
            if !print_help then
                Cli.Print.print help
            else if !print_usage || !print_version || !print_license then
                begin
                    (if !print_usage then
                        Cli.Print.print usage);
                    (if !print_version then
                        Cli.Print.print version);
                    (if !print_license then
                        Cli.Print.print license)
                end
            else
                begin
                    if List.length source_paths = 0 then
                        begin
                            Cli.Print.error "no source file(s) specified"
                        end;
                    if List.length !output_paths = 0; then
                        begin
                            Cli.Print.error "no output file(s) specified"
                        end;
                    if !Cli.Print.error_code = 0 then
                        Cli.Print.print (String.concat " " source_paths)
                end
        end
end;
    handle_exit ()
end with
    x -> Cli.Print.error (String.concat "" ["uncaught exception: "; (Printexc.to_string x)]); handle_exit ()