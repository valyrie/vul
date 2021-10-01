(* language, main module *)

(* parse options *)

type opt_keys = 
    Key of string 
    | Keys of string list
type opt_fn =
    Apply_Unit of (unit -> unit)
    | Apply_String of (string -> unit)
    | None
type opt = {keys: opt_keys; fn: opt_fn; help: string}

let opt_pad (padding: int): string =
    String.init padding (fun _ -> ' ')

let rec keys_length (keys: opt_keys): int =
    match keys with
        Key k -> String.length k
        | Keys ks -> match ks with
            [] -> 0
            | [k] -> String.length k
            | k :: tail -> (String.length k) + (String.length ", ") + (keys_length (Keys tail))

let rec opt_keys_length (opts: opt list): int =
    match opts with
        [] -> 0
        | opt :: tail -> max (keys_length opt.keys) (opt_keys_length tail)

let print_opt (klen: int) (opt: opt): string =
    String.concat " " [opt_pad (klen - (keys_length opt.keys));
        begin match opt.keys with
            Key k -> k
            | Keys ks -> String.concat ", " ks
        end;
        opt.help]

let print_opts (opts: opt list): string =
    let klen = opt_keys_length opts in
        String.concat "\n" (List.map (fun opt -> print_opt klen opt) opts) 

let rec arg_in_keys (arg: string) (keys: opt_keys): bool =
    match keys with
        Key k -> arg = k
        | Keys ks -> match ks with
            [] -> false
            | k :: rest -> if arg = k then
                true
            else
                arg_in_keys arg (Keys rest)

let rec apply_opt (argv: string list) (opts: opt list): (string list) =
    match argv with
        [] -> []
        | arg :: tail -> match opts with
            [] -> argv
            | opt :: rem_opts -> if not (arg_in_keys arg opt.keys) then
                apply_opt argv rem_opts
            else
                match opt.fn with
                    Apply_Unit f -> f (); tail
                    | Apply_String f -> f (List.hd tail); List.tl tail
                    | None -> tail                
let is_switch (s: string): bool =
    (String.get s 0) = '-'
let rec parse_opts (argv: string list) (opts: opt list): (string list) =
    match argv with
        [] -> []
        | _ -> let p = apply_opt argv opts in
            match p with
            [] -> []
            | s :: _ -> if is_switch s then
                    parse_opts p opts
                else
                    p

let set_bool (b: bool ref) =
    b := true

let inc_int (i: int ref) =
    i := !i + 1

let append_to_list (l: 'a list ref) (x: 'a) =
    l := x :: !l

(* commandline options *)

let print_help = ref false
let print_version = ref false
let print_usage = ref false
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
let usage = String.concat " " [basename; "[-o OUTPATH]"; "[-i INCDIR]"; "FILE..."]
let opts = [
    {keys = Keys ["-h"; "-?"; "--help"; "--?"]; fn = Apply_Unit (fun () -> set_bool print_help); help = "print this help and exit."};
    {keys = Keys ["-V"; "--version"]; fn = Apply_Unit (fun () -> set_bool print_version); help = "display version and exit."};
    {keys = Key "--usage"; fn = Apply_Unit (fun () -> set_bool print_usage); help = "print usage and exit."};
    {keys = Keys ["-v"; "--verbose"]; fn = Apply_Unit (fun () -> inc_int verbosity); help = "increase verbosity; may be specified multiple times."};
    {keys = Keys ["-o"; "--output"]; fn = Apply_String (append_to_list output_paths); help = "specify an output path; may be specified multiple times."};
    {keys = Keys ["-i"; "--include"]; fn = Apply_String (append_to_list include_paths) ; help = "specify a directory to include; may be specified multiple times."};
    {keys = Key "--"; fn = None; help = "explicitly terminate options."}
]
let help = String.concat "\n" [
    usage;
    "";
    print_opts opts;
    "";
    version;
    license]

let () = print_endline help(*(String.concat " " (parse_opts (List.tl (Array.to_list Sys.argv)) opts))*)