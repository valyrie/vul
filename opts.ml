(* parse commandline options *)

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
