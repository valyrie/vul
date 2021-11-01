(* wrangle different kinds of outputs under one interface *)

open File
exception BadSpec of string
type kind = Ast
module Spec = Map.Make(String)
type t = {dst: string; wrt: string option; output: Output.t; kind: kind}
let open_output s =
    match s with
        "-" -> Output.stdout
        | _ -> Output.open_path (Path.of_string s |> Path.normalize_partial)
let output_bytes a b =
    File.Output.output_bytes a.output b
let close a =
    Output.close a.output
let destroy a =
    Output.destroy a.output
let parse_spec_kind s =
    match s with
        "ast" -> Ast
        | _ -> raise (BadSpec (String.concat "" ["unrecognized artifact kind: "; s]))
let parse_spec_is_kind s =
    match s with
        "ast" -> true
        | _ -> false
let rec parse_spec_list s l =
    match l with
        [] -> s
        | arg :: tl -> match String.split_on_char '=' arg with
            [k]
            | ["kind"; k] when parse_spec_is_kind k -> parse_spec_list (Spec.add "kind" k s) tl
            | ["wrt"; w] -> parse_spec_list (Spec.add "wrt" w s) tl
            | _ -> raise (BadSpec (String.concat "" ["unrecognized artifact specification argument: "; arg]))
let parse_spec s =
    match String.split_on_char ',' s with
        [] -> raise (Invalid_argument "empty artifact specification")
        | hd :: tl -> parse_spec_list (Spec.add "path" hd Spec.empty) tl
let of_spec s =
    let spec = parse_spec s in
        try {dst = Spec.find "path" spec;
            wrt = Spec.find_opt "wrt" spec;
            output = open_output (Spec.find "path" spec);
            kind = parse_spec_kind (Spec.find "kind" spec)}
        with
            Not_found -> raise (BadSpec (String.concat "" ["Incomplete artifact specification: "; s]))