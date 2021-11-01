(* wrangle different kinds of outputs under one interface *)

open File
type kind = Ast
type t = {dst: string; wrt: string option; output: Output.t; kind: kind}
let open_output s =
    match s with
        "-" -> Output.stdout
        | _ -> Output.open_path (Path.of_string s |> Path.normalize_partial)
let ast_of s w =
    {dst = s; wrt = w; output = open_output s; kind = Ast}
let ast s =
    ast_of s None