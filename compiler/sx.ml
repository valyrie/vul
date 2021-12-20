(* symbolic expression type *)

open Numbers
module Syms = Symbols.Make(Bytestring)
type proc_fn =
    t -> t Syms.t -> t * t Syms.t
and t =
    Name of Bytestring.t
    | Number of Z.t
    | String of Bytestring.t
    | Procedure of proc_fn
    | List of t list
let rec print x =
    let open Printf in
    match x with
        Name n ->
            if Bytestring.is_printable n then 
                Bytestring.to_string n
            else
                sprintf "i\"%s\""
                    @@ Bytestring.escaped_str_of n
        | Number z ->
            Z.to_string z
        | String b ->
            sprintf "\"%s\"" @@ Bytestring.escaped_str_of b
        | Procedure _ ->
            "PROCEDURE"
        | List l ->
            String.concat " "
                @@ List.map print l