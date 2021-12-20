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
let unwrap_proc x =
    match x with
        Procedure f -> f
        | _ -> raise @@ Invalid_argument "cannot get fn of non-procedure"
let hd x =
    match x with
        List l -> List.hd l
        | _ -> raise @@ Invalid_argument "cannot hd non-list"
let tl x =
    match x with
        List l -> List (List.tl l)
        | _ -> raise @@ Invalid_argument "cannot tl non-list"
let cons a b =
    match b with
        List l -> List (a :: l)
        | _ -> List [a; b]
let len x =
    match x with
        List l -> List.length l
        | _ -> raise @@ Invalid_argument "cannot get length of a non-list"
let map f x =
    match x with
        List l -> List (List.map f l)
        | _ -> raise @@ Invalid_argument "cannot map non-list"
let iter f x =
    match x with
        List l -> List.iter f l
        | _ -> raise @@ Invalid_argument "cannot iter non-list"
let fold_left f i x =
    match x with
        List l -> List.fold_left f i l
        | _ -> raise @@ Invalid_argument "cannot fold_left non-list"
module Eval = struct
    exception Unbound_name
    exception Invalid_arguments
    exception Not_applicable
    let rec proc_apply x s =
        if len x > 1 then
            let proc, s =
                proc_apply
                    (hd x)
                    s in
            let args = tl x in
            try (unwrap_proc proc) args s with
                Invalid_argument _ -> raise Not_applicable
        else
            match x with
                Name n ->
                    begin match Syms.recall n s with
                        Some x -> x, s
                        | _ -> raise Unbound_name
                    end
                | _ ->
                    x, s
    and proc_sequence x s =
        match x with
            List [] ->
                List [], s
            | List (hd :: tl) ->
                let _, s = proc_apply hd, s in
                proc_sequence (List tl) s
            | _ -> raise Invalid_arguments
end
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