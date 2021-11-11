(* bytestring literals *)

type t =
    {bytes: bytes}
let to_bytes t =
    t.bytes
let is_printable t =
    let rec is_printable_charlist l =
        match l with
            [] -> true
            | hd :: tl -> if Char.code hd >= 0x20 && Char.code hd <= 0x7E then
                    is_printable_charlist tl
                else
                    false in
    is_printable_charlist @@ List.of_seq @@ Bytes.to_seq @@ to_bytes t
let of_bytes b =
    if is_printable {bytes = b} then
        {bytes = b}
    else
        raise (Invalid_argument "Cannot create nonprintable bytestring")
let of_string s =
    of_bytes @@ Bytes.of_string s
let to_string t =
    Bytes.to_string t.bytes
let unescaped_of_bytes b =
    {bytes = Bytes.of_string @@ Scanf.unescaped @@ Bytes.to_string b}
let escaped_bytes_of t =
    Bytes.of_string @@ String.escaped @@ Bytes.to_string t.bytes
let unescaped_of_str s =
    {bytes = Bytes.of_string @@ Scanf.unescaped s}
let escaped_str_of t =
    String.escaped @@ Bytes.to_string t.bytes
let compare a b =
    Bytes.compare (to_bytes a) (to_bytes b)