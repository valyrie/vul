(* bytestring literals *)

type t =
    {bytes: bytes}
let to_bytes t =
    t.bytes
let append b c =
    let b = Bytes.extend b 0 1 in
    Bytes.set b (Bytes.length b - 1) c;
    b
let cat b a =
    Bytes.cat b a
let hd b =
    Bytes.get b 0
let tl b =
    if Bytes.length b > 0 then
        Bytes.sub b 1 (Bytes.length b - 1)
    else
        Bytes.empty
let rec escape_bytes a b =
    if Bytes.length b > 0 then
        begin
        if hd b = '\"' then
            escape_bytes
                (cat a @@ Bytes.of_string "\\\"")
                (tl b)
        else if hd b = '\\' then
            escape_bytes
                (cat a @@ Bytes.of_string "\\\\")
                (tl b)
        else if hd b >= Char.chr 0x20 && hd b <= Char.chr 0x7E then
            escape_bytes
                (append a @@ hd b)
                (tl b)
        else
            escape_bytes
                (cat a
                    @@ Bytes.of_string
                    @@ Printf.sprintf "\\x%02x"
                    @@ Char.code @@ hd b)
                (tl b)
        end
    else
        a
let rec capture_bytes a b =
    if Bytes.length b > 0 then
        begin
        if hd b = '\\' then
            match hd @@ tl b with
                '\\' ->
                    capture_bytes
                        (append a @@ '\\')
                        (tl @@ tl b)
                | '\"' ->
                    capture_bytes
                        (append a @@ '\"')
                        (tl @@ tl b)
                | _ ->
                    capture_bytes
                        (append a @@ '\\')
                        (tl b)
        else
            capture_bytes
                (append a @@ hd b)
                (tl b)
        end
    else
        a
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
        raise (Invalid_argument (Printf.sprintf
            "Cannot create nonprintable bytestring: %s"
            @@ Bytes.to_string b))
let of_string s =
    of_bytes @@ Bytes.of_string s
let to_string t =
    Bytes.to_string t.bytes
let captured_of_bytes b =
    {bytes = capture_bytes Bytes.empty b}
let escaped_bytes_of t =
    escape_bytes t.bytes
let captured_of_str s =
    captured_of_bytes @@ Bytes.of_string s
let escaped_str_of t =
    Bytes.to_string @@ escape_bytes Bytes.empty t.bytes
let compare a b =
    Bytes.compare (to_bytes a) (to_bytes b)