(* lex source files *)
type lexpos =
{offset: int; stop: int}
type 'a t = {v: 'a list; offset: int; source: File.Source.t}
let bytes_of_chars l =
    Bytes.init (List.length l) (List.nth l)
let bytes_of_char c =
    Bytes.init 1 (fun _ -> c)
let look l n =
    File.Source.read_byte l.source (l.offset + n) 
let advance l n =
    {l with offset = l.offset + n}
let push v l =
    {l with v = v :: l.v}
module Token = struct
    type from =
        {offset: int; stop: int; source: File.Source.t}
    module Base = struct
        type t =
            Binary
            | Octal
            | Decimal
            | Hexadecimal
        let rebase a b =
            match (a, b) with
                Hexadecimal, _ -> Hexadecimal
                | _, Hexadecimal -> Hexadecimal
                | Decimal, _ -> Decimal
                | _, Decimal -> Decimal
                | Octal, _ -> Octal
                | _, Octal -> Octal
                | Binary, Binary -> Binary
        let within a b =
            rebase a b = b
        let of_char_opt c =
            match c with
                'b' | 'B' | 'y' | 'Y' -> Some Binary
                | 'o' | 'O' | 'q' | 'Q' -> Some Octal
                | 'd' | 'D' | 't' | 'T' -> Some Decimal
                | 'h' | 'H' | 'x' | 'X' -> Some Hexadecimal
                | _ -> None
        let is_base c =
            of_char_opt c != None
        let of_char c =
            match of_char_opt c with
                Some b -> b
                | None -> raise (Invalid_argument "unknown base")
        let of_digit_opt c =
            match c with
                '0' | '1' -> Some Binary
                | '2' | '3' | '4'
                | '5' | '6' | '7' -> Some Octal
                | '8' | '9' -> Some Decimal
                | 'a' | 'A' | 'b' | 'B' | 'c' | 'C'
                | 'd' | 'D' | 'e' | 'E' | 'f' | 'F' -> Some Hexadecimal
                | _ -> None
        let of_digit c =
            match of_digit_opt c with
                Some b -> b
                | None -> raise (Invalid_argument "unknown digit")
        let is_digit_of c b =
            within (of_digit c) b
        let is_digit c =
            is_digit_of c Hexadecimal
    end
    module Sign = struct
        type t =
            Negative
            | Positive
        let of_char_opt c =
            match c with
                '-' -> Some Negative
                | '+' -> Some Positive
                | _ -> None
        let is_sign c =
            of_char_opt c != None
        let of_char c =
            match of_char_opt c with
                Some b -> b
                | None -> raise (Invalid_argument "unknown sign")
    end
    type t =
        L_parenthesis of from
        | R_parenthesis of from
        | Quote of from
        | Eol of from
        | Unclosed_mlrem_body of from
        | Unknown_escape_string of bytes * from
        | String of bytes * from
        | Unclosed_string_body of from
        | Forbidden_identifier of from
        | Integer of Sign.t * bytes * Base.t * from
        | Malformed_integer of from
        | Identifier of bytes * from
        | Unknown_escape_identifier of bytes * from
        | Unclosed_identifier_body of from
        | Wildcard_identifier of from
end
let from offset stop source: Token.from =
    {offset = offset; stop = stop; source = source}
let of_source s =
    {v = [Token.L_parenthesis (from 0 0 s)]; offset = 0; source = s}
let tell l =
    l.offset
let tell_of l n =
    l.offset + n
let is_iws c =
    c = ' ' || c = '\t'
let is_bad_ident_break c =
    is_iws c || c = '\n'
let is_implicit_break c =
    is_bad_ident_break c || c = ')'
let is_forbidden_sigil c =
    c = '"' || c = '\''  || c = '('
let rec skip_iws l =
    match look l 0 with
        Some c when is_iws c -> advance l 1 |> skip_iws
        | Some '\\' -> begin match look l 1 with
            Some '\n' -> advance l 2 |> skip_iws
            | _ -> l
        end
        | _ -> l
let rec lex_mlrem_body s l =
    match look l 0 with
        Some '#' -> begin match look l 1 with
            Some ']' -> advance l 2
            | Some '[' -> advance l 2 |> lex_mlrem_body (l.offset) |> lex_mlrem_body s
            | _ -> advance l 1 |> lex_mlrem_body s
        end
        | None -> l |> push (Token.Unclosed_mlrem_body (from s l.offset l.source))
        | _ -> advance l 1 |> lex_mlrem_body s
let rec lex_slrem_body l =
    match look l 0 with
        Some '\n' -> l
        | Some '\\' -> begin match look l 1 with
            Some '\n' -> advance l 2 |> lex_slrem_body
            | _ -> advance l 1 |> lex_slrem_body
        end
        | None -> l
        | _ -> advance l 1 |> lex_slrem_body
let rec sublex_octal s o l =
    let open Token.Base in
        if String.length s < 1 then
            match look l 1 with
                Some c when is_digit_of c Octal -> advance l 1 |> sublex_octal (String.concat "" [s; String.make 1 c]) o
                | _ -> l, None
        else if String.length s < 3 then
            match look l 1 with
                Some c when is_digit_of c Octal -> advance l 1 |> sublex_octal (String.concat "" [s; String.make 1 c]) o
                | _ -> advance l 1, Some (Char.chr (int_of_string (String.concat "" ["0o" ;s])))
        else
            let code = int_of_string (String.concat "" ["0o" ;s]) in
                if code <= 0xFF then
                    advance l 1, Some (Char.chr code)
                else
                    l, Some (Char.chr (int_of_string (String.concat "" ["0o"; (String.sub s 1 (String.length s - 1))])))
let rec sublex_hexadecimal s l =
    let open Token.Base in
        if String.length s < 1 then
            match look l 1 with
                Some c when is_digit_of c Hexadecimal -> sublex_hexadecimal (String.concat "" [s; String.make 1 c]) (advance l 1)
                | _ -> l, None
        else if String.length s < 2 then
            match look l 1 with
                Some c when is_digit_of c Hexadecimal -> sublex_hexadecimal (String.concat "" [s; String.make 1 c]) (advance l 1)
                | _ -> advance l 1, Some (Char.chr (int_of_string (String.concat "" ["0x" ;s])))
        else
            advance l 1, Some (Char.chr (int_of_string (String.concat "" ["0x" ;s])))
let sublex_escape_body l =
    match look l 0 with
        Some '"' -> advance l 1, Some '"'
        | Some '\\' -> advance l 1, Some '\\'
        | Some 'n' -> advance l 1, Some '\n'
        | Some 'r' -> advance l 1, Some '\r'
        | Some 't' -> advance l 1, Some '\t'
        | Some 'b' -> advance l 1, Some '\b'
        | Some 'o' -> l |> sublex_octal "" l
        | Some 'x' -> l |> sublex_hexadecimal ""
        | None -> l, None
        | Some _ -> l, None
let rec lex_unknown_escape_str_body b s l =
    match look l 0 with
        Some '"' -> advance l 1 |> push (Token.Unknown_escape_string (b, from s l.offset l.source))
        | Some '\\' -> begin match advance l 1 |> sublex_escape_body with
            sl, Some ch -> sl |> lex_unknown_escape_str_body (Bytes.cat b (bytes_of_char ch)) s
            | sl, None -> lex_unknown_escape_str_body (Bytes.cat b (bytes_of_char '\\')) s sl
        end
        | None -> l |> push (Token.Unclosed_string_body (from s l.offset l.source))
        | Some c -> advance l 1 |> lex_unknown_escape_str_body (Bytes.cat b (bytes_of_char c)) s
let rec lex_str_body raw b s l =
    match look l 0 with
        Some '"' -> advance l 1 |> push (Token.String (b, from s (l.offset + 1) l.source))
        | Some '\\' when not raw -> begin match advance l 1 |> sublex_escape_body with
            sl, Some ch -> sl |> lex_str_body raw (Bytes.cat b (bytes_of_char ch)) s
            | _, None -> lex_unknown_escape_str_body b s l
        end
        | None -> l |> push (Token.Unclosed_string_body (from s l.offset l.source))
        | Some c -> advance l 1 |> lex_str_body raw (Bytes.cat b (bytes_of_char c)) s
let rec lex_mal_int_body s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Malformed_integer (from s l.offset l.source))
        | None -> l |> push (Token.Malformed_integer (from s l.offset l.source))
        | Some _ -> advance l 1 |> lex_mal_int_body s
let produce_integer n p e b s l =
    match p with
        None -> l |> push (Token.Integer (n, b, e, from s l.offset l.source))
        | Some prefix -> if Token.Base.within e prefix then
                l |> push (Token.Integer (n, b, prefix, from s l.offset l.source))
            else
                l |> push (Token.Malformed_integer (from s l.offset l.source))
let rec lex_int_body n p e b s l =
    let open Token.Base in
        match look l 0 with
            Some c when is_digit c -> advance l 1 |> lex_int_body n p (rebase e (of_digit c)) (Bytes.cat b (bytes_of_char c)) s
            | Some '_' -> advance l 1 |> lex_int_body n p e b s
            | Some c when is_base c -> if p = None then
                    advance l 1 |> produce_integer n (of_char_opt c) e b s
                else
                    l |> lex_mal_int_body s
            | Some c when is_implicit_break c -> if p = None then
                    l |> produce_integer n (Some Decimal) e b s
            else
                    l |> produce_integer n p e b s    
            | None -> l |> produce_integer n p e b s
            | Some _ -> l |> lex_mal_int_body s
let lex_prefixed_int_head n p s l =
    let open Token.Base in
        match look l 0 with
            Some c when is_digit c -> l |> lex_int_body n p (of_digit c) Bytes.empty s
            | _ -> l |> lex_mal_int_body s
let lex_int_prefix_body n s l =
    let open Token.Base in
        match look l 0 with
            Some c when is_base c -> advance l 1 |> lex_prefixed_int_head n (Some (of_char c)) s
            | _ -> l |> lex_int_body n None Decimal Bytes.empty s
let rec lex_forbidden_ident_body s l =
    match look l 0 with
        Some c when is_bad_ident_break c -> l |> push (Token.Forbidden_identifier (from s l.offset l.source))
        | None -> l |> push (Token.Forbidden_identifier (from s l.offset l.source))
        | Some _ -> advance l 1 |> lex_forbidden_ident_body s
let rec lex_ident_body b s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Identifier (b, from s l.offset l.source))
        | Some c when is_forbidden_sigil c -> l |> lex_forbidden_ident_body s
        | None -> l |> push (Token.Identifier (b, from s l.offset l.source))
        | Some c -> advance l 1 |> lex_ident_body (Bytes.cat b (bytes_of_char c)) s
let rec lex_unknown_escape_special_ident_body b s l =
    match look l 0 with
        Some '"' -> advance l 1 |> push (Token.Unknown_escape_identifier (b, from s l.offset l.source))
        | Some '\\' -> begin match advance l 1 |> sublex_escape_body with
            sl, Some ch -> sl |> lex_unknown_escape_special_ident_body (Bytes.cat b (bytes_of_char ch)) s
            | sl, None -> lex_unknown_escape_special_ident_body (Bytes.cat b (bytes_of_char '\\')) s sl
        end
        | None -> l |> push (Token.Unclosed_identifier_body (from s l.offset l.source))
        | Some c -> advance l 1 |> lex_unknown_escape_special_ident_body (Bytes.cat b (bytes_of_char c)) s
let rec lex_special_ident_body b s l =
    match look l 0 with
        Some '"' -> advance l 1 |> push (Token.Identifier (b, from s (l.offset + 1) l.source))
        | Some '\\' -> begin match advance l 1 |> sublex_escape_body with
            sl, Some ch -> sl |> lex_special_ident_body (Bytes.cat b (bytes_of_char ch)) s
            | _, None -> lex_unknown_escape_special_ident_body b s l
        end
        | None -> l |> push (Token.Unclosed_identifier_body (from s l.offset l.source))
        | Some c -> advance l 1 |> lex_special_ident_body (Bytes.cat b (bytes_of_char c)) s
let lex_token l =
    let open Token in
        let l = skip_iws l in
            match look l 0 with
                (* SIGILS *)
                | Some '(' -> advance l 1 |> push (Token.L_parenthesis (from l.offset (l.offset + 1) l.source))
                | Some ')' -> begin match look l 1 with
                    Some c when is_implicit_break c -> advance l 1 |> push (Token.R_parenthesis (from l.offset (l.offset + 1) l.source))
                    | None -> advance l 1 |> push (Token.R_parenthesis (from l.offset (l.offset + 1) l.source))
                    | Some _ -> l |> lex_forbidden_ident_body l.offset
                end
                | Some '\'' -> advance l 1 |> push (Token.Quote (from l.offset (l.offset + 1) l.source))
                | None -> advance l 1 |> push (Token.R_parenthesis (from l.offset l.offset l.source))
                (* EOL *)
                | Some '\n' -> begin match look l 1 with
                    Some '\r' -> advance l 2 |> push (Token.Eol (from l.offset (l.offset + 2) l.source))
                    | _ -> advance l 1 |> push (Token.Eol (from l.offset (l.offset + 1) l.source))
                end
                (* REMARKS *)
                | Some '#' -> begin match look l 1 with
                    Some '[' -> advance l 2 |> lex_mlrem_body l.offset
                    | _ -> advance l 1 |> lex_slrem_body
                end
                (* STRINGS *)
                | Some 'r' -> begin match look l 1 with
                    Some '"' -> advance l 2 |> lex_str_body true Bytes.empty l.offset
                    | _ -> advance l 1 |> lex_ident_body (bytes_of_char 'r') l.offset
                end
                | Some '"' -> advance l 1 |> lex_str_body false Bytes.empty l.offset
                (* INTEGERS *)
                | Some c when Sign.is_sign c -> begin match look l 1 with
                    Some '0' -> advance l 1 |> lex_int_prefix_body (Sign.of_char c) l.offset
                    | Some c when Base.is_digit_of c Decimal -> l |> lex_int_body (Sign.of_char c) None Decimal Bytes.empty l.offset
                    | _ -> l |> lex_ident_body Bytes.empty l.offset
                end
                | Some '0' -> advance l 1 |> lex_int_prefix_body Sign.Positive l.offset
                | Some c when Base.is_digit_of c Decimal -> l |> lex_int_body Sign.Positive None Decimal Bytes.empty l.offset
                (* IDENTIFIERS *)
                | Some 'i' -> begin match look l 1 with
                    Some '"' -> advance l 2 |> lex_special_ident_body Bytes.empty l.offset
                    | _ -> advance l 1 |> lex_ident_body (bytes_of_char 'i') l.offset
                end
                | Some '_' -> begin match look l 1 with
                    Some c when is_implicit_break c -> advance l 1 |> push (Token.Wildcard_identifier (from l.offset (l.offset + 1) l.source))
                    | _ -> advance l 1 |> lex_ident_body (bytes_of_char '_') l.offset
                end
                | Some c -> advance l 1 |> lex_ident_body (bytes_of_char c) l.offset