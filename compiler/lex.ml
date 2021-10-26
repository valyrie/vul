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
        | Binary_integer of bytes * from
        | Octal_integer of bytes * from
        | Decimal_integer of bytes * from
        | Hexadecimal_integer of bytes * from
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
let is_implicit_break c =
    is_iws c || c = '\n' || c = '(' || c = ')'
let is_forbidden_sigil c =
    c = '"' || c = '\'' 
let is_bin_digit c =
    c = '0' || c = '1'
let is_oct_digit c =
    is_bin_digit c || c = '2' || c = '3' || c = '4' || c = '5' || c = '6' || c = '7'
let is_dec_digit c =
    is_oct_digit c || c = '8' || c = '9'
let is_hex_digit c =
    is_dec_digit c
    || c = 'a' || c = 'A'
    || c = 'b' || c = 'B'
    || c = 'c' || c = 'C'
    || c = 'd' || c = 'D'
    || c = 'e' || c = 'E'
    || c = 'f' || c = 'F'
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
let sublex_escape_body l =
    match look l 0 with
        Some '"' -> advance l 1, Some '"'
        | Some '\\' -> advance l 1, Some '\\'
        | Some 'n' -> advance l 1, Some '\n'
        | Some 'r' -> advance l 1, Some '\r'
        | Some 't' -> advance l 1, Some '\t'
        | Some 'b' -> advance l 1, Some '\b'
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
let rec lex_str_body b s l =
    match look l 0 with
        Some '"' -> advance l 1 |> push (Token.String (b, from s (l.offset + 1) l.source))
        | Some '\\' -> begin match advance l 1 |> sublex_escape_body with
            sl, Some ch -> sl |> lex_str_body (Bytes.cat b (bytes_of_char ch)) s
            | _, None -> lex_unknown_escape_str_body b s l
        end
        | None -> l |> push (Token.Unclosed_string_body (from s l.offset l.source))
        | Some c -> advance l 1 |> lex_str_body (Bytes.cat b (bytes_of_char c)) s
let rec lex_mal_int_body s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Malformed_integer (from s l.offset l.source))
        | None -> l |> push (Token.Malformed_integer (from s l.offset l.source))
        | Some _ -> advance l 1 |> lex_mal_int_body s
let rec lex_binint_body b s l =
    match look l 0 with
        Some c when is_bin_digit c -> advance l 1 |> lex_binint_body (Bytes.cat b (bytes_of_char c)) s
        | Some '_' -> advance l 1 |> lex_binint_body b s
        | Some c when is_implicit_break c -> l |> push (Token.Binary_integer (b, from s l.offset l.source))
        | None -> l |> push (Token.Binary_integer (b, from s l.offset l.source))
        | Some _ -> l |> lex_mal_int_body s
let rec lex_octint_body b s l =
    match look l 0 with
        Some c when is_oct_digit c -> advance l 1 |> lex_octint_body (Bytes.cat b (bytes_of_char c)) s
        | Some '_' -> advance l 1 |> lex_octint_body b s
        | Some c when is_implicit_break c -> l |> push (Token.Octal_integer (b, from s l.offset l.source))
        | None -> l |> push (Token.Octal_integer (b, from s l.offset l.source))
        | Some _ -> l |> lex_mal_int_body s
let rec lex_decint_body b s l =
    match look l 0 with
        Some c when is_dec_digit c -> advance l 1 |> lex_decint_body (Bytes.cat b (bytes_of_char c)) s
        | Some '_' -> advance l 1 |> lex_decint_body b s
        | Some c when is_implicit_break c -> l |> push (Token.Decimal_integer (b, from s l.offset l.source))
        | None -> l |> push (Token.Decimal_integer (b, from s l.offset l.source))
        | Some _ -> l |> lex_mal_int_body s
let rec lex_hexint_body b s l =
    match look l 0 with
        Some c when is_hex_digit c -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char c)) s
        | Some '_' -> advance l 1 |> lex_hexint_body b s
        | Some c when is_implicit_break c -> l |> push (Token.Hexadecimal_integer (b, from s l.offset l.source))
        | None -> l |> push (Token.Hexadecimal_integer (b, from s l.offset l.source))
        | Some _ -> l |> lex_mal_int_body s
let rec lex_forbidden_ident_body s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Forbidden_identifier (from s l.offset l.source))
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
let lex_prefixed_binint_head s l =
    match look l 0 with
        Some c when is_bin_digit c -> l |> lex_binint_body Bytes.empty s
        | _ -> l |> lex_mal_int_body s
let lex_prefixed_octint_head s l =
    match look l 0 with
        Some c when is_oct_digit c -> l |> lex_octint_body Bytes.empty s
        | _ -> l |> lex_mal_int_body s
let lex_prefixed_decint_head s l =
    match look l 0 with
        Some c when is_dec_digit c -> l |> lex_decint_body Bytes.empty s
        | _ -> l |> lex_mal_int_body s
let lex_prefixed_hexint_head s l =
    match look l 0 with
        Some c when is_hex_digit c -> l |> lex_hexint_body Bytes.empty s
        | _ -> l |> lex_mal_int_body s
let lex_token l =
    let l = skip_iws l in
        match look l 0 with
            (* SIGILS *)
            | Some '(' -> advance l 1 |> push (Token.L_parenthesis (from l.offset (l.offset + 1) l.source))
            | Some ')' -> advance l 1 |> push (Token.R_parenthesis (from l.offset (l.offset + 1) l.source))
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
            | Some '"' -> advance l 1 |> lex_str_body Bytes.empty l.offset
            (* INTEGERS *)
            | Some '0' -> begin match look l 1 with
                Some ('b' | 'B' | 'y' | 'Y') -> advance l 2 |> lex_prefixed_binint_head l.offset
                | Some ('o' | 'O' | 'q' | 'Q') -> advance l 2 |> lex_prefixed_octint_head l.offset
                | Some ('d' | 'D' | 't' | 'T') -> advance l 2 |> lex_prefixed_decint_head l.offset
                | Some ('h' | 'H' | 'x' | 'X') -> advance l 2 |> lex_prefixed_hexint_head l.offset
                | Some c when is_dec_digit c -> l |> lex_decint_body Bytes.empty l.offset
                | _ -> l |> lex_decint_body Bytes.empty l.offset
            end
            | Some c when is_dec_digit c -> l |> lex_decint_body Bytes.empty l.offset
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