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
    type t =
        L_parenthesis of int * int
        | R_parenthesis of int * int
        | Quote of int * int
        | Eol of int * int
        | Unclosed_mlrem_body of int * int
        | Unknown_escape_string of bytes * int * int
        | String of bytes * int * int
        | Unclosed_string_body of int * int
        | Identifier of bytes * int * int
        | Forbidden_identifier of int * int
        | Binary_integer of bytes * int * int
        | Malformed_binary_integer of int * int
        | Octal_integer of bytes * int * int
        | Malformed_octal_integer of int * int
        | Decimal_integer of bytes * int * int
        | Malformed_decimal_integer of int * int
        | Hexadecimal_integer of bytes * int * int
        | Malformed_hexadecimal_integer of int * int
end
let of_source s =
    {v = [Token.L_parenthesis (0, 0)]; offset = 0; source = s}
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
let is_digit c =
    is_oct_digit c || c = '8' || c = '9'
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
        | None -> l |> push (Token.Unclosed_mlrem_body (s, l.offset))
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
        Some '"' -> advance l 1 |> push (Token.Unknown_escape_string (b, s, l.offset))
        | Some '\\' -> begin match advance l 1 |> sublex_escape_body with
            sl, Some ch -> sl |> lex_unknown_escape_str_body (Bytes.cat b (bytes_of_char ch)) s
            | sl, None -> lex_unknown_escape_str_body (Bytes.cat b (bytes_of_char '\\')) s sl
        end
        | None -> l |> push (Token.Unclosed_string_body (s, l.offset))
        | Some c -> advance l 1 |> lex_unknown_escape_str_body (Bytes.cat b (bytes_of_char c)) s
let rec lex_str_body b s l =
    match look l 0 with
        Some '"' -> advance l 1 |> push (Token.String (b, s, l.offset + 1))
        | Some '\\' -> begin match advance l 1 |> sublex_escape_body with
            sl, Some ch -> sl |> lex_str_body (Bytes.cat b (bytes_of_char ch)) s
            | _, None -> lex_unknown_escape_str_body b s l
        end
        | None -> l |> push (Token.Unclosed_string_body (s, l.offset))
        | Some c -> advance l 1 |> lex_str_body (Bytes.cat b (bytes_of_char c)) s
let rec lex_mal_binint_body s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Malformed_binary_integer (s, l.offset))
        | None -> l |> push (Token.Malformed_binary_integer (s, l.offset))
        | Some _ -> advance l 1 |> lex_mal_binint_body s
let rec lex_binint_body b s l =
    match look l 0 with
        Some c when is_bin_digit c -> advance l 1 |> lex_binint_body (Bytes.cat b (bytes_of_char c)) s
        | Some '_' -> advance l 1 |> lex_binint_body b s
        | Some c when is_implicit_break c -> l |> push (Token.Binary_integer (b, s, l.offset))
        | None -> l |> push (Token.Binary_integer (b, s, l.offset))
        | Some _ -> l |> lex_mal_binint_body s
let rec lex_mal_octint_body s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Malformed_octal_integer (s, l.offset))
        | None -> l |> push (Token.Malformed_octal_integer (s, l.offset))
        | Some _ -> advance l 1 |> lex_mal_octint_body s
let rec lex_octint_body b s l =
    match look l 0 with
        Some c when is_oct_digit c -> advance l 1 |> lex_octint_body (Bytes.cat b (bytes_of_char c)) s
        | Some '_' -> advance l 1 |> lex_octint_body b s
        | Some c when is_implicit_break c -> l |> push (Token.Octal_integer (b, s, l.offset))
        | None -> l |> push (Token.Octal_integer (b, s, l.offset))
        | Some _ -> l |> lex_mal_octint_body s
let rec lex_mal_decint_body s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Malformed_decimal_integer (s, l.offset))
        | None -> l |> push (Token.Malformed_decimal_integer (s, l.offset))
        | Some _ -> advance l 1 |> lex_mal_decint_body s
let rec lex_decint_body b s l =
    match look l 0 with
        Some c when is_digit c -> advance l 1 |> lex_decint_body (Bytes.cat b (bytes_of_char c)) s
        | Some '_' -> advance l 1 |> lex_decint_body b s
        | Some c when is_implicit_break c -> l |> push (Token.Decimal_integer (b, s, l.offset))
        | None -> l |> push (Token.Decimal_integer (b, s, l.offset))
        | Some _ -> l |> lex_mal_decint_body s
let rec lex_mal_hexint_body s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Malformed_hexadecimal_integer (s, l.offset))
        | None -> l |> push (Token.Malformed_hexadecimal_integer (s, l.offset))
        | Some _ -> advance l 1 |> lex_mal_hexint_body s
let rec lex_hexint_body b s l =
    match look l 0 with
        Some c when is_digit c -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char c)) s
        | Some '_' -> advance l 1 |> lex_hexint_body b s
        | Some ('a' | 'A') -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char 'A')) s
        | Some ('b' | 'B') -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char 'B')) s
        | Some ('c' | 'C') -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char 'C')) s
        | Some ('d' | 'D') -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char 'D')) s
        | Some ('e' | 'E') -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char 'E')) s
        | Some ('f' | 'F') -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char 'F')) s
        | Some c when is_implicit_break c -> l |> push (Token.Hexadecimal_integer (b, s, l.offset))
        | None -> l |> push (Token.Hexadecimal_integer (b, s, l.offset))
        | Some _ -> l |> lex_mal_hexint_body s
let rec lex_forbidden_ident_body s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Forbidden_identifier (s, l.offset))
        | None -> l |> push (Token.Forbidden_identifier (s, l.offset))
        | Some _ -> advance l 1 |> lex_forbidden_ident_body s
let rec lex_ident_body b s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Identifier (b, s, l.offset))
        | Some c when is_forbidden_sigil c -> l |> lex_forbidden_ident_body s
        | None -> l |> push (Token.Identifier (b, s, l.offset))
        | Some c -> advance l 1 |> lex_ident_body (Bytes.cat b (bytes_of_char c)) s
let lex_token l =
    let stripped = skip_iws l in
        match look stripped 0 with
            (* SIGILS *)
            | Some '(' -> advance l 1 |> push (Token.L_parenthesis (l.offset, l.offset + 1))
            | Some ')' -> advance l 1 |> push (Token.R_parenthesis (l.offset, l.offset + 1))
            | Some '\'' -> advance l 1 |> push (Token.Quote (l.offset, l.offset + 1))
            | None -> advance l 1 |> push (Token.R_parenthesis (l.offset, l.offset))
            (* EOL *)
            | Some '\n' -> begin match look l 1 with
                Some '\r' -> advance l 2 |> push (Token.Eol (l.offset, l.offset + 2))
                | _ -> advance l 1 |> push (Token.Eol (l.offset, l.offset + 1))
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
                Some ('b' | 'B' | 'y' | 'Y') -> advance l 2 |> lex_binint_body Bytes.empty l.offset
                | Some ('o' | 'O' | 'q' | 'Q') -> advance l 2 |> lex_octint_body Bytes.empty l.offset
                | Some ('d' | 'D' | 't' | 'T') -> advance l 2 |> lex_decint_body Bytes.empty l.offset
                | Some ('h' | 'H' | 'x' | 'X') -> advance l 2 |> lex_hexint_body Bytes.empty l.offset
                | Some c when is_digit c -> l |> lex_decint_body Bytes.empty l.offset
                | _ -> l |> lex_decint_body Bytes.empty l.offset
            end
            | Some c when is_digit c -> l |> lex_decint_body Bytes.empty l.offset
            (* IDENTIFIERS *)
            | Some c -> advance l 1 |> lex_ident_body (bytes_of_char c) l.offset