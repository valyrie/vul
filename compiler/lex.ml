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
    {l with v = v ::l.v}
module Token = struct
    type t =
        Lparen of int * int
        | Rparen of int * int
        | Eol of int * int
        | Err_unclosed_mlrem_body of int * int
        | Unknown_escape_string of bytes * int * int
        | String of bytes * int * int
        | Err_unclosed_string_body of int * int
        | Identifier of bytes * int * int
        | Binary_int of bytes * int * int
        | Malformed_binary_int of int * int
        | Octal_int of bytes * int * int
        | Malformed_octal_int of int * int
        | Decimal_int of bytes * int * int
        | Malformed_decimal_int of int * int
        | Hexadecimal_int of bytes * int * int
        | Malformed_hexadecimal_int of int * int
end
let of_source s =
    {v = [Token.Lparen (0, 0)]; offset = 0; source = s}
let is_iws c =
    c = ' ' || c = '\t'
let is_implicit_break c =
    is_iws c || c = '\n' || c = '(' || c = ')' || c = '"'
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
let lex_sigil l =
    match look l 0 with
        Some '(' -> advance l 1 |> push (Token.Lparen (l.offset, l.offset + 1))
        | Some ')' -> advance l 1 |> push (Token.Rparen (l.offset, l.offset + 1))
        | None -> advance l 1 |> push (Token.Rparen (l.offset, l.offset))
        | _ -> l
let lex_eol l =
    match look l 0 with
        Some '\n' -> begin match look l 1 with
            Some '\r' -> advance l 2 |> push (Token.Eol (l.offset, l.offset + 2))
            | _ -> advance l 1 |> push (Token.Eol (l.offset, l.offset + 1))
        end
        | _ -> l
let rec lex_mlrem_body s l =
    match look l 0 with
        Some '#' -> begin match look l 1 with
            Some ']' -> advance l 2
            | Some '[' -> advance l 2 |> lex_mlrem_body (l.offset) |> lex_mlrem_body s
            | _ -> advance l 1 |> lex_mlrem_body s
        end
        | None -> l |> push (Token.Err_unclosed_mlrem_body (s, l.offset))
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
let lex_rem l =
    match look l 0 with
        Some '#' -> begin match look l 1 with
            Some '[' -> advance l 2 |> lex_mlrem_body l.offset
            | _ -> advance l 1 |> lex_slrem_body
        end
        | _ -> l
let rec lex_unknown_escape_str_body b s l =
    match look l 0 with
        Some '"' -> advance l 1 |> push (Token.Unknown_escape_string (b, s, l.offset))
        | Some '\\' -> begin match look l 1 with
            Some '"' -> advance l 2 |> lex_unknown_escape_str_body (Bytes.cat b (bytes_of_char '"')) s
            | Some '\\' -> advance l 2 |> lex_unknown_escape_str_body (Bytes.cat b (bytes_of_char '\\')) s
            | Some c -> advance l 2 |> lex_unknown_escape_str_body (Bytes.cat b (bytes_of_chars ['\\'; c])) s
            | None -> advance l 1 |> push (Token.Err_unclosed_string_body (s, l.offset))
        end
        | None -> l |> push (Token.Err_unclosed_string_body (s, l.offset))
        | Some c -> advance l 1 |> lex_unknown_escape_str_body (Bytes.cat b (bytes_of_char c)) s
let rec lex_str_body b s l =
    match look l 0 with
        Some '"' -> advance l 1 |> push (Token.String (b, s, l.offset + 1))
        | Some '\\' -> begin match look l 1 with
            Some '"' -> advance l 2 |> lex_str_body (Bytes.cat b (bytes_of_char '"')) s
            | Some '\\' -> advance l 2 |> lex_str_body (Bytes.cat b (bytes_of_char '\\')) s
            | None -> advance l 1 |> push (Token.Err_unclosed_string_body (s, l.offset))
            | Some _ -> lex_unknown_escape_str_body b s l
        end
        | None -> l |> push (Token.Err_unclosed_string_body (s, l.offset))
        | Some c -> advance l 1 |> lex_str_body (Bytes.cat b (bytes_of_char c)) s
let lex_str l =
    match look l 0 with
        Some '"' -> advance l 1 |> lex_str_body Bytes.empty l.offset
        | _ -> l
let rec lex_ident_body b s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Identifier (b, s, l.offset))
        | None -> l |> push (Token.Identifier (b, s, l.offset))
        | Some c -> advance l 1 |> lex_ident_body (Bytes.cat b (bytes_of_char c)) s
let lex_ident l =
    match look l 0 with
        Some c when is_implicit_break c -> l
        | None -> l
        | Some c -> advance l 1 |> lex_ident_body (bytes_of_char c) l.offset
let rec lex_mal_binint_body s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Malformed_binary_int (s, l.offset))
        | None -> l |> push (Token.Malformed_binary_int (s, l.offset))
        | Some _ -> advance l 1 |> lex_mal_binint_body s
let rec lex_binint_body b s l =
    match look l 0 with
        Some c when is_bin_digit c -> advance l 1 |> lex_binint_body (Bytes.cat b (bytes_of_char c)) s
        | Some '_' -> advance l 1 |> lex_binint_body b s
        | Some ('b' | 'y' | 'B' | 'Y') -> advance l 1 |> push (Token.Binary_int (b, s, l.offset + 1))
        | Some c when is_implicit_break c -> l |> push (Token.Binary_int (b, s, l.offset))
        | None -> l |> push (Token.Binary_int (b, s, l.offset))
        | Some _ -> l |> lex_mal_binint_body s
let rec lex_mal_octint_body s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Malformed_octal_int (s, l.offset))
        | None -> l |> push (Token.Malformed_octal_int (s, l.offset))
        | Some _ -> advance l 1 |> lex_mal_octint_body s
let rec lex_octint_body b s l =
    match look l 0 with
        Some c when is_oct_digit c -> advance l 1 |> lex_octint_body (Bytes.cat b (bytes_of_char c)) s
        | Some '_' -> advance l 1 |> lex_octint_body b s
        | Some ('o' | 'q' | 'O' | 'Q') -> advance l 1 |> push (Token.Octal_int (b, s, l.offset + 1))
        | Some c when is_implicit_break c -> l |> push (Token.Octal_int (b, s, l.offset))
        | None -> l |> push (Token.Octal_int (b, s, l.offset))
        | Some _ -> l |> lex_mal_octint_body s
let rec lex_mal_decint_body s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Malformed_decimal_int (s, l.offset))
        | None -> l |> push (Token.Malformed_decimal_int (s, l.offset))
        | Some _ -> advance l 1 |> lex_mal_decint_body s
let rec lex_decint_body b s l =
    match look l 0 with
        Some c when is_digit c -> advance l 1 |> lex_decint_body (Bytes.cat b (bytes_of_char c)) s
        | Some '_' -> advance l 1 |> lex_decint_body b s
        | Some ('d' | 't' | 'D' | 'T') -> advance l 1 |> push (Token.Decimal_int (b, s, l.offset + 1))
        | Some c when is_implicit_break c -> l |> push (Token.Decimal_int (b, s, l.offset))
        | None -> l |> push (Token.Decimal_int (b, s, l.offset))
        | Some _ -> l |> lex_mal_decint_body s
let rec lex_mal_hexint_body s l =
    match look l 0 with
        Some c when is_implicit_break c -> l |> push (Token.Malformed_hexadecimal_int (s, l.offset))
        | None -> l |> push (Token.Malformed_hexadecimal_int (s, l.offset))
        | Some _ -> advance l 1 |> lex_mal_hexint_body s
let rec lex_hexint_body b s l =
    match look l 0 with
        Some c when is_digit c -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char c)) s
        | Some '_' -> advance l 1 |> lex_hexint_body b s
        | Some ('a' | 'A') -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char 'A')) s
        | Some ('b' | 'B') -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char 'A')) s
        | Some ('c' | 'C') -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char 'A')) s
        | Some ('d' | 'D') -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char 'A')) s
        | Some ('e' | 'E') -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char 'A')) s
        | Some ('f' | 'F') -> advance l 1 |> lex_hexint_body (Bytes.cat b (bytes_of_char 'A')) s
        | Some ('h' | 'x' | 'H' | 'X') -> advance l 1 |> push (Token.Hexadecimal_int (b, s, l.offset + 1))
        | Some c when is_implicit_break c -> l |> push (Token.Hexadecimal_int (b, s, l.offset))
        | None -> l |> push (Token.Hexadecimal_int (b, s, l.offset))
        | Some _ -> l |> lex_mal_hexint_body s
(* TODO integer numbers *)
(* TODO fractional numbers *)
(* TODO more string escape sequences *)
(* TODO symbols? *)
(* TODO lex_token *)