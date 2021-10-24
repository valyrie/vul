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
end
let of_source s =
    {v = [Token.Lparen (0, 0)]; offset = 0; source = s}
let is_iws c =
    c = ' ' || c = '\t'
let is_implicit_break c =
    is_iws c || c = '\n' || c = '(' || c = ')' || c = '"'
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
        Some '"' -> advance l 1 |> push (Token.Unknown_escape_string (b, s, l.offset + 1))
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
        Some c when is_implicit_break c -> l |> push (Token.Identifier (b, s, l.offset + 1))
        | None -> l |> push (Token.Identifier (b, s, l.offset + 1))
        | Some c -> advance l 1 |> lex_ident_body (Bytes.cat b (bytes_of_char c)) s
let lex_ident l =
    match look l 0 with
        Some c when is_implicit_break c -> l
        | None -> l
        | Some c -> advance l 1 |> lex_ident_body (bytes_of_char c) l.offset
(* TODO numbers *)
(* TODO symbols? *)