(* parse source files *)

module From = struct
    type t =
        {offset: int; stop: int; source: File.Source.t}
    let make offset stop source =
        {offset = offset; stop = stop; source = source}
end
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
module rec Expr: sig
    module Error: sig
        type kind =
            Orphaned_structural_token
            | Unclosed_block_remark
            | Unknown_escape_string_literal
            | Unclosed_string_literal
            | Malformed_number_literal
            | Forbidden_identifier
            | Unknown_escape_identifier
            | Unclosed_identifier
            | Unclosed_parenthesis
        type t = {from: From.t; kind: kind}
    end
    module Token: sig
        module Structural: sig
            type beginning_of_source = {from: From.t}
            type ending_of_source = {from: From.t}
            type left_parenthesis = {from: From.t}
            type right_parenthesis = {from: From.t}
            type quote = {from: From.t}
            type end_of_line = {from: From.t}
            type t =
                Beginning_of_source of beginning_of_source
                | Ending_of_source of ending_of_source
                | Left_parenthesis of left_parenthesis
                | Right_parenthesis of right_parenthesis
                | Quote of quote
                | End_of_line of end_of_line
        end
        module Atomic: sig
            type string_literal = {bytes: bytes; from: From.t}
            type integer_literal = {sign: Sign.t; digits: bytes; base: Base.t; from: From.t}
            type identifier = {bytes: bytes; from: From.t}
            type wildcard_identifier = {from: From.t}
            type unit = {from: From.t}
        end
    end
    module Nonterminal: sig
        type pair = {left: Expr.t; right: Expr.t}
        type parentheses = {x: Expr.t; left: Token.Structural.left_parenthesis; right: Token.Structural.right_parenthesis}
        type unclosed_parentheses = {x: Expr.t; left: Token.Structural.left_parenthesis}
        type quoted = {x: Expr.t; quote: Token.Structural.quote}
        type source = {x: Expr.t; beginning: Token.Structural.beginning_of_source; ending: Token.Structural.ending_of_source}
    end
    type t =
        None
        | Error of Error.t
        (* STRUCTURAL TOKENS *)
        | Structural of Token.Structural.t
        (* ATOMIC TOKENS *)
        | String_literal of Token.Atomic.string_literal
        | Integer_literal of Token.Atomic.integer_literal
        | Identifier of Token.Atomic.identifier
        | Wildcard_identifier of Token.Atomic.wildcard_identifier
        | Unit of Token.Atomic.unit
        (* NONTERMINALS *)
        | Pair of Nonterminal.pair
        | Parentheses of Nonterminal.parentheses
        | Quoted of Nonterminal.quoted
        | Source of Nonterminal.source
end = Expr
module Lexer = struct
    open Expr
    open Token
    open Structural
    open Atomic
    type lex_state =
        Ready
        | Lexing
        | Done
    type t = {offset: int; source: File.Source.t; state: lex_state}
    let bytes_of_chars l =
        Bytes.init (List.length l) (List.nth l)
    let bytes_of_char c =
        Bytes.init 1 (fun _ -> c)
    let look l n =
        File.Source.read_byte l.source (l.offset + n)
    let advance l n =
        {l with offset = l.offset + n}
    let set_state s l =
        {l with state = s}
    let from = From.make
    let of_source s =
        {offset = 0; source = s; state = Ready}
    let is_iws c =
        c = ' ' || c = '\t'
    let is_bad_ident_break c =
        is_iws c || c = '\n'
    let is_implicit_break c =
        is_bad_ident_break c || c = ')'
    let is_forbidden_sigil c =
        c = '"' || c = '\'' || c = '('
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
                Some ']' -> advance l 2, None
                | Some '[' -> let (sl, _) = advance l 2 |> lex_mlrem_body (l.offset) in sl |> lex_mlrem_body s
                | _ -> advance l 1 |> lex_mlrem_body s
            end
            | None -> l, Error {from = from s l.offset l.source; kind = Unclosed_block_remark}
            | _ -> advance l 1 |> lex_mlrem_body s
    let rec lex_slrem_body l =
        match look l 0 with
            Some '\n' -> l, None
            | Some '\\' -> begin match look l 1 with
                Some '\n' -> advance l 2 |> lex_slrem_body
                | _ -> advance l 1 |> lex_slrem_body
            end
            | None -> l, None
            | _ -> advance l 1 |> lex_slrem_body
    let rec sublex_octal s l =
        let open Option in
            let open Base in
                if String.length s < 1 then
                    match look l 1 with
                        Some c when is_digit_of c Octal -> advance l 1 |> sublex_octal (String.concat "" [s; String.make 1 c])
                        | _ -> l, None
                else if String.length s < 3 then
                    match look l 1 with
                        Some c when is_digit_of c Octal -> advance l 1 |> sublex_octal (String.concat "" [s; String.make 1 c])
                        | _ -> advance l 1, Some (Char.chr (int_of_string (String.concat "" ["0o" ;s])))
                else
                    let code = int_of_string (String.concat "" ["0o" ;s]) in
                        if code <= 0xFF then
                            advance l 1, Some (Char.chr code)
                        else
                            l, Some (Char.chr (int_of_string (String.concat "" ["0o"; (String.sub s 1 (String.length s - 1))])))
    let rec sublex_hexadecimal s l =
        let open Option in
            let open Base in
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
            | Some 'o' -> l |> sublex_octal ""
            | Some 'x' -> l |> sublex_hexadecimal ""
            | None -> l, None
            | Some _ -> l, None
    let rec lex_unknown_escape_str_body s l =
        match look l 0 with
            Some '"' -> advance l 1, Error {from = from s l.offset l.source; kind = Unknown_escape_string_literal}
            | Some '\\' -> begin match advance l 1 |> sublex_escape_body with
                sl, _ -> sl |> lex_unknown_escape_str_body s
            end
            | None -> l, Error {from = from s l.offset l.source; kind = Unclosed_string_literal}
            | Some _ -> advance l 1 |> lex_unknown_escape_str_body  s
    let rec lex_str_body raw b s l =
        match look l 0 with
            Some '"' -> advance l 1, String_literal {bytes = b; from = from s (l.offset + 1) l.source}
            | Some '\\' when not raw -> begin match advance l 1 |> sublex_escape_body with
                sl, Some ch -> sl |> lex_str_body raw (Bytes.cat b (bytes_of_char ch)) s
                | _, None -> lex_unknown_escape_str_body s l
            end
            | None -> l, Error {from = from s l.offset l.source; kind = Unclosed_string_literal}
            | Some c -> advance l 1 |> lex_str_body raw (Bytes.cat b (bytes_of_char c)) s
    let rec lex_mal_int_body s l =
        match look l 0 with
            Some c when is_implicit_break c -> l, Error {from = from s l.offset l.source; kind = Malformed_number_literal}
            | None -> l, Error {from = from s l.offset l.source; kind = Malformed_number_literal}
            | Some _ -> advance l 1 |> lex_mal_int_body s
    let produce_integer n p e b s l =
        let open Option in
            match p with
                None -> l, Integer_literal {sign = n; digits = b; base = e; from = from s l.offset l.source}
                | Some prefix -> if Base.within e prefix then
                        l, Integer_literal {sign = n; digits = b; base = prefix; from = from s l.offset l.source}
                    else
                        l, Error {from = from s l.offset l.source; kind = Malformed_number_literal}
    let rec lex_int_body n p e b s l =
        let open Base in
            match look l 0 with
                Some c when is_digit c -> advance l 1 |> lex_int_body n p (rebase e (of_digit c)) (Bytes.cat b (bytes_of_char c)) s
                | Some '_' -> advance l 1 |> lex_int_body n p e b s
                | Some c when is_base c -> if p = Option.None then
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
        let open Base in
            match look l 0 with
                Some c when is_digit c -> l |> lex_int_body n p (of_digit c) Bytes.empty s
                | _ -> l |> lex_mal_int_body s
    let lex_int_prefix_body n s l =
        let open Base in
            match look l 0 with
                Some c when is_base c -> advance l 1 |> lex_prefixed_int_head n (Some (of_char c)) s
                | _ -> l |> lex_int_body n None Decimal Bytes.empty s
    let rec lex_forbidden_ident_body s l =
        match look l 0 with
            Some c when is_bad_ident_break c -> l, Error {from = from s l.offset l.source; kind = Forbidden_identifier}
            | None -> l, Error {from = from s l.offset l.source; kind = Forbidden_identifier}
            | Some _ -> advance l 1 |> lex_forbidden_ident_body s
    let rec lex_ident_body b s l =
        match look l 0 with
            Some c when is_implicit_break c -> l, Identifier {bytes = b; from = from s l.offset l.source}
            | Some c when is_forbidden_sigil c -> l |> lex_forbidden_ident_body s
            | None -> l, Identifier {bytes = b; from = from s l.offset l.source}
            | Some c -> advance l 1 |> lex_ident_body (Bytes.cat b (bytes_of_char c)) s
    let rec lex_unknown_escape_special_ident_body s l =
        match look l 0 with
            Some '"' -> advance l 1, Error {from = from s l.offset l.source; kind = Unknown_escape_identifier}
            | Some '\\' -> begin match advance l 1 |> sublex_escape_body with
                sl, _ -> sl |> lex_unknown_escape_special_ident_body s
            end
            | None -> l, Error {from = from s l.offset l.source; kind = Unclosed_identifier}
            | Some _ -> advance l 1 |> lex_unknown_escape_special_ident_body s
    let rec lex_special_ident_body b s l =
        match look l 0 with
            Some '"' -> advance l 1, Identifier {bytes = b; from = from s (l.offset + 1) l.source}
            | Some '\\' -> begin match advance l 1 |> sublex_escape_body with
                sl, Some ch -> sl |> lex_special_ident_body (Bytes.cat b (bytes_of_char ch)) s
                | _, None -> lex_unknown_escape_special_ident_body s l
            end
            | None -> l, Error {from = from s l.offset l.source; kind = Unclosed_identifier}
            | Some c -> advance l 1 |> lex_special_ident_body (Bytes.cat b (bytes_of_char c)) s
    let lex_token l =
        match l.state with
            Done -> l, None
            | Ready -> l |> set_state Lexing, Structural (Beginning_of_source {from = from l.offset l.offset l.source})
            | Lexing -> let l = skip_iws l in
                match look l 0 with
                    None -> l |> set_state Done, Structural (Ending_of_source {from = from l.offset l.offset l.source})
                    (* SIGILS *)
                    | Some '(' -> begin match look l 1 with
                        | Some ')' -> advance l 2, Unit {from = from l.offset (l.offset + 1) l.source}
                        | _ -> advance l 1, Structural (Left_parenthesis {from = from l.offset (l.offset + 1) l.source})
                    end
                    | Some ')' -> begin match look l 1 with
                        Some c when is_implicit_break c -> advance l 1, Structural (Right_parenthesis {from = from l.offset (l.offset + 1) l.source})
                        | None -> advance l 1, Structural (Right_parenthesis {from = from l.offset (l.offset + 1) l.source})
                        | Some _ -> l |> lex_forbidden_ident_body l.offset
                    end
                    | Some '\'' -> advance l 1, Structural (Quote {from = from l.offset (l.offset + 1) l.source})
                    (* EOL *)
                    | Some '\n' -> begin match look l 1 with
                        Some '\r' -> advance l 2, Structural (End_of_line {from = from l.offset (l.offset + 2) l.source})
                        | _ -> advance l 1, Structural (End_of_line {from = from l.offset (l.offset + 1) l.source})
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
                    | Some '0' -> advance l 1 |> lex_int_prefix_body Positive l.offset
                    | Some c when Base.is_digit_of c Decimal -> l |> lex_int_body Positive None Decimal Bytes.empty l.offset
                    (* IDENTIFIERS *)
                    | Some 'i' -> begin match look l 1 with
                        Some '"' -> advance l 2 |> lex_special_ident_body Bytes.empty l.offset
                        | _ -> advance l 1 |> lex_ident_body (bytes_of_char 'i') l.offset
                    end
                    | Some '_' -> begin match look l 1 with
                        Some c when is_implicit_break c -> advance l 1, Wildcard_identifier {from = from l.offset (l.offset + 1) l.source}
                        | _ -> advance l 1 |> lex_ident_body (bytes_of_char '_') l.offset
                    end
                    | Some c -> advance l 1 |> lex_ident_body (bytes_of_char c) l.offset
end
module Is = struct
    type t =
        None
        | Error
        | Structural
        | Identifier
        | Literal
        | Wildcard
        | Nonterminal
    let is (x: Expr.t) =
        match x with
            None -> None
            (* ERROR *)
            | Error _ -> Error
            (* STRUCTURAL *)
            | Structural _ -> Structural
            (* ATOMIC TOKENS *)
            | String_literal _
            | Integer_literal _ -> Literal
            | Identifier _ -> Identifier
            | Wildcard_identifier _ -> Wildcard
            | Unit _ -> Literal
            (* NONTERMINALS *)
            | Pair _
            | Parentheses _
            | Quoted _
            | Source _ -> Nonterminal
    let is_none x =
        is x = None
    let is_structural x =
        is x = Structural
    let is_identifier x =
        is x = Identifier
    let is_literal x =
        is x = Literal
    let is_wildcard x =
        is x = Wildcard
    let is_error x =
        is x = Error
    let is_nonterminal x =
        is x = Nonterminal
    let is_atom x =
        is_identifier x
        || is_literal x
        || is_wildcard x
    let is_expr x =
        is_atom x
        || is_nonterminal x
end
type 'a t =
    {v: 'a list; lexer: Lexer.t}
let of_lexer l =
    {v = []; lexer = l}
let of_source s =
    of_lexer (Lexer.of_source s)
let lex_token p =
    match Lexer.lex_token p.lexer with
        l, t -> {p with lexer = l}, t
let shift p =
    let (p, t) = lex_token p in
        {p with v = t :: p.v}
let rec consume p n =
    if n != 0 then
        match p.v with
            [] -> raise (Invalid_argument "unable to consume expressions which do not exist")
            | _ :: tl -> consume {p with v = tl} (n - 1)
    else
        p
let v_nth p n =
    List.nth p.v n
let v_la p =
    match lex_token p with
        _, t -> t
let rec parse_expr p =
    if v_la p != None then
        (* THERE IS AT LEAST ONE UNPARSED TOKEN -- SOURCE IS NOT EMPTY *)
        if List.length p.v > 0 then
            (* THERE IS AT LEAST ONE EXPR IN THE REWRITE BUFFER *)
            ()
        else
            (* NO EXPRS IN THE REWRITE BUFFER -- SHIFT *)
            shift p |> parse_expr
    else
        (* NO MORE TOKENS TO PARSE *)
        ()