(* parser type *)

open Ast
open File
type t =
    {v: Expr.t list; offset: int; source: Source.t}
let make_from off stop: From.t option =
    Some {offset = off.offset; stop = stop.offset; source = off.source}
let make_from1 off: From.t option =
    Some {offset = off.offset; stop = off.offset + 1; source = off.source}
let of_source s =
    {v = []; offset = 0; source = s}
let look p n =
    Source.read_byte p.source (p.offset + n)
let advance p n =
    {p with offset = p.offset + n}
let is_iws c =
    c = ' ' || c = '\t'
let is_break c =
    is_iws c || c = '\n' || c = '\r' || c = '(' || c = ')'
let is_digit c =
    match c with
        '0' | '1' | '2' | '3' | '4'
        | '5' | '6' | '7' | '8' | '9' -> true
        | _ -> false
let is_digit_or_spacer c =
    match c with
        '_' -> true
        | _ -> is_digit c
let rec skip_iws p =
    match look p 0 with
        Some c when is_iws c -> advance p 1 |> skip_iws
        | Some '\n' -> advance p 1 |> skip_iws
        | Some '\r' -> begin match look p 1 with
            Some '\n' -> advance p 2 |> skip_iws
            | _ -> p
        end
        | _ -> p
let rec lex_ident_body s p: t * Expr.t =
    match look p 0 with
        Some c when not (is_break c) -> advance p 1 |> lex_ident_body s
        | _ -> p, Identifier {bytes = Bytestring.of_bytes @@ Source.read_bytes s.source s.offset (p.offset - s.offset); from = make_from s p}
let rec lex_malformed_token_body s p: t * Expr.t =
    match look p 0 with
        Some c when not (is_break c) -> advance p 1 |> lex_malformed_token_body s
        | _ -> p, Malformed_token {bytes = Bytestring.of_bytes @@ Source.read_bytes s.source s.offset (p.offset - s.offset); from = make_from s p}
let rec get_number_body b z =
    let open Numbers in
    if Bytes.length b > 0 then
        if Bytes.get b 0 != '_' then
            get_number_body (Bytes.sub b 1 @@ Bytes.length b - 1) 
                @@ Z.trim
                @@ Z.add (Z.of_i32 @@ Int32.of_string @@ Bytes.sub_string b 0 1)
                @@ Z.mul z @@ Z.of_int 10
        else
            get_number_body (Bytes.sub b 1 @@ Bytes.length b - 1) z
    else
        z
let get_number b =
    let open Numbers in
        get_number_body b Z.zero
let rec lex_number_body s p =
    match look p 0 with
        Some c when is_digit_or_spacer c -> advance p 1 |> lex_number_body s
        | Some c when not @@ is_break c -> lex_malformed_token_body s p
        | _ -> p, Number {z = get_number @@ Source.read_bytes s.source s.offset (p.offset - s.offset); from = make_from s p}
let lex_token p: t * Expr.t =
    let p = skip_iws p in
    match look p 0 with
        None -> p, None
        | Some '(' -> advance p 1, Left_parenthesis {from = make_from1 p}
        | Some ')' -> advance p 1, Right_parenthesis {from = make_from1 p}
        | Some '\'' -> advance p 1, Quote {from = make_from1 p}
        | Some c when is_digit c -> advance p 1 |> lex_number_body p
        | Some _ -> advance p 1 |> lex_ident_body p
let push x p =
    {p with v = x :: p.v}
let rec drop n p =
    if n > 0 then
        drop (n - 1) {p with v = List.tl p.v}
    else
        p
let la1 p =
    let (_, x) = lex_token p in
        x
let rec shift p =
    let (px, x) = lex_token p in
        if x != None then
            push x px |> parse_expr
        else
            parse_expr p
and reduce n x p =
    drop n p |> push x |> parse_expr
and parse_expr p: Expr.t =
    match (la1 p, p.v) with
        (* REDUCE QUOTE *)
        | _, x :: (Quote q) :: _ when Expr.is_expr x -> reduce 2 (Quoted {x = x; quote = q}) p
        | Right_parenthesis _, (Quote q) :: _ -> reduce 1 (Orphaned_structural_token {x = Quote q}) p
        (* REDUCE UNIT *)
        | _, (Right_parenthesis r) :: (Left_parenthesis l) :: _ -> reduce 2 (Unit {left = l; right = r}) p
        (* REDUCE PARENS *)
        | _, (Right_parenthesis r) :: x :: (Left_parenthesis l) :: _ -> reduce 3 (Parentheses {x = x; left = l; right = r}) p
        (* REDUCE CONS *)
        | la, r :: l :: _ when
            not (Expr.is_structural r)
            && not (Expr.is_structural l)
            && Expr.is_cons_break la -> reduce 2 (Cons {left = l; right = r}) p
        (* REDUCE ORPHANED TOKENS *)
        | None, x :: _ when Expr.is_structural x -> reduce 1 (Orphaned_structural_token {x = x}) p
        (* RETURN *)
        | None, [x] -> x
        | None, [] -> None
        (* SHIFT *)
        | _, _ -> shift p