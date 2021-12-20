(* parser type *)

open Ast
open File
type t =
    {v: Expr.t list; offset: int; source: Source.t}
let make_from off stop: From.t option =
    Some {offset = off.offset; stop = stop.offset; source = off.source}
let make_from1 off: From.t =
    {offset = off.offset; stop = off.offset + 1; source = off.source}
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
let is_negative c =
    c = '-'
let is_sign c =
    match c with
        '+' -> true
        | _ -> is_negative c
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
        | _ -> p, Expr.identifier (Bytestring.of_bytes @@ Source.read_bytes s.source s.offset (p.offset - s.offset)) @@ make_from s p
let rec lex_malformed_token_body s p =
    match look p 0 with
        Some c when not (is_break c) -> advance p 1 |> lex_malformed_token_body s
        | _ -> p, Expr.malformed_token (Bytestring.of_bytes @@ Source.read_bytes s.source s.offset (p.offset - s.offset)) @@ make_from s p
let rec get_number_body b s z =
    let open Numbers in
    if Bytes.length b > 0 then
        if Bytes.get b 0 != '_' then
            get_number_body (Bytes.sub b 1 @@ Bytes.length b - 1) s
                @@ Z.trim
                @@ Z.add (Z.of_i32 @@ Int32.of_string @@ Bytes.sub_string b 0 1)
                @@ Z.mul z @@ Z.of_int 10
        else
            get_number_body (Bytes.sub b 1 @@ Bytes.length b - 1) s z
    else
        if not s then
            z
        else
            Z.neg z
let get_number b =
    let open Numbers in
        match Bytes.get b 0 with
            '-' -> get_number_body (Bytes.sub b 1 @@ Bytes.length b - 1) true Z.zero
            | '+' -> get_number_body (Bytes.sub b 1 @@ Bytes.length b - 1) false Z.zero
            | _ -> get_number_body b false Z.zero
let rec lex_number_body s p =
    match look p 0 with
        Some c when is_digit_or_spacer c -> advance p 1 |> lex_number_body s
        | Some c when not @@ is_break c -> lex_malformed_token_body s p
        | _ -> p, Expr.number (get_number @@ Source.read_bytes s.source s.offset (p.offset - s.offset)) @@ make_from s p
let get_string b =
    Bytestring.captured_of_bytes (Bytes.sub b 1 (Bytes.length b - 2))
let rec lex_string_body s p =
    match look p 0, look p 1 with
        Some '"', _ -> advance p 1, Expr.string (get_string @@ Source.read_bytes s.source s.offset (p.offset - s.offset + 1)) @@ make_from s @@ advance p 1
        | Some '\\', Some '\"' -> advance p 2 |> lex_string_body s
        | Some '\\', Some '\\' -> advance p 2 |> lex_string_body s
        | Some _, _ -> advance p 1 |> lex_string_body s
        | None, _ -> lex_malformed_token_body s p
let lex_token p =
    let p = skip_iws p in
    match look p 0, look p 1 with
        None, _ -> p, Expr.Null
        | Some '(', _ -> advance p 1, Expr.left_parenthesis @@ make_from1 p
        | Some ')', _ -> advance p 1, Expr.right_parenthesis @@ make_from1 p
        | Some '"', _ -> advance p 1 |> lex_string_body p
        | Some s, Some c when
            is_sign s
            && is_digit c -> advance p 2 |> lex_number_body p
        | Some c, _ when is_digit c -> advance p 1 |> lex_number_body p
        | Some _, _ -> advance p 1 |> lex_ident_body p
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
        if x != Null then
            push x px |> parse_expr
        else
            parse_expr p
and reduce n x p =
    drop n p |> push x |> parse_expr
and parse_expr p: Expr.t =
    (* Printf.printf "LA:\n %s\n%s"
            (Expr.print @@ la1 p)
            (String.concat "" @@ List.mapi
                (fun i x -> Printf.sprintf "%d:\n%s" i @@ Expr.print x) p.v);
    let _ = read_line () in
    Printf.printf "\n[BREAK]\n"; *)
    match (la1 p, p.v) with
        (* REDUCE UNIT *)
        | _, (Right_parenthesis r) :: (Left_parenthesis l) :: _ -> reduce 2 (Expr.unit @@ Expr.parentheses l r) p
        (* REDUCE PARENTHESES *)
        | _, Right_parenthesis r :: Cons y :: x :: Left_parenthesis l :: _ -> reduce 4 (Expr.cons x (Some (Cons y)) @@ Expr.parentheses l r) p
        | _, Right_parenthesis r :: x :: Left_parenthesis l :: _ -> reduce 3 (Expr.cons x None @@ Expr.parentheses l r) p
        (* REDUCE CONS *)
        | la, l :: _ when
            not (Expr.is_structural l)
            && not (Expr.is_cons l)
            && Expr.is_cons_break la -> reduce 1 (Expr.cons l None None) p
        | Right_parenthesis _, _ :: Left_parenthesis _ :: _ -> shift p
        | Right_parenthesis _, _ :: _ :: Left_parenthesis _ :: _ -> shift p
        | la, r :: l :: _ when
            Expr.is_cons_break la
            && Expr.is_cons r
            && not (Expr.is_structural l) -> reduce 2 (Expr.cons l (Some r) None) p
        (* REDUCE ORPHANED TOKENS *)
        | Null, x :: _ when Expr.is_structural x -> reduce 1 (Expr.orphaned x) p
        (* RETURN *)
        | Null, [x] -> x
        | Null, [] -> Null
        (* SHIFT *)
        | _, _ -> shift p