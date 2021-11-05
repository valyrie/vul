(* AST types *)

open File
module From = struct
    type t =
        {offset: int; stop: int; source: Source.t}
end
module rec Expr : sig
    type orphaned_structural_token = {x: Expr.t}
    type cons = {left: Expr.t; right: Expr.t}
    type identifier = {bytes: bytes; from: From.t}
    type left_parenthesis = {from: From.t}
    type right_parenthesis = {from: From.t}
    type unit = {left: left_parenthesis; right: right_parenthesis}
    type parentheses = {x: Expr.t; left: left_parenthesis; right: right_parenthesis}
    type quote = {from: From.t}
    type quoted = {x: Expr.t; quote: quote}
    type t =
        (* avoid having to wrap in an option type *)
        None
        (* orphaned structural tokens *)
        | Orphaned_structural_token of orphaned_structural_token
        (* cons *)
        | Cons of cons
        (* identifiers *)
        | Identifier of identifier
        (* parentheses *)
        | Left_parenthesis of left_parenthesis
        | Right_parenthesis of right_parenthesis
        | Unit of unit
        | Parentheses of parentheses
        (* quote *)
        | Quote of quote
        | Quoted of quoted
end = Expr
open Expr
let is_atom x =
    match x with
        Identifier _ -> true
        | _ -> false
let is_structural x =
    match x with
        Left_parenthesis _
        | Right_parenthesis _
        | Quote _ -> true
        | _ -> false
let is_expr x =
    match x with
        Orphaned_structural_token _
        | Cons _
        | Unit _
        | Parentheses _
        | Quoted _ -> true
        | _ -> is_atom x
let rec print_expr ?indent:(indent=0) x =
    let open Printf in
    let open File in
    let print_from (f: From.t) =
        if f.stop - f.offset > 1 then
            sprintf "%s:[%d-%d]:" (Source.path f.source |> Path.to_string) f.offset (f.stop - 1)
        else
            sprintf "%s:%d:" (Source.path f.source |> Path.to_string) f.offset in
    let print_bytes b =
        Bytes.to_string b |> String.escaped in
    sprintf "%s%s\n" (String.make indent ' ') @@ String.trim
        begin match x with
            None -> "None"
            | Orphaned_structural_token o -> sprintf "Orphaned_structural_token\n%s" (print_expr ~indent:(indent + 1) o.x)
            | Cons c -> sprintf "Cons\n%s%s" (print_expr ~indent:(indent + 1) c.left) (print_expr ~indent:(indent + 1) c.right)
            | Identifier i -> sprintf "%s Identifier \"%s\"" (print_from i.from) (print_bytes i.bytes)
            | Left_parenthesis l -> sprintf "%s (" (print_from l.from)
            | Right_parenthesis r -> sprintf "%s )" (print_from r.from)
            | Unit u -> sprintf "Unit\n%s%s"
                (print_expr ~indent:(indent + 1) (Left_parenthesis u.left))
                (print_expr ~indent:(indent + 1) (Right_parenthesis u.right))
            | Parentheses p -> sprintf "Parentheses\n%s%s%s"
                (print_expr ~indent:(indent + 1) (Left_parenthesis p.left))
                (print_expr ~indent:(indent + 1) p.x)
                (print_expr ~indent:(indent + 1) (Right_parenthesis p.right))
            | Quote q -> sprintf "%s '" (print_from q.from)
            | Quoted q -> sprintf "Quoted\n%s%s"
                (print_expr ~indent:(indent + 1) (Quote q.quote))
                (print_expr ~indent:(indent + 1) q.x)
        end