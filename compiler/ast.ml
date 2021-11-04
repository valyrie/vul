(* AST types *)

open File
module From = struct
    type t =
        {offset: int; stop: int; source: Source.t}
end
module rec Expr : sig
    type cons = {left: Expr.t; right: Expr.t}
    type identifier = {bytes: bytes; from: From.t}
    type left_parenthesis = {from: From.t}
    type right_parenthesis = {from: From.t}
    type unit = {left: left_parenthesis; right: right_parenthesis}
    type parentheses = {x: Expr.t; left: left_parenthesis; right: right_parenthesis}
    type t =
        (* avoid having to wrap in an option type *)
        None
        (* cons *)
        | Cons of cons
        (* identifiers *)
        | Identifier of identifier
        (* parentheses *)
        | Left_parenthesis of left_parenthesis
        | Right_parenthesis of right_parenthesis
        | Unit of unit
        | Parentheses of parentheses
end = Expr
open Expr
let is_atom x =
    match x with
        Identifier _ -> true
        | _ -> false
let is_structural x =
    match x with
        Left_parenthesis _
        | Right_parenthesis _ -> true
        | _ -> false