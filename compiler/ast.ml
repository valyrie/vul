(* AST types *)

open File
module From = struct
    type t =
        {offset: int; stop: int; source: Source.t}
end
module rec Expr : sig
    type t =
        (* avoid having to wrap in an option type *)
        None
        (* cons *)
        | Cons of {left: Expr.t; right: Expr.t}
        (* identifiers *)
        | Identifier of {bytes: bytes; from: From.t}
        (* parentheses *)
        | Left_parenthesis of {from: From.t}
        | Right_parenthesis of {from: From.t}
        | Unit of {left: Expr.t; right: Expr.t}
        | Parentheses of {x: Expr.t; left: Expr.t; right: Expr.t}
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