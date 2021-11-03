(* AST types *)

open File
module From = struct
    type t =
        {offset: int64; stop: int64; source: Source.t}
end
module rec Expr : sig
    type t =
        (* avoid having to wrap in an option type *)
        None
        (* cons *)
        | Cons of {left: Expr.t; right: Expr.t}
        (* structural whitespace tokens *)
        | End_of_line of {from: From.t}
        | Whitespace_break of {from: From.t}
        (* identifiers *)
        | Identifier of {bytes: bytes; from: From.t}
        (* parentheses *)
        | Left_parenthesis of {from: From.t}
        | Right_parenthesis of {from: From.t}
        | Unit of {left: Expr.t; right: Expr.t}
        | Parentheses of {x: Expr.t; left: Expr.t; right: Expr.t}
end = Expr