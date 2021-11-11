module From :
  sig
    type t = { offset : int; stop : int; source : File.Source.t; }
    val print : t -> string
  end
module rec Expr :
  sig
    type orphaned_structural_token = { x : Expr.t; }
    type malformed_token = { bytes : Bytestring.t; from : From.t; }
    type cons = { left : Expr.t; right : Expr.t; }
    type identifier = { bytes : Bytestring.t; from : From.t; }
    type left_parenthesis = { from : From.t; }
    type right_parenthesis = { from : From.t; }
    type unit = { left : left_parenthesis; right : right_parenthesis; }
    type parentheses = {
      x : Expr.t;
      left : left_parenthesis;
      right : right_parenthesis;
    }
    type quote = { from : From.t; }
    type quoted = { x : Expr.t; quote : quote; }
    type number = { z : Numbers.Z.t; from : From.t; }
    type t =
        None
      | Orphaned_structural_token of orphaned_structural_token
      | Malformed_token of malformed_token
      | Cons of cons
      | Identifier of identifier
      | Left_parenthesis of left_parenthesis
      | Right_parenthesis of right_parenthesis
      | Unit of unit
      | Parentheses of parentheses
      | Quote of quote
      | Quoted of quoted
      | Number of number
  end
val is_atom : Expr.t -> bool
val is_structural : Expr.t -> bool
val is_expr : Expr.t -> bool
val is_cons_break : Expr.t -> bool
val print_expr : ?indent:int -> Expr.t -> string