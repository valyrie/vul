module From :
  sig
    type t = { offset : int; stop : int; source : File.Source.t; }
    val print : t option -> string
  end
module Expr :
  sig
    [@@@ocaml.warning "-30"]
    type orphaned_structural_token = { x : t; }
    and malformed_token = { bytes : Bytestring.t; from : From.t option; }
    and cons = { left : t; right : t; }
    and identifier = { bytes : Bytestring.t; from : From.t option; }
    and left_parenthesis = { from : From.t option; }
    and right_parenthesis = { from : From.t option; }
    and unit = { left : left_parenthesis; right : right_parenthesis; }
    and parentheses = {
      x : t;
      left : left_parenthesis;
      right : right_parenthesis;
    }
    and quote = { from : From.t option; }
    and quoted = { x : t; quote : quote; }
    and number = { z : Numbers.Z.t; from : From.t option; }
    and t =
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
    [@@@ocaml.warning "+30"]
    val is_atom : t -> bool
    val is_structural : t -> bool
    val is_expr : t -> bool
    val is_cons_break : t -> bool
    val is_cons : t -> bool
    val fold_left : ('a -> t -> 'a) -> 'a -> t -> 'a
    val print : ?indent:int -> t -> string
  end
