module From :
  sig
    type t = { offset : int; stop : int; source : File.Source.t; }
    val print : t option -> string
  end
module Expr :
  sig
    [@@@ocaml.warning "-30"]
    type orphaned_expr = { x : t; }
    and malformed_token = { bytes : Bytestring.t; from : From.t option; }
    and parentheses = { left : left_parenthesis; right : right_parenthesis; }
    and cons = { left : t; right : t; parentheses : parentheses option; }
    and unit = { parentheses : parentheses; }
    and identifier = { bytes : Bytestring.t; from : From.t option; }
    and left_parenthesis = { from : From.t option; }
    and right_parenthesis = { from : From.t option; }
    and quote = { from : From.t option; }
    and quoted = { x : t; quote : quote; }
    and number = { z : Numbers.Z.t; from : From.t option; }
    and string = { bytes : Bytestring.t; from : From.t option; }
    and builtin = { name : Bytestring.t; fn : t -> t; }
    and t =
        None
      | Orphaned_expr of orphaned_expr
      | Malformed_token of malformed_token
      | Cons of cons
      | Left_parenthesis of left_parenthesis
      | Right_parenthesis of right_parenthesis
      | Unit of unit
      | Identifier of identifier
      | Quote of quote
      | Quoted of quoted
      | Number of number
      | String of string
      | Builtin of builtin
    [@@@ocaml.warning "+30"]
    val is_atom : t -> bool
    val is_structural : t -> bool
    val is_expr : t -> bool
    val is_cons_break : t -> bool
    val is_cons : t -> bool
    val is_applicable : t -> bool
    val fold_left : ('a -> t -> 'a) -> 'a -> t -> 'a
    val len : t -> int
    val print : ?indent:int -> t -> String.t
  end
