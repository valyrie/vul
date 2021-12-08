module From :
  sig
    type t = { offset : int; stop : int; source : File.Source.t; }
    val from : int -> int -> File.Source.t -> t
    val opt_from : int -> int -> File.Source.t -> t option
    val print : t -> string
    val maybe_print : t option -> string
  end
module Expr :
  sig
    [@@@ocaml.warning "-30"]
    type malformed_token = { bytes : Bytestring.t; from : From.t option; }
    type left_parenthesis = { from : From.t; }
    type right_parenthesis = { from : From.t; }
    type quote = { from : From.t; }
    type parentheses = {
      left : left_parenthesis;
      right : right_parenthesis;
    }
    val parentheses :
      left_parenthesis -> right_parenthesis -> parentheses option
    type unit = { parentheses : parentheses option; }
    type number = { z : Numbers.Z.t; from : From.t option; }
    type string = { bytes : Bytestring.t; from : From.t option; }
    type literal = Unit of unit | Number of number | String of string
    type identifier = { bytes : Bytestring.t; from : From.t option; }
    type orphaned = { x : t; }
    and cons = {
      left : t;
      right : t option;
      parentheses : parentheses option;
    }
    and quoted = { x : t; quote : quote; }
    and procedure = { name : Bytestring.t; fn : t -> t Symbols.Make(Bytestring).t -> t * t Symbols.Make(Bytestring).t; }
    and t =
        Null
      | Orphaned of orphaned
      | Malformed_token of malformed_token
      | Cons of cons
      | Left_parenthesis of left_parenthesis
      | Right_parenthesis of right_parenthesis
      | Identifier of identifier
      | Quote of quote
      | Quoted of quoted
      | Literal of literal
      | Procedure of procedure
    [@@@ocaml.warning "+30"]
    val left_parenthesis : From.t -> t
    val right_parenthesis : From.t -> t
    val quote : From.t -> t
    val literal : literal -> t
    val unit : parentheses option -> t
    val number : Numbers.Z.t -> From.t option -> t
    val string : Bytestring.t -> From.t option -> t
    val identifier : Bytestring.t -> From.t option -> t
    val orphaned : t -> t
    val malformed_token : Bytestring.t -> From.t option -> t
    val cons : t -> t option -> parentheses option -> t
    val quoted : t -> quote -> t
    val is_atom : t -> bool
    val is_structural : t -> bool
    val is_expr : t -> bool
    val is_error : t -> bool
    val is_cons_break : t -> bool
    val is_cons : t -> bool
    val is_applicable : t -> bool
    val fold_left : ('a -> t -> 'a) -> 'a -> t -> 'a
    val len : t -> int
    val nth : t -> int -> t
    val hd : t -> t
    val tl : t -> t
    val to_list : t -> t list
    val print : ?indent:int -> t -> String.t
  end
