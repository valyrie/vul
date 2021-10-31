module From :
  sig
    type t = { offset : int; stop : int; source : File.Source.t; }
  end
module Base :
  sig
    type t = Binary | Octal | Decimal | Hexadecimal
  end
module Sign :
  sig
    type t = Negative | Positive
  end
module rec Expr :
  sig
    module Error :
      sig
        type t =
            Orphaned_structural_token of Expr.t
          | Unclosed_block_remark of From.t
          | Unknown_escape_sring_literal of From.t
          | Unclosed_string_literal of From.t
          | Malformed_number_literal of From.t
          | Forbidden_identifier of From.t
          | Unknown_escape_identifier of From.t
          | Unclosed_identifier of From.t
          | Unclosed_parenthesis of Expr.Token.Structural.left_parenthesis *
              Expr.t
      end
    module Token :
      sig
        module Structural :
          sig
            type left_parenthesis = { from : From.t; }
            type right_parenthesis = { from : From.t; }
            type quote = { from : From.t; }
            type end_of_line = { from : From.t; }
          end
        module Atomic :
          sig
            type remark = { from : From.t; }
            type string_literal = { bytes : bytes; from : From.t; }
            type integer_literal = {
              sign : Sign.t;
              digits : bytes;
              base : Base.t;
              from : From.t;
            }
            type identifier = { bytes : bytes; from : From.t; }
            type wildcard_identifier = { from : From.t; }
          end
      end
    module Nonterminal :
      sig
        type pair = { left : Expr.t; right : Expr.t; }
        type parentheses = {
          x : Expr.t;
          left : Token.Structural.left_parenthesis;
          right : Token.Structural.right_parenthesis;
        }
        type unit = {
          left : Token.Structural.left_parenthesis;
          right : Token.Structural.right_parenthesis;
        }
        type quoted = { x : Expr.t; quote : Token.Structural.quote; }
      end
    type t =
        None
      | Incomplete_parse
      | Error of Error.t
      | Left_parenthesis of Token.Structural.left_parenthesis
      | Right_parenthesis of Token.Structural.right_parenthesis
      | Quote of Token.Structural.quote
      | End_of_line of Token.Structural.end_of_line
      | Remark of Token.Atomic.remark
      | String_literal of Token.Atomic.string_literal
      | Integer_literal of Token.Atomic.integer_literal
      | Identifier of Token.Atomic.identifier
      | Wildcard_identifier of Token.Atomic.wildcard_identifier
      | Pair of Nonterminal.pair
      | Parentheses of Nonterminal.parentheses
      | Unit of Nonterminal.unit
      | Quoted of Nonterminal.quoted
  end
module Lexer :
  sig
    type lex_state = Ready | Lexing | Done
    type t = { offset : int; source : File.Source.t; state : lex_state; }
  end
type 'a t = { v : 'a list; lexer : Lexer.t; }
val of_lexer : Lexer.t -> 'a t
val of_source : File.Source.t -> 'a t
val lex_token : 'a t -> 'a t * Expr.t
val parse_expr : Expr.t t -> Expr.t
