module type Reader =
    sig
      type t
      val path_of : t -> string
      val read_bytes : t -> int -> int -> bytes
      val read_byte : t -> int -> char option
end
exception Parser_error
module Make :
    functor (R : Reader) ->
        sig
            module Expr :
                sig
                    [@@@ocaml.warning "-30"]
                    type from = { start : int; stop : int; reader : R.t; }
                    type word = { from : from; }
                    type lpar = { from : from; }
                    type rpar = { from : from; }
                    type empty = { left : lpar; right : rpar; }
                    type orphaned = { expr : t; prev : error option; }
                    and malformed = { from : from; prev : error option; }
                    and error = Orphaned of orphaned | Malformed of malformed
                    and parens = { left : lpar; expr : t; right : rpar; }
                    and cons = { left : t; right : t option; }
                    and t =
                          Word of word
                        | Lpar of lpar
                        | Rpar of rpar
                        | Empty of empty
                        | Error of error
                        | Parens of parens
                        | Cons of cons
                    [@@@ocaml.warning "+30"]
            end
            val word : Expr.from -> Expr.t
            val lpar : Expr.from -> Expr.t
            val rpar : Expr.from -> Expr.t
            val empty : Expr.lpar -> Expr.rpar -> Expr.t
            val orphaned : Expr.t -> Expr.error option -> Expr.error
            val malformed : Expr.from -> Expr.error option -> Expr.error
            val error : Expr.error -> Expr.t
            val parens : Expr.lpar -> Expr.t -> Expr.rpar -> Expr.t
            val cons : Expr.t -> Expr.t option -> Expr.t
            val parse : R.t -> Expr.t * Expr.error option
    end
