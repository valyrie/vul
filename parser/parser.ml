(* source lexing/parsing *)

module type Reader = sig
    type t
    val path_of : t -> string
    val read_bytes : t -> int -> int -> bytes
    val read_byte : t -> int -> char option
end

module Make (R: Reader) = struct
    module Expr = struct
        [@@@ocaml.warning "-30"]
        type from = {start: int; stop: int; reader: R.t}
        type ident = {from: from}
        type lpar = {from: from}
        type rpar = {from: from}
        type empty = {left: lpar; right: rpar}
        type num = {from: from}
        type str = {from: from}
        type lit =
              Empty of empty
            | Num of num
            | Str of str
        type orphaned = {expr: t; prev: error option}
        and malformed = {from: from; prev: error option}
        and error =
              Orphaned of orphaned
            | Malformed of malformed
        and parens = {left: lpar; expr: t; right: rpar}
        and cons = {left: t; right: t option}
        and t =
              Ident of ident
            | Lpar of lpar
            | Rpar of rpar
            | Empty of empty
            | Lit of lit
            | Error of error
            | Parens of parens
            | Cons of cons
        [@@@ocaml.warning "+30"]
    end
    (* TODO *)
end