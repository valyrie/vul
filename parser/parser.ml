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
        type word = {from: from}
        type lpar = {from: from}
        type rpar = {from: from}
        type empty = {left: lpar; right: rpar}
        type orphaned = {expr: t; prev: error option}
        and malformed = {from: from; prev: error option}
        and error =
              Orphaned of orphaned
            | Malformed of malformed
        and parens = {left: lpar; expr: t; right: rpar}
        and cons = {left: t; right: t option}
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
    type t = {offset: int; reader: R.t}
    let of_reader r = {offset = 0; reader = r}
    let advance ?(ahead = 1) p = {p with offset = p.offset + ahead}
    let look_byte ?(ahead = 0) p = R.read_byte p.reader (p.offset + ahead)
    let look_bytes ?(ahead = 0) p n = R.read_bytes p.reader (p.offset + ahead) n
    let at_eof p = Option.is_none @@ look_byte p
    let is_ws c =
        c = ' ' || c = '\t' || c = '\n' || c = '\r'
    let rec skip_ws p =
        match look_byte p with
              Some c when is_ws c -> skip_ws @@ advance p
            | _ -> p
    
    (* TODO *)
end