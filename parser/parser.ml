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
    let word f = Expr.Word {from = f}
    let lpar f = Expr.Lpar {from = f}
    let rpar f = Expr.Rpar {from = f}
    let empty l r = Expr.Empty {left = l; right = r}
    let orphaned x p = Expr.Orphaned {expr = x; prev = p}
    let malformed f p = Expr.Malformed {from = f; prev = p}
    let error e = Expr.Error e
    let parens l x r = Expr.Parens {left = l; expr = x; right = r}
    let cons l r = Expr.Cons {left = l; right = r}
    type t = {offset: int; reader: R.t}
    let of_reader r = {offset = 0; reader = r}
    let advance ?(ahead = 1) p = {p with offset = p.offset + ahead}
    let look_byte ?(ahead = 0) p = R.read_byte p.reader (p.offset + ahead)
    let look_bytes ?(ahead = 0) p n = R.read_bytes p.reader (p.offset + ahead) n
    let at_eof p = Option.is_none @@ look_byte p
    let make_from start p: Expr.from = {start = start; stop = p.offset; reader = p.reader}
    let is_ws c =
        c = ' ' || c = '\t' || c = '\n' || c = '\r'
    let is_word_break c =
        is_ws c || c = '(' || c = ')'
    let is_oct_digit c =
        c = '0' || c = '1' || c = '2' || c = '3' || c = '4'
        || c = '5' || c = '6' || c = '7'
    let is_digit c =
         is_oct_digit c || c = '8' || c = '9'
    let is_hex_digit c =
        is_digit c || c = 'a' || c = 'A' || c = 'b' || c = 'B' || c = 'c' || c = 'C'
        || c = 'd' || c = 'D' || c = 'e' || c = 'E' || c = 'f' || c = 'F'
    let rec skip_ws p =
        match look_byte p with
              Some c when is_ws c -> skip_ws @@ advance p
            | _ -> p
    let rec lex_malformed_body start p =
        match look_byte p with
              Some c when is_word_break c -> lex_malformed_body start @@ advance p
            | _ -> p, Some (error @@ malformed (make_from start p) None)
    let rec lex_word_body_quoted_escape_hex start p =
        match look_byte p, look_byte ~ahead:1 p with
              Some a, Some b when is_hex_digit a && is_hex_digit b -> lex_word_body_quoted start @@ advance ~ahead:2 p
            | Some a, _ when is_hex_digit a -> lex_word_body_quoted start @@ advance p
            | _ -> lex_malformed_body start p
    and lex_word_body_quoted_escape start p =
        match look_byte p with
              Some '"' | Some '\\' | Some 'n' | Some 'r' | Some 't' | Some 's'
            | Some 'b' | Some 'd' | Some 'f' | Some 'v' | Some 'a' | Some 'e' -> lex_word_body_quoted start @@ advance p
            | Some '0' -> lex_word_body_quoted start @@ advance p
            | Some 'x' -> lex_word_body_quoted_escape_hex start @@ advance p
            | Some _ | None -> lex_malformed_body start p
    and lex_word_body_quoted start p =
        match look_byte p with
              Some '"' -> lex_word_body start @@ advance p
            | Some '\\' -> lex_word_body_quoted start @@ advance ~ahead:2 p
            | Some _ -> lex_word_body_quoted start @@ advance p
            | None -> lex_malformed_body start p
    and lex_word_body start p =
        match look_byte p with
              Some '"' -> lex_word_body_quoted start @@ advance p
            | Some c when not @@ is_word_break c -> lex_word_body start @@ advance p
            | _ -> p, Some (word @@ make_from start p)
    let lex p =
        let p = skip_ws p in
        match look_byte p with
              None -> p, None
            | Some '(' -> advance p, Some (lpar @@ make_from p.offset @@ advance p)
            | Some ')' -> advance p, Some (rpar @@ make_from p.offset @@ advance p)
            | Some _ -> lex_word_body p.offset p
    (* TODO *)
end