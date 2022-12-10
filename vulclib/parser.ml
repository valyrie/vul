(* source lexing/parsing *)

module type Source = sig
    type t
    val path_of : t -> string
    val read_bytes : t -> int -> int -> bytes
    val read_byte : t -> int -> char option
end
exception Parser_error
module Expr = struct
    [@@@ocaml.warning "-30"]
    type from = {text: bytes; start: int; stop: int; path: string}
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
module Make (S: Source) = struct
    let word f = Expr.Word {from = f}
    let lpar f = Expr.Lpar {from = f}
    let rpar f = Expr.Rpar {from = f}
    let empty l r = Expr.Empty {left = l; right = r}
    let orphaned x p = Expr.Orphaned {expr = x; prev = p}
    let malformed f p = Expr.Malformed {from = f; prev = p}
    let error e = Expr.Error e
    let parens l x r = Expr.Parens {left = l; expr = x; right = r}
    let cons l r = Expr.Cons {left = l; right = r}
    type t = {offset: int; source: S.t; last_error: Expr.error option}
    let of_source r = {offset = 0; source = r; last_error = None}
    let advance ?(ahead = 1) p = {p with offset = p.offset + ahead}
    let look_byte ?(ahead = 0) p = S.read_byte p.source (p.offset + ahead)
    let make_from start p: Expr.from = {text = S.read_bytes p.source start p.offset; start = start; stop = p.offset; path = S.path_of p.source}
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
            | _ -> let err = malformed (make_from start p) p.last_error in
                {p with last_error = Some err}, Some (error @@ err)
    let rec lex_word_body_quoted_escape_hex start p =
        match look_byte p, look_byte ~ahead:1 p with
              Some a, Some b when is_hex_digit a && is_hex_digit b -> lex_word_body_quoted start @@ advance ~ahead:2 p
            | Some a, _ when is_hex_digit a -> lex_word_body_quoted start @@ advance p
            | _ -> lex_malformed_body start p
    and lex_word_body_quoted_escape start p =
        match look_byte p with
              Some '"' | Some '\\' | Some 'n' | Some 'r' | Some 't' | Some 's' | Some ' '
            | Some 'b' | Some 'd' | Some 'f' | Some 'v' | Some 'a' | Some 'e' | Some '0' -> lex_word_body_quoted start @@ advance p
            | Some 'x' -> lex_word_body_quoted_escape_hex start @@ advance p
            | Some _ | None -> lex_malformed_body start p
    and lex_word_body_quoted start p =
        match look_byte p with
              Some '"' -> lex_word_body start @@ advance p
            | Some '\\' -> lex_word_body_quoted_escape start @@ advance p
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
    let is_syntax (t: Expr.t) =
        match t with
              Lpar _ | Rpar _ -> true
            | _ -> false
    let is_cons_break (t: Expr.t option) =
        match t with
              None -> true
            | Some (Rpar _) -> true
            | _ -> false
    let is_cons (t: Expr.t) =
        match t with
              Cons _ -> true
            | _ -> false
    let la1 p =
        let (_, t) = lex p in
        t
    let rec drop ?(n = 1) l =
        match n with
              0 -> l
            | _ -> drop ~n:(n - 1) @@ List.tl l
    let rec shift p l = 
        let p, o = lex p in
        match o with
              None -> raise Parser_error
            | Some t -> parse_step p @@ t :: l
    and reduce n x p l =
        parse_step p @@ x :: (drop ~n:n l)
    and parse_step p v =
        match la1 p, v with
            (* reduce: empty *)
              _, Rpar r :: Lpar l :: _ -> reduce 2 (empty l r) p v
            (* reduce: parenthesis *)
            | _, Rpar r :: x :: Lpar l :: _ -> reduce 3 (parens l x r) p v
            (* reduce: cons *)
            | la, hd :: pv :: _ when
                   is_cons hd
                && not @@ is_syntax pv
                && is_cons_break la ->
                    reduce 2 (cons pv @@ Some hd) p v
            | la, hd :: pv :: _ when
                   not @@ is_cons hd
                && not @@ is_syntax hd
                && not @@ is_syntax pv
                && is_cons_break la ->
                    reduce 1 (cons hd @@ None) p v
            (* reduce orphaned syntax *)
            | None, x :: _ when is_syntax x ->
                let err = orphaned x p.last_error in
                let p = {p with last_error = Some err} in
                reduce 1 (error err) p v
            (* return *)
            | None, [x] -> x, p.last_error
            (* shift *)
            | _, _ -> shift p v
    let parse r = parse_step (of_source r) []
end