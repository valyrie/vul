(* AST types *)

module From = struct
    open File
    type t =
        {offset: int; stop: int; source: Source.t}
    let from o s r =
        {offset = o; stop = s; source = r}
    let opt_from o s r =
        Some (from o s r)
    let print f =
        let open Printf in
        if f.stop - f.offset > 1 then
            sprintf "%s:[%d-%d]:"
            (Source.path f.source |> Path.to_string) f.offset (f.stop - 1)
        else
            sprintf "%s:%d:"
            (Source.path f.source |> Path.to_string) f.offset
    let maybe_print f =
        match f with
            None -> "_"
            | Some f -> print f
end
module Expr = struct
    open Numbers
    [@@@ocaml.warning "-30"]
    type malformed_token = {bytes: Bytestring.t; from: From.t option}
    type left_parenthesis = {from: From.t}
    type right_parenthesis = {from: From.t}
    type quote = {from: From.t}
    type parentheses = {left: left_parenthesis; right: right_parenthesis}
    let parentheses l r =
        Some {left = l; right = r}
    type unit = {parentheses: parentheses option}
    type number = {z: Z.t; from: From.t option}
    type string = {bytes: Bytestring.t; from: From.t option}
    type literal =
        Unit of unit
        | Number of number
        | String of string
    type identifier = {bytes: Bytestring.t; from: From.t option}
    type orphaned = {x: t}
    and cons = {left: t; right: t option; parentheses: parentheses option}
    and quoted = {x: t; quote: quote}
    and builtin = {name: Bytestring.t; fn: t -> t}
    and t =
        (* avoid wrapping in option *)
        Null
        (* orphaned *)
        | Orphaned of orphaned
        (* malformed literal token *)
        | Malformed_token of malformed_token
        (* cons *)
        | Cons of cons
        (* parenthesis *)
        | Left_parenthesis of left_parenthesis
        | Right_parenthesis of right_parenthesis
        (* identifier *)
        | Identifier of identifier
        (* quote *)
        | Quote of quote
        | Quoted of quoted
        (* literals *)
        | Literal of literal
        (* builtins *)
        | Builtin of builtin
    [@@@ocaml.warning "+30"]
    let left_parenthesis f =
        Left_parenthesis {from = f}
    let right_parenthesis f =
        Right_parenthesis {from = f}
    let quote f =
        Quote {from = f}
    let literal l =
        Literal l
    let unit p =
        Literal (Unit {parentheses = p})
    let number z f =
        Literal (Number {z = z; from = f})
    let string b f =
        Literal (String {bytes = b; from = f})
    let identifier b f =
        Identifier {bytes = b; from = f}
    let orphaned x =
        Orphaned {x = x}
    let malformed_token b f =
        Malformed_token {bytes = b; from = f}
    let cons l r p =
        Cons {left = l; right = r; parentheses = p}
    let quoted x q =
        Quoted {x = x; quote = q}
    let is_atom x =
        match x with
            Malformed_token _
            | Identifier _
            | Literal _
            | Builtin _ -> true
            | _ -> false
    let is_structural x =
        match x with
            Left_parenthesis _
            | Right_parenthesis _
            | Quote _ -> true
            | _ -> false
    let is_expr x =
        match x with
            Orphaned _
            | Cons _
            | Quoted _ -> true
            | _ -> is_atom x
    let is_error x =
        match x with
            Orphaned _
            | Malformed_token _ -> true
            | _ -> false
    let is_cons_break x =
        match x with
            Right_parenthesis _ -> true
            | _ -> false
    let is_cons x =
        match x with
            Cons _ -> true
            | _ -> false
    let is_applicable x =
        match x with
            Builtin _ -> true
            | _ -> false
    let rec fold_left f i x =
        match x with
            Cons {left = left; right = Some right; _} ->
                fold_left f (f i left) right
            | Cons {left = left; right = None; _} ->
                f i left
            | Literal Unit _ ->
                i
            | _ -> raise @@ Invalid_argument "cannot fold_left on non-lists"
    let len x =
        let inner n _ =
            n + 1 in
        fold_left inner 0 x
    let break_indent i s =
        let open Printf in
        match s with
            "" -> ""
            | _ ->
                sprintf "%s%s\n" (String.make i ' ')
                    @@ String.trim s
    let rec print_cons_right ?indent:(indent=0) x =
        let open Printf in
        break_indent indent
            @@ match x with
                Cons {left = l; right = None; parentheses = None} ->
                    print ~indent:(indent) l
                | Cons {left = l; right = Some r; parentheses = None} ->
                    sprintf "%s%s"
                        (print ~indent:(indent) l)
                        (print_cons_right ~indent:(indent) r)
                | _ ->
                    print ~indent:(indent) x
    and print ?indent:(indent=0) x =
        let open Printf in
        break_indent indent
            @@ match x with
                Null -> "Null"
                | Orphaned o ->
                    sprintf "Orphaned\n%s"
                        (print ~indent:(indent + 1) o.x)
                | Malformed_token t ->
                    sprintf "%s Malformed_token %s"
                        (From.maybe_print t.from)
                        (Bytestring.escaped_str_of t.bytes)
                | Cons {
                    left = l;
                    right = r;
                    parentheses = None} ->
                        sprintf "Cons\n%s%s"
                            (print ~indent:(indent + 1) l)
                            @@ Option.fold
                                ~none:""
                                ~some:(print_cons_right ~indent:(indent + 1))
                                r
                | Cons {
                    left = l;
                    right = r;
                    parentheses = Some p} ->
                        sprintf "Cons\n%s%s%s%s"
                            (print ~indent:(indent + 1)
                                (Left_parenthesis p.left))
                            (print ~indent:(indent + 1) l)
                            (Option.fold
                                ~none:""
                                ~some:(print_cons_right ~indent:(indent + 1))
                                r)
                            (print ~indent:(indent + 1)
                                (Right_parenthesis p.right))
                | Identifier i ->
                    sprintf "%s Identifier %s" (From.maybe_print i.from)
                        (if Bytestring.is_printable i.bytes then 
                            Bytestring.to_string i.bytes
                        else
                            sprintf "i\"%s\""
                                @@ Bytestring.escaped_str_of i.bytes)
                | Left_parenthesis l ->
                    sprintf "%s (" (From.print l.from)
                | Right_parenthesis r ->
                    sprintf "%s )" (From.print r.from)
                | Quote q ->
                    sprintf "%s '" (From.print q.from)
                | Quoted q ->
                    sprintf "Quoted\n%s%s"
                        (print ~indent:(indent + 1) (Quote q.quote))
                        (print ~indent:(indent + 1) q.x)
                | Literal l ->
                    begin match l with
                        Unit {parentheses = Some parens} ->
                            sprintf "Unit\n%s%s"
                                (print ~indent:(indent + 1)
                                    (Left_parenthesis parens.left))
                                (print ~indent:(indent + 1)
                                    (Right_parenthesis parens.right))
                        | Unit {parentheses = None} ->
                            "Unit"
                        | Number n ->
                            sprintf "%s Number %s"
                                (From.maybe_print n.from)
                                (Numbers.Z.to_string n.z)
                        | String s ->
                            sprintf "%s String %s"
                                (From.maybe_print s.from)
                                (sprintf "\"%s\""
                                    @@ Bytestring.escaped_str_of s.bytes)
                end
                | Builtin b ->
                    sprintf "Builtin %s"
                        (if Bytestring.is_printable b.name then 
                            Bytestring.to_string b.name
                        else
                            sprintf "i\"%s\""
                                @@ Bytestring.escaped_str_of b.name)
end