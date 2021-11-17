(* AST types *)

module From = struct
    open File
    type t =
        {offset: int; stop: int; source: Source.t}
    let print f =
        let open Printf in
        match f with
            None -> "_"
            | Some f ->
                if f.stop - f.offset > 1 then
                sprintf "%s:[%d-%d]:" (Source.path f.source |> Path.to_string) f.offset (f.stop - 1)
            else
                sprintf "%s:%d:" (Source.path f.source |> Path.to_string) f.offset
end
module Expr = struct
    open Numbers
    [@@@ocaml.warning "-30"]
    type orphaned_expr = {x: t}
    and malformed_token = {bytes: Bytestring.t; from: From.t option}
    and parentheses = {left: left_parenthesis; right: right_parenthesis}
    and cons = {left: t; right: t; parentheses: parentheses option}
    and unit = {parentheses: parentheses}
    and identifier = {bytes: Bytestring.t; from: From.t option}
    and left_parenthesis = {from: From.t option}
    and right_parenthesis = {from: From.t option}
    and quote = {from: From.t option}
    and quoted = {x: t; quote: quote}
    and number = {z: Z.t; from: From.t option}
    and string = {bytes: Bytestring.t; from: From.t option}
    and t =
        (* avoid having to wrap in an option type *)
        None
        (* orphaned *)
        | Orphaned_expr of orphaned_expr
        (* malformed literal token *)
        | Malformed_token of malformed_token
        (* cons *)
        | Cons of cons
        (* parentheses *)
        | Left_parenthesis of left_parenthesis
        | Right_parenthesis of right_parenthesis
        | Unit of unit
        (* identifier *)
        | Identifier of identifier
        (* quote *)
        | Quote of quote
        | Quoted of quoted
        (* numeric literal *)
        | Number of number
        (* string literal *)
        | String of string
    [@@@ocaml.warning "+30"]
    let is_atom x =
        match x with
            Malformed_token _
            | Identifier _
            | Unit _
            | Number _ -> true
            | _ -> false
    let is_structural x =
        match x with
            Left_parenthesis _
            | Right_parenthesis _
            | Quote _ -> true
            | _ -> false
    let is_expr x =
        match x with
            Orphaned_expr _
            | Cons _
            | Quoted _ -> true
            | _ -> is_atom x
    let is_cons_break x =
        match x with
            Right_parenthesis _
            | None -> true
            | _ -> false
    let is_cons x =
        match x with
            Cons _ -> true
            | _ -> false
    let rec fold_left f i x =
        match x with
            Cons c -> fold_left f (f i c.left) c.right 
            | _ -> f i x
    let break_indent i s =
        let open Printf in
        match s with
            "" -> ""
            | _ -> sprintf "%s%s\n" (String.make i ' ')
                @@ String.trim s
    let rec print_cons_right ?indent:(indent=0) x =
        let open Printf in
        break_indent indent
            @@ match x with
                Cons c when
                    c.parentheses = None -> sprintf "%s%s"
                    (print ~indent:(indent) c.left)
                    (print_cons_right ~indent:(indent) c.right)
                | _ -> print ~indent:(indent) x
    and print ?indent:(indent=0) x =
        let open Printf in
        break_indent indent
            @@ match x with
                None -> ""
                | Orphaned_expr o -> sprintf "Orphaned_expr\n%s"
                    (print ~indent:(indent + 1) o.x)
                | Malformed_token t -> sprintf "%s Malformed_token %s"
                    (From.print t.from)
                    (Bytestring.escaped_str_of t.bytes)
                | Cons c -> sprintf "Cons\n%s%s%s%s"
                    (Option.fold
                        ~none:""
                        ~some:(fun p ->
                            (print ~indent:(indent + 0) (Left_parenthesis p.left)))
                        c.parentheses)
                    (print ~indent:(indent + 1) c.left)
                    (print_cons_right ~indent:(indent + 1) c.right)
                    @@ Option.fold
                        ~none:""
                        ~some:(fun p ->
                            (print ~indent:(indent + 0) (Right_parenthesis p.right)))
                        c.parentheses
                | Identifier i -> sprintf "%s Identifier %s" (From.print i.from)
                    (if Bytestring.is_printable i.bytes then 
                        Bytestring.to_string i.bytes
                    else
                        sprintf "i\"%s\"" @@ Bytestring.escaped_str_of i.bytes)
                | Left_parenthesis l -> sprintf "%s (" (From.print l.from)
                | Right_parenthesis r -> sprintf "%s )" (From.print r.from)
                | Unit u -> sprintf "Unit\n%s%s"
                    (print ~indent:(indent + 1) (Left_parenthesis u.parentheses.left))
                    (print ~indent:(indent + 1) (Right_parenthesis u.parentheses.right))
                | Quote q -> sprintf "%s '" (From.print q.from)
                | Quoted q -> sprintf "Quoted\n%s%s"
                    (print ~indent:(indent + 1) (Quote q.quote))
                    (print ~indent:(indent + 1) q.x)
                | Number n -> sprintf "%s Number %s"
                    (From.print n.from)
                    (Numbers.Z.to_string n.z)
                | String s -> sprintf "%s String %s"
                        (From.print s.from)
                        (sprintf "\"%s\"" @@ Bytestring.escaped_str_of s.bytes)
end