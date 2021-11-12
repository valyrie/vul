(* AST types *)

module From = struct
    open File
    type t =
        {offset: int; stop: int; source: Source.t}
    let print (f: t) =
        let open Printf in
        if f.stop - f.offset > 1 then
            sprintf "%s:[%d-%d]:" (Source.path f.source |> Path.to_string) f.offset (f.stop - 1)
        else
            sprintf "%s:%d:" (Source.path f.source |> Path.to_string) f.offset
end
module Expr = struct
    open Numbers
    [@@@ocaml.warning "-30"]
    type orphaned_structural_token = {x: t}
    and malformed_token = {bytes: Bytestring.t; from: From.t}
    and cons = {left: t; right: t}
    and identifier = {bytes: Bytestring.t; from: From.t}
    and left_parenthesis = {from: From.t}
    and right_parenthesis = {from: From.t}
    and unit = {left: left_parenthesis; right: right_parenthesis}
    and parentheses = {x: t; left: left_parenthesis; right: right_parenthesis}
    and quote = {from: From.t}
    and quoted = {x: t; quote: quote}
    and number = {z: Z.t; from: From.t}
    and t =
        (* avoid having to wrap in an option type *)
        None
        (* orphaned structural token *)
        | Orphaned_structural_token of orphaned_structural_token
        (* malformed literal token *)
        | Malformed_token of malformed_token
        (* cons *)
        | Cons of cons
        (* identifier *)
        | Identifier of identifier
        (* parentheses *)
        | Left_parenthesis of left_parenthesis
        | Right_parenthesis of right_parenthesis
        | Unit of unit
        | Parentheses of parentheses
        (* quote *)
        | Quote of quote
        | Quoted of quoted
        (* numeric literal *)
        | Number of number
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
            Orphaned_structural_token _
            | Cons _
            | Parentheses _
            | Quoted _ -> true
            | _ -> is_atom x
    let is_cons_break x =
        match x with
            Right_parenthesis _
            | None -> true
            | _ -> false
    let rec print ?indent:(indent=0) x =
    let open Printf in
    sprintf "%s%s\n" (String.make indent ' ') @@ String.trim
        begin match x with
            None -> "None"
            | Orphaned_structural_token o -> sprintf "Orphaned_structural_token\n%s"
                (print ~indent:(indent + 1) o.x)
            | Malformed_token t -> sprintf "%s Malformed_token \"%s\""
                (From.print t.from)
                (Bytestring.escaped_str_of t.bytes)
            | Cons c -> sprintf "Cons\n%s%s"
                (print ~indent:(indent + 1) c.left)
                (print ~indent:(indent + 1) c.right)
            | Identifier i -> sprintf "%s Identifier %s" (From.print i.from)
                (if Bytestring.is_printable i.bytes then 
                    Bytestring.to_string i.bytes
                else
                    sprintf "i\"%s\"" @@ Bytestring.escaped_str_of i.bytes)
            | Left_parenthesis l -> sprintf "%s (" (From.print l.from)
            | Right_parenthesis r -> sprintf "%s )" (From.print r.from)
            | Unit u -> sprintf "Unit\n%s%s"
                (print ~indent:(indent + 1) (Left_parenthesis u.left))
                (print ~indent:(indent + 1) (Right_parenthesis u.right))
            | Parentheses p -> sprintf "Parentheses\n%s%s%s"
                (print ~indent:(indent + 1) (Left_parenthesis p.left))
                (print ~indent:(indent + 1) p.x)
                (print ~indent:(indent + 1) (Right_parenthesis p.right))
            | Quote q -> sprintf "%s '" (From.print q.from)
            | Quoted q -> sprintf "Quoted\n%s%s"
                (print ~indent:(indent + 1) (Quote q.quote))
                (print ~indent:(indent + 1) q.x)
            | Number n -> sprintf "%s Number %s"
                (From.print n.from)
                (Numbers.Z.to_string n.z)
        end
end