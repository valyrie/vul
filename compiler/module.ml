(* module evaluation *)

open Ast
module Syms = Symbols.Make(Bytestring)
let rec proc_eval x s =
    let hd = Expr.hd x in
    let tl = Expr.tl x in
    let r, s = proc_eval_expr hd s in
    match tl with
        Expr.Literal Unit _ -> r, s
        | _ -> proc_eval tl s
and proc_apply x s =
    if Expr.len x > 1 then
        let proc = proc_eval_expr (Expr.hd x) s in
        let args = Expr.fold_left
            (fun l x ->
                match l with
                    Expr.Literal Unit _ ->
                        let v, _ = proc_eval_expr x s in
                        Expr.cons v None None 
                    | _ ->
                        let v, _ = proc_eval_expr x s in
                        Expr.cons v (Some l) None)
            (Expr.unit None)
            @@ Expr.tl x in
        match proc with
            Expr.Procedure {fn = fn; _}, _ ->
                fn args s
            | _ ->
                raise @@ Invalid_argument "cannot apply non-procedure"
    else
        raise @@ Invalid_argument "invalid number of arguments"
and proc_eval_expr x s =
    match x with
        Expr.Literal _
        | Expr.Procedure _ -> x, s
        | Expr.Quoted {x = quoted; _} -> quoted, s
        | Expr.Cons _ when Expr.len x > 1 -> proc_apply x s
        | Expr.Cons {left = l; _} -> proc_eval_expr l s
        | Expr.Identifier {bytes = b; _} ->
            begin match Syms.recall b s with
                None -> raise @@ Invalid_argument "unbound name"
                | Some bound -> bound, s
            end
        | _ -> raise @@ Invalid_argument "non-evaluable expression"
let proc_let x s =
    let b, v = match Expr.to_list x with
        [Expr.Identifier {bytes = b; _}; v] ->
            b, v
        | _ ->
            raise @@ Invalid_argument "invalid arguments" in
    Expr.unit None, Syms.bind b v s
let init_syms = Syms.of_list [
            Bytestring.of_string "eval", proc_eval;
            Bytestring.of_string "apply", proc_apply;
            Bytestring.of_string "eval_expr", proc_eval_expr;
            Bytestring.of_string "let", proc_let;]
