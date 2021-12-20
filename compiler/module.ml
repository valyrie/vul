(* module evaluation *)

module Syms = Symbols.Make(Bytestring)
let proc_let x s =
    let b, v = match x with
        Sx.List [Sx.Name b; v] ->
            b, v
        | _ ->
            raise @@ Sx.Eval.Invalid_arguments in
    Sx.List [], Syms.bind b v s
let proc_eval x s =
    Sx.Eval.proc_apply
        (Sx.cons (Sx.Name (Bytestring.of_string "sequence")) x)
        s
let proc_repr x s =
    Sx.String (Bytestring.of_string @@ Sx.print x), s
let init_syms = Syms.of_list [
            Bytestring.of_string "apply", Sx.Eval.proc_apply;
            Bytestring.of_string "sequence", Sx.Eval.proc_sequence;
            Bytestring.of_string "let", proc_let;
            Bytestring.of_string "eval", proc_eval;
            Bytestring.of_string "repr", proc_repr]