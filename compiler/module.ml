(* source module *)
open File

type t =
    {
        name: string;
        source: File.Source.t;
        includes: File.Include.t list;
    }
let create s i =
    {name = Path.base (Source.path s) ;source = s; includes = i}
let expr_of m =
    Parser.parse_expr (Parser.of_source m.source)
(* TODO *)