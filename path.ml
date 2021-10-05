(* internal representation of filesystem paths *)

type path =
    Root
    | Rel
    | Here of path
    | Name of path * string
    | Up of path

(* TODO basic manipulation fns *)

let rec is_relative (p: path): bool =
    match p with
        Root -> false
        | Rel -> true
        | Here sub -> is_relative sub
        | Name (sub, _) -> is_relative sub
        | Up sub -> is_relative sub

let rec path_to_string_list (p: path) (l: string list): string list =
    match p with
        Root -> "" :: l
        | Rel -> "." :: l
        | Here sub -> path_to_string_list sub l
        | Name (sub, name) -> path_to_string_list sub (name :: l)
        | Up sub -> path_to_string_list sub (".." :: l)
let to_string (p: path): string =
    String.concat "/" (path_to_string_list p [])

(* TODO: go up *)
(* TODO: normalize a path *)
(* TODO: of_string *)
(* TODO: apply / cd*)