(* internal representation of filesystem paths *)

exception NoUp of string

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

let rec up (p: path): path =
    match p with
        Root -> raise (NoUp "path is root")
        | Rel -> raise (NoUp "path is at relative root")
        | Here sub -> up sub
        | Name (sub, _) -> sub
        | Up sub -> sub

let rec up_opt (p: path): path option =
    try Some (up p) with
        NoUp _ -> None

let rec normalize (p: path): path =
    match p with
        Root -> Root
        | Rel -> Rel
        | Here sub -> normalize sub
        | Name (sub, name) -> Name (normalize sub, name)
        | Up sub -> normalize (up sub)

let rec normalize_partial (p: path): path =
    match p with
        Root -> Root
        | Rel -> Rel
        | Here sub -> normalize_partial sub
        | Name (sub, name) -> Name (normalize_partial sub, name)
        | Up sub -> match up_opt sub with
            Some upped -> normalize_partial upped
            | None -> p

(* TODO: of_string *)
(* TODO: apply / cd *)