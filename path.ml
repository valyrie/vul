(* internal representation of filesystem paths *)

exception NoUp of string
exception MalformedPath of string
exception NonRelativePath of string
type path =
    Root
    | Rel
    | Here of path
    | Name of path * string
    | Up of path
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
let up_opt (p: path): path option =
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
let rec path_of_string_list (p: path) (l: string list): path =
    match l with
        [] -> p
        | head :: tail -> match head with
            "" -> raise (MalformedPath "cannot create a malformed path")
            | "." -> path_of_string_list (Here p) tail
            | ".." -> path_of_string_list (Up p) tail
            | name -> path_of_string_list (Name (p, name)) tail
let of_string (s: string): path =
    match String.split_on_char '/' (String.map (fun c: char -> if not (c = '\\') then c else '/') s) with
        [] -> Root
        | head :: tail -> match head with
            "" -> path_of_string_list Root tail
            | "." -> path_of_string_list Rel tail
            | _ -> path_of_string_list Rel (head :: tail)
let rec append (a: path) (b: path): path =
    match b with
        Root -> raise (NonRelativePath "cannot append a nonrelative path")
        | Rel -> a
        | Here sub -> Here (append a sub)
        | Name (sub, name) -> Name (append a sub, name)
        | Up sub -> Up (append a sub)