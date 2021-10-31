(* internal representation of filesystem paths *)

exception NoUp of string
exception MalformedPath of string
exception NonRelativePath of string
type t =
    Root
    | Rel
    | Here of t
    | Name of t * string
    | Up of t
let sep = Filename.dir_sep
let rel = Filename.current_dir_name
let par = Filename.parent_dir_name
let rec is_relative p =
    match p with
        Root -> false
        | Rel -> true
        | Here sub -> is_relative sub
        | Name (sub, _) -> is_relative sub
        | Up sub -> is_relative sub
let rec to_string_list p l =
    match p with
        Root -> "" :: l
        | Rel -> rel :: l
        | Here sub -> to_string_list sub l
        | Name (sub, name) -> to_string_list sub (name :: l)
        | Up sub -> to_string_list sub (par :: l)
let to_string p =
    String.concat sep (to_string_list p [])
let rec up p =
    match p with
        Root -> raise (NoUp "path is root")
        | Rel -> raise (NoUp "path is at relative root")
        | Here sub -> up sub
        | Name (sub, _) -> sub
        | Up sub -> sub
let up_opt p =
    try Some (up p) with
        NoUp _ -> None
let rec normalize p =
    match p with
        Root -> Root
        | Rel -> Rel
        | Here sub -> normalize sub
        | Name (sub, name) -> Name (normalize sub, name)
        | Up sub -> normalize (up sub)
let rec normalize_partial p =
    match p with
        Root -> Root
        | Rel -> Rel
        | Here sub -> normalize_partial sub
        | Name (sub, name) -> Name (normalize_partial sub, name)
        | Up sub -> match up_opt sub with
            Some upped -> normalize_partial upped
            | None -> p
let rec of_string_list p l =
    match l with
        [] -> p
        | head :: tail -> match head with
            "" -> raise (MalformedPath "cannot create a malformed path")
            | "." -> of_string_list (Here p) tail
            | ".." -> of_string_list (Up p) tail
            | name -> of_string_list (Name (p, name)) tail
let of_string s =
    match String.split_on_char '/' (String.map (fun c: char -> if not (c = '\\') then c else '/') s) with
        [] -> Root
        | head :: tail -> match head with
            "" -> of_string_list Root tail
            | "." -> of_string_list Rel tail
            | _ -> of_string_list Rel (head :: tail)
let rec append a b =
    match b with
        Root -> raise (NonRelativePath "cannot append a nonrelative path")
        | Rel -> a
        | Here sub -> Here (append a sub)
        | Name (sub, name) -> Name (append a sub, name)
        | Up sub -> Up (append a sub)
let rec base p =
    match p with
        Root -> sep
        | Rel -> rel
        | Here t -> base t
        | Name (_, s) -> Filename.remove_extension s
        | Up t -> base (up t)