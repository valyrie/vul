type kind = Ast
type t = {
  dst : string;
  wrt : string option;
  output : File.Output.t;
  kind : kind;
}
val open_output : string -> File.Output.t
val close : t -> unit
val destroy : t -> unit
val ast_of : string -> string option -> t
val ast : string -> t
