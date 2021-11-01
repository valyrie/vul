type kind = Ast
type t = {
  dst : string;
  wrt : string option;
  output : File.Output.t;
  kind : kind;
}
val output_bytes : t -> bytes -> unit
val close : t -> unit
val destroy : t -> unit
val of_spec : string -> t
