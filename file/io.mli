exception ReadError of string
exception WriteError of string
exception FileNotFound of string
exception WrongFileOrDir of string
val output_bytes : out_channel -> bytes -> unit
val input_bytes : in_channel -> int -> bytes