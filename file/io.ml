(* file i/o operations *)

exception ReadError of string
exception WriteError of string
exception FileNotFound of string
exception WrongFileOrDir of string
let output_bytes chan buf =
    output_bytes chan buf

let rec input_bytes_prepend chan len prepend =
    if len > 0 then
      let buf = Bytes.create len in
        let readin = input chan buf 0 len in
          if readin > 0 then
            let readbuf = Bytes.sub buf 0 readin in
              input_bytes_prepend chan (len - readin) (Bytes.cat prepend readbuf)
          else
            prepend
    else
      prepend
  
  let input_bytes chan len =
    input_bytes_prepend chan len Bytes.empty