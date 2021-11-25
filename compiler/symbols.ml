(* generalized, shadowable symbol bindings *)

module type Key = sig
    type t
    val compare : t -> t -> int
end
module Make (K: Key) = struct
    type 'a bound =
        {k: K.t; v: 'a; shadowed: 'a t}
    and 'a t =
        Null
        | Bound of 'a bound
    let empty = Null
    let bind k v shadowed =
        Bound {k = k; v = v; shadowed = shadowed}
    let make k v =
        bind k v Null
    let rec recall k symbols =
        match symbols with
            Null -> None
            | Bound b ->
                if K.compare k b.k = 0 then
                    Some b.v
                else
                    recall k b.shadowed
    let mem k symbols =
        match recall k symbols with
            None -> false
            | Some _ -> true
end