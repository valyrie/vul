(* generalized, shadowable symbol bindings *)

module Make (K: Map.OrderedType) = struct
    module Inner_map = Map.Make(K)
    type 'a shadow =
        Terminal of 'a
        | Shadowed of 'a * 'a shadow
    type 'a t = 'a shadow Inner_map.t
    let empty: 'a t = Inner_map.empty
    let mem k (symbols: 'a t) = Inner_map.mem k symbols
    let recall k (symbols: 'a t) =
        match Inner_map.find_opt k symbols with
            None -> None
            | Some s -> match s with
                Terminal a -> Some a
                | Shadowed (a, _) -> Some a
    let bind k v symbols: 'a t =
        if not @@ mem k symbols then
            Inner_map.add k (Terminal v) symbols
        else
            let shadowed = Inner_map.find k symbols in
            Inner_map.add k (Shadowed (v, shadowed)) symbols
end