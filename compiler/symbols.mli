module Make :
  functor (K : Map.OrderedType) ->
    sig
      type 'a t
      val empty : 'a t
      val mem : K.t -> 'a t -> bool
      val recall : K.t -> 'a t -> 'a option
      val bind : K.t -> 'a -> 'a t -> 'a t
      val of_list : (K.t * 'a) list -> 'a t
    end
