type ('k, 'v) t

val create : ?stripes:int -> ?max_level:int -> ?p:float -> ('k -> 'k -> int) -> ('k, 'v) t
val get : ('k, 'v) t -> 'k -> 'v option
val contains_key : ('k, 'v) t -> 'k -> bool
val insert : ('k, 'v) t -> 'k -> 'v -> unit
val remove : ('k, 'v) t -> 'k -> bool
val length : ('k, 'v) t -> int
val is_empty : ('k, 'v) t -> bool
val to_list : ('k, 'v) t -> ('k * 'v) list
