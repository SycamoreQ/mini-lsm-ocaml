(** A mutable SkipList implementation. *)
type ('k, 'v) t

(** [create ?max_level ?p compare] creates a new empty skip list.
    [compare] is used to order the keys. *)
val create : ?max_level:int -> ?p:float -> ('k -> 'k -> int) -> ('k, 'v) t

(** Returns the number of elements currently in the skip list. *)
val length : ('k, 'v) t -> int

(** Returns [true] if the skip list contains no elements. *)
val is_empty : ('k, 'v) t -> bool

(** Retrieves the value associated with the given key, or [None] if not found. *)
val get : ('k, 'v) t -> 'k -> 'v option

(** Checks if the given key exists in the skip list. *)
val contains_key : ('k, 'v) t -> 'k -> bool

(** Inserts a [key] and [value]. If the key already exists, overwrites its value in place. *)
val insert : ('k, 'v) t -> 'k -> 'v -> unit

(** Visits every entry in ascending key order. *)
val iter : ('k, 'v) t -> ('k -> 'v -> unit) -> unit

(** Returns a lazy sequence of the entries in ascending key order. *)
val to_seq : ('k, 'v) t -> ('k * 'v) Seq.t
