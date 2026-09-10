module type ATOMIC = sig
  type 'a t
  val make : 'a -> 'a t
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
  val exchange : 'a t -> 'a -> 'a
  val compare_and_set : 'a t -> 'a -> 'a -> bool
  val fetch_and_add : int t -> int -> int
  val incr : int t -> unit
  val decr : int t -> unit
end

module type MUTEX = sig
  type t
  val create : unit -> t
  val lock : t -> unit
  val unlock : t -> unit
end

module Make_spinlock (_ : ATOMIC) : MUTEX

module type S = sig
  type ('k, 'v) t
  val create : ?stripes:int -> ?max_level:int -> ?p:float -> ('k -> 'k -> int) -> ('k, 'v) t
  val get : ('k, 'v) t -> 'k -> 'v option
  val contains_key : ('k, 'v) t -> 'k -> bool
  val insert : ('k, 'v) t -> 'k -> 'v -> unit
  val remove : ('k, 'v) t -> 'k -> bool
  val length : ('k, 'v) t -> int
  val is_empty : ('k, 'v) t -> bool
  val to_list : ('k, 'v) t -> ('k * 'v) list
end

module Make (_ : ATOMIC) (_ : MUTEX) : S

include S
