(** Represents a single entry in the memtable. *)
type memtable_entry = {
  key : bytes;
  value : bytes option;
  timestamp : int64;
  deleted : bool;
}

(** The array-based memtable holding sorted entries. *)
type memtable = {
  mutable entries : memtable_entry array;
  mutable size : int;
}

(** Standard binary search.
    Returns [Ok index] if found, [Error insertion_index] otherwise. *)
val binary_search_by : 'a array -> ('a -> int) -> (int, int) result

(** Binary search using a key extraction function. *)
val binary_search_by_key : 'a array -> 'b -> ('a -> 'b) -> (int, int) result

(** Performs binary search to find a record in the memtable.
    [Ok i] if found (index of the record); [Error i] if not found
    (index at which the record should be inserted to keep [entries] sorted). *)
val get_index : memtable -> bytes -> (int, int) result

(** Inserts or updates a key with a value and timestamp. *)
val set : memtable -> bytes -> bytes -> int64 -> unit

(** Marks a key as deleted in the memtable (inserts a tombstone).
    Note: Signature order is [key -> timestamp -> memtable -> unit]. *)
val delete : bytes -> int64 -> memtable -> unit
