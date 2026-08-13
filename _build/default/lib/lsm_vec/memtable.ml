type memtable_entry = {
  key : bytes;
  value : bytes option;
  timestamp : int64;
  deleted : bool;
}

type memtable = {
  mutable entries : memtable_entry array;
  mutable size : int;
}

let binary_search_by (arr : 'a array) (f : 'a -> int) : (int, int) result =
  let len = Array.length arr in
  if len = 0 then Error 0
  else begin
    let size = ref len in
    let base = ref 0 in
    while !size > 1 do
      let half = !size / 2 in
      let mid = !base + half in
      let cmp = f arr.(mid) in
      base := if cmp > 0 then !base else mid;
      size := !size - half
    done;
    let cmp = f arr.(!base) in
    if cmp = 0 then Ok !base
    else Error (!base + if cmp < 0 then 1 else 0)
  end

let binary_search_by_key (arr : 'a array) (key : 'b) (f : 'a -> 'b) : (int, int) result =
  binary_search_by arr (fun x -> compare (f x) key)

(** Performs binary search to find a record in the memtable.
    [Ok i] if found (index of the record); [Error i] if not found
    (index at which the record should be inserted to keep [entries] sorted). *)
let get_index (t : memtable) (key : bytes) : (int, int) result =
  binary_search_by_key t.entries key (fun e -> e.key)


let set (t : memtable) (key : bytes) (value : bytes) (timestamp : int64) : unit =
  let entry = { key; value = Some value; timestamp; deleted = false } in
  match get_index t key with
  | Ok idx ->
    (match t.entries.(idx).value with
     | Some v ->
       if Bytes.length value < Bytes.length v then
         t.size <- t.size - (Bytes.length v - Bytes.length value)
       else
         t.size <- t.size + (Bytes.length value - Bytes.length v)
     | None -> ());
    t.entries.(idx) <- entry
  | Error idx ->
    t.size <- t.size + Bytes.length key + Bytes.length value + 16 + 1;
    let len = Array.length t.entries in
    let new_entries = Array.make (len + 1) entry in
    Array.blit t.entries 0 new_entries 0 idx;
    Array.blit t.entries idx new_entries (idx + 1) (len - idx);
    t.entries <- new_entries


let delete key timestamp (t: memtable)=
  let entry = {key; value = None ; timestamp ; deleted = true} in
  match get_index t key with
    | Ok idx ->
      (match t.entries.(idx).value with
        | Some v ->
          t.size <- t.size - Bytes.length v
        | None -> ()
      );
      t.entries.(idx) <- entry

    | Error idx ->
      t.size <- Bytes.length key + 16 + 1;
      let len = Array.length t.entries in
      let new_entries = Array.make (len + 1) entry in
      Array.blit t.entries 0 new_entries 0 idx;
      Array.blit t.entries idx new_entries (idx + 1) (len - idx);
      t.entries <- new_entries
