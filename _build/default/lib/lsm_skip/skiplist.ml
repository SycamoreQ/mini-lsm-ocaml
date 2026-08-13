type ('k, 'v) node = {
  key : 'k;
  mutable value : 'v;
  forward : ('k, 'v) node option array;  (* forward.(i) = next node at level i *)
}

type ('k, 'v) t = {
  compare : 'k -> 'k -> int;
  max_level : int;
  p : float;                            (* probability of promoting to the next level *)
  mutable level : int;                  (* highest level currently in use (>= 1) *)
  header : ('k, 'v) node option array;  (* header.(i) = first node at level i *)
  mutable size : int;
}

let create ?(max_level = 32) ?(p = 0.5) (compare : 'k -> 'k -> int) : ('k, 'v) t =
  { compare; max_level; p; level = 1; header = Array.make max_level None; size = 0 }

let length t = t.size
let is_empty t = t.size = 0

let random_level t =
  let lvl = ref 1 in
  while Random.float 1.0 < t.p && !lvl < t.max_level do
    incr lvl
  done;
  !lvl

(* Walks from the highest occupied level down to level 0, advancing as far
   right as possible at each level before dropping down. Returns the
   per-level predecessor array (`None` = header) — Pugh's "update" vector,
   used by both [insert] (to splice in a new node) and [get] (whose
   update.(0) forward pointer is the search candidate). *)
let find_predecessors t key =
  let update = Array.make t.max_level None in
  let current_forward = ref t.header in
  let current_node = ref None in
  for i = t.level - 1 downto 0 do
    let advancing = ref true in
    while !advancing do
      match !current_forward.(i) with
      | Some next_node when t.compare next_node.key key < 0 ->
        current_node := Some next_node;
        current_forward := next_node.forward
      | _ -> advancing := false
    done;
    update.(i) <- !current_node
  done;
  update

let candidate_after t update =
  match update.(0) with
  | Some pred -> pred.forward.(0)
  | None -> t.header.(0)

let get t key : 'v option =
  let update = find_predecessors t key in
  match candidate_after t update with
  | Some node when t.compare node.key key = 0 -> Some node.value
  | _ -> None

let contains_key t key = Option.is_some (get t key)

(** Insert [key, value]. If [key] already exists, overwrite its value in
    place — mini-lsm requires this so a single memtable never holds more
    than one entry per key. *)
let insert t key value : unit =
  let update = find_predecessors t key in
  match candidate_after t update with
  | Some node when t.compare node.key key = 0 -> node.value <- value
  | _ ->
    let lvl = random_level t in
    if lvl > t.level then begin
      for i = t.level to lvl - 1 do
        update.(i) <- None (* brand-new level: header is the only predecessor *)
      done;
      t.level <- lvl
    end;
    let forward = Array.make lvl None in
    let new_node = { key; value; forward } in
    for i = 0 to lvl - 1 do
      match update.(i) with
      | Some pred ->
        forward.(i) <- pred.forward.(i);
        pred.forward.(i) <- Some new_node
      | None ->
        forward.(i) <- t.header.(i);
        t.header.(i) <- Some new_node
    done;
    t.size <- t.size + 1

(** Visit every entry in ascending key order. The level-0 chain is already
    a fully sorted singly linked list, so this is just a walk. *)
let iter t (f : 'k -> 'v -> unit) : unit =
  let rec go = function
    | None -> ()
    | Some node -> f node.key node.value; go node.forward.(0)
  in
  go t.header.(0)

(** Same walk as [iter], but as a lazy [Seq.t] — closer to Rust's
    `Iterator`, and what your merge-iterator work over several
    memtables/SSTables will want to compose against. *)
let to_seq t : ('k * 'v) Seq.t =
  let rec go node () =
    match node with
    | None -> Seq.Nil
    | Some node -> Seq.Cons ((node.key, node.value), go node.forward.(0))
  in
  go t.header.(0)
