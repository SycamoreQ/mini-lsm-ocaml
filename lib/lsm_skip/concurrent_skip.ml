type ('k, 'v) node = {
  key : 'k;
  value : 'v Atomic.t;
  top_level : int;
  next : ('k, 'v) node option Atomic.t array;
  marked : bool Atomic.t;
  fully_linked : bool Atomic.t;
  stripe : int;
}

type ('k, 'v) t = {
  compare : 'k -> 'k -> int;
  max_level : int;
  p : float;
  num_stripes : int;
  stripes : Mutex.t array;
  level : int Atomic.t;
  header : ('k, 'v) node option Atomic.t array;
  size : int Atomic.t;
}

let create ?(stripes = 32) ?(max_level = 32) ?(p = 0.5) (compare : 'k -> 'k -> int) :
    ('k, 'v) t =
  {
    compare;
    max_level;
    p;
    num_stripes = stripes;
    stripes = Array.init stripes (fun _ -> Mutex.create ());
    level = Atomic.make 1;
    header = Array.init max_level (fun _ -> Atomic.make None);
    size = Atomic.make 0;
  }

let random_level t =
  let lvl = ref 1 in
  while Random.float 1.0 < t.p && !lvl < t.max_level do
    incr lvl
  done;
  !lvl

let stripe_of t key = Hashtbl.hash key mod t.num_stripes
let header_stripe = 0

let pred_stripe (pred : ('k, 'v) node option) : int =
  match pred with Some node -> node.stripe | None -> header_stripe

(* Unsynchronized traversal, reading only through [Atomic.get] — same shape
   as the single-threaded search. Marked nodes stay correctly positioned
   by key until physically unlinked, so the walk doesn't special-case
   them; only the terminal candidate check looks at [marked]/[fully_linked]. *)
let find_predecessors t key =
  let preds = Array.make t.max_level None in
  let succs = Array.make t.max_level None in
  let cur_arr = ref t.header in
  let cur_node = ref None in
  let level = Atomic.get t.level in
  for i = level - 1 downto 0 do
    let advancing = ref true in
    while !advancing do
      match Atomic.get !cur_arr.(i) with
      | Some next_node when t.compare next_node.key key < 0 ->
        cur_node := Some next_node;
        cur_arr := next_node.next
      | other ->
        succs.(i) <- other;
        advancing := false
    done;
    preds.(i) <- !cur_node
  done;
  (preds, succs)

let get t key : 'v option =
  let _, succs = find_predecessors t key in
  match succs.(0) with
  | Some node
    when t.compare node.key key = 0
         && Atomic.get node.fully_linked
         && not (Atomic.get node.marked) ->
    Some (Atomic.get node.value)
  | _ -> None

let contains_key t key = Option.is_some (get t key)

let lock_stripes t stripes_needed = List.iter (fun s -> Mutex.lock t.stripes.(s)) stripes_needed
let unlock_stripes t stripes_needed =
  List.iter (fun s -> Mutex.unlock t.stripes.(s)) stripes_needed

let rec insert t key value : unit =
  let preds, succs = find_predecessors t key in
  match succs.(0) with
  | Some node when t.compare node.key key = 0 ->
    Mutex.lock t.stripes.(node.stripe);
    if Atomic.get node.marked then begin
      Mutex.unlock t.stripes.(node.stripe);
      insert t key value
    end
    else begin
      Atomic.set node.value value;
      Mutex.unlock t.stripes.(node.stripe)
    end
  | _ ->
    let top_level = random_level t in
    let needed_stripes =
      List.init top_level (fun i -> pred_stripe preds.(i)) |> List.sort_uniq compare
    in
    lock_stripes t needed_stripes;
    let valid = ref true in
    for i = 0 to top_level - 1 do
      let actual =
        match preds.(i) with Some node -> Atomic.get node.next.(i) | None -> Atomic.get t.header.(i)
      in
      let pred_ok = match preds.(i) with Some node -> not (Atomic.get node.marked) | None -> true in
      if (not pred_ok) || actual != succs.(i) then valid := false
    done;
    if not !valid then begin
      unlock_stripes t needed_stripes;
      insert t key value
    end
    else begin
      if top_level > Atomic.get t.level then Atomic.set t.level top_level;
      let next = Array.init top_level (fun i -> Atomic.make succs.(i)) in
      let new_node =
        {
          key;
          value = Atomic.make value;
          top_level;
          next;
          marked = Atomic.make false;
          fully_linked = Atomic.make false;
          stripe = stripe_of t key;
        }
      in
      for i = 0 to top_level - 1 do
        match preds.(i) with
        | Some node -> Atomic.set node.next.(i) (Some new_node)
        | None -> Atomic.set t.header.(i) (Some new_node)
      done;
      Atomic.set new_node.fully_linked true;
      Atomic.incr t.size;
      unlock_stripes t needed_stripes
    end

let remove t key : bool =
  let _, succs = find_predecessors t key in
  match succs.(0) with
  | Some node when t.compare node.key key = 0 -> begin
      Mutex.lock t.stripes.(node.stripe);
      if Atomic.get node.marked || not (Atomic.get node.fully_linked) then begin
        Mutex.unlock t.stripes.(node.stripe);
        false
      end
      else begin
        Atomic.set node.marked true;
        Mutex.unlock t.stripes.(node.stripe);
        let rec unlink () =
          let preds', _ = find_predecessors t key in
          let needed_stripes =
            List.init node.top_level (fun i -> pred_stripe preds'.(i)) |> List.sort_uniq compare
          in
          lock_stripes t needed_stripes;
          let valid = ref true in
          for i = 0 to node.top_level - 1 do
            let points_at_node =
              match preds'.(i) with
              | Some p -> (match Atomic.get p.next.(i) with Some n -> n == node | None -> false)
              | None -> (match Atomic.get t.header.(i) with Some n -> n == node | None -> false)
            in
            if not points_at_node then valid := false
          done;
          if not !valid then begin
            unlock_stripes t needed_stripes;
            unlink ()
          end
          else begin
            for i = 0 to node.top_level - 1 do
              match preds'.(i) with
              | Some p -> Atomic.set p.next.(i) (Atomic.get node.next.(i))
              | None -> Atomic.set t.header.(i) (Atomic.get node.next.(i))
            done;
            unlock_stripes t needed_stripes
          end
        in
        unlink ();
        Atomic.decr t.size;
        true
      end
    end
  | _ -> false

let length t = Atomic.get t.size
let is_empty t = Atomic.get t.size = 0

let to_list t : ('k * 'v) list =
  let rec go acc node_opt =
    match node_opt with
    | None -> List.rev acc
    | Some node ->
      let acc =
        if Atomic.get node.fully_linked && not (Atomic.get node.marked) then
          (node.key, Atomic.get node.value) :: acc
        else acc
      in
      go acc (Atomic.get node.next.(0))
  in
  go [] (Atomic.get t.header.(0))
