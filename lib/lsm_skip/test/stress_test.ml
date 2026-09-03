(* test/stress_test.ml *)

let () =
  let t = Skiplist_concurrent.create compare in
  let n_threads = 8 in
  let ops_per_thread = 20_000 in
  let key_space = 500 in
  let done_count = Atomic.make 0 in

  let worker id =
    let st = Random.State.make [| id |] in
    for _ = 1 to ops_per_thread do
      let k = Random.State.int st key_space in
      match Random.State.int st 3 with
      | 0 -> Skiplist_concurrent.insert t k (id * 1_000_000 + k)
      | 1 -> ignore (Skiplist_concurrent.remove t k)
      | _ -> ignore (Skiplist_concurrent.get t k)
    done;
    Atomic.incr done_count
  in

  let threads = Array.init n_threads (fun id -> Thread.create worker id) in
  Array.iter Thread.join threads;
  assert (Atomic.get done_count = n_threads);

  (* Structural invariants after the storm: no duplicate keys, strictly
     ascending order, [length] agrees with what [to_list] actually sees,
     and every surviving key is independently gettable. *)
  let entries = Skiplist_concurrent.to_list t in
  let keys = List.map fst entries in
  let rec strictly_increasing = function
    | a :: (b :: _ as rest) -> a < b && strictly_increasing rest
    | _ -> true
  in
  assert (strictly_increasing keys);
  assert (Skiplist_concurrent.length t = List.length entries);
  List.iter (fun (k, v) -> assert (Skiplist_concurrent.get t k = Some v)) entries;

  Printf.printf
    "STRESS TEST PASSED: %d threads x %d ops, final size = %d\n"
    n_threads ops_per_thread (List.length entries)
