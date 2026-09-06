open Lsm_skip

let () =
  let t = Concurrent_skip.create compare in
  let n_threads = 8 in
  let ops_per_thread = 20_000 in
  let key_space = 50 in
  let done_count = Atomic.make 0 in

  let worker id =
    let st = Random.State.make [| id |] in
    for _ = 1 to ops_per_thread do
      let k = Random.State.int st key_space in
      match Random.State.int st 3 with
      | 0 -> Concurrent_skip.insert t k (id * 1_000_000 + k)
      | 1 -> ignore (Concurrent_skip.remove t k)
      | _ -> ignore (Concurrent_skip.get t k)
    done;
    Atomic.incr done_count
  in

  let threads = Array.init n_threads (fun id -> Thread.create worker id) in
  Array.iter Thread.join threads;
  assert (Atomic.get done_count = n_threads);

  (* Structural invariants after the storm: no duplicate keys, strictly
     ascending order, [length] agrees with what [to_list] actually sees,
     and every surviving key is independently gettable. *)
  let entries = Concurrent_skip.to_list t in
  let keys = List.map fst entries in
  let rec strictly_increasing = function
    | a :: (b :: _ as rest) -> a < b && strictly_increasing rest
    | _ -> true
  in
  assert (strictly_increasing keys);
  assert (Concurrent_skip.length t = List.length entries);
  List.iter (fun (k, v) -> assert (Concurrent_skip.get t k = Some v)) entries;

  Printf.printf
    "STRESS TEST PASSED: %d threads x %d ops, final size = %d\n"
    n_threads ops_per_thread (List.length entries)
