let time_it label f =
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  Printf.printf "%-45s %8.3fs\n%!" label (t1 -. t0)

let shuffled n =
  let arr = Array.init n Fun.id in
  Random.self_init ();
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done;
  arr

(* Apples-to-apples: both structures, one thread, no contention possible.
   This is the number that answers "what does Concurrent_skip's atomics +
   locking cost even when nobody's actually contending?" *)
let bench_single_threaded n =
  Printf.printf "\n=== Single-threaded insert + get, n = %d ===\n" n;
  let keys = shuffled n in

  let t1 = Lsm_skip.Skiplist.create compare in
  time_it "Skiplist: insert" (fun () -> Array.iter (fun k -> Lsm_skip.Skiplist.insert t1 k k) keys);
  time_it "Skiplist: get (every key)" (fun () ->
      Array.iter (fun k -> ignore (Lsm_skip.Skiplist.get t1 k)) keys);

  let t2 = Lsm_skip.Concurrent_skip.create compare in
  time_it "Concurrent_skip: insert (1 domain)" (fun () ->
      Array.iter (fun k -> Lsm_skip.Concurrent_skip.insert t2 k k) keys);
  time_it "Concurrent_skip: get (1 domain, every key)" (fun () ->
      Array.iter (fun k -> ignore (Lsm_skip.Concurrent_skip.get t2 k)) keys)

(* Concurrent_skip only: does throughput actually scale with domain count?
   IMPORTANT CAVEAT: this exercises real concurrent insert/remove/get on
   the same structure - exactly the situation with the open liveness bug.
   Treat these numbers as provisional, not a verdict, until that's settled.
   key_space is set large relative to ops specifically to keep collision
   rate (and therefore risk of tripping the bug) low while still getting
   a real scaling signal - it's a mitigation, not a fix, so run this one
   under `timeout` too. *)
let bench_domain_scaling ~ops_per_domain ~key_space domain_counts =
  Printf.printf "\n=== Concurrent_skip scaling, %d ops/domain, key_space = %d ===\n"
    ops_per_domain key_space;
  List.iter
    (fun n_domains ->
      let t = Lsm_skip.Concurrent_skip.create compare in
      let worker id () =
        let st = Random.State.make [| id |] in
        for _ = 1 to ops_per_domain do
          let k = Random.State.int st key_space in
          match Random.State.int st 3 with
          | 0 -> Lsm_skip.Concurrent_skip.insert t k k
          | 1 -> ignore (Lsm_skip.Concurrent_skip.remove t k)
          | _ -> ignore (Lsm_skip.Concurrent_skip.get t k)
        done
      in
      let t0 = Unix.gettimeofday () in
      let domains = Array.init n_domains (fun id -> Domain.spawn (worker id)) in
      Array.iter Domain.join domains;
      let elapsed = Unix.gettimeofday () -. t0 in
      let total_ops = n_domains * ops_per_domain in
      Printf.printf "%2d domains: %8.3fs total, %10.0f ops/sec\n%!" n_domains elapsed
        (float_of_int total_ops /. elapsed))
    domain_counts

let () =
  bench_single_threaded 100_000;
  bench_domain_scaling ~ops_per_domain:50_000 ~key_space:50_000 [ 1; 2; 4; 8 ]
