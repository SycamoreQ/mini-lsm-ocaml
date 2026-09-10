open Lsm_skip

module Cs = Concurrent_skip.Make (Dscheck.TracedAtomic) (Concurrent_skip.Make_spinlock (Dscheck.TracedAtomic))

let test_insert_remove_same_key_race () =
  (* ~p:0.0 disables Random.float control-flow divergence, keeping the trace deterministic *)
  let t = Cs.create ~p:0.0 compare in

  Dscheck.TracedAtomic.spawn (fun () -> Cs.insert t 1 "a"; ignore (Cs.remove t 1));
  Dscheck.TracedAtomic.spawn (fun () -> Cs.insert t 1 "b");

  Dscheck.TracedAtomic.final (fun () ->
      Dscheck.TracedAtomic.check (fun () ->
          let entries = Cs.to_list t in
          let keys = List.map fst entries in
          List.length keys = List.length (List.sort_uniq compare keys)
          && Cs.length t = List.length entries
          && List.for_all (fun (k, v) -> Cs.get t k = Some v) entries))

let () = Dscheck.TracedAtomic.trace test_insert_remove_same_key_race
