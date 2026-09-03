let make () = Skiplist_concurrent.create compare

let test_basic_insert_get_upsert () =
  let t = make () in
  List.iter (fun k -> Skiplist_concurrent.insert t k (string_of_int k)) [ 5; 3; 8; 1; 9; 2; 7; 4; 6; 0 ];
  Alcotest.(check int) "length" 10 (Skiplist_concurrent.length t);
  for k = 0 to 9 do
    Alcotest.(check (option string)) (Printf.sprintf "get %d" k) (Some (string_of_int k))
      (Skiplist_concurrent.get t k)
  done;
  Alcotest.(check (option string)) "missing key" None (Skiplist_concurrent.get t 42);
  (* upsert: overwriting an existing key updates the value, doesn't grow *)
  Skiplist_concurrent.insert t 5 "FIVE";
  Alcotest.(check int) "length unchanged after upsert" 10 (Skiplist_concurrent.length t);
  Alcotest.(check (option string)) "upserted value" (Some "FIVE") (Skiplist_concurrent.get t 5)

let test_sorted_iteration () =
  let t = make () in
  List.iter (fun k -> Skiplist_concurrent.insert t k (string_of_int k)) [ 5; 3; 8; 1; 9; 2; 7; 4; 6; 0 ];
  let keys = Skiplist_concurrent.to_list t |> List.map fst in
  Alcotest.(check (list int)) "ascending" [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ] keys

let test_remove () =
  let t = make () in
  List.iter (fun k -> Skiplist_concurrent.insert t k (string_of_int k)) [ 5; 3; 8; 1; 9; 2; 7; 4; 6; 0 ];
  Alcotest.(check bool) "removes an existing key" true (Skiplist_concurrent.remove t 5);
  Alcotest.(check bool) "removing again fails" false (Skiplist_concurrent.remove t 5);
  Alcotest.(check (option string)) "gone" None (Skiplist_concurrent.get t 5);
  Alcotest.(check int) "length shrank" 9 (Skiplist_concurrent.length t);
  let keys = Skiplist_concurrent.to_list t |> List.map fst in
  Alcotest.(check (list int)) "still sorted without 5" [ 0; 1; 2; 3; 4; 6; 7; 8; 9 ] keys;
  Alcotest.(check bool) "removing a key that never existed" false (Skiplist_concurrent.remove t 999)

(* Shuffled 2,000-key insert, remove an unrelated (array-position, not
   key-value) half, and check what remains is still exactly right and
   still strictly sorted. This is the one that actually exercises multi-
   level physical unlinking, not just single-node splices. *)
let test_large_shuffled_insert_and_partial_remove () =
  let t = make () in
  let n = 2000 in
  let arr = Array.init n Fun.id in
  Random.self_init ();
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done;
  Array.iter (fun k -> Skiplist_concurrent.insert t k (k * k)) arr;
  Alcotest.(check int) "all inserted" n (Skiplist_concurrent.length t);
  Array.iter (fun k -> Alcotest.(check (option int)) (string_of_int k) (Some (k * k)) (Skiplist_concurrent.get t k)) arr;
  let sorted = Skiplist_concurrent.to_list t |> List.map fst in
  Alcotest.(check (list int)) "fully sorted" (List.init n Fun.id) sorted;

  let removed = Hashtbl.create n in
  Array.iteri
    (fun idx k ->
      if idx mod 2 = 0 then begin
        Alcotest.(check bool) (Printf.sprintf "remove %d" k) true (Skiplist_concurrent.remove t k);
        Hashtbl.replace removed k ()
      end)
    arr;
  Alcotest.(check int) "half remain" (n / 2) (Skiplist_concurrent.length t);
  let remaining = Skiplist_concurrent.to_list t |> List.map fst in
  let expected = List.init n Fun.id |> List.filter (fun k -> not (Hashtbl.mem removed k)) in
  Alcotest.(check (list int)) "survivors exactly right, still sorted" expected remaining

let () =
  Alcotest.run "skiplist_concurrent_sequential"
    [ ( "single-threaded",
        [ Alcotest.test_case "insert / get / upsert" `Quick test_basic_insert_get_upsert;
          Alcotest.test_case "sorted iteration" `Quick test_sorted_iteration;
          Alcotest.test_case "remove" `Quick test_remove;
          Alcotest.test_case "2000-key shuffled insert + partial remove" `Quick
            test_large_shuffled_insert_and_partial_remove
        ] )
    ]
