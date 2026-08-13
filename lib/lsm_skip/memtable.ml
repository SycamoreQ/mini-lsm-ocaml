open skiplist
open wal
open Atomic
open Eio

let ( / ) = Eio.Path.( / )

let write_data env =
  let open Eio.Path in
  let cwd = Eio.Stdenv.cwd env in
  save ~create:(`Exclusive 0o600) (cwd / "log.txt") "System OK"

type t = {
  map : (bytes, bytes) Skiplist.t;
  wal : Wal.t option;
  id : int;
  mutable approximate_size : int;
}


type 'a bound =
  | Included of 'a
  | Excluded of 'a
  | Unbounded

let map_bound (bound : bytes bound) : bytes bound =
  match bound with
  | Included x -> Included (Bytes.copy x)
  | Excluded x -> Excluded (Bytes.copy x)
  | Unbounded -> Unbounded

let create (id: int): t  =
  {
    map: Skiplist.create Bytes.compare; wal = None ; id; approximate_size = 0
  }


let create_with_wal t (id: int) (wal: Wal.t option) =
  match wal with
    | Some wal -> {map: Skiplist.create Bytes.compare; wal = wal; id; approximate_size = 0 }
    | None -> create id


let get_uint32_be (s : string) (pos : int) : int =
  (Char.code s.[pos] lsl 24)
  lor (Char.code s.[pos + 1] lsl 16)
  lor (Char.code s.[pos + 2] lsl 8)
  lor Char.code s.[pos + 3]

(* Stops (rather than raising) at a truncated trailing record — the WAL's
   whole purpose is surviving a crash mid-append, so a torn last record is
   an expected outcome to discard, not a corruption to fail on. *)
let read_records (data : string) : (bytes * bytes) list =
  let len = String.length data in
  let rec go pos acc =
    if pos + 4 > len then List.rev acc
    else
      let key_len = get_uint32_be data pos in
      let key_start = pos + 4 in
      if key_start + key_len + 4 > len then List.rev acc
      else
        let key = Bytes.of_string (String.sub data key_start key_len) in
        let value_len_pos = key_start + key_len in
        let value_len = get_uint32_be data value_len_pos in
        let value_start = value_len_pos + 4 in
        if value_start + value_len > len then List.rev acc
        else
          let value = Bytes.of_string (String.sub data value_start value_len) in
          go (value_start + value_len) ((key, value) :: acc)
  in
  go 0 []

let recover_from_wal (id : int) (path : _ Eio.Path.t) : (t, Wal.error) result =
  let native_path = Eio.Path.native_exn path in
  if not (Sys.file_exists native_path) then
    Ok { map = Skiplist.create Bytes.compare; wal = None; id; approximate_size = 0 }
  else
    match Eio.Path.load path with
    | exception exn ->
      Error (Wal.Wal_error (Printf.sprintf "WAL recovery failed: %s" (Printexc.to_string exn)))
    | data ->
      let records = read_records data in
      let map = Skiplist.create Bytes.compare in
      List.iter (fun (key, value) -> Skiplist.insert map key value) records;
      let approximate_size =
        List.fold_left (fun acc (k, v) -> acc + Bytes.length k + Bytes.length v) 0 records
      in
      Ok { map; wal = None; id; approximate_size }


let get (t: t) (key: bytes): bytes option =
  Skiplist.get t.map key
