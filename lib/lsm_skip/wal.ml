type error = Wal_error of string

type config = {
  data_directory : string;
  wal_max_bytes : int;
}

let default_config = {
  data_directory = "./db_data";
  wal_max_bytes = 32 * 1024 * 1024;
}

type t = {
  path : string;
  fd : Unix.file_descr;
  lock : Mutex.t;
  mutable current_size : int;
  max_size : int;
  mutable active : bool;
}

let with_lock (t : t) (f : unit -> 'a) : 'a =
  Mutex.lock t.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.lock) f

let create ?(config = default_config) () =
  if not (Sys.file_exists config.data_directory) then
    Unix.mkdir config.data_directory 0o755;
  let path = Filename.concat config.data_directory "current.wal" in
  let flags = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ] in
  let fd = Unix.openfile path flags 0o644 in
  let stats = Unix.fstat fd in
  {
    path;
    fd;
    lock = Mutex.create ();
    current_size = stats.Unix.st_size;
    max_size = config.wal_max_bytes;
    active = true;
  }

let append t payload =
  with_lock t (fun () ->
      if not t.active then
        Error (Wal_error "WAL is closed; cannot append new transactions")
      else
        try
          let line = payload ^ "\n" in
          let len = String.length line in
          let _written = Unix.write_substring t.fd line 0 len in
          Unix.fsync t.fd;
          t.current_size <- t.current_size + len;
          if t.current_size >= t.max_size then
            Printf.printf
              "[WAL] Capacity reached (%d bytes). Checkpoint required.\n%!"
              t.current_size;
          Ok ()
        with Unix.Unix_error (err, _, _) ->
          Error (Wal_error (Printf.sprintf "WAL write failed: %s" (Unix.error_message err))))


let append_bytes t (payload : bytes) =
  with_lock t (fun () ->
      if not t.active then
        Error (Wal_error "WAL is closed; cannot append new transactions")
      else
        try
          let len = Bytes.length payload in
          let _written = Unix.write t.fd payload 0 len in
          Unix.fsync t.fd;
          t.current_size <- t.current_size + len;
          if t.current_size >= t.max_size then
            Printf.printf
              "[WAL] Capacity reached (%d bytes). Checkpoint required.\n%!"
              t.current_size;
          Ok ()
        with Unix.Unix_error (err, _, _) ->
          Error (Wal_error (Printf.sprintf "WAL write failed: %s" (Unix.error_message err))))

let append_batch _t _payloads =
  Error (Wal_error "Append batch not implemented yet")

let log_drop t graph_name =
  let payload = Printf.sprintf "DROP:%s" graph_name in
  append t payload

let close t =
  with_lock t (fun () ->
      if not t.active then Ok ()
      else
        try
          Unix.close t.fd;
          t.active <- false;
          Ok ()
        with Unix.Unix_error (err, _, _) ->
          Error (Wal_error (Printf.sprintf "Failed to close WAL: %s" (Unix.error_message err))))

let checkpoint (t : t) ~page_store =
  ignore page_store;
  ignore t;
  Error (Wal_error "Checkpoint not implemented yet")

let recover config =
  let file_path = Filename.concat config.data_directory "current.wal" in
  if not (Sys.file_exists file_path) then Ok []
  else
    try
      let ic = open_in file_path in
      let lines = ref [] in
      (try
         while true do
           lines := input_line ic :: !lines
         done
       with End_of_file -> ());
      close_in ic;
      Ok (List.rev !lines)
    with Sys_error msg -> Error (Wal_error (Printf.sprintf "WAL recovery failed: %s" msg))


let sync t =
  with_lock t (fun () ->
      if not t.active then Error (Wal_error "WAL is closed; cannot sync")
      else
        try
          Unix.fsync t.fd;
          Ok ()
        with Unix.Unix_error (err, _, _) ->
          Error (Wal_error (Printf.sprintf "WAL sync failed: %s" (Unix.error_message err))))
