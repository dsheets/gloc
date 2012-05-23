let id = `POSIX
let get_year () = (Unix.gmtime (Unix.time())).Unix.tm_year + 1900
let eprint = Lwt_io.write Lwt_io.stderr

let string_of_inchan inchan =
  let s = String.create 1024 in
  let b = Buffer.create 1024 in
  let rec consume pos =
    lwt k = Lwt_io.read_into inchan s 0 1024 in
    if k=0 then Lwt.return (Buffer.contents b)
    else begin Buffer.add_substring b s 0 k; consume (pos+k) end
  in consume 0

let out_of_filename fn fdf =
  if fn="-"
  then fdf Lwt_io.stdout
  else Lwt_io.with_file ~mode:Lwt_io.output fn fdf

let in_of_filename fn =
  if fn="-"
  then string_of_inchan Lwt_io.stdin
  else Lwt_io.with_file ~mode:Lwt_io.input fn string_of_inchan
