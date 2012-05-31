let id = `POSIX
let get_year () = (Unix.gmtime (Unix.time())).Unix.tm_year + 1900
let eprint = Lwt_io.write Lwt_io.stderr

let out_of_filename path s =
  if path="-"
  then Lwt_io.write Lwt_io.stdout s
  else Lwt_io.with_file ~mode:Lwt_io.output path
    (fun oc -> Lwt_io.write oc s)

let in_of_filename path =
  if path="-"
  then Io_util.string_of_inchan Lwt_io.stdin
  else Lwt_io.with_file ~mode:Lwt_io.input path Io_util.string_of_inchan
