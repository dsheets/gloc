let id = `POSIX
let get_year () = (Unix.gmtime (Unix.time())).Unix.tm_year + 1900
let eprint = prerr_string

let string_of_inchan inchan =
  let s = String.create 1024 in
  let b = Buffer.create 1024 in
  let rec consume pos =
    let k = input inchan s 0 1024 in
    if k=0 then Buffer.contents b
    else begin Buffer.add_substring b s 0 k; consume (pos+k) end
  in consume 0

let out_of_filename fn fdf =
  let b = Buffer.create 1024 in
  let r = fdf b in
  let fd = if fn="-" then stdout else open_out fn in
  Buffer.output_buffer fd b;
  close_out fd;
  r

let in_of_filename fn fdf =
  let fd = if fn="-" then stdin else open_in fn in
  let s = string_of_inchan fd in
  close_in fd;
  fdf s
