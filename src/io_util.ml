let string_of_inchan inchan =
  let s = String.create 1024 in
  let b = Buffer.create 1024 in
  let rec consume pos =
    lwt k = Lwt_io.read_into inchan s 0 1024 in
    if k=0 then Lwt.return (Buffer.contents b)
    else begin Buffer.add_substring b s 0 k; consume (pos+k) end
  in consume 0
