type glo = {
  glo:version;
  target:string * version;
  meta:meta option;
  units: u array;
  linkmap:(string,string) Hashtbl.t
}
and meta = {
  author:(string * url) list;
  license:string * url;
  library:(string * url) option;
  version:version option;
  build:string option;
}
and u = {
  insym:string list;
  outsym:string list;
  inmac:string list;
  opmac:string list;
  outmac:string list;
  source:string;
}
and url = string
and version = int * int * int
with json

let glo_version = (0,1,0)
let no_license year author =
  (Printf.sprintf "Copyright (C) %d %s. All rights reserved." year author, "")
