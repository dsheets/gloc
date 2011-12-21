type glo = {
  glo:version;
  target:string * version;
  meta:meta option;
  units: u array;
  linkmap:(string,string) Hashtbl.t
}
and meta = {
  copyright:string * year * url;
  author:(string * url) list;
  license:(string * url) option;
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
and year = int
and version = int * int * int
and glom = (string * glo) array
with json

let glo_version = (0,1,0)
let no_license year author =
  (Printf.sprintf "Copyright (C) %d %s. All rights reserved." year author, "")

let string_of_glo ?(compact=true) glo =
  Json_io.string_of_json ~compact (json_of_glo glo)
let string_of_glom ?(compact=true) glom =
  Json_io.string_of_json ~compact (json_of_glom glom)

