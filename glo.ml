open Printf

type glo = {
  glo:version;
  target:string * version;
  meta:meta option;
  units: u list;
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

let glo_version = (0,8,0)
let no_license author =
  (sprintf "Copyright (C) %d %s. All rights reserved."
     ((Unix.gmtime (Unix.time ())).Unix.tm_year + 1900)
     author,
   "")

let compile target origexpr tokslst =
  let env = Essl_lib.parse_essl (Pp_lib.proj_pptok_expr (List.hd tokslst)) in
  {glo=glo_version; target;
   meta=Some {
     author=[];
     license=no_license "";
     library=None;
     version=None;
     build=None
   };
   units=[];
   linkmap=Hashtbl.create 0}
