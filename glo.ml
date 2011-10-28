open Printf
open Pp_lib
open Essl_lib

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

let create_header expr =
  (* TODO: lift extension exprs *)
  (* TODO: lift pragma invariant *)
  {insym=[]; outsym=[]; inmac=[]; opmac=[]; outmac=[]; source=""}
let create_body expr envs =
  (* TODO: abstract *)
  let apply_to_if fn ({v=(ce,tb,ofb)} as t) =
    let tm, tb = match fn tb with
      | tm, Some tb -> tm, tb
      | tm, None -> tm, empty_pptok_expr tb
    in
    let fm, ofb = match ofb with
      | None -> [], None
      | Some fb -> fn fb
    in tm@fm, Some (If { t with v=(ce,tb,ofb) }) (* TODO: rebuild If *)
  in
  let apply_to_list fn ({v=ppel} as t) =
    let ml, ppel = List.fold_left
      (fun (ml,ppel) ppe -> match fn ppe with
	 | sml, None -> (sml@ml,ppel)
	 | sml, Some ppe -> (sml@ml,ppe::ppel)
      )
      ([],[]) ppel
    in ml, Some (fuse_pptok_expr (List.rev ppel))
  in
  let rec process_version e = match e with
    | Comments _ | Chunk _ | Def _ | Fun _ | Undef _
    | Err _ | Pragma _ | Extension _ | Line _ -> [], Some e
    | Version itt -> ["GLOC_VERSION_"^(string_of_int itt.v.v)], None
    | If t -> apply_to_if process_version t
    | List t -> apply_to_list process_version t
  in
    (*
  let rec process_line prefix e = let prefixl = String.length prefix in
    match e with
      | Comments _ | Chunk _ | Def _ | Fun _ | Undef _
      | Err _ | Pragma _ | Extension _ | Version _ -> [], Some e
      | Line ({ v=(Some ft,_) } as t) -> let linedir =
	  Line {t with
		  span={t.span with
			  z={t.span.z with col=t.span.z.col+prefixl}};
		  scan=fun loc ->
		    let loc, s = t.scan loc in
		    let ll = String.length s in
		    let fns = string_of_int ft.v in
		    let fnl = String.length fns in
		      ({loc with col=loc.col+prefixl},
		       (String.sub s 0 (ll-fnl))^prefix^fns)
	       }
	in [], Some linedir
      | Line _ -> [], Some e
      | If t -> apply_to_if (process_line prefix) t
      | List t -> apply_to_list (process_line prefix) t
  in *)
  let syml, expr = match process_version expr with
    | syml, Some e -> syml, e
    | syml, None -> syml, empty_pptok_expr expr
  in
    (*  let syml, expr = match process_line "GLOC_" expr with
    | syml', Some e -> syml@syml', e
    | syml', None -> syml@syml', empty_pptok_expr expr
	in *)
  (* TODO: rename GLOC_* to GLOC_GLOC_* *)
  (* TODO: remove extension *)
  (* TODO: rewrite line directives *)
  {insym=[]@syml; outsym=[]; inmac=[]; opmac=[]; outmac=[];
   source=(snd ((proj_pptok_expr expr).scan {file={src=(-1);input=0};
					     line={src=0;input=0};
					     col=0}))}

let compile target origexpr ~inmac ~opmac tokslst =
  let envs = List.map
    (fun ppexpr ->
       let s = stream_of_pptok_expr ppexpr in
       let ts = essl_tokenize s in
	 parse_essl (essl_lexerfn ts)
    ) tokslst
  in
    {glo=glo_version; target;
     meta=Some {
       author=[];
       license=no_license "";
       library=None;
       version=None;
       build=None
     };
     units=[(*create_header origexpr;*)
	    {(create_body origexpr envs) with inmac; opmac}];
     linkmap=Hashtbl.create 0}
