(* Copyright (c) 2012 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

open Pp_lib
open Pp
open Esslpp_lex
open Esslpp
open Yojson

include Glo_t

type 'a glom =
  | Leaf of 'a
  | Glom of (string * 'a glom) list
  | Source of string
  | Other of Yojson.Safe.json

type metaspan = NewMeta of meta | EndMeta | NoMeta

exception UnserializableGlom of string
exception InvalidGlom of string
exception InvalidGlo of string

let glo_version = (1,0,0)

let glo_of_string s =
  Glo_j.glo_of_string
    (fun lexstate buf ->
      match Safe.from_lexbuf ~stream:true lexstate buf with
        | `String s -> s
        | _ -> raise (InvalidGlo "glo unit source field must be JSON string")
    ) s

let string_of_glo glo =
  Glo_j.string_of_glo
    (fun b s ->
      Bi_outbuf.add_string b (Safe.to_string (`String s))
    ) glo

let rec json_of_glom : string glo glom -> Yojson.Safe.json = function
  (* TODO: don't serialize! *)
  | Leaf glo -> Safe.from_string (string_of_glo glo)
  | Glom glom ->
    `List (List.map (fun (n,glom) -> `List [`String n; json_of_glom glom]) glom)
  | Source s ->
    raise (UnserializableGlom "Raw source has no canonical glom serialization")
  | Other json -> json

let glo_of_json = function
  (* TODO: don't serialize! *)
  | `Assoc al -> glo_of_string (Safe.to_string ~std:true (`Assoc al))
  | _ -> raise (Json_error "glo must be object")

let rec glom_of_json = function
  | `List jl -> Glom
    (List.map
       (function
         | `List [`String n; (`List _) as v]
         | `List [`String n; (`Assoc _) as v] ->
           (n, (glom_of_json v))
         | _ ->
           raise (InvalidGlom "glom array type must be (string * (glo|glom)) list")
       ) jl)
  | `Assoc al -> Leaf (glo_of_json (`Assoc al))
  | _ -> raise (Json_error "expected glom array or glo object")

let glom_of_string s =
  try glom_of_json (Glo_j.glom_of_string s)
  with Json_error _ | Failure _ -> Source s

let string_of_version (maj,min,rev) =
  let soi = string_of_int in
  (soi maj)^"."^(soi min)^"."^(soi rev)

let parse lang source =
  let () = reset () in
  let lexbuf = Ulexing.from_utf8_string source in
  let parse = MenhirLib.Convert.traditional2revised
    (fun t -> t)
    (fun _ -> Lexing.dummy_pos)
    (fun _ -> Lexing.dummy_pos)
    translation_unit in
  let ppexpr = parse (fun () -> lex lang lexbuf) in
  normalize_ppexpr ppexpr

let builtin_macros_of_language = function
  | {Language.dialect=Language.WebGL} ->
    List.fold_left (fun map (n,f) -> Env.add n f map)
      Env.empty [
        "__LINE__",(fun e w ->
          {name=Some "__LINE__"; args=None;
           stream=fun _ -> [int_replace_word w w.span.a.line.src]});
        "__FILE__",(fun e w ->
          {name=Some "__FILE__"; args=None;
           stream=fun _ -> [int_replace_word w w.span.a.file.src]});
        "__VERSION__",(fun _ _ -> omacro "__VERSION__" (synth_int (Dec,100)));
        "GL_ES",(fun _ _ -> omacro "GL_ES" (synth_int (Dec,1)))
      ]

let extract_meta comments =
  let href_field_re field =
    Re_str.regexp ("^ *"^field^":? *\\([^<]+\\) *\\(<[^>]*>\\)?")
  in
  let end_span_re name = Re_str.regexp ("^ *End:? *"^name) in
  let trim s =
    let eoc = Re_str.search_forward (Re_str.regexp "\\( *\\)$") s 0 in
    Re_str.string_before s eoc
  in
  let url_of_angled s =
    try
      ignore (Re_str.search_forward (Re_str.regexp "<\\([^>]*\\)>") s 0);
      Re_str.matched_group 1 s
    with Not_found -> ""
  in
  let extract_href field s =
    let re = href_field_re field in
    ignore (Re_str.search_forward re s 0);
    let title = Re_str.matched_group 1 s in
    let url = try Re_str.matched_group 2 s with Not_found -> ""
    in (trim title, url_of_angled url)
  in
  let comments = List.flatten
    (List.map
       (fun c -> List.map (fun c -> c.Pp_lib.v) c.Pp_lib.v) comments) in
  let crre = Re_str.regexp
    "^ *Copyright:? *\\([0-9][0-9][0-9][0-9]\\) *\\([^<]+\\) *\\(<[^>]*>\\)?" in
  let rec find_copyright = function
    | [] -> None
    | ln::r -> begin
      try
        ignore (Re_str.search_forward crre ln 0);
        let year = int_of_string (Re_str.matched_group 1 ln) in
        let title = Re_str.matched_group 2 ln in
        let url = try url_of_angled (Re_str.matched_group 3 ln)
          with Not_found -> ""
        in Some (year,(trim title,url))
      with Not_found -> find_copyright r
    end in
  match find_copyright comments with
    | Some copyright ->
      let triple_re = Re_str.regexp
        "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" in
      NewMeta begin List.fold_right
        (fun ln meta ->
          try {meta with author=(extract_href "Author" ln)::meta.author}
          with Not_found ->
          try {meta with license=Some (extract_href "License" ln)}
          with Not_found ->
          try {meta with library=Some (extract_href "Library" ln)}
          with Not_found ->
          try
            let (title,url) = extract_href "Version" ln in
            ignore (Re_str.search_forward triple_re title 0);
            let version = (int_of_string (Re_str.matched_group 1 title),
                           int_of_string (Re_str.matched_group 2 title),
                           int_of_string (Re_str.matched_group 3 title))
            in {meta with version=Some (version,url)}
          with Not_found ->
          try {meta with build=Some (extract_href "Build" ln)}
          with Not_found -> meta
        ) comments {copyright; author=[];
                    license=None; library=None;
                    version=None; build=None}
      end
    | None ->
      if List.exists
        (fun ln -> Re_str.string_match (end_span_re "Copyright") ln 0)
        comments
      then EndMeta
      else NoMeta
