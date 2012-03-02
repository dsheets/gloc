(* Copyright (c) 2012 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

open Yojson

include Glo_t

type 'a glom =
  | Glo of 'a glo
  | Glom of (string * 'a glom) list
  | Source of string
  | Other of Yojson.Safe.json

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

let rec json_of_glom : string glom -> Yojson.Safe.json = function
  (* TODO: don't serialize! *)
  | Glo glo -> Safe.from_string (string_of_glo glo)
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
  | `Assoc al -> Glo (glo_of_json (`Assoc al))
  | _ -> raise (Json_error "expected glom array or glo object")

let glom_of_string s =
  try glom_of_json (Glo_j.glom_of_string s)
  with Json_error _ | Failure _ -> Source s

let string_of_version (maj,min,rev) =
  let soi = string_of_int in
  (soi maj)^"."^(soi min)^"."^(soi rev)

let extract_meta comments =
  let href_field_re field =
    Re_str.regexp ("^\\b*"^field^":?\\b+\\([^<]*\\)<\\([^>]*\\)>")
  in
  let trim s =
    let eoc = Re_str.search_forward (Re_str.regexp "\\(\b*\\)$") s 0 in
    Re_str.string_before s eoc
  in
  let comments = List.flatten
    (List.map
       (fun c -> List.map (fun c -> c.Pp_lib.v) c.Pp_lib.v) comments) in
  let crre = Re_str.regexp
    "^\\b*Copyright\\b+\\([0-9][0-9][0-9][0-9]\\)\\b+\\([^<]*\\)<\\([^>]*\\)>" in
  let copyright = List.fold_left
    (fun cro ln ->
      try
        ignore (Re_str.search_forward crre ln 0);
        let year = int_of_string (Re_str.matched_group 1 ln) in
        let title = Re_str.matched_group 2 ln in
        let url = Re_str.matched_group 3 ln in
        Some (year,(trim title,url))
      with Not_found -> None
    ) None comments in
  match copyright with
    | Some copyright ->
      let author_re = href_field_re "Author" in
      let license_re = href_field_re "License" in
      let library_re = href_field_re "Library" in
      let version_re = href_field_re "Version" in
      let build_re = href_field_re "Build" in
      let triple_re = Re_str.regexp
        "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" in
      Some begin List.fold_right
        (fun ln meta ->
          try
            ignore (Re_str.search_forward author_re ln 0);
            let title = trim (Re_str.matched_group 1 ln) in
            let url = Re_str.matched_group 2 ln in
            {meta with author=(title,url)::meta.author}
          with Not_found ->
          try
            ignore (Re_str.search_forward license_re ln 0);
            let title = trim (Re_str.matched_group 1 ln) in
            let url = Re_str.matched_group 2 ln in
            {meta with license=Some (title,url)}
          with Not_found ->
          try
            ignore (Re_str.search_forward library_re ln 0);
            let title = trim (Re_str.matched_group 1 ln) in
            let url = Re_str.matched_group 2 ln in
            {meta with library=Some (title,url)}
          with Not_found ->
          try
            ignore (Re_str.search_forward version_re ln 0);
            let title = trim (Re_str.matched_group 1 ln) in
            let url = Re_str.matched_group 2 ln in
            ignore (Re_str.search_forward triple_re title 0);
            let version = (int_of_string (Re_str.matched_group 1 title),
                           int_of_string (Re_str.matched_group 2 title),
                           int_of_string (Re_str.matched_group 3 title))
            in {meta with version=Some (version,url)}
          with Not_found ->
          try
            ignore (Re_str.search_forward build_re ln 0);
            let title = trim (Re_str.matched_group 1 ln) in
            let url = Re_str.matched_group 2 ln in
            {meta with build=Some (title,url)}
          with Not_found -> meta
        ) comments {copyright; author=[];
                    license=None; library=None;
                    version=None; build=None}
      end
    | None -> None
