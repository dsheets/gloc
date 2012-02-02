(* Copyright (c) 2012 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

open Yojson

include Glo_t

type glom =
  | Glo of glo
  | Glom of (string * glom) list
  | Source of string
  | Other of Yojson.Safe.json

exception UnserializableGlom of string
exception InvalidGlom of string
exception InvalidGlo of string

let glo_version = (0,1,0)

let rec json_of_glom = function
  (* TODO: don't serialize! *)
  | Glo glo -> Safe.from_string (Glo_j.string_of_glo glo)
  | Glom glom ->
    `List (List.map (fun (n,glom) -> `List [`String n; json_of_glom glom]) glom)
  | Source s ->
    raise (UnserializableGlom "Raw source has no canonical glom serialization")
  | Other json -> json

let glo_of_json = function
  (* TODO: don't serialize! *)
  | `Assoc al -> Glo_j.glo_of_string (Safe.to_string ~std:true (`Assoc al))
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
