(* Copyright (c) 2012 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

open Printf
open Pp_lib
open Essl_lib
open Glo_lib

type require = Pdir of string | Edir of string * behavior | Vdir of int

let unique lst =
  let rec dedupe prev = function
    | x::y::r when x=y -> dedupe prev (x::r)
    | x::r -> dedupe (x::prev) r
    | [] -> prev
  in dedupe [] (List.rev (List.sort compare lst))

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
    in tm@fm, Some (synth_pp_if { t with v=(ce,tb,ofb) })
  in
  let apply_to_list fn ({v=ppel}) =
    let ml, ppel = List.fold_left
      (fun (ml,ppel) ppe -> match fn ppe with
        | sml, None -> (sml@ml,ppel)
        | sml, Some ppe -> (sml@ml,ppe::ppel)
      )
      ([],[]) ppel
    in ml, Some (fuse_pptok_expr (List.rev ppel))
  in
  let rec print_stream = function
    | [] -> ()
    | (Int _)::r -> print_endline "Int"; print_stream r
    | (Float _)::r -> print_endline "Float"; print_stream r
    | (Word _)::r -> print_endline "Word"; print_stream r
    | (Call _)::r -> print_endline "Call"; print_stream r
    | (Punc _)::r -> print_endline "Punc"; print_stream r
    | (Comma _)::r -> print_endline "Comma"; print_stream r
    | (Leftp _)::r -> print_endline "Leftp"; print_stream r
    | (Rightp _)::r -> print_endline "Rightp"; print_stream r
  in
  let rec process_requires e = match e with
    | Comments _ | Chunk _ | Def _ | Fun _ | Undef _
    | Err _ | Line _ -> [], Some e
    | Extension {v=({v=name},{v=behavior})} ->
      [Edir (name, behavior)], None
    | Pragma ({v=[Word {v=("STDGL",_)};
                  Word {v=("invariant",_)};
                  Leftp _;
                  Word {v=("all",_)};
                  Rightp _;
                 ]} as t) -> [Pdir (snd (t.scan t.span.a))], None
    | Pragma {v} -> [], Some e
    | Version itt -> [Vdir itt.v.v], None
    | If t -> apply_to_if process_requires t
    | List t -> apply_to_list process_requires t
  in
  let rec process_line e = match e with
    | Comments _ | Chunk _ | Def _ | Fun _ | Undef _
    | Err _ | Pragma _ | Extension _ | Version _ -> [], Some e
    | Line ({ v=(Some ft,_) } as t) ->
      let linedir = synth_pp_line_armored t in [ft.v], Some linedir
    | Line _ -> [], Some e
    | If t -> apply_to_if process_line t
    | List t -> apply_to_list process_line t
  in
  let directives_of_requires requires =
    List.fold_left
      (fun (pl,el,vo) -> function
        | Pdir s -> (s::pl,el,vo)
        | Edir (n,Require) -> (pl,(n,"require")::el,vo)
        | Edir (n,Enable) -> (pl,(n,"enable")::el,vo)
        | Edir (n,Warn) -> (pl,(n,"warn")::el,vo)
        | Edir (n,Disable) -> (pl,(n,"disable")::el,vo)
        | Vdir n -> (pl,el,Some n)
      ) ([],[],None) requires
  in
  let file_nums, expr = match process_line expr with
    | file_nums, Some e -> file_nums, e
    | file_nums, None -> file_nums, empty_pptok_expr expr
  in
  (* TODO: enforce ESSL 1-declare, 1-define rule *)
  let prototypes = List.fold_left
    (fun l e -> Sl_lib.SymMap.fold
      (fun k bindings l ->
        if List.for_all (fun b -> not (Sl_lib.definitionp b)) bindings
        then k::l else l
      ) (List.hd (List.rev e.Sl_lib.ctxt)) l)
    [] envs
  in
  let file_nums, start = match expr with
    | List {v=(Line {v=(Some _,_)})::_}
    | Line {v=(Some _,_)} ->
      file_nums, {file={src=0;input=0}; line={src=1;input=1}; col=0}
    | _ ->
      (0::file_nums), {file={src=(-1);input=(-1)}; line={src=1;input=1}; col=0}
  in
  let (pdir,edir,vdir),expr = match process_requires expr with
    | requires, Some e -> directives_of_requires requires, e
    | requires, None -> directives_of_requires requires, empty_pptok_expr expr
  in
    (* TODO: rename GLOC_* to GLOC_GLOC_* *)
  ({pdir; edir; vdir;
    insym=List.fold_left
      (fun l e -> List.fold_left
        (fun l s -> (* TODO: inference *)
          if List.mem s l then l
          else if List.mem_assoc s builtins then l
          else s::l)
        l e.Sl_lib.opensyms)
      prototypes envs;
    outsym=List.fold_left
      (fun l e -> Sl_lib.SymMap.fold
        (fun k bindings l -> (* TODO: inference *)
          if (List.mem k l) or not (List.exists Sl_lib.definitionp bindings)
          then l else k::l)
        (List.hd (List.rev e.Sl_lib.ctxt)) l)
      [] envs;
    inmac=[]; opmac=[]; outmac=[];
    source=(snd ((proj_pptok_expr expr).scan start))},
   unique file_nums)

let env_of_ppexpr ppexpr =
  let s = stream_of_pptok_expr ppexpr in
  let ts = essl_tokenize s in
  parse_essl (essl_lexerfn ts)

let compile ?meta target fn origexpr ~inmac ~opmac tokslst =
  let envs = List.map env_of_ppexpr tokslst in
  let body_unit, file_nums = create_body origexpr envs in
  let linkmap = List.map
    (fun n -> (string_of_int n, sprintf "%s#n=%d" fn n))
    file_nums
  in {glo=glo_version; target; meta;
      units=[|{body_unit with
        inmac=inmac@body_unit.inmac;
        opmac=opmac@body_unit.opmac
      }|];
      linkmap}

