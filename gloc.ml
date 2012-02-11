(* Copyright (c) 2011 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

open Printf
module A = Arg

open Pp_lib
open Pp
open Esslpp_lex
open Glo_lib
open Gloc_lib

let gloc_version = (0,1,0)
let gloc_distributor = "Ashima Arts"

let arg_of_stage = function
  | Contents -> "--source"
  | ParsePP -> "-e"
  | Preprocess -> "-E"
  | Compile -> "-c"
  | XML -> "--xml"
  | Link -> "<default>"

let set_inlang map = fun () -> exec_state.inlang := (map !(exec_state.inlang))
let set_outlang map = fun () -> exec_state.outlang := (map !(exec_state.outlang))
let set_accuracy a = exec_state.accuracy := a
let set_stage stage = fun () ->
  if !(exec_state.stage)=Link
  then exec_state.stage := stage
  else raise (CompilerError (Command,[IncompatibleArguments
					(arg_of_stage !(exec_state.stage),
					 arg_of_stage stage)]))

let string_of_inchan inchan =
  let s = String.create 1024 in
  let b = Buffer.create 1024 in
  let rec consume pos =
    let k = input inchan s 0 1024 in
      if k=0 then Buffer.contents b
      else begin Buffer.add_substring b s 0 k; consume (pos+k) end
  in consume 0

let out_of_filename fn fdf =
  let fd = if fn="-" then stdout else open_out fn in
    try (let r = fdf fd in (close_out fd; r)) with e -> (close_out fd; raise e)

let in_of_filename fn fdf =
  let fd = if fn="-" then stdin else open_in fn in
    try (let r = fdf fd in (close_in fd; r)) with e -> (close_in fd; raise e)

let meta_of_path p = in_of_filename p
  (fun fd -> match (Glo_j.glo_of_string (string_of_inchan fd)).meta
    with None -> default_meta | Some meta -> meta)

(* TODO: add per warning check args *)
(* TODO: add partial preprocess (only ambiguous conds with dep macros) *)
(* TODO: add partial preprocess (maximal preprocess without semantic change) *)
(* TODO: make verbose more... verbose *)
let arguments =
  ["-c", A.Unit (set_stage Compile),
   "produce glo and halt; do not link";
   "-E", A.Unit (set_stage Preprocess),
   "preprocess and halt; do not parse SL";
   "-e", A.Unit (set_stage ParsePP),
   "parse preprocessor and halt; do not preprocess";
   "-u", A.String (fun u -> exec_state.symbols := u::!(exec_state.symbols)),
   "required symbol (default ['main'])";
   "-D", A.String (fun m -> exec_state.inputs := (Define m)::!(exec_state.inputs)),
   "define a macro";
   "-o", A.String (fun o -> exec_state.output := Some o), "output file";
   (*"-w", A.Unit (set_inlang (with_bond Ignore)), "inhibit all warning messages";*)
   "--accuracy", A.Symbol (["best";"preprocess"],
			   fun s -> set_accuracy (Language.accuracy_of_string s)),
   " output accuracy";
   "-L", A.Clear exec_state.linectrl,
   "disregard incoming line control for errors";
   "-x", A.Symbol (["webgl"],
		   fun s -> set_inlang (Language.with_dialect s) ()),
   " input language";
   "-t", A.Symbol (["webgl"],
		   fun s -> set_outlang (Language.with_dialect s) ()),
   " target language";
   "-v", A.Set exec_state.verbose, "verbose compilation or version information";
   "--meta", A.String (fun m -> exec_state.metadata := (meta_of_path m)),
   "the prototypical glo file to use for metadata";
   "--xml", A.Unit (set_stage XML),
   "produce glo XML documents from source or glo";
   "--source", A.Unit (set_stage Contents),
   "strip the glo format and return the contained source";
  ]
let anon_fun arg = exec_state.inputs := (Stream arg)::!(exec_state.inputs)

let string_of_version (maj,min,rev) = sprintf "%d.%d.%d" maj min rev
let usage_msg = sprintf "gloc version %s (%s)"
  (string_of_version gloc_version)
  gloc_distributor

let () = A.parse arguments anon_fun usage_msg

let print_errors errors =
  List.iter (fun e -> eprintf "%s\n" (string_of_error e)) (List.rev errors)

let compiler_error k errs =
  let code = exit_code_of_component k in
    print_errors errs;
    eprintf "Fatal: %s (%d)\n"
      (string_of_component_error k) code;
  exit code

let rec write_glom fn fd = function
  | Source s -> write_glom fn fd (make_glo fn s)
  | Other o ->
    output_string fd (if !(exec_state.verbose)
      then Yojson.Safe.pretty_to_string ~std:true o
      else Yojson.Safe.to_string ~std:true o)
  | Glo glo ->
    let s = Glo_j.string_of_glo glo in
    output_string fd
      ((if !(exec_state.verbose)
	then Yojson.Safe.prettify ~std:true s else s)^"\n")
  | Glom glom ->
    let json = json_of_glom (Glom glom) in
    output_string fd
      ((if !(exec_state.verbose)
	then Yojson.Safe.pretty_to_string ~std:true json
	else Yojson.Safe.to_string ~std:true json)^"\n")

let rec source_of_glom fn = function
  | Source s -> begin match glom_of_string s with Source s -> s
      | glom -> source_of_glom fn glom end
  | Glo glo -> if (Array.length glo.units)=1
    then Glol.armor glo.meta (glo.linkmap,0) []
      glo.units.(0).source
    else raise (CompilerError (PPDiverge, [MultiUnitGloPPOnly fn]))
  | Glom ((n,glom)::[]) -> source_of_glom (fn^"/"^n) glom
  | Glom _ -> raise (CompilerError (PPDiverge, [GlomPPOnly fn]))
  | Other o -> raise (Failure "cannot get source of unknown json") (* TODO *)

let write_glopp fn fd glom =
  let ppexpr = parse (source_of_glom fn glom) in
    output_string fd
      ((string_of_ppexpr start_loc
	  (match !(exec_state.stage) with
	    | ParsePP -> ppexpr
	    | Preprocess -> check_pp_divergence (preprocess ppexpr)
	    | s -> (* TODO: stage? error message? tests? *)
	      raise (CompilerError (PPDiverge, [GloppStageError s]))
	  ))^"\n")

let streamp = function Stream _ -> true | _ -> false
let stream_inputp il = List.exists streamp il
let glom_of_input = function Stream f -> f | Define f -> f
;;

let req_sym = List.rev !(exec_state.symbols) in
let stdin_input () = ("[stdin]", Stream (Source (string_of_inchan stdin))) in
let inputs = match !(exec_state.inputs) with [] -> [stdin_input ()]
  | il -> List.map
      (function
	 | Stream fn ->
	     begin try (fn, Stream (Source (string_of_inchan (open_in fn))))
	     with e -> compiler_error Command [e] end
	 | Define ds -> if ds="" then ("[-D]", Define (Source ""))
	   else ("[-D "^ds^"]",
		 Define (Glo (glo_of_u !(exec_state.metadata)
				(Lang.target_of_language !(exec_state.outlang))
				(make_define_unit ds))))
      )	il
in let inputs = if stream_inputp (List.map snd inputs) then inputs
  else (stdin_input ())::inputs
in try begin match (!(exec_state.output),
		    !(exec_state.stage)) with
  | None, Contents ->
    output_string stdout (source_of_glom "" (make_glom inputs))
  | None, XML ->
    output_string stdout
      (Glo_xml.xml_of_glom ~xsl:"glosse.xsl" ~pretty:true (make_glom inputs))
  | None, Link ->
    let glom = make_glom inputs in
    let src = link req_sym glom in
    output_string stdout
      (if !(exec_state.accuracy)=Lang.Preprocess
       then string_of_ppexpr start_loc
	  (check_pp_divergence (preprocess (parse src)))
       else src)
  | None, Compile -> if stream_inputp !(exec_state.inputs)
    then List.iter
      (function (fn,Define d) -> ()
	 | (fn,Stream s) -> if not (is_glo_file fn)
	   then out_of_filename (glo_file_name fn)
	     (fun fd -> write_glom fn fd s)) inputs
    else write_glom (fst (List.hd inputs)) stdout
      (glom_of_input (snd (List.hd inputs)))
  | None, Preprocess | None, ParsePP ->
      if stream_inputp !(exec_state.inputs)
      then List.iter
	(function (fn,Define d) -> () (* TODO: use -D in PP only as well *)
	   | (fn,Stream s) -> if not (is_glopp_file fn)
	     then out_of_filename (glopp_file_name fn)
	       (fun fd -> write_glopp fn fd s)) inputs
      else write_glopp (fst (List.hd inputs)) stdout
	(glom_of_input (snd (List.hd inputs)))
  | Some fn, Contents ->
    out_of_filename fn
      (fun fd -> output_string fd (source_of_glom fn (make_glom inputs)))
  | Some fn, XML ->
    out_of_filename fn
      (fun fd ->
	output_string fd
	  (Glo_xml.xml_of_glom ~xsl:"glo.xsl" ~pretty:true (make_glom inputs)))
  | Some fn, Link -> let glom = make_glom inputs in
    let src = link req_sym glom in
    out_of_filename fn
      (fun fd ->
	output_string fd
	  ((if !(exec_state.accuracy)=Lang.Preprocess
	    then string_of_ppexpr start_loc
	      (check_pp_divergence (preprocess (parse src)))
	    else src)))
  | Some fn, Compile -> (* TODO: consolidate glo *)
    out_of_filename fn
      (fun fd ->
	if stream_inputp !(exec_state.inputs)
	then let glom = make_glom inputs in
	     write_glom fn fd glom
	else write_glom fn fd (glom_of_input (snd (List.hd inputs))))
  | Some fn, Preprocess | Some fn, ParsePP ->
    out_of_filename fn
      (fun fd ->
	if (List.length (List.filter streamp !(exec_state.inputs)))=1
	then List.iter
	  (function (_,Define d) -> () (* TODO: use -D in PP only as well *)
	    | (_,Stream s) -> write_glopp fn fd s) inputs
	else write_glopp (fst (List.hd inputs)) stdout
	  (glom_of_input (snd (List.hd inputs))))
end
  with CompilerError (k,errs) -> compiler_error k errs
