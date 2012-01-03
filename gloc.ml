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
open Gloc_lib

let gloc_version = (0,1,0)
let gloc_distributor = "Ashima Arts"

let arg_of_stage = function
  | ParsePP -> "-e"
  | Preprocess -> "-E"
  | Compile -> "-c"
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
   (* TODO: do *)
   (*"--edit-meta", A.Unit (fun () -> ()), "interactive glo meta segment mutator";*)
   (* TODO: do *)
   (*"--license", A.Unit (fun () -> ()),
   "glo license field override, accepts SPDX license identifiers";*)
  ]
let anon_fun arg = exec_state.inputs := (Stream arg)::!(exec_state.inputs)

let string_of_version (maj,min,rev) = sprintf "%d.%d.%d" maj min rev
let usage_msg = sprintf "gloc version %s (%s)"
  (string_of_version gloc_version)
  gloc_distributor

let () = A.parse arguments anon_fun usage_msg

let string_of_tokpos
    ({span={a={file=af; line=al; col=ac};
	    z={file=zf; line=zl; col=zc}}}) =
  let af,al,zf,zl = if !(exec_state.linectrl)
  then (af.src,al.src,zf.src,zl.src)
  else (af.input,al.input,zf.input,zl.input)
  in if af=zf then
      if al=zl
      then if ac=zc
      then sprintf "File %d, line %d, col %d" af al ac
      else sprintf "File %d, line %d, col %d - %d" af al ac zc
      else sprintf "File %d, l%d c%d - l%d c%d" af al ac zl zc
    else sprintf "F%d l%d c%d - F%d l%d c%d" af al ac zf zl zc

let string_of_error = function
  | UnknownBehavior t ->
      sprintf "%s:\nunknown behavior \"%s\"\n" (string_of_tokpos t) t.v
  | UnterminatedComment t ->
      sprintf "%s:\nunterminated comment\n" (string_of_tokpos t)
  | UnterminatedConditional t ->
      sprintf "%s:\nunterminated conditional \"%s\"\n" (string_of_tokpos t)
	(snd (t.scan t.span.a))
  | UnknownCharacter t ->
      sprintf "%s:\nunknown character '%s'\n" (string_of_tokpos t)
	(snd (t.scan t.span.a))
  | LineContinuationUnsupported t ->
      sprintf "%s:\nline continuation officially unsupported\n" (string_of_tokpos t)
  | InvalidDirectiveLocation t ->
      sprintf "%s:\ninvalid directive location\n" (string_of_tokpos t)
  | InvalidDirective t ->
      sprintf "%s:\ninvalid directive \"%s\"\n" (string_of_tokpos t) t.v
  | InvalidOctal t ->
      sprintf "%s:\ninvalid octal constant \"%s\"\n" (string_of_tokpos t) t.v
  | HolyVersion t ->
      sprintf "%s:\nversion must be first semantic token\n" (string_of_tokpos t)
  | UnsupportedVersion t ->
      sprintf "%s:\nversion %d is unsupported\n" (string_of_tokpos t) t.v
  | InvalidVersionBase t ->
      sprintf "%s:\nversion must be specified in decimal\n" (string_of_tokpos t)
  | InvalidLineBase t ->
      sprintf "%s:\nline control arguments must be specified in decimal\n"
	(string_of_tokpos t)
  | InvalidVersionArg t ->
      sprintf "%s:\ninvalid version argument\n" (string_of_tokpos t)
  | InvalidLineArg t ->
      sprintf "%s:\ninvalid line argument\n" (string_of_tokpos t)
  | MacroArgUnclosed t ->
      sprintf "%s:\nunclosed macro argument list\n" (string_of_tokpos t)
  | MacroArgInnerParenUnclosed t ->
      sprintf "%s:\nunclosed inner parenthesis in macro argument list\n"
	(string_of_tokpos t)
  | MacroArgTooFew (t,a,e) ->
      sprintf "%s:\ntoo few macro arguments: expected %d, got %d\n"
	(string_of_tokpos t) e a
  | MacroArgTooMany (t,a,e) ->
      sprintf "%s:\ntoo many macro arguments: expected %d, got %d\n"
	(string_of_tokpos t) e a
  | ReservedKeyword t ->
      sprintf "%s:\n\"%s\" is a reserved keyword and may not be used\n"
	(string_of_tokpos t) t.v
  | RedefineReservedMacro t ->
      sprintf "%s:\n\"%s\" is a reserved macro and may not be redefined\n"
	(string_of_tokpos t) t.v
  | UndefineReservedMacro t ->
      sprintf "%s:\n\"%s\" is a reserved macro and may not be undefined\n"
	(string_of_tokpos t) t.v
  | ErrorDirective t ->
      sprintf "%s:\n%s\n" (string_of_tokpos t) (snd (t.scan t.span.a))
  | UnsupportedPPOp t ->
      sprintf "%s:\n\"%s\" is not supported in preprocessor expressions\n"
	(string_of_tokpos t) (snd (t.scan t.span.a))
  | FloatUnsupported t ->
      sprintf "%s:\nfloating point is not supported in preprocessor expressions\n"
	(string_of_tokpos t)
  | PPCondExprParseError t ->
      sprintf "%s:\nerror parsing conditional expression \"%s\"\n"
	(string_of_tokpos t) (snd (t.scan t.span.a))
  | UncaughtException e -> sprintf "Uncaught exception:\n%s\n" (Printexc.to_string e)
  | AmbiguousPreprocessorConditional t ->
      sprintf "%s:\nambiguous preprocessor conditional branch: %s\n"
	(string_of_tokpos t) t.v
  | GlomPPOnly fn ->
      sprintf "Source '%s' is a glom with multiple source units.\n" fn
  | MultiUnitGloPPOnly fn -> (* TODO: test *)
      sprintf "Source '%s' is a glo with multiple source units.\n" fn
  | Glol.CircularDependency ual -> List.fold_left
      (fun s (fn,un) ->
	 sprintf "%s%s#u=%d\n" s fn un
      ) "Circular dependency linking:\n" ual
  | Glol.MissingMacro ((fn,un),mn) ->
      sprintf "%s#u=%d requires macro '%s' which cannot be found.\n" fn un mn
  | Glol.MissingSymbol ((fn,un),mn) ->
      sprintf "%s#u=%d requires symbol '%s' which cannot be found.\n" fn un mn
  | Glol.SymbolConflict (ssym,csym,(sfn,sun),(cfn,cun)) ->
      sprintf "%s#u=%d provides '%s' but exposes '%s' which conflicts with %s#u=%d\n"
	sfn sun ssym csym cfn cun
  | Sys_error m -> sprintf "System error:\n%s\n" m
  | exn -> sprintf "Unknown error:\n%s\n" (Printexc.to_string exn)

let print_errors errors =
  List.iter (fun e -> eprintf "%s\n" (string_of_error e)) (List.rev errors)

let compiler_error k errs =
  let code = exit_code_of_component k in
    print_errors errs;
    eprintf "Fatal: %s (%d)\n"
      (string_of_component_error k) code;
  exit code

let string_of_inchan inchan =
  let s = String.create 1024 in
  let b = Buffer.create 1024 in
  let rec consume pos =
    let k = input inchan s 0 1024 in
      if k=0 then Buffer.contents b
      else begin Buffer.add_substring b s 0 k; consume (pos+k) end
  in consume 0

let chan_of_filename fn fdf =
  let fd = if fn="-" then stdout else open_out fn in
    try (fdf fd; close_out fd) with e -> (close_out fd; raise e)

let rec write_glo fn fd = function
  | Source s -> write_glo fn fd (make_glo fn s)
  | Glo glo -> output_string fd
      ((Glo_lib.string_of_glo ~compact:(not !(exec_state.verbose)) glo)^"\n")
  | Glom glom -> output_string fd
      ((Glo_lib.string_of_glom ~compact:(not !(exec_state.verbose)) glom)^"\n")

(* TODO: expose --source *)
let rec source_of_fmt fn = function
  | Source s -> begin try source_of_fmt fn (glo_of_string s)
    with Json_type.Json_error _ -> s end
  | Glo glo -> if (Array.length glo.Glo_lib.units)=1
    then Glol.armor (glo.Glo_lib.linkmap,0) []
      glo.Glo_lib.units.(0).Glo_lib.source
    else raise (CompilerError (PPDiverge, [MultiUnitGloPPOnly fn]))
  | Glom glom -> let glo = snd glom.(0) in if ((Array.length glom)=1
	&& (Array.length glo.Glo_lib.units)=1)
    then Glol.armor (glo.Glo_lib.linkmap,0) []
      glo.Glo_lib.units.(0).Glo_lib.source
    else raise (CompilerError (PPDiverge, [GlomPPOnly fn]))

let write_glopp fn fd fmt =
  let ppexpr = parse (source_of_fmt fn fmt) in
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
let fmt_of_input = function Stream f -> f | Define f -> f
;;

let req_sym = match !(exec_state.symbols) with [] -> ["main"] | l -> l in
let stdin_input () = ("<stdin>", Stream (Source (string_of_inchan stdin))) in
let inputs = match !(exec_state.inputs) with [] -> [stdin_input ()]
  | il -> List.map
      (function
	 | Stream fn ->
	     begin try (fn, Stream (Source (string_of_inchan (open_in fn))))
	     with e -> compiler_error Command [e] end
	 | Define ds -> if ds="" then ("<-D>", Define (Source ""))
	   else ("<-D "^ds^">",
		 Define (Glo (glo_of_u !(exec_state.metadata)
				(Lang.target_of_language !(exec_state.outlang))
				(make_define_unit ds))))
      )	il
in let inputs = if stream_inputp (List.map snd inputs) then inputs
  else (stdin_input ())::inputs
in try begin match (!(exec_state.output),
		    !(exec_state.stage)) with
  | None, Link -> let glom = make_glom inputs in
    let src = link req_sym (Array.to_list glom) in
      output_string stdout
	((if !(exec_state.accuracy)=Lang.Preprocess
	  then string_of_ppexpr start_loc
	    (check_pp_divergence (preprocess (parse src)))
	  else src)^"\n")
  | None, Compile -> if stream_inputp !(exec_state.inputs)
    then List.iter
      (function (fn,Define d) -> ()
	 | (fn,Stream s) -> if not (is_glo_file fn)
	   then chan_of_filename (glo_file_name fn)
	     (fun fd -> write_glo fn fd s)) inputs
    else write_glo (fst (List.hd inputs)) stdout
      (fmt_of_input (snd (List.hd inputs)))
  | None, Preprocess | None, ParsePP ->
      if stream_inputp !(exec_state.inputs)
      then List.iter
	(function (fn,Define d) -> () (* TODO: use -D in PP only as well *)
	   | (fn,Stream s) -> if not (is_glopp_file fn)
	     then chan_of_filename (glopp_file_name fn)
	       (fun fd -> write_glopp fn fd s)) inputs
      else write_glopp (fst (List.hd inputs)) stdout
	(fmt_of_input (snd (List.hd inputs)))
  | Some fn, Link -> let glom = make_glom inputs in
    let src = link req_sym (Array.to_list glom) in
      chan_of_filename fn
	(fun fd ->
	   output_string fd
	     ((if !(exec_state.accuracy)=Lang.Preprocess
	       then string_of_ppexpr start_loc
		 (check_pp_divergence (preprocess (parse src)))
	       else src)^"\n"))
  | Some fn, Compile -> (* TODO: consolidate glo *)
      chan_of_filename fn
	(fun fd ->
	   if stream_inputp !(exec_state.inputs)
	   then let glom = make_glom inputs in
	     write_glo fn fd (Glom glom)
	   else write_glo fn fd (fmt_of_input (snd (List.hd inputs))))
  | Some fn, Preprocess | Some fn, ParsePP ->
      chan_of_filename fn
	(fun fd ->
	   if (List.length (List.filter streamp !(exec_state.inputs)))=1
	   then List.iter
	     (function (_,Define d) -> () (* TODO: use -D in PP only as well *)
		| (_,Stream s) -> write_glopp fn fd s) inputs
	   else write_glopp (fst (List.hd inputs)) stdout
	     (fmt_of_input (snd (List.hd inputs))))
  end
  with CompilerError (k,errs) -> compiler_error k errs
