(* Copyright (c) 2011 David Sheets, Ashima Arts.
 * All rights reserved.
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

open Printf
open Pp_lib
open Pp
open Esslpp_lex
open Esslpp

type dialect = WebGL
type version = int * int * int
type accuracy = Best | Decomment | Minify
type bondage = Error | Warn | Ignore
type language = {dialect:dialect;
		 version:version;
		 accuracy:accuracy;
		 bond:bondage;
		}

type states = {preprocess:bool ref;
	       compile:bool ref;
	       output:string option ref;
	       lang:language ref;
	      }
	       
let default_lang = { dialect=WebGL;
		     version=(1,0,0);
		     accuracy=Best;
		     bond=Warn }
		     
let exec_states = { preprocess=ref false;
		    compile=ref false;
		    output=ref None;
		    inlang=ref default_lang;
		    outlang=ref default_lang }

let arguments =
  ["-E", Set exec_states.preprocess, "preprocess output source";
   "-c", Set exec_states.compile, "produce glo object";
   "-o", String (fun o -> exec_states.output := Some o), "output file";
   "-w", , "inhibit all warning messages";
   "-x",,;
   "-out",,;
  ]
			 

let string_of_tokpos
    ({span={a={file={src=af}; line={src=al}; col=ac};
	    z={file={src=zf}; line={src=zl}; col=zc}}}) =
  if af=zf then
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
  | ReservedMacro t ->
      sprintf "%s:\n\"%s\" is a reserved macro and may not be used\n"
	(string_of_tokpos t) t.v
  | ErrorDirective t ->
      sprintf "%s:\n%s\n" (string_of_tokpos t) (snd (t.scan t.span.a))
  | UnsupportedPPOp t ->
      sprintf "%s:\n\"%s\" is not supported in preprocessor expressions\n"
	(string_of_tokpos t) (snd (t.scan t.span.a))
  | FloatUnsupported t ->
      sprintf "%s:\nfloating point is not support in preprocessor expressions\n"
	(string_of_tokpos t)
  | exn -> sprintf "Unknown error:\n%s\n" (Printexc.to_string exn)
;;

let lexbuf = Ulexing.from_utf8_channel stdin in
let parse = MenhirLib.Convert.traditional2revised
  (fun t -> t)
  (fun _ -> Lexing.dummy_pos) (* TODO: fixme? *)
  (fun _ -> Lexing.dummy_pos)
  translation_unit in
let ppexpr = try parse (fun () -> lex lexbuf) with
  | err -> eprintf "Uncaught exception:\n%s\n" (Printexc.to_string err);
    eprintf "Fatal: unrecoverable internal parser error (1)";
    exit 1
in
let () = if (List.length !errors) > 0
then (List.iter (fun e -> eprintf "%s\n" (string_of_error e))
	(List.rev !errors);
      eprintf "Fatal: unrecoverable parse error (2)"
      exit 2)
in
let ppexpr = normalize_ppexpr ppexpr in
let macros = Env.add "__VERSION__" (omacro "__VERSION__" (synth_int (Dec,100)))
  (Env.singleton "GL_ES" (omacro "GL_ES" (synth_int (Dec,1)))) in
let ppl = preprocess_ppexpr {macros; extensions=Env.empty} ppexpr in
  if (List.length !errors) > 0 then
    (List.iter (fun e -> eprintf "%s\n" (string_of_error e))
       (List.rev !errors);
     eprintf "Fatal: unrecoverable preprocessor error (3)";
     exit 3)
  else
    (*printf "%s\n" (string_of_ppexpr_tree ppexpr);*)
    List.iter (fun (env,ppexpr) ->
		 printf "%s\n"
		   (snd ((proj_pptok_expr ppexpr).scan
			   {file={src=0;input=0};line={src=1;input=1};col=0})))
      ppl
