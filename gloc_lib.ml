(* Copyright (c) 2012 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

module Lang = Language
open Printf
open Pp_lib
open Pp
open Esslpp_lex
open Esslpp
open Glo_lib

module List = struct
    include List
    let unique l =
      let h = Hashtbl.create (List.length l) in
      let l = List.fold_left
	(fun l v -> if Hashtbl.mem h v
	 then l else begin Hashtbl.replace h v (); v::l end)
	[] l
      in List.rev l
end

(* TODO: harmonize? *)
type stage = Contents | ParsePP | Preprocess | Compile | Link | XML
type component =
  | Command
  | Format
  | Parser
  | PPParser
  | PPError
  | PPDiverge
  | SLParser
  | Analyzer
  | Linker

let exit_code_of_component = function
  | Command -> 1
  | Format -> 2
  | Parser -> 3
  | PPParser -> 4
  | PPError -> 5
  | PPDiverge -> 6
  | SLParser -> 7
  | Analyzer -> 8
  | Linker -> 9

let string_of_component_error = function
  | Command -> "unrecoverable command error"
  | Format -> "unrecoverable format read error"
  | Parser -> "unrecoverable internal parser error"
  | PPParser -> "unrecoverable preprocessor parse error"
  | PPError -> "unrecoverable preprocessor error"
  | PPDiverge -> "unrecoverable preprocessor divergence"
  | SLParser -> "unrecoverable parse error"
  | Analyzer -> "unrecoverable analysis error"
  | Linker ->"unrecoverable link error"

exception UnrecognizedJsonFormat
exception UncaughtException of exn
exception AmbiguousPreprocessorConditional of string pptok
exception MultiUnitGloPPOnly of string (* TODO: error message, test *)
exception GlomPPOnly of string
exception IncompatibleArguments of string * string
exception GloppStageError of stage

exception CompilerError of component * exn list

type 'a input = Stream of 'a | Define of 'a

type state = {stage:stage ref;
	      verbose:bool ref;
	      linectrl:bool ref;
	      metadata:meta ref;
	      symbols:string list ref;
	      output:string option ref;
	      inputs:string input list ref;
	      inlang:Lang.language ref;
	      outlang:Lang.language ref;
	      accuracy:Lang.accuracy ref;
	     }

let default_lang = { Lang.dialect=Lang.WebGL;
		     Lang.version=(1,0,0);
		     Lang.bond=Lang.Warn }

let default_meta =
  { copyright=((Unix.gmtime (Unix.time())).Unix.tm_year + 1900,("",""));
    author=[]; license=None; library=None; version=None; build=None }

let exec_state = { stage=ref Link;
		   verbose=ref false;
		   linectrl=ref true;
		   metadata=ref default_meta;
		   symbols=ref [];
		   output=ref None;
		   inputs=ref [];
		   inlang=ref default_lang;
		   outlang=ref default_lang;
		   accuracy=ref Lang.Best;
		 } (* one-shot monad *)

let builtin_macros = List.fold_left
  (fun map (n,f) -> Env.add n f map)
  Env.empty [
    "__LINE__",(fun e w ->
		  {name=None; args=None;
		   stream=fun _ -> [int_replace_word w w.span.a.line.src]});
    "__FILE__",(fun e w ->
		  {name=None; args=None;
		   stream=fun _ -> [int_replace_word w w.span.a.file.src]});
    "__VERSION__",(fun _ _ -> omacro "__VERSION__" (synth_int (Dec,100)));
    "GL_ES",(fun _ _ -> omacro "GL_ES" (synth_int (Dec,1)))
  ]

let maybe_fatal_error k =
  if (List.length !errors) > 0
  then begin let errs = !errors in
    errors := [];
    raise (CompilerError (k,errs))
  end

let parse source =
  let () = reset () in
  let lexbuf = Ulexing.from_utf8_string source in
  let parse = MenhirLib.Convert.traditional2revised
    (fun t -> t)
    (fun _ -> Lexing.dummy_pos) (* TODO: fixme? *)
    (fun _ -> Lexing.dummy_pos)
    translation_unit in
  let ppexpr = try parse (fun () -> lex !(exec_state.inlang) lexbuf) with
    | err -> raise (CompilerError (Parser, [UncaughtException err]))
  in maybe_fatal_error PPParser;
    normalize_ppexpr ppexpr

let preprocess ppexpr =
  let ppl = preprocess_ppexpr {macros=Env.empty;
			       builtin_macros;
			       extensions=Env.empty;
			       inmacros=[]} ppexpr in
    maybe_fatal_error PPError;
    ppl

let check_pp_divergence ppl =
  if List.length ppl > 1
  then let o = List.fold_left
    (fun dl pp -> List.fold_left
       (fun dl om ->
	  if List.exists (fun m -> om.v=m.v) dl then dl else om::dl)
       dl (fst pp).inmacros)
    [] ppl
  in raise (CompilerError
	      (PPDiverge, List.rev_map (fun t -> AmbiguousPreprocessorConditional t) o))
  else match ppl with (_,e)::_ -> e
    | [] -> Chunk { span={a=start_loc;z=start_loc};
		    scan=(fun loc -> (loc,""));
		    comments=([],ref []);
		    v=[] }

let compile fn source =
  let ppexpr = parse source in
  let ppl = preprocess ppexpr in

  let slexpr =
    if (!(exec_state.accuracy)=Lang.Preprocess
	&& !(exec_state.stage)=Compile)
    then check_pp_divergence ppl
    else ppexpr
  in

  let env_collect (f1,f2) vl (env,_) = (f1 env, f2 env)::vl in
  let get_inmac env = List.map (fun t -> t.v) env.inmacros in
  let get_opmac env = Env.fold (fun s _ l -> s::l) env.macros [] in
  let inmac,opmac = List.split
    (List.fold_left (env_collect (get_inmac,get_opmac))
       [] ppl) in

  let outlang = !(exec_state.outlang) in
  let target = (Lang.string_of_dialect outlang.Lang.dialect,
		outlang.Lang.version) in
  let meta = !(exec_state.metadata) in
    try Glo.compile ~meta target fn slexpr
      ~inmac:(List.unique (List.flatten inmac))
      ~opmac:(List.unique (List.flatten opmac))
      (List.map snd ppl)
    with err -> begin
      error (Essl_lib.EsslParseError ((Printexc.to_string err),
				      !(Pp_lib.file),!(Pp_lib.line)));
      maybe_fatal_error SLParser;
      raise err
    end

let link required glom =
  try Glol.link "" required (Glol.flatten "" glom)
  with e -> raise (CompilerError (Linker,[e]))

let file_extp ext fn =
  if (String.length fn)<((String.length ext)+1)
  then false
  else (Str.last_chars fn ((String.length ext)+1))="."^ext
let file_ext ext fn = (* TODO: paths with dots but no file extension *)
  let dotre = Str.regexp_string "." in
  let parts = List.rev (Str.split dotre fn) in
    if (List.length parts) = 1 then fn^"."^ext
    else List.fold_left
      (fun a s -> if a="" then s else a^"."^s)
      "" (List.rev (ext::(List.tl parts)))

let is_glo_file = file_extp "glo"
let glo_file_name = file_ext "glo"
let is_glopp_file = file_extp "glopp"
let glopp_file_name = file_ext "glopp"

let macro_name m = (* TODO: parser? *)
  let fn_patt = Str.regexp "\\([^(]+\\)\\(([^)]+)\\)?" in
  let _ = Str.search_forward fn_patt m 0 in
    Str.matched_group 1 m

let make_define_unit ds =
  let u m source =
    {pdir=[]; edir=[]; vdir=None; insym=[]; outsym=[]; inmac=[]; opmac=[];
     outmac=if m="" then [] else [macro_name m]; source}
  in match Str.bounded_split (Str.regexp_string "=") ds 2 with
    | [] -> u "" ""
    | bare::[] -> u bare ("#define "^bare^"\n")
    | m::d::_ -> u m ("#define "^m^" "^d^"\n")

let glo_of_u meta target u =
  {glo=glo_version; target; meta=Some meta; units=[|u|]; linkmap=[]}

let make_glo fn s =
  try match glom_of_string s with
    | Source s -> Glo (compile fn s)
    | glom -> glom
  with
    | e -> raise (CompilerError (Format,[e])) (* TODO: msg *)

let make_glom inputs = Glom
  (List.fold_left
     (fun al -> function
       | (fn, Stream (Source s)) | (fn, Define (Source s)) -> 
	 begin try (fn,make_glo fn s)::al
	   with e -> raise (CompilerError (Format,[e])) end (* TODO: msg *)
       | (fn, Stream glom) | (fn, Define glom) -> (fn,glom)::al
     ) [] inputs)

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
  | Sys_error m -> sprintf "System error:\n%s\n" m
  | exn -> try Glol.string_of_error exn with e -> raise e
