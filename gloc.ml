(* Copyright (c) 2011 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

(*open Printf*)
module A = Arg

open Pp_lib
open Pp
open Esslpp_lex
open Glo_lib
open Gloc_lib

module type Platform = sig
  val id : [> `POSIX | `JS ]
  val get_year : unit -> int
  val eprint : string -> unit
  val out_of_filename : string -> (Buffer.t -> 'a) -> 'a
  val in_of_filename : string -> (string -> 'a) -> 'a
end

exception Exit of int

let gloc_version = (1,0,0)
let gloc_distributor = "Ashima Arts"

type group = Stage of stage
type set = LinkSymbols | LinkDefines | LinkGlobalMacros
type option = DisableLineControl | Verbose
type filetype = Output | Meta | Input
type iface =
    Group of group
  | Set of set
  | List of iface
  | Filename of filetype
  | Option of option
  | Choice of string list * (state -> string -> unit)

(* TODO: add per warning check args *)
(* TODO: add partial preprocess (only ambiguous conds with dep macros) *)
(* TODO: add partial preprocess (maximal preprocess without semantic change) *)
(* TODO: make verbose more... verbose *)
let cli = [
  "", List (Filename Input), "source input";
  "", Group (Stage Link), "produce linked SL";
  "-c", Group (Stage Compile), "produce glo and halt; do not link";
  "--xml", Group (Stage XML), "produce glo XML document";
  "-E", Group (Stage Preprocess), "preprocess and halt; do not parse SL";
  "-e", Group (Stage ParsePP), "parse preprocessor and halt; do not preprocess";
  "--source", Group (Stage Contents),
  "strip the glo format and return the contained source";
  "-u", List (Set LinkSymbols), "required symbol (default ['main'])";
  "--define", List (Set LinkDefines), "define a macro unit";
  "-D", List (Set LinkGlobalMacros), "define a global macro";
  "-o", Filename Output, "output file";
  "--accuracy", Choice (["best";"preprocess"],
                        fun exec_state s ->
                          exec_state.accuracy := (Language.accuracy_of_string s)),
  "output accuracy";
  "-L", Option DisableLineControl, "disregard incoming line directives";
  "-x", Choice (["webgl"],
                fun exec_state s ->
                  exec_state.inlang :=
                    (Language.with_dialect s !(exec_state.inlang))),
  "source language";
  "-t", Choice (["webgl"],
                fun exec_state s ->
                  exec_state.outlang :=
                    (Language.with_dialect s !(exec_state.outlang))),
  "target language";
  "-v", Option Verbose, "verbose";
  "--meta", Filename Meta, "prototypical glo file to use for metadata";
]

let arg_of_stage cli stage =
  let rec search = function
    | (arg,Group (Stage x),_)::_ when x=stage -> arg
    | _::r -> search r
    | [] -> "<default>"
  in search cli

let set_of_group state cli =
  let set_stage stage = fun () ->
    if !(state.stage)=Link
    then state.stage := stage
    else raise (CompilerError (Command,[IncompatibleArguments
                                           (arg_of_stage cli !(state.stage),
                                            arg_of_stage cli stage)])) in
  function Stage s -> set_stage s

let string_of_version (maj,min,rev) = Printf.sprintf "%d.%d.%d" maj min rev
let usage_msg = Printf.sprintf "gloc version %s (%s)"
  (string_of_version gloc_version)
  gloc_distributor

module Make(P : Platform) = struct

  let default_meta =
    { copyright=(P.get_year (),("",""));
      author=[]; license=None; library=None; version=None; build=None }

  let meta_of_path p = P.in_of_filename p
    (fun s -> match (Glo_j.glo_of_string s).meta
      with None -> default_meta | Some meta -> meta)

  let spec_of_iface exec_state cli = function
    | Group g -> A.Unit (set_of_group exec_state cli g)
    | List (Set LinkSymbols) ->
      A.String (fun u -> exec_state.symbols := u::!(exec_state.symbols))
    | List (Set LinkDefines) ->
      A.String (fun m -> exec_state.inputs := (Define m)::!(exec_state.inputs))
    | List (Set LinkGlobalMacros) ->
      A.String (fun m -> exec_state.prologue := m::!(exec_state.prologue))
    | Filename Output ->
      A.String (fun m -> exec_state.output := m)
    | List (Filename Input) ->
      A.String (fun m -> exec_state.inputs := (Stream m)::!(exec_state.inputs))
    | Filename Meta ->
      A.String (fun m -> exec_state.metadata := (meta_of_path m))
    | Option DisableLineControl -> A.Clear exec_state.linectrl
    | Option Verbose -> A.Set exec_state.verbose
    | Choice (syms, setfn) -> A.Symbol (syms, setfn exec_state)
    | Set _ -> raise (Failure "no untagged set symbols") (* TODO: real exn *)
    | List _ -> raise (Failure "other lists unimplemented") (* TODO: real exn *)
    | Filename Input -> raise (Failure "input files always list") (* TODO: real exn *)

  let arg_of_cli exec_state cli = List.fold_right
    (fun (flag, iface, descr) (argl, anon) ->
      if flag="" then (argl, match spec_of_iface exec_state cli iface with
        | A.String sfn -> sfn
        | _ -> fun _ -> ()
      ) else ((flag, spec_of_iface exec_state cli iface, descr)::argl, anon)
    ) cli ([], fun _ -> ())

  let print_errors linectrl errors =
    List.iter (fun e -> P.eprint ((string_of_error linectrl e)^"\n"))
      (List.rev errors)

  let compiler_error exec_state k errs =
    let code = exit_code_of_component k in
    print_errors !(exec_state.linectrl) errs;
    P.eprint ("Fatal: "^(string_of_component_error k)
              ^" ("^(string_of_int code)^")\n");
    raise (Exit code)

  let rec write_glom exec_state fn b = function
    | Source s -> write_glom exec_state fn b (make_glo exec_state fn s)
    | Other o ->
      Buffer.add_string b (if !(exec_state.verbose)
        then Yojson.Safe.pretty_to_string ~std:true o
        else Yojson.Safe.to_string ~std:true o)
    | Glo glo ->
      let s = Glo_j.string_of_glo glo in
      Buffer.add_string b
        ((if !(exec_state.verbose)
          then Yojson.Safe.prettify ~std:true s else s)^"\n")
    | Glom glom ->
      let json = json_of_glom (Glom glom) in
      Buffer.add_string b
        ((if !(exec_state.verbose)
          then Yojson.Safe.pretty_to_string ~std:true json
          else Yojson.Safe.to_string ~std:true json)^"\n")

  let rec source_of_glom fn = function
    | Source s -> begin match glom_of_string s with Source s -> s
        | glom -> source_of_glom fn glom end
    | Glo glo -> if (Array.length glo.units)=1
      then Glol.armor glo.meta (fn,glo.linkmap,0) []
        glo.units.(0).source
      else raise (CompilerError (PPDiverge, [MultiUnitGloPPOnly fn]))
    | Glom ((n,glom)::[]) -> source_of_glom (fn^"/"^n) glom
    | Glom _ -> raise (CompilerError (PPDiverge, [GlomPPOnly fn]))
    | Other o -> raise (Failure "cannot get source of unknown json") (* TODO *)

  let write_glopp exec_state fn b prologue glom =
    let ppexpr = parse exec_state (source_of_glom fn glom) in
    Buffer.add_string b
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

  let prologue exec_state = List.fold_left
    (fun s ds -> (make_define_line ds)^s) "" !(exec_state.prologue)

  let gloc exec_state strin =
    let req_sym = List.rev !(exec_state.symbols) in
    let stdin_input () = ("[stdin]", Stream (Source (strin ()))) in
    let inputs = match !(exec_state.inputs) with [] -> [stdin_input ()]
      | il -> List.map
        (function
          | Stream fn ->
            begin try (fn, Stream (Source (P.in_of_filename fn (fun s -> s))))
              with e -> compiler_error exec_state Command [e] end
          | Define ds -> if ds="" then ("[--define]", Define (Source ""))
            else ("[--define "^ds^"]",
                  Define (Glo (glo_of_u !(exec_state.metadata)
                                 (Language.target_of_language !(exec_state.outlang))
                                 (make_define_unit ds))))
        ) il
    in let inputs = if stream_inputp (List.map snd inputs) then inputs
      else (stdin_input ())::inputs in
       let fn = !(exec_state.output) in
       try begin match !(exec_state.stage) with
         | Contents ->
           P.out_of_filename fn
             (fun b -> Buffer.add_string b (source_of_glom fn
                                              (make_glom exec_state inputs)))
         | XML ->
           P.out_of_filename fn
             (fun b ->
               Buffer.add_string b
                 (Glo_xml.xml_of_glom ~xsl:"glocide.xsl" ~pretty:true
                    (make_glom exec_state inputs)))
         | Link -> let glom = make_glom exec_state inputs in
                   let prologue = prologue exec_state in
                   let src = link prologue req_sym glom in
                   P.out_of_filename fn
                     (fun b ->
                       Buffer.add_string b
                         ((if !(exec_state.accuracy)=Language.Preprocess
                           then string_of_ppexpr start_loc
                             (check_pp_divergence
                                (preprocess (parse exec_state src)))
                           else src)))
         | Compile -> (* TODO: consolidate glo *)
           P.out_of_filename fn
             (fun b ->
               let glom = make_glom exec_state inputs in
               write_glom exec_state fn b (minimize_glom glom))
         | Preprocess | ParsePP ->
           P.out_of_filename fn
             (fun b ->
               if (List.length (List.filter (fun (_,i) -> streamp i) inputs))=1
               then List.iter
                 (function (_,Define d) -> ()
                   | (_,Stream s) ->
                     let prologue = prologue exec_state in
                     write_glopp exec_state fn b prologue s) inputs
               else (* TODO: real exception *)
                 raise (Failure "too many input streams to preprocess"))
       end
       with CompilerError (k,errs) -> compiler_error exec_state k errs
end
