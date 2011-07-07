open Printf
open Pp
open Esslpp_lex
open Esslpp
;;
(*
let rec pr_ppexpr = function
  | Comments (_,cl) -> 
  | Chunk (_,c) ->
  | If (_,cond,cont,alto) ->
  | Def (_,macro,c) ->
  | Fun (_,fmacro,args,c) ->
  | Undef (_,macro) ->
  | Err (_,toks) ->
  | Pragma (_,toks) ->
  | Version (_,v) ->
  | Extension (_,ext,b) ->
  | Line (_,f,l) ->
  | Concat (_,a,b) ->
and pr_comment tok = match tok.v with
  | [] -> ()
  | l::[] -> printf "//%s\n" l
  | mlc -> printf "/*"; List.iter (printf "%s") mlc; 
*)
let string_of_tokpos ({loc={file={src=file}; line={src=line}; col}}) =
  sprintf "File %d, line %d, col %d" file line col

let string_of_error = function
  | UnknownBehavior t ->
    sprintf "%s:\nunknown behavior \"%s\"\n" (string_of_tokpos t) t.v
  | UnterminatedComment t ->
    sprintf "%s:\nunterminated comment\n" (string_of_tokpos t)
  | UnknownCharacter t ->
    sprintf "%s:\nunknown character '%s'\n" (string_of_tokpos t) t.scan
  | InvalidDirectiveLocation t ->
    sprintf "%s:\ninvalid directive location\n" (string_of_tokpos t)
  | InvalidDirective t ->
    sprintf "%s:\ninvalid directive \"%s\"\n" (string_of_tokpos t) t.v
  | InvalidOctal t ->
    sprintf "%s:\ninvalid octal constant \"%s\"\n" (string_of_tokpos t) t.v
  | exn -> sprintf "Unknown error:\n%s\n" (Printexc.to_string exn)
;;

let lexbuf = Ulexing.from_utf8_channel stdin in
let parse = MenhirLib.Convert.traditional2revised
  (fun t -> t)
  (fun _ -> Lexing.dummy_pos) (* TODO: fixme *)
  (fun _ -> Lexing.dummy_pos)
  translation_unit in
let ppexpr = try parse (fun () -> lex lexbuf) with
  | err -> printf "Uncaught exception:\n%s\n" (Printexc.to_string err);
    exit 1
in
  (*pr_ppexpr ppexpr;*)
  List.iter (fun e -> printf "%s\n" (string_of_error e)) (List.rev !errors)
