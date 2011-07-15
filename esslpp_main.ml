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
in let ppexpr = normalize_ppexpr ppexpr in
   let ppexpr = macro_expand_ppexpr ppexpr in
   if (List.length !errors) > 0 then
     (List.iter (fun e -> printf "%s\n" (string_of_error e))
	(List.rev !errors);
      exit 1)
   else
    (*printf "%s\n" (string_of_ppexpr_tree ppexpr);*)
     printf "%s\n"
       (snd ((proj_pptok_expr ppexpr).scan
		{file={src=0;input=0};line={src=1;input=1};col=0}))
