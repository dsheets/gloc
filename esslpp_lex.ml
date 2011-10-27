(* Copyright (c) 2011 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

module Lex = Ulexing

open Pp_lib
open Esslpp

module String = struct
    include String
    let after s k = sub s k ((length s)-k)
end

exception UnterminatedComment of unit pptok
exception UnknownCharacter of unit pptok
exception InvalidDirectiveLocation of unit pptok
exception InvalidDirective of string pptok
exception InvalidOctal of string pptok

let head = ref true
let colo = ref 0
let first_tok = ref true
let ppdirective = ref false
let comment_stack = ref []
let last_comment_ref = ref (ref [])

let newline ?(comment=false) lexbuf =
  if not comment then
    ((if !colo - (Lex.lexeme_start lexbuf) = 0
      then head := false); (* \n\n ends header *)
     first_tok := true);
  line := {src=(!line).src + 1; input=(!line).input + 1};
  colo := (Lex.lexeme_end lexbuf)

let tok ?(comment=false) ?(pre="") ?(drop=0) ?(rewind=0) lexbuf v =
  let comments = if comment then ([],ref [])
  else (head := false; (* semantic token ends header *)
	first_tok := false;
	let pre_comments = List.rev !comment_stack in
	let () = comment_stack := [] in
	let post_comment = ref [] in
	let () = last_comment_ref := post_comment in
	  (pre_comments,post_comment)
       ) in
  let prelen = String.length pre in
  let s = Lex.utf8_lexeme lexbuf in
  let s = String.sub s drop ((String.length s) - drop)  in
  let a = {file = !file; line = !line;
	   col=(Lex.lexeme_start lexbuf) - !colo - prelen + drop - rewind} in
  let z = {file = !file; line = !line;
	   col=(Lex.lexeme_end lexbuf) - !colo - rewind} in
  let scan = scan_of_string {a;z} comments (pre^s) in
    {span={a;z}; scan; comments; v}

let add_post_comment comment =
  (!last_comment_ref) := comment::!(!last_comment_ref)

let regexp digit = ['0'-'9']
let regexp letter= ['a'-'z''A'-'Z''_']
let regexp hex   = ['a'-'f''A'-'F''0'-'9']
let regexp expo  = ['E''e']['+''-']?digit+
let regexp octal = ['0'-'7']
let regexp not_white = [^'\n''\t'' ''\r']

let rec lex = lexer
  | "//"[^'\n']* -> Lex.rollback lexbuf; comment_lex lex lexbuf
  | "/*" -> Lex.rollback lexbuf; comment_lex lex lexbuf
  | "\n" -> let t = tok ~comment:true ~drop:1 ~rewind:1 lexbuf () in
      newline lexbuf;
      if !ppdirective
      then (ppdirective := false; ENDPPDIRECTIVE t)
      else lex lexbuf
  | "#" -> (if not !first_tok
	    then error (InvalidDirectiveLocation (tok lexbuf ())));
      ppdirective := true; head:= false; ppdir_lex lexbuf
  | letter (letter | digit)* "("? ->
      let t = tok lexbuf (Lex.utf8_lexeme lexbuf) in
	begin match String.sub t.v ((String.length t.v)-1) 1 with
	  | "(" -> CALL {t with v=String.sub t.v 0 ((String.length t.v)-1)}
	  | _ -> WORD t
	end
  | "0"['x''X']hex+ ->
      INTCONSTANT (tok lexbuf (Hex,int_of_string (Lex.utf8_lexeme lexbuf)))
  | "0"octal+ ->
      let tail = String.after (Lex.utf8_lexeme lexbuf) 1 in
	INTCONSTANT (tok lexbuf (Oct,int_of_string ("0o"^tail)))
  | "0"digit+ -> let t = tok lexbuf (Lex.utf8_lexeme lexbuf) in
      error (InvalidOctal t);
      INTCONSTANT {t with v=(Dec,int_of_string t.v)}
  | digit+ -> INTCONSTANT (tok lexbuf (Dec,int_of_string (Lex.utf8_lexeme lexbuf)))
  | digit+expo | digit+"."digit*expo? | "."digit+expo? ->
      FLOATCONSTANT (tok lexbuf (float_of_string (Lex.utf8_lexeme lexbuf)))

  | "+=" -> ADD_ASSIGN (tok lexbuf (Punc.ADD_ASSIGN))
  | "-=" -> SUB_ASSIGN (tok lexbuf (Punc.SUB_ASSIGN))
  | "*=" -> MUL_ASSIGN (tok lexbuf (Punc.MUL_ASSIGN))
  | "/=" -> DIV_ASSIGN (tok lexbuf (Punc.DIV_ASSIGN))
  | "%=" -> MOD_ASSIGN (tok lexbuf (Punc.MOD_ASSIGN))
  | "<<="-> LEFT_ASSIGN (tok lexbuf (Punc.LEFT_ASSIGN))
  | ">>="-> RIGHT_ASSIGN (tok lexbuf (Punc.RIGHT_ASSIGN))
  | "&=" -> AND_ASSIGN (tok lexbuf (Punc.AND_ASSIGN))
  | "^=" -> XOR_ASSIGN (tok lexbuf (Punc.XOR_ASSIGN))
  | "|=" -> OR_ASSIGN (tok lexbuf (Punc.OR_ASSIGN))

  | "++" -> INC_OP (tok lexbuf (Punc.INC_OP))
  | "--" -> DEC_OP (tok lexbuf (Punc.DEC_OP))
  | "&&" -> AND_OP (tok lexbuf (Punc.AND_OP))
  | "||" -> OR_OP (tok lexbuf (Punc.OR_OP))
  | "^^" -> XOR_OP (tok lexbuf (Punc.XOR_OP))
  | "<=" -> LE_OP (tok lexbuf (Punc.LE_OP))
  | ">=" -> GE_OP (tok lexbuf (Punc.GE_OP))
  | "==" -> EQ_OP (tok lexbuf (Punc.EQ_OP))
  | "!=" -> NE_OP (tok lexbuf (Punc.NE_OP))
  | "<<" -> LEFT_OP (tok lexbuf (Punc.LEFT_OP))
  | ">>" -> RIGHT_OP (tok lexbuf (Punc.RIGHT_OP))

  | ";"  -> SEMICOLON (tok lexbuf (Punc.SEMICOLON))
  | "{" | "<%" -> LEFT_BRACE (tok lexbuf (Punc.LEFT_BRACE))
  | "}" | "%>" -> RIGHT_BRACE (tok lexbuf (Punc.RIGHT_BRACE))
  | ","  -> COMMA (tok lexbuf (Punc.COMMA))
  | ":"  -> COLON (tok lexbuf (Punc.COLON))
  | "="  -> EQUAL (tok lexbuf (Punc.EQUAL))
  | "("  -> LEFT_PAREN (tok lexbuf (Punc.LEFT_PAREN))
  | ")"  -> RIGHT_PAREN (tok lexbuf (Punc.RIGHT_PAREN))
  | "[" | "<:" -> LEFT_BRACKET (tok lexbuf (Punc.LEFT_BRACKET))
  | "]" | ":>" -> RIGHT_BRACKET (tok lexbuf (Punc.RIGHT_BRACKET))
  | "."  -> DOT (tok lexbuf (Punc.DOT))

  | "!"  -> BANG (tok lexbuf (Punc.BANG))
  | "-"  -> DASH (tok lexbuf (Punc.DASH))
  | "~"  -> TILDE (tok lexbuf (Punc.TILDE))
  | "+"  -> PLUS (tok lexbuf (Punc.PLUS))
  | "*"  -> STAR (tok lexbuf (Punc.STAR))
  | "/"  -> SLASH (tok lexbuf (Punc.SLASH))
  | "%"  -> PERCENT (tok lexbuf (Punc.PERCENT))
  | "<"  -> LEFT_ANGLE (tok lexbuf (Punc.LEFT_ANGLE))
  | ">"  -> RIGHT_ANGLE (tok lexbuf (Punc.RIGHT_ANGLE))
  | "|"  -> VERTICAL_BAR (tok lexbuf (Punc.VERTICAL_BAR))
  | "^"  -> CARET (tok lexbuf (Punc.CARET))
  | "&"  -> AMPERSAND (tok lexbuf (Punc.AMPERSAND))
  | "?"  -> QUESTION (tok lexbuf (Punc.QUESTION))

  | " " | "\t" | "\r" -> lex lexbuf
  | eof -> if !ppdirective
    then let t = tok lexbuf () in
      (ppdirective := false; Lex.rollback lexbuf; ENDPPDIRECTIVE t)
    else EOF (tok lexbuf ())
  | _ -> error (UnknownCharacter (tok lexbuf ())); lex lexbuf
and block_comment start r lines = lexer
  | "\n" -> let t = tok ~comment:true ~drop:1 ~rewind:(r+1) lexbuf "" in
      newline ~comment:true lexbuf;
      block_comment start 0 (t::lines) lexbuf
  | " "*"*/" ->
    let l = Lex.lexeme_length lexbuf in
    List.rev ((tok ~comment:true ~drop:l ~rewind:(r+l) lexbuf "")::lines)
  | ([^'*''\n']|('*'+[^'/''\n']))* ->
      let t = tok ~comment:true ~rewind:r lexbuf (Lex.utf8_lexeme lexbuf) in
	block_comment start r (t::lines) lexbuf
  | eof -> error (UnterminatedComment start);
      Lex.rollback lexbuf; List.rev lines
and comment_lex klex = lexer
  | "//"[^'\n']* ->
    let start = !first_tok in
    let comment = tok ~comment:true ~drop:2 lexbuf
      (String.after (Lex.utf8_lexeme lexbuf) 2) in
    let comment = {(fuse_pptok [comment]) with v=[comment]} in
    if !head then
      (add_post_comment comment;
       if !colo = 0 && start then
	 BOF {comment with comments=([],!last_comment_ref); v=()}
       else klex lexbuf)
    else if start then (comment_stack := comment::!comment_stack; klex lexbuf)
    else (add_post_comment comment; klex lexbuf)
  | "/*"" "* ->
    let start = !first_tok in
    let startcolo = !colo in
    let l = Lex.lexeme_length lexbuf in
    let openc = tok ~comment:true ~drop:l ~rewind:(l-2) lexbuf "" in
    let lines = block_comment {openc with v=()} (l-2) [openc] lexbuf in
    let comment = {(fuse_pptok lines) with v=lines} in
    if !head then
      (add_post_comment comment;
       if startcolo = 0 && start then
	 BOF {openc with comments=([],!last_comment_ref); v=()}
       else klex lexbuf)
    else if start then
      (comment_stack := comment::!comment_stack; klex lexbuf)
    else (add_post_comment comment; klex lexbuf)
  | " " | "\t" | "\r" -> comment_lex klex lexbuf
  | _ -> Lex.rollback lexbuf; klex lexbuf
and ppdir_lex = lexer
  | " " | "\t" -> ppdir_lex lexbuf
  | "\n" -> let t = tok ~drop:1 lexbuf () in
      newline lexbuf; ppdirective := false; ENDPPDIRECTIVE t
  | "//"[^'\n']* -> Lex.rollback lexbuf; comment_lex ppdir_lex lexbuf
  | "/*" -> Lex.rollback lexbuf; comment_lex ppdir_lex lexbuf
  | "extension" -> EXTENSION (tok ~pre:"#" lexbuf ())
  | "version" -> VERSION (tok ~pre:"#" lexbuf ())
  | "line" -> LINE (tok ~pre:"#" lexbuf ())
  | "define" -> DEFINE (tok ~pre:"#" lexbuf ())
  | "undef" -> UNDEF (tok ~pre:"#" lexbuf ())
  | "if" -> IF (tok ~pre:"#" lexbuf ())
  | "ifdef" -> IFDEF (tok ~pre:"#" lexbuf ())
  | "ifndef" -> IFNDEF (tok ~pre:"#" lexbuf ())
  | "else" -> ELSE (tok ~pre:"#" lexbuf ())
  | "elif" -> ELIF (tok ~pre:"#" lexbuf ())
  | "endif" -> ENDIF (tok ~pre:"#" lexbuf ())
  | "error" -> ERROR (tok ~pre:"#" lexbuf ())
  | "pragma" -> PRAGMA (tok ~pre:"#" lexbuf ())
  | letter+ | _ -> error (InvalidDirective (tok lexbuf (Lex.utf8_lexeme lexbuf)));
      lex lexbuf
