module Lex = Ulexing

open Pp
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

let newline lexbuf =
  if !first_tok then head := false; (* \n\n ends header *)
  first_tok := true;
  line := {src=(!line).src + 1; input=(!line).input + 1};
  colo := Lex.lexeme_start lexbuf
let tok ?(comment=false) lexbuf v =
  let comments = if comment then
      let post_comment = ref [] in
      let () = last_comment_ref := post_comment in ([],post_comment)
    else (head := false; (* semantic token ends header *)
	  first_tok := false;
	  let pre_comments = List.rev !comment_stack in
	  let () = comment_stack := [] in
	  let post_comment = ref [] in
	  let () = last_comment_ref := post_comment in
	  (pre_comments,post_comment)
    ) in
  let loc = {file = !file; line = !line;
	     col=(Lex.lexeme_start lexbuf) - !colo} in
  let scan = Lex.utf8_lexeme lexbuf in
  {loc; scan; comments; v}

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
  | "\n" -> newline lexbuf;
    if !ppdirective then (ppdirective := false; ENDPPDIRECTIVE (tok lexbuf ()))
    else lex lexbuf
  | "#" | "%:" -> if !first_tok (* TODO: %:? *)
    then (ppdirective := true; ppdir_lex lexbuf)
    else (error (InvalidDirectiveLocation (tok lexbuf ())); lex lexbuf)
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
  | "0"digit+ -> error (InvalidOctal (tok lexbuf (Lex.utf8_lexeme lexbuf)));
    lex lexbuf
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
  | eof -> EOF (tok lexbuf ())
  | _ -> error (UnknownCharacter (tok lexbuf ())); lex lexbuf
and block_comment start lines = lexer
  | "\n" -> newline lexbuf; block_comment start lines lexbuf
  | "*/" -> lines
  | ([^'*']|('*'+[^'/']))* ->
    block_comment start ((Lex.utf8_lexeme lexbuf)::lines) lexbuf
  | eof -> error (UnterminatedComment start);
    Lex.rollback lexbuf; lines
and comment_lex klex = lexer
  | "//"[^'\n']* ->
    let start = !first_tok in
    let comment = tok ~comment:true lexbuf
      [String.after (Lex.utf8_lexeme lexbuf) 2] in
    if !head then
      if !colo = 0 && start then
	(add_post_comment comment; BOF {comment with v=()})
      else (add_post_comment comment; klex lexbuf)
    else if start then (comment_stack := comment::!comment_stack; klex lexbuf)
    else (add_post_comment comment; klex lexbuf)
  | "/*" ->
    let start = !first_tok in
    let openc = tok ~comment:true lexbuf () in
    let lines = block_comment openc [] lexbuf in
    let comment = {openc with v=lines} in
    if !head then
      if !colo = 0 && start then
	(add_post_comment comment; BOF openc)
      else (add_post_comment comment; klex lexbuf)
    else if start then (comment_stack := comment::!comment_stack; klex lexbuf)
    else (add_post_comment comment; klex lexbuf)
  | " " | "\t" | "\r" -> comment_lex klex lexbuf
  | _ -> Lex.rollback lexbuf; klex lexbuf
and ppdir_lex = lexer
  | " " | "\t" | "\r" -> ppdir_lex lexbuf
  | "//"[^'\n']* -> Lex.rollback lexbuf; comment_lex ppdir_lex lexbuf
  | "/*" -> Lex.rollback lexbuf; comment_lex ppdir_lex lexbuf
  | "extension" -> EXTENSION (tok lexbuf ())
  | "version" -> VERSION (tok lexbuf ())
  | "line" -> LINE (tok lexbuf ())
  | "define" -> DEFINE (tok lexbuf ())
  | "undef" -> UNDEF (tok lexbuf ())
  | "if" -> IF (tok lexbuf ())
  | "ifdef" -> IFDEF (tok lexbuf ())
  | "ifndef" -> IFNDEF (tok lexbuf ())
  | "else" -> ELSE (tok lexbuf ())
  | "elif" -> ELIF (tok lexbuf ())
  | "endif" -> ENDIF (tok lexbuf ())
  | "error" -> ERROR (tok lexbuf ())
  | "pragma" -> PRAGMA (tok lexbuf ())
  | not_white+ ->
    error (InvalidDirective (tok lexbuf (Lex.utf8_lexeme lexbuf)));
    lex lexbuf
