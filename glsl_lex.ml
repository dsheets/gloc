module Lex = Ulexing

open Glsl

type symtype = Var | Fun | Struct
type symbol = {symType:symtype}
module String = struct
    include String
    let after s k = sub s k ((length s)-k)
end

exception UnterminatedComment of unit tok
exception ReservedWord of string tok
exception UnknownCharacter of unit tok
exception InvalidDirective of unit tok

let line = ref 1
let colo = ref 0
let errors = ref []
let first_tok = ref true
let ppdirective = ref false
let error exc = errors := exc::!errors
let newline lexbuf =
  first_tok := true;
  line := !line + 1;
  colo := Lex.lexeme_start lexbuf
let tok lexbuf v =
  first_tok := false;
  let loc = (!line,
	     (Lex.lexeme_start lexbuf) - !colo,
	     (Lex.lexeme_end lexbuf) - !colo) in
  {loc; v}
let lookup name = match !env with
  | [] -> raise Not_found
  | top::rest -> SymMap.find top name

let regexp digit = ['0'-'9']
let regexp letter= ['a'-'z''A'-'Z''_']
let regexp hex   = ['a'-'f''A'-'F''0'-'9']
let regexp expo  = ['E''e']['+''-']?digit+
let regexp octal = ['0'-'7']

let type_decl = ref false
let in_paren  = ref false
let field_sel = ref false
let rec lex = lexer
  | "//"[^'\n']* ->
    COMMENT (tok lexbuf [String.after 2 (Lex.utf8_lexeme lexbuf)])
  | "/*" -> COMMENT (tok lexbuf (block_comment [] lexbuf))
  | "\n" -> newline lexbuf;
    if !ppdirective
    then (ppdirective := false; ENDPPDIRECTIVE (tok lexbuf ()))
    else lex lexbuf
  | "#" -> if !first_tok
    then (ppdirective := true; PPDIRECTIVE (tok lexbuf ()))
    else error (InvalidDirective (tok lexbuf ()))

  | "invariant" ->    INVARIANT (tok lexbuf ())
  | "highp" ->        HIGH_PRECISION (tok lexbuf ())
  | "mediump" ->      MEDIUM_PRECISION (tok lexbuf ())
  | "lowp" ->         LOW_PRECISION (tok lexbuf ())
  | "precision" ->    PRECISION (tok lexbuf ())
  | "attribute" ->    ATTRIBUTE (tok lexbuf ())
  | "const" ->        CONST_QUAL (tok lexbuf ())
  | "uniform" ->      UNIFORM (tok lexbuf ())
  | "varying" ->      VARYING (tok lexbuf ())
  | "break" ->        BREAK (tok lexbuf ())
  | "continue" ->     CONTINUE (tok lexbuf ())
  | "do" ->           DO (tok lexbuf ())
  | "for" ->          FOR (tok lexbuf ())
  | "while" ->        WHILE (tok lexbuf ())
  | "if" ->           IF (tok lexbuf ())
  | "else" ->         ELSE (tok lexbuf ())
  | "in" ->           IN_QUAL (tok lexbuf ())
  | "out" ->          OUT_QUAL (tok lexbuf ())
  | "inout" ->        INOUT_QUAL (tok lexbuf ())

  | "float" ->        type_decl := true; FLOAT_TYPE (tok lexbuf ())
  | "int" ->          type_decl := true; INT_TYPE (tok lexbuf ())
  | "void" ->         type_decl := true; VOID_TYPE (tok lexbuf ())
  | "bool" ->         type_decl := true; BOOL_TYPE (tok lexbuf ())
  | "true" ->         BOOLCONSTANT (tok lexbuf true)
  | "false" ->        BOOLCONSTANT (tok lexbuf false)
  | "discard" ->      DISCARD (tok lexbuf ())
  | "return" ->       RETURN (tok lexbuf ())
  | "mat2" ->         type_decl := true; MATRIX2 (tok lexbuf ())
  | "mat3" ->         type_decl := true; MATRIX3 (tok lexbuf ())
  | "mat4" ->         type_decl := true; MATRIX4 (tok lexbuf ())
  | "vec2" ->         type_decl := true; VEC2 (tok lexbuf ())
  | "vec3" ->         type_decl := true; VEC3 (tok lexbuf ())
  | "vec4" ->         type_decl := true; VEC4 (tok lexbuf ())
  | "ivec2" ->        type_decl := true; IVEC2 (tok lexbuf ())
  | "ivec3" ->        type_decl := true; IVEC3 (tok lexbuf ())
  | "ivec4" ->        type_decl := true; IVEC4 (tok lexbuf ())
  | "bvec2" ->        type_decl := true; BVEC2 (tok lexbuf ())
  | "bvec3" ->        type_decl := true; BVEC3 (tok lexbuf ())
  | "bvec4" ->        type_decl := true; BVEC4 (tok lexbuf ())
  | "sampler2D" ->    type_decl := true; SAMPLER2D (tok lexbuf ())
  | "samplerCube" ->  type_decl := true; SAMPLERCUBE (tok lexbuf ())
  | "struct" ->       type_decl := true; STRUCT (tok lexbuf ())

  | "asm" | "class" | "union" | "enum" | "typedef" | "template"
  | "this" | "packed" | "goto" | "switch" | "default" | "inline"
  | "noinline" | "volatile" | "public" | "static" | "extern" | "external"
  | "interface" | "long" | "short" | "double" | "half" | "fixed"
  | "unsigned" | "input" | "output" | "hvec2" | "hvec3" | "hvec4"
  | "fvec2" | "fvec3" | "fvec4" | "dvec2" | "dvec3" | "dvec4"
  | "sizeof" | "cast" | "namespace" | "using"
    -> error (ReservedWord (tok lexbuf (Lex.utf8_lexeme lexbuf))); lex lexbuf

  | letter (letter | digit)* ->
    let symbol = Lex.utf8_lexeme lexbuf in
    begin if !field_sel
      then (field_sel := false; FIELD_SELECTION (tok lexbuf symbol))
      else
	try let {symType} = lookup symbol in
	    if !type_decl then IDENTIFIER (tok lexbuf (!env,symbol))
	    else match symType with
	      | Struct -> type_decl:= true; TYPE_NAME (tok lexbuf symbol)
	      | _ -> IDENTIFIER (tok lexbuf (!env,symbol))
	with Failure "hd" | Not_found ->
	  IDENTIFIER (tok lexbuf (!env,symbol))
    end
  | "0"['x''X']hex+ ->
    INTCONSTANT (tok lexbuf (int_of_string (Lex.utf8_lexeme lexbuf)))
  | "0"octal+ ->
    let tail = String.after (Lex.utf8_lexeme lexbuf) 1 in
    INTCONSTANT (tok lexbuf (int_of_string ("0o"^tail)))
  | "0"digit+ -> error (InvalidOctal (tok lexbuf (Lex.utf8_lexeme lexbuf)));
    lex lexbuf
  | digit+ -> INTCONSTANT (tok lexbuf (int_of_string (Lex.utf8_lexeme lexbuf)))
  | digit+expo | digit+"."digit*expo? | "."digit+expo? ->
    FLOATCONSTANT (tok lexbuf (float_of_string (Lex.utf8_lexeme lexbuf)))

  | "+=" -> ADD_ASSIGN (tok lexbuf ())
  | "-=" -> SUB_ASSIGN (tok lexbuf ())
  | "*=" -> MUL_ASSIGN (tok lexbuf ())
  | "/=" -> DIV_ASSIGN (tok lexbuf ())
  | "%=" -> MOD_ASSIGN (tok lexbuf ())
  | "<<="-> LEFT_ASSIGN (tok lexbuf ())
  | ">>="-> RIGHT_ASSIGN (tok lexbuf ())
  | "&=" -> AND_ASSIGN (tok lexbuf ())
  | "^=" -> XOR_ASSIGN (tok lexbuf ())
  | "|=" -> OR_ASSIGN (tok lexbuf ())

  | "++" -> INC_OP (tok lexbuf ())
  | "--" -> DEC_OP (tok lexbuf ())
  | "&&" -> AND_OP (tok lexbuf ())
  | "||" -> OR_OP (tok lexbuf ())
  | "^^" -> XOR_OP (tok lexbuf ())
  | "<=" -> LE_OP (tok lexbuf ())
  | ">=" -> GE_OP (tok lexbuf ())
  | "==" -> EQ_OP (tok lexbuf ())
  | "!=" -> NE_OP (tok lexbuf ())
  | "<<" -> LEFT_OP (tok lexbuf ())
  | ">>" -> RIGHT_OP (tok lexbuf ())

  | ";"  -> type_decl := false; SEMICOLON (tok lexbuf ())
  | "{"  -> type_decl := false; LEFT_BRACE (tok lexbuf ())
  | "}"  -> RIGHT_BRACE (tok lexbuf ())
  | ","  -> (if !in_paren then type_decl := false); COMMA (tok lexbuf ())
  | ":"  -> COLON (tok lexbuf ())
  | "="  -> type_decl := false; EQUAL (tok lexbuf ())
  | "("  -> type_decl := false; in_paren := true; LEFT_PAREN (tok lexbuf ())
  | ")"  -> in_paren := false; RIGHT_PAREN (tok lexbuf ())
  | "["  -> LEFT_BRACKET (tok lexbuf ())
  | "]"  -> RIGHT_BRACKET (tok lexbuf ())
  | "."  -> field_sel := true; DOT (tok lexbuf ())

  | "!"  -> BANG (tok lexbuf ())
  | "-"  -> DASH (tok lexbuf ())
  | "~"  -> TILDE (tok lexbuf ())
  | "+"  -> PLUS (tok lexbuf ())
  | "*"  -> STAR (tok lexbuf ())
  | "/"  -> SLASH (tok lexbuf ())
  | "%"  -> PERCENT (tok lexbuf ())
  | "<"  -> LEFT_ANGLE (tok lexbuf ())
  | ">"  -> RIGHT_ANGLE (tok lexbuf ())
  | "|"  -> VERTICAL_BAR (tok lexbuf ())
  | "^"  -> CARET (tok lexbuf ())
  | "&"  -> AMPERSAND (tok lexbuf ())
  | "?"  -> QUESTION (tok lexbuf ())

  | " " | "\t" | "\r" -> lex lexbuf
  | eof -> EOF
  | _ -> error (UnknownCharacter (tok lexbuf ())); lex lexbuf
and block_comment lines = lexer
  | "\n" -> newline lexbuf; block_comment lines lexbuf
  | "*/" -> lines
  | _ -> block_comment ((Lex.utf8_lexeme lexbuf)::lines) lexbuf
  | eof -> error (UnterminatedComment (tok lexbuf ())); lex lexbuf
