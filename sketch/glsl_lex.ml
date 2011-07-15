module Lex = Ulexing

open Glsl

module String = struct
    include String
    let after s k = sub s k ((length s)-k)
end

exception UnterminatedComment of unit tok
exception ReservedWord of string tok
exception UnknownCharacter of unit tok
exception InvalidDirective of unit tok

let file = ref (0,0)
let line = ref (1,1)
let colo = ref 0
let errors = ref []
let first_tok = ref true
let ppdirective = ref false
let error exc = errors := exc::!errors
let newline lexbuf =
  first_tok := true;
  line := (fst !line + 1, snd !line + 1);
  colo := Lex.lexeme_start lexbuf
let tok lexbuf v =
  first_tok := false;
  let loc = (!file, !line,
	     ((Lex.lexeme_start lexbuf) - !colo,
	      (Lex.lexeme_end lexbuf) - !colo)) in
  {loc; v}

let regexp digit = ['0'-'9']
let regexp letter= ['a'-'z''A'-'Z''_']
let regexp hex   = ['a'-'f''A'-'F''0'-'9']
let regexp expo  = ['E''e']['+''-']?digit+
let regexp octal = ['0'-'7']

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

  | "float" ->        FLOAT_TYPE (tok lexbuf ())
  | "int" ->          INT_TYPE (tok lexbuf ())
  | "void" ->         VOID_TYPE (tok lexbuf ())
  | "bool" ->         BOOL_TYPE (tok lexbuf ())
  | "true" ->         BOOLCONSTANT (tok lexbuf true)
  | "false" ->        BOOLCONSTANT (tok lexbuf false)
  | "discard" ->      DISCARD (tok lexbuf ())
  | "return" ->       RETURN (tok lexbuf ())
  | "mat2" ->         MATRIX2 (tok lexbuf ())
  | "mat3" ->         MATRIX3 (tok lexbuf ())
  | "mat4" ->         MATRIX4 (tok lexbuf ())
  | "vec2" ->         VEC2 (tok lexbuf ())
  | "vec3" ->         VEC3 (tok lexbuf ())
  | "vec4" ->         VEC4 (tok lexbuf ())
  | "ivec2" ->        IVEC2 (tok lexbuf ())
  | "ivec3" ->        IVEC3 (tok lexbuf ())
  | "ivec4" ->        IVEC4 (tok lexbuf ())
  | "bvec2" ->        BVEC2 (tok lexbuf ())
  | "bvec3" ->        BVEC3 (tok lexbuf ())
  | "bvec4" ->        BVEC4 (tok lexbuf ())
  | "sampler2D" ->    SAMPLER2D (tok lexbuf ())
  | "samplerCube" ->  SAMPLERCUBE (tok lexbuf ())
  | "struct" ->       STRUCT (tok lexbuf ())

  | "asm" | "class" | "union" | "enum" | "typedef" | "template"
  | "this" | "packed" | "goto" | "switch" | "default" | "inline"
  | "noinline" | "volatile" | "public" | "static" | "extern" | "external"
  | "interface" | "long" | "short" | "double" | "half" | "fixed"
  | "unsigned" | "input" | "output" | "hvec2" | "hvec3" | "hvec4"
  | "fvec2" | "fvec3" | "fvec4" | "dvec2" | "dvec3" | "dvec4"
  | "sizeof" | "cast" | "namespace" | "using"
    -> error (ReservedWord (tok lexbuf (Lex.utf8_lexeme lexbuf))); lex lexbuf

  | letter (letter | digit)* ->
    IDENTIFIER (tok lexbuf (Lex.utf8_lexeme lexbuf))
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

  | ";"  -> SEMICOLON (tok lexbuf ())
  | "{"  -> LEFT_BRACE (tok lexbuf ())
  | "}"  -> RIGHT_BRACE (tok lexbuf ())
  | ","  -> COMMA (tok lexbuf ())
  | ":"  -> COLON (tok lexbuf ())
  | "="  -> EQUAL (tok lexbuf ())
  | "("  -> LEFT_PAREN (tok lexbuf ())
  | ")"  -> RIGHT_PAREN (tok lexbuf ())
  | "["  -> LEFT_BRACKET (tok lexbuf ())
  | "]"  -> RIGHT_BRACKET (tok lexbuf ())
  | "."  -> DOT (tok lexbuf ())

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
