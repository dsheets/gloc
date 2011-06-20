module Lex = Ulexing

exception UnterminatedComment
exception ReservedWord of string

let line = ref 1
let colo = ref 0
let newline lexbuf =
  line := !line + 1;
  colo := Lex.lexeme_start lexbuf

let regexp digit = ['0'-'9']
let regexp letter= ['a'-'z''A'-'Z'_]
let regexp hex   = ['a'-'f''A'-'F''0'-'9']
let regexp expo  = ['E''e']['+''-']?digit+
let regexp octal = ['0'-'7']

let type_decl = ref false
let rec lex = lexer
  | "//"[^'\n']* -> lex lexbuf
  | "/*" -> block_comment lexbuf
  | "\n" -> newline lexbuf; lex lexbuf
  | "invariant" ->    INVARIANT
  | "highp" ->        HIGH_PRECISION
  | "mediump" ->      MEDIUM_PRECISION
  | "lowp" ->         LOW_PRECISION
  | "precision" ->    PRECISION
  | "attribute" ->    ATTRIBUTE
  | "const" ->        CONST_QUAL
  | "uniform" ->      UNIFORM
  | "varying" ->      VARYING
  | "break" ->        BREAK
  | "continue" ->     CONTINUE
  | "do" ->           DO
  | "for" ->          FOR
  | "while" ->        WHILE
  | "if" ->           IF
  | "else" ->         ELSE
  | "in" ->           IN_QUAL
  | "out" ->          OUT_QUAL
  | "inout" ->        INOUT_QUAL
  | "float" ->        type_decl := true; FLOAT_TYPE
  | "int" ->          type_decl := true; INT_TYPE
  | "void" ->         type_decl := true; VOID_TYPE
  | "bool" ->         type_decl := true; BOOL_TYPE
  | "true" ->         BOOLCONSTANT true
  | "false" ->        BOOLCONSTANT false
  | "discard" ->      DISCARD
  | "return" ->       RETURN
  | "mat2" ->         type_decl := true; MATRIX2
  | "mat3" ->         type_decl := true; MATRIX3
  | "mat4" ->         type_decl := true; MATRIX4
  | "vec2" ->         type_decl := true; VEC2
  | "vec3" ->         type_decl := true; VEC3
  | "vec4" ->         type_decl := true; VEC4
  | "ivec2" ->        type_decl := true; IVEC2
  | "ivec3" ->        type_decl := true; IVEC3
  | "ivec4" ->        type_decl := true; IVEC4
  | "bvec2" ->        type_decl := true; BVEC2
  | "bvec3" ->        type_decl := true; BVEC3
  | "bvec4" ->        type_decl := true; BVEC4
  | "sampler2D" ->    type_decl := true; SAMPLER2D
  | "samplerCube" ->  type_decl := true; SAMPLERCUBE
  | "struct" ->       type_decl := true; STRUCT
  | "asm" | "class" | "union" | "enum" | "typedef" | "template"
  | "this" | "packed" | "goto" | "switch" | "default" | "inline"
  | "noinline" | "volatile" | "public" | "static" | "extern" | "external"
  | "interface" | "long" | "short" | "double" | "half" | "fixed"
  | "unsigned" | "input" | "output" | "hvec2" | "hvec3" | "hvec4"
  | "fvec2" | "fvec3" | "fvec4" | "dvec2" | "dvec3" | "dvec4"
  | "sizeof" | "cast" | "namespace" | "using"
    -> raise (ReservedWord (Lex.utf8_lexeme lexbuf))
  | letter (letter | digit)* ->
    let symbol = Lex.utf8_lexeme lexbuf in
    try let {symType} = Hashtbl.find symTbl symbol in
	match symType with
	  | Tvar
    with Not_found -> 
  | eof -> EOF
and block_comment = lexer
  | "\n" -> newline lexbuf; block_comment lexbuf
  | "*/" -> lex lexbuf
  | _ -> block_comment lexbuf
  | eof -> raise UnterminatedComment
;;

let print_tok fn lb =
  let rec loop lb =
    let t = try lex lb
      with Lex.Error ->
	(Printf.printf "File \"%s\", line %d, character %d:\n"
	   fn !line ((Lex.lexeme_start lb) - !colo);
	 Printf.printf "Error: unknown lexical symbol\n";
	 exit 1)
    in match t with
      | GREET -> print_endline "Greetings!"; loop lb
      | NUM k -> print_endline ("Half of "^(string_of_int (k*2))); loop lb
      | EOF -> print_endline "fin"
  in loop lb
;;

Arg.parse []
  (fun fn -> print_tok fn (Lex.from_utf8_channel (open_in_bin fn)))
  "Paths of GLSL files"
