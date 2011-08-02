open Pp_lib
open Essl
module P = Punc

exception ReservedEsslOper of Punc.tok pptok
exception EsslParseError of string * src_loc * src_loc

let rec stream_of_pptok_expr = function
  | Comments _ -> []
  | Chunk t -> t.v
  | If {v=(_,expr,_)} -> stream_of_pptok_expr expr (* TODO: fixme? *)
  | Def _ | Fun _ | Undef _ | Err _ -> []
  | Pragma _ -> [] (* TODO: thread pragmas into essl parser *)
  | Version _ -> []
  | Extension _ -> []
  | Line _ -> []
  | List {v=exprl} -> List.fold_left
      (fun ptl expr -> ptl@(stream_of_pptok_expr expr))
	[] exprl

let essl_token_of_word w = match w.v with
  | "highp" -> HIGH_PRECISION (proj w)
  | "mediump" -> MEDIUM_PRECISION (proj w)
  | "lowp" -> LOW_PRECISION (proj w)
  | "precision" -> PRECISION (proj w)
  | "invariant" -> INVARIANT (proj w)
  | "attribute" -> ATTRIBUTE (proj w)
  | "const" -> CONST (proj w)
  | "bool" -> BOOL (proj w)
  | "float" -> FLOAT (proj w)
  | "int" -> INT (proj w)
  | "break" -> BREAK (proj w)
  | "continue" -> CONTINUE (proj w)
  | "do" -> DO (proj w)
  | "else" -> ELSE (proj w)
  | "for" -> FOR (proj w)
  | "if" -> IF (proj w)
  | "discard" -> DISCARD (proj w)
  | "return" -> RETURN (proj w)
  | "bvec2" -> BVEC2 (proj w)
  | "bvec3" -> BVEC3 (proj w)
  | "bvec4" -> BVEC4 (proj w)
  | "ivec2" -> IVEC2 (proj w)
  | "ivec3" -> IVEC3 (proj w)
  | "ivec4" -> IVEC4 (proj w)
  | "vec2" -> VEC2 (proj w)
  | "vec3" -> VEC3 (proj w)
  | "vec4" -> VEC4 (proj w)
  | "mat2" -> MAT2 (proj w)
  | "mat3" -> MAT3 (proj w)
  | "mat4" -> MAT4 (proj w)
  | "in" -> IN (proj w)
  | "out" -> OUT (proj w)
  | "inout" -> INOUT (proj w)
  | "uniform" -> UNIFORM (proj w)
  | "varying" -> VARYING (proj w)
  | "struct" -> STRUCT (proj w)
  | "void" -> VOID (proj w)
  | "while" -> WHILE (proj w)
  | "sampler2D" -> SAMPLER2D (proj w)
  | "samplerCube" -> SAMPLERCUBE (proj w)
  | _ -> IDENTIFIER w

let essl_token_of_punc p = match p.v with
  | P.INC_OP -> INC_OP p
  | P.DEC_OP -> DEC_OP p
  | P.LE_OP -> LE_OP p
  | P.GE_OP -> GE_OP p
  | P.EQ_OP -> EQ_OP p
  | P.NE_OP -> NE_OP p
  | P.AND_OP -> AND_OP p
  | P.OR_OP -> OR_OP p
  | P.XOR_OP -> XOR_OP p
  | P.MUL_ASSIGN -> MUL_ASSIGN p
  | P.DIV_ASSIGN -> DIV_ASSIGN p
  | P.ADD_ASSIGN -> ADD_ASSIGN p
  | P.SUB_ASSIGN -> SUB_ASSIGN p
  | P.LEFT_PAREN -> LEFT_PAREN p
  | P.RIGHT_PAREN -> RIGHT_PAREN p
  | P.LEFT_BRACKET -> LEFT_BRACKET p
  | P.RIGHT_BRACKET -> RIGHT_BRACKET p
  | P.LEFT_BRACE -> LEFT_BRACE p
  | P.RIGHT_BRACE -> RIGHT_BRACE p
  | P.DOT -> DOT p
  | P.COMMA -> COMMA p
  | P.COLON -> COLON p
  | P.EQUAL -> EQUAL p
  | P.SEMICOLON -> SEMICOLON p
  | P.BANG -> BANG p
  | P.DASH -> DASH p
  | P.PLUS -> PLUS p
  | P.STAR -> STAR p
  | P.SLASH -> SLASH p
  | P.LEFT_ANGLE -> LEFT_ANGLE p
  | P.RIGHT_ANGLE -> RIGHT_ANGLE p
  | P.QUESTION -> QUESTION p
  | P.AMPERSAND | P.CARET | P.VERTICAL_BAR | P.PERCENT | P.TILDE | P.OR_ASSIGN
  | P.XOR_ASSIGN | P.AND_ASSIGN | P.RIGHT_ASSIGN | P.LEFT_ASSIGN | P.MOD_ASSIGN
  | P.RIGHT_OP | P.LEFT_OP ->
      error (ReservedEsslOper p);
      raise (ParserError "Reserved operator in incoming essl stream")

let essl_tokenize s = List.map
  (function
     | Int i -> INTCONSTANT { i with v=(snd i.v) }
     | Float f -> FLOATCONSTANT f
     | Word w -> essl_token_of_word { w with v=(fst w.v) }
     | Call c ->
	 raise (ParserError "Call token found in incoming essl stream")
     | Punc p -> essl_token_of_punc p
     | Comma c -> COMMA c
     | Leftp p -> LEFT_PAREN p
     | Rightp p -> RIGHT_PAREN p
  ) s

let essl_lexerfn ts =
  let s = ref ts in
    fun () -> match !s with
      | h::r -> s := r; h
      | [] -> EOF

let parse_essl lex =
  let parse = MenhirLib.Convert.traditional2revised
    (fun t -> t)
    (fun _ -> Lexing.dummy_pos) (* TODO: fixme? *)
    (fun _ -> Lexing.dummy_pos)
    translation_unit in parse lex
