open Pp_lib
open Esslpp_ce
module P = Punc
  
let ce_token_of_punc p = match p.v with
  | P.LEFT_OP -> LEFT_OP p
  | P.RIGHT_OP -> RIGHT_OP p
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
  | P.MOD_ASSIGN -> MOD_ASSIGN p
  | P.LEFT_ASSIGN -> LEFT_ASSIGN p
  | P.RIGHT_ASSIGN -> RIGHT_ASSIGN p
  | P.AND_ASSIGN -> AND_ASSIGN p
  | P.XOR_ASSIGN -> XOR_ASSIGN p
  | P.OR_ASSIGN -> OR_ASSIGN p
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
  | P.TILDE -> TILDE p
  | P.PLUS -> PLUS p
  | P.STAR -> STAR p
  | P.SLASH -> SLASH p
  | P.PERCENT -> PERCENT p
  | P.LEFT_ANGLE -> LEFT_ANGLE p
  | P.RIGHT_ANGLE -> RIGHT_ANGLE p
  | P.VERTICAL_BAR -> VERTICAL_BAR p
  | P.CARET -> CARET p
  | P.AMPERSAND -> AMPERSAND p
  | P.QUESTION -> QUESTION p
      
let ce_tokenize s = List.map
  (function
     | Int i -> INTCONSTANT i
     | Float f -> FLOATCONSTANT f
     | Word w -> WORD { w with v=(fst w.v) }
     | Call c ->
	 raise (ParserError "Call token found in incoming cond_expr stream")
     | Punc p -> ce_token_of_punc p
     | Comma c -> COMMA c
     | Leftp p -> LEFT_PAREN p
     | Rightp p -> RIGHT_PAREN p
  ) s
  
let ce_lexerfn ts =
  let s = ref ts in
    fun () -> match !s with
      | h::r -> s := r; h
      | [] -> EOF

let parse_cond_expr lex =
  let parse = MenhirLib.Convert.traditional2revised
    (fun t -> t)
    (fun _ -> Lexing.dummy_pos) (* TODO: fixme? *)
    (fun _ -> Lexing.dummy_pos)
    ce in parse lex
