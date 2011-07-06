%{
  open Pp
%}

%token <unit Pp.pptok> BOF EOF ENDPPDIRECTIVE
%token <unit Pp.pptok> EXTENSION VERSION LINE
%token <unit Pp.pptok> DEFINE UNDEF IF IFDEF IFNDEF ELSE ELIF ENDIF ERROR
%token <unit Pp.pptok> PRAGMA
%token <string Pp.pptok> WORD CALL
%token <float Pp.pptok> FLOATCONSTANT
%token <(Pp.base * int) Pp.pptok> INTCONSTANT

%token <Punc.tok Pp.pptok> LEFT_OP RIGHT_OP INC_OP DEC_OP LE_OP GE_OP EQ_OP
%token <Punc.tok Pp.pptok> NE_OP AND_OP OR_OP XOR_OP MUL_ASSIGN DIV_ASSIGN
%token <Punc.tok Pp.pptok> ADD_ASSIGN MOD_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN
%token <Punc.tok Pp.pptok> AND_ASSIGN XOR_ASSIGN OR_ASSIGN SUB_ASSIGN
%token <Punc.tok Pp.pptok> LEFT_PAREN RIGHT_PAREN LEFT_BRACKET RIGHT_BRACKET
%token <Punc.tok Pp.pptok> LEFT_BRACE RIGHT_BRACE DOT COMMA COLON EQUAL
%token <Punc.tok Pp.pptok> SEMICOLON BANG DASH TILDE PLUS STAR SLASH PERCENT
%token <Punc.tok Pp.pptok> LEFT_ANGLE RIGHT_ANGLE VERTICAL_BAR CARET
%token <Punc.tok Pp.pptok> AMPERSAND QUESTION

%type <Pp.pptok_expr> translation_unit

%start translation_unit

%left OR_OP
%left AND_OP
%left VERTICAL_BAR
%left CARET
%left AMPERSAND
%left EQ_OP NE_OP
%left LEFT_ANGLE RIGHT_ANGLE LE_OP GE_OP
%left LEFT_OP RIGHT_OP
%left PLUS DASH
%left STAR SLASH PERCENT

%%
punc
: p=LEFT_OP | p=RIGHT_OP | p=INC_OP | p=DEC_OP | p=LE_OP | p=GE_OP | p=EQ_OP
| p=NE_OP | p=AND_OP | p=OR_OP | p=XOR_OP | p=MUL_ASSIGN | p=DIV_ASSIGN
| p=ADD_ASSIGN | p=MOD_ASSIGN | p=LEFT_ASSIGN | p=RIGHT_ASSIGN
| p=AND_ASSIGN | p=XOR_ASSIGN | p=OR_ASSIGN | p=SUB_ASSIGN
| p=LEFT_PAREN | p=RIGHT_PAREN | p=LEFT_BRACKET | p=RIGHT_BRACKET
| p=LEFT_BRACE | p=RIGHT_BRACE | p=DOT | p=COMMA | p=COLON | p=EQUAL
| p=SEMICOLON | p=BANG | p=DASH | p=TILDE | p=PLUS | p=STAR | p=SLASH
| p=PERCENT | p=LEFT_ANGLE | p=RIGHT_ANGLE | p=VERTICAL_BAR | p=CARET
| p=AMPERSAND | p=QUESTION { p }
    
source
: w=WORD { Word w }
| c=CALL { Call c }
| f=FLOATCONSTANT { Float f }
| i=INTCONSTANT { Int i }
| p=punc { Punc p }

cond_unop
: LEFT_PAREN; e=cond_expr RIGHT_PAREN { Group e }
| PLUS; e=cond_unop { Pos e }
| DASH; e=cond_unop { Neg e }
| TILDE; e=cond_unop { BitNot e }
| BANG; e=cond_unop { Not e }
| int=INTCONSTANT { Constant {int with v=snd int.v} }
| macro=CALL; args=separated_list(COMMA,WORD); RIGHT_PAREN
    { Fmacro (macro,args) } (* TODO: other args? *)
| op=WORD macro=WORD? {
    match macro with
      | Some operand -> if op.v="defined" then Defined operand
	else Omacros [op;operand]
      | None -> Omacros [op]
  } (* TODO: correct macro strings + precedence *)

cond_expr (* TODO: check prec *)
: a=cond_expr; STAR; b=cond_expr { Mul (a,b) }
| a=cond_expr; SLASH; b=cond_expr { Div (a,b) }
| a=cond_expr; PERCENT; b=cond_expr { Mod (a,b) }
| a=cond_expr; PLUS; b=cond_expr { Add (a,b) }
| a=cond_expr; DASH; b=cond_expr { Sub (a,b) }
| a=cond_expr; LEFT_OP; b=cond_expr { BitLeft (a,b) }
| a=cond_expr; RIGHT_OP; b=cond_expr { BitRight (a,b) }
| a=cond_expr; LEFT_ANGLE; b=cond_expr { Lt (a,b) }
| a=cond_expr; RIGHT_ANGLE; b=cond_expr { Gt (a,b) }
| a=cond_expr; LE_OP; b=cond_expr { Lte (a,b) }
| a=cond_expr; GE_OP; b=cond_expr { Gte (a,b) }
| a=cond_expr; EQ_OP; b=cond_expr { Eq (a,b) }
| a=cond_expr; NE_OP; b=cond_expr { Neq (a,b) }
| a=cond_expr; AMPERSAND; b=cond_expr { BitAnd (a,b) }
| a=cond_expr; CARET; b=cond_expr { BitXor (a,b) }
| a=cond_expr; VERTICAL_BAR; b=cond_expr { BitOr (a,b) }
| a=cond_expr; AND_OP; b=cond_expr { And (a,b) }
| a=cond_expr; OR_OP; b=cond_expr { Or (a,b) }
| u=cond_unop { u }

behavior
: b=WORD { match b.v with
  | "require" -> {b with v=Require}
  | "enable" -> {b with v=Enable}
  | "warn" -> {b with v=Warn}
  | "disable" -> {b with v=Disable}
  | _ -> error (UnknownBehavior b); {b with v=Disable}
}

cond_continue
: ENDIF ENDPPDIRECTIVE { None }
| first=ELIF; c=cond_expr; last=ENDPPDIRECTIVE; b=body; f=cond_continue {
  Some (If ({first; last}, c, b, f))
}
| ELSE; ENDPPDIRECTIVE; b=body; ENDIF {
  Some b
}

directive
: first=IF; c=cond_expr; last=ENDPPDIRECTIVE; b=body; f=cond_continue {
  If ({first; last},c,b,f)
}
| first=IFDEF; m=WORD; last=ENDPPDIRECTIVE; b=body; f=cond_continue {
  If ({first; last},Defined m,b,f)
}
| first=IFNDEF; m=WORD; last=ENDPPDIRECTIVE; b=body; f=cond_continue {
  If ({first; last},Not (Defined m),b,f)
}
| first=DEFINE; m=WORD; stream=source* {
  let last = last_pptok {m with v=()} stream in
  Def ({first; last}, m, {macros=Macros.empty; stream})
}
| first=DEFINE; m=CALL; args=separated_list(COMMA,WORD); last=RIGHT_PAREN; stream=source* {
  let last = last_pptok (proj last) stream in
  Fun ({first; last}, m, args, {macros=Macros.empty; stream})
}
| first=UNDEF; m=WORD { Undef ({first; last={m with v=()}}, m) }
| first=ERROR; stream=source* {
  let last = last_pptok first stream in
  Err ({first; last}, stream)
}
| first=PRAGMA; stream=source* {
  let last = last_pptok first stream in
  Pragma ({first; last}, stream)
}
| first=EXTENSION; ext=WORD; COLON; b=behavior {
  Extension ({first; last={b with v=()}}, ext, b)
}
| first=VERSION; v=INTCONSTANT { (* TODO: Check only decimal base *)
  Version ({first; last={v with v=()}},snd v.v)
}
| first=LINE; l=INTCONSTANT { (* TODO: Check only decimal base *)
  Line ({first; last={l with v=()}}, (!file).src, snd l.v)
}
| first=LINE; l=INTCONSTANT; src=INTCONSTANT { (* TODO: Check only decimal base *)
  Line ({first; last={src with v=()}}, snd src.v, snd l.v)
}

ppdir
: dir=directive; last=ENDPPDIRECTIVE; rest=body? {
  match rest with None -> dir
    | Some expr ->
      Concat (span_concat
		(Chunk ({first=(span_of_pptok_expr dir).first;last},
			{macros=Macros.empty; stream=[]}))
		expr, dir, expr)
}
| last=ENDPPDIRECTIVE; rest=body? {
  match rest with None -> Chunk ({first=last; last}, {macros=Macros.empty; stream=[]})
    | Some expr -> expr
}

chunk
: s=source r=source* {
  match span_of_pptok_types (s::r) with
    | Some span -> Chunk (span,{macros=Macros.empty; stream=s::r})
    | None -> raise (ParserError 1)
}

body
: pp=ppdir { pp }
| chunk=chunk; pp=ppdir? {
  match pp with None -> chunk
    | Some expr -> Concat (span_concat chunk expr, chunk, expr)
}

translation_unit
: top=BOF?; body=body?; bot=EOF {
  let cspan cs default = match span_of_comments cs with
    | Some s -> s | None -> default
  in
  let append_comments expr cs =
    let espan = span_of_pptok_expr expr in
    let csspan = cspan cs espan in
    Concat ({first=espan.first; last=csspan.last},expr, Comments (csspan, cs))
  in
  let prepend_comments expr cs =
    let espan = span_of_pptok_expr expr in
    let csspan = cspan cs espan in
    Concat ({first=csspan.first; last=espan.last}, Comments (csspan,cs),expr)
  in let def = {first=bot; last=bot} in
     match top, body, fst bot.comments with
       | None,None,[] ->
	 Chunk (def,{macros=Macros.empty; stream=[]})
       | None,Some body,[] -> body
       | None,None,cs -> Comments (cspan cs def,cs)
       | None,Some body,cs -> append_comments body cs
       | Some t,None,[] -> let cs = !(snd t.comments) in
			   Comments (cspan cs def,cs)
       | Some t,Some body,[] -> prepend_comments body !(snd t.comments)
       | Some t,None,cs ->
	 prepend_comments (Comments (cspan cs def,cs)) !(snd t.comments)
       | Some t,Some body,cs ->
	 let m = append_comments body cs in
	 prepend_comments m !(snd t.comments)
}
%%
