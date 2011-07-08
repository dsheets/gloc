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
: l=LEFT_PAREN; e=cond_expr; r=RIGHT_PAREN {
  Group {(fuse_pptok [proj l;proj e;proj r]) with v=e}
}
| o=PLUS; e=cond_unop { Pos {(fuse_pptok [proj o; proj e]) with v=e} }
| o=DASH; e=cond_unop { Neg {(fuse_pptok [proj o; proj e]) with v=e} }
| o=TILDE; e=cond_unop { BitNot {(fuse_pptok [proj o; proj e]) with v=e} }
| o=BANG; e=cond_unop { Not {(fuse_pptok [proj o; proj e]) with v=e} }
| int=INTCONSTANT { Constant {int with v=snd int.v} }
| macro=CALL; args=separated_list(COMMA,source*); r=RIGHT_PAREN {
    let tokl = (proj macro)::(List.append args [r]) in
      Fmacro {(fuse_pptok tokl) with v=(macro,args)}
  }
| op=WORD macro=WORD? {
    match macro with
      | Some operand -> if op.v="defined"
	then Defined {(fuse_pptok [proj op; proj operand]) with v=operand}
	else Omacros {(fuse_pptok [proj op; proj operand]) with v=[op;operand]}
      | None -> Omacros {(fuse_pptok [proj op]) with v=[op]}
  } (* TODO: correct macro strings + precedence *)

cond_expr (* TODO: check prec *)
: a=cond_expr; o=STAR; b=cond_expr {
  Mul {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
}
| a=cond_expr; o=SLASH; b=cond_expr {
    Div {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=PERCENT; b=cond_expr {
    Mod {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=PLUS; b=cond_expr {
    Add {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=DASH; b=cond_expr {
    Sub {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=LEFT_OP; b=cond_expr {
    BitLeft {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=RIGHT_OP; b=cond_expr {
    BitRight {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=LEFT_ANGLE; b=cond_expr {
    Lt {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=RIGHT_ANGLE; b=cond_expr {
    Gt {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=LE_OP; b=cond_expr {
    Lte {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=GE_OP; b=cond_expr {
    Gte {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=EQ_OP; b=cond_expr {
    Eq {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=NE_OP; b=cond_expr {
    Neq {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=AMPERSAND; b=cond_expr {
    BitAnd {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=CARET; b=cond_expr {
    BitXor {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=VERTICAL_BAR; b=cond_expr {
    BitOr {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=AND_OP; b=cond_expr {
    And {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
| a=cond_expr; o=OR_OP; b=cond_expr {
    Or {(fuse_pptok [proj a; proj o; proj b]) with v=(a,b)}
  }
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
: d=ENDIF; ENDPPDIRECTIVE { {d with v=None} }
| first=ELIF; c=cond_expr; last=ENDPPDIRECTIVE; b=body; f=cond_continue {
    {(fuse_pptok [first; proj c; last; proj b; proj f])
     with v=Some (If (c, b, f.v)) }
  }
| d=ELSE; ENDPPDIRECTIVE; b=body; e=ENDIF {
    {(fuse_pptok [d; proj b; e]) with v=Some b}
  }

directive
: first=IF; c=cond_expr; last=ENDPPDIRECTIVE; b=body; f=cond_continue {
  If {(fuse_pptok [first; proj c; last; proj b; proj f]) with v=(c,b,f.v)}
}
| first=IFDEF; m=WORD; last=ENDPPDIRECTIVE; b=body; f=cond_continue {
    If {(fuse_pptok [first; proj m; last; proj b; proj f])
	with v=(Defined {m with v=m},b,f.v)}
  }
| first=IFNDEF; m=WORD; last=ENDPPDIRECTIVE; b=body; f=cond_continue {
    If {(fuse_pptok [first; proj m; last; proj b; proj f])
	with v=(Not {m with v=Defined {m with v=m}},b,f.v)}
  }
| first=DEFINE; m=WORD; stream=source* {
    Def {(fuse_pptok (first::(proj m)::stream)) with v=(m, stream)}
  }
| d=DEFINE; m=CALL; args=separated_list(COMMA,WORD); r=RIGHT_PAREN; s=source* {
    let t = fuse_pptok (List.append (d::(proj m)::args) (r::s)) in
      Fun {t with v=(m, args, s)}
  }
| first=UNDEF; m=WORD {
    Undef {(fuse_pptok [first; proj m]) with v=m}
  }
| first=ERROR; stream=source* {
    Err {(fuse_pptok (first::(List.map proj stream))) with v=stream}
  }
| first=PRAGMA; stream=source* {
    Pragma {(fuse_pptok (first::(List.map proj stream))) with v=stream}
  }
| first=EXTENSION; ext=WORD; c=COLON; b=behavior {
    Extension {(fuse_pptok [first; proj ext; c; proj b]) with v=(ext, b)}
  }
| first=VERSION; v=INTCONSTANT { (* TODO: Check only decimal base *)
    Version {(fuse_pptok [first; proj v]) with v={v with v=snd v.v}}
  }
| first=LINE; l=INTCONSTANT { (* TODO: Check only decimal base *)
    Line {(fuse_pptok [first; proj l]) with v=(None, {l with v=snd l.v})}
  }
| first=LINE; l=INTCONSTANT; src=INTCONSTANT { (* TODO: Check only decimal base *)
    Line {(fuse_pptok [first; proj l; proj src])
	  with v=({src with v=snd src.v}, snd l.v)}
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
