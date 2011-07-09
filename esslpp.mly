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

%%
punc
: p=LEFT_OP | p=RIGHT_OP | p=INC_OP | p=DEC_OP | p=LE_OP | p=GE_OP | p=EQ_OP
| p=NE_OP | p=AND_OP | p=OR_OP | p=XOR_OP | p=MUL_ASSIGN | p=DIV_ASSIGN
| p=ADD_ASSIGN | p=MOD_ASSIGN | p=LEFT_ASSIGN | p=RIGHT_ASSIGN
| p=AND_ASSIGN | p=XOR_ASSIGN | p=OR_ASSIGN | p=SUB_ASSIGN
| p=LEFT_BRACKET | p=RIGHT_BRACKET | p=LEFT_BRACE | p=RIGHT_BRACE
| p=DOT | p=COLON | p=EQUAL | p=SEMICOLON | p=BANG | p=DASH
| p=TILDE | p=PLUS | p=STAR | p=SLASH | p=PERCENT
| p=LEFT_ANGLE | p=RIGHT_ANGLE | p=VERTICAL_BAR | p=CARET
| p=AMPERSAND | p=QUESTION { p }

source_ncnp
: w=WORD { Word w }
| f=FLOATCONSTANT { Float f }
| i=INTCONSTANT { Int i }
| p=punc { Punc p }

source_np
: c=COMMA { Comma c }
| s=source_ncnp { s }
    
source
: s=source_np { s }
| l=LEFT_PAREN { Leftp l }
| r=RIGHT_PAREN { Rightp r }

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
| first=ELIF; s=source+; last=ENDPPDIRECTIVE; b=body; f=cond_continue {
    let proj_s = List.map proj_pptok_type s in
    let t = fuse_pptok ((first::proj_s)
			@(last::(proj_pptok_expr b)::(proj f)::[])) in
      {t with v=Some
	  (If {t with v=(Opaque {(fuse_pptok proj_s) with v=s}, b, f.v)})}
  }
| d=ELSE; ENDPPDIRECTIVE; b=body; e=ENDIF {
    {(fuse_pptok [d; proj_pptok_expr b; e]) with v=Some b}
  }

directive
: first=IF; s=source+; last=ENDPPDIRECTIVE; b=body; f=cond_continue {
  let proj_s = List.map proj_pptok_type s in
    If {(fuse_pptok ((first::proj_s)@(last::(proj_pptok_expr b)::(proj f)::[])))
	with v=(Opaque {(fuse_pptok proj_s) with v=s},b,f.v)}
}
| first=IFDEF; m=WORD; last=ENDPPDIRECTIVE; b=body; f=cond_continue {
    If {(fuse_pptok [first; proj m; last; proj_pptok_expr b; proj f])
	with v=(Defined {m with v=m},b,f.v)}
  }
| first=IFNDEF; m=WORD; last=ENDPPDIRECTIVE; b=body; f=cond_continue {
    If {(fuse_pptok [first; proj m; last; proj_pptok_expr b; proj f])
	with v=(Not {m with v=Defined {m with v=m}},b,f.v)}
  }
| first=DEFINE; m=WORD; stream=source* {
    Def {(fuse_pptok (first::(proj m)::(List.map proj_pptok_type stream)))
	  with v=(m, stream)}
  }
| d=DEFINE; m=CALL; args=separated_list(COMMA,WORD); r=RIGHT_PAREN; s=source* {
  let t = fuse_pptok (List.append (d::(proj m)
				   ::(List.map proj args))
			((proj r)::(List.map proj_pptok_type s))) in
  Fun {t with v=(m, args, s)}
  }
| first=UNDEF; m=WORD {
    Undef {(fuse_pptok [first; proj m]) with v=m}
  }
| first=ERROR; stream=source* {
    Err {(fuse_pptok (first::(List.map proj_pptok_type stream)))
    with v=stream}
  }
| first=PRAGMA; stream=source* {
  Pragma {(fuse_pptok (first::(List.map proj_pptok_type stream)))
  with v=stream}
  }
| first=EXTENSION; ext=WORD; c=COLON; b=behavior {
    Extension {(fuse_pptok [first; proj ext; proj c; proj b]) with v=(ext, b)}
  }
| first=VERSION; v=INTCONSTANT { (* TODO: Check only decimal base *)
    Version {(fuse_pptok [first; proj v]) with v={v with v=snd v.v}}
  }
| first=LINE; l=INTCONSTANT { (* TODO: Check only decimal base *)
    Line {(fuse_pptok [first; proj l]) with v=(None, {l with v=snd l.v})}
  }
| first=LINE; l=INTCONSTANT; src=INTCONSTANT {
    (* TODO: Check only decimal base *)
    Line {(fuse_pptok [first; proj l; proj src])
	  with v=(Some {src with v=snd src.v}, {l with v=snd l.v})}
  }

ppdir
: dir=directive; last=ENDPPDIRECTIVE; rest=body? {
  match rest with None -> dir
    | Some expr -> fuse_pptok_expr [dir; expr]
}
| last=ENDPPDIRECTIVE; rest=body? {
  match rest with
    | None -> Chunk {(fuse_pptok [last]) with v=[]}
    | Some expr -> expr
}

chunk
: s=source r=source* {
  Chunk {(fuse_pptok (List.map proj_pptok_type (s::r))) with v=s::r}
}

body
: pp=ppdir { pp }
| chunk=chunk; pp=ppdir? {
  match pp with None -> chunk
    | Some expr -> fuse_pptok_expr [chunk; expr]
}

translation_unit
: top=BOF?; body=body?; bot=EOF {
  let cexpr cs = Comments {(fuse_pptok cs) with v=cs} in
  match top, body, fst bot.comments with
    | None,None,[] -> Chunk {bot with v=[]}
    | None,Some body,[] -> body
    | None,None,cs -> cexpr cs
    | None,Some body,cs -> fuse_pptok_expr [body; cexpr cs]
    | Some t,None,[] -> cexpr !(snd t.comments)
    | Some t,Some body,[] ->
      fuse_pptok_expr [cexpr !(snd t.comments); body]
    | Some t,None,cs ->
      fuse_pptok_expr [cexpr !(snd t.comments); cexpr cs]
    | Some t,Some body,cs ->
      fuse_pptok_expr [cexpr !(snd t.comments); body; cexpr cs]
}
%%
