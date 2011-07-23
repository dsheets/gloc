%{
(* Copyright (c) 2011 David Sheets, Ashima Arts.
 * All rights reserved.
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

  open Pp_lib
%}

%token <unit Pp_lib.pptok> BOF EOF ENDPPDIRECTIVE
%token <unit Pp_lib.pptok> EXTENSION VERSION LINE
%token <unit Pp_lib.pptok> DEFINE UNDEF IF IFDEF IFNDEF ELSE ELIF ENDIF ERROR
%token <unit Pp_lib.pptok> PRAGMA
%token <string Pp_lib.pptok> WORD CALL
%token <float Pp_lib.pptok> FLOATCONSTANT
%token <(Pp_lib.base * int) Pp_lib.pptok> INTCONSTANT

%token <Punc.tok Pp_lib.pptok> LEFT_OP RIGHT_OP INC_OP DEC_OP LE_OP GE_OP EQ_OP
%token <Punc.tok Pp_lib.pptok> NE_OP AND_OP OR_OP XOR_OP MUL_ASSIGN DIV_ASSIGN
%token <Punc.tok Pp_lib.pptok> ADD_ASSIGN MOD_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN
%token <Punc.tok Pp_lib.pptok> AND_ASSIGN XOR_ASSIGN OR_ASSIGN SUB_ASSIGN
%token <Punc.tok Pp_lib.pptok> LEFT_PAREN RIGHT_PAREN LEFT_BRACKET RIGHT_BRACKET
%token <Punc.tok Pp_lib.pptok> LEFT_BRACE RIGHT_BRACE DOT COMMA COLON EQUAL
%token <Punc.tok Pp_lib.pptok> SEMICOLON BANG DASH TILDE PLUS STAR SLASH PERCENT
%token <Punc.tok Pp_lib.pptok> LEFT_ANGLE RIGHT_ANGLE VERTICAL_BAR CARET
%token <Punc.tok Pp_lib.pptok> AMPERSAND QUESTION

%type <Pp_lib.pptok_expr> translation_unit

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
: w=WORD { Word { w with v=(w.v,Env.empty)} }
| f=FLOATCONSTANT { Float f }
| i=INTCONSTANT { Int i }
| p=punc { Punc p }

source_np
: c=COMMA { Comma c }
| s=source_ncnp { s }
    
source
: s=source_np { s }
| c=CALL { Call c }
| l=LEFT_PAREN { Leftp l }
| r=RIGHT_PAREN { Rightp r }

not_int
: WORD | punc | FLOATCONSTANT | COMMA | CALL | LEFT_PAREN | RIGHT_PAREN { () }

behavior
: b=WORD { match b.v with
  | "require" -> {b with v=Require}
  | "enable" -> {b with v=Enable}
  | "warn" -> {b with v=Warn}
  | "disable" -> {b with v=Disable}
  | _ -> error (UnknownBehavior b); {b with v=Disable}
}

cond_continue
: d=ENDIF { {d with v=None} }
| first=ELIF; s=source+; last=ENDPPDIRECTIVE; b=body*; f=cond_continue {
    let proj_s = List.map proj_pptok_type s in
    let b = pptok_expr_of_body b last in
    let t = fuse_pptok ((first::proj_s)
			@(last::(proj_pptok_expr b)::(proj f)::[])) in
      {t with v=Some
	  (If {t with v=(Opaque {(fuse_pptok proj_s) with v=s}, b, f.v)})}
  }
| first=ELIF; s=source+; last=ENDPPDIRECTIVE; b=body*; error {
    let proj_s = List.map proj_pptok_type s in
    let b = pptok_expr_of_body b last in
    let t = fuse_pptok ((first::proj_s)@(last::(proj_pptok_expr b)::[])) in
      error (UnterminatedConditional first);
      {t with v=Some
	  (If {t with v=(Opaque {(fuse_pptok proj_s) with v=s}, b, None)})}
  }
| d=ELSE; ENDPPDIRECTIVE; b=body*; e=ENDIF {
  let b = pptok_expr_of_body b e in
  {(fuse_pptok [d; proj_pptok_expr b; e]) with v=Some b}
}

arg : a=WORD; c=COMMA? { (a,c) }

directive
  : first=IF; s=source+; last=ENDPPDIRECTIVE; b=body*; f=cond_continue {
    let proj_s = List.map proj_pptok_type s in
    let b = pptok_expr_of_body b last in
      If {(fuse_pptok ((first::proj_s)@(last::(proj_pptok_expr b)::(proj f)::[])))
	  with v=(Opaque {(fuse_pptok proj_s) with v=s},b,f.v)}
  }
| first=IF; s=source+; last=ENDPPDIRECTIVE; b=body*; error {
    let proj_s = List.map proj_pptok_type s in
    let b = pptok_expr_of_body b last in
      error (UnterminatedConditional first);
      If {(fuse_pptok ((first::proj_s)@(last::(proj_pptok_expr b)::[])))
	  with v=(Opaque {(fuse_pptok proj_s) with v=s},b,None)}
  }
| first=IFDEF; m=WORD; last=ENDPPDIRECTIVE; b=body*; f=cond_continue {
    let b = pptok_expr_of_body b last in
      If {(fuse_pptok [first; proj m; last; proj_pptok_expr b; proj f])
	  with v=(Defined {m with v=m},b,f.v)}
  }
| first=IFDEF; m=WORD; last=ENDPPDIRECTIVE; b=body*; error {
    let b = pptok_expr_of_body b last in
      error (UnterminatedConditional first);
      If {(fuse_pptok [first; proj m; last; proj_pptok_expr b])
	  with v=(Defined {m with v=m},b,None)}
  }
| first=IFNDEF; m=WORD; last=ENDPPDIRECTIVE; b=body*; f=cond_continue {
    let b = pptok_expr_of_body b last in
      If {(fuse_pptok [first; proj m; last; proj_pptok_expr b; proj f])
	  with v=(Not {m with v=Defined {m with v=m}},b,f.v)}
  }
| first=IFNDEF; m=WORD; last=ENDPPDIRECTIVE; b=body*; error {
    let b = pptok_expr_of_body b last in
      error (UnterminatedConditional first);
      If {(fuse_pptok [first; proj m; last; proj_pptok_expr b])
	  with v=(Not {m with v=Defined {m with v=m}},b,None)}
  }
| first=DEFINE; m=WORD; stream=source* {
    Def {(fuse_pptok (first::(proj m)::(List.map proj_pptok_type stream)))
	 with v=(m, stream)}
  }
| d=DEFINE; m=CALL; args=list(arg); r=RIGHT_PAREN; s=source* {
  let t = fuse_pptok ((d::(proj m)
		       ::(List.flatten
			    (List.map
			       (function
				 | a,None -> [proj a]
				 | a,Some c -> [proj a; proj c] 
			       ) args)))
		      @((proj r)::(List.map proj_pptok_type s))) in
  Fun {t with v=(m, (List.map fst args), s)}
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
| first=VERSION; v=INTCONSTANT {
    check_version_base v;
    Version {(fuse_pptok [first; proj v]) with v={v with v=snd v.v}}
  }
| first=VERSION; error {
    error (InvalidVersionArg first);
    Version {first with v={first with v=100}}
  }
(* TODO: support macro expansion in line directives as per spec
 * This requires performing streaming macro definition and expansion
 * as well as streaming conditional evaluation. It is an error to
 * put a line directive in an open conditional or to use an open or
 * open conditional macro in a line directive.
 *)
| first=LINE; l=INTCONSTANT {
    check_line_base l;
    line := {!line with src=snd l.v};
    Line {(fuse_pptok
	     ~zloc:{line={src=(!line).src-1; input=(!line).input-1};
		    file=(!file); col=l.span.z.col}
	     [first; proj l])
	  with v=(None, {l with v=snd l.v})}
  }
| first=LINE; l=INTCONSTANT; src=INTCONSTANT {
    check_line_base l;
    check_line_base src;
    line := {!line with src=snd l.v};
    file := {!file with src=snd src.v};
    Line {(fuse_pptok
	     ~zloc:{line={src=(!line).src-1; input=(!line).input-1};
		    file=(!file); col=src.span.z.col}
	     [first; proj l; proj src])
	  with v=(Some {src with v=snd src.v}, {l with v=snd l.v})}
  }
| first=LINE; not_int; source* {
    error (InvalidLineArg first);
    Line {first with v=(None, {first with v=(!line).src})}
  }

body
: dir=directive; ENDPPDIRECTIVE | dir=directive; error { dir }
| last=ENDPPDIRECTIVE { Chunk {(fuse_pptok [last]) with v=[]} }
| s=source {
    Chunk {(proj_pptok_type s) with v=[s]}
  }

translation_unit
: bot=EOF {
  match fst bot.comments with
    | [] -> pptok_expr_of_body [] bot
    | cs -> cexpr cs
}
| body=body+; bot=EOF {
  let b = pptok_expr_of_body body bot in
  match fst bot.comments with
    | [] -> b
    | cs -> fuse_pptok_expr [b; cexpr cs]
}
| top=BOF; bot=EOF {
  match !(snd top.comments), fst bot.comments with
    | [], [] -> pptok_expr_of_body [] bot
    | [], cs -> cexpr cs
    | cs, [] -> cexpr (List.rev cs)
    | ts, bs -> fuse_pptok_expr [cexpr (List.rev ts); cexpr bs]
}
| top=BOF; body=body+; bot=EOF {
  let b = pptok_expr_of_body body bot in
  match !(snd top.comments), fst bot.comments with
    | [], [] -> b
    | [], cs -> fuse_pptok_expr [b; cexpr cs]
    | cs, [] -> fuse_pptok_expr [cexpr (List.rev cs); b]
    | ts, bs -> fuse_pptok_expr [cexpr (List.rev ts); b; cexpr bs]
}
%%
