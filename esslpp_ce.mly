%{
(* Copyright (c) 2011 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

  open Pp_lib
%}

%token EOF
%token <string Pp_lib.pptok> WORD
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

%type <Pp_lib.cond_expr> ce
%start ce

(* This is the unused ops precedence catch-all *)
%left XOR_OP XOR_ASSIGN SUB_ASSIGN RIGHT_ASSIGN OR_ASSIGN MUL_ASSIGN MOD_ASSIGN
%left LEFT_ASSIGN EQUAL DOT DIV_ASSIGN AND_ASSIGN ADD_ASSIGN SEMICOLON COMMA
%left QUESTION COLON

(* These are the real ops *)
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

%inline
unused_binop
: o=XOR_OP | o=XOR_ASSIGN | o=SUB_ASSIGN | o=RIGHT_ASSIGN | o=OR_ASSIGN
| o=MUL_ASSIGN | o=MOD_ASSIGN | o=LEFT_ASSIGN | o=EQUAL | o=DOT
| o=DIV_ASSIGN | o=AND_ASSIGN | o=ADD_ASSIGN | o=SEMICOLON | o=COMMA { o }

cond_unop
: l=LEFT_PAREN; e=cond_expr; r=RIGHT_PAREN {
  Group {(fuse_pptok [proj l;proj_cond_expr e;proj r]) with v=e}
}
| o=PLUS; e=cond_unop {
    Pos {(fuse_pptok [proj o; proj_cond_expr e]) with v=e}
  }
| o=DASH; e=cond_unop {
    Neg {(fuse_pptok [proj o; proj_cond_expr e]) with v=e}
  }
| o=TILDE; e=cond_unop {
    BitNot {(fuse_pptok [proj o; proj_cond_expr e]) with v=e}
  }
| o=BANG; e=cond_unop {
    Not {(fuse_pptok [proj o; proj_cond_expr e]) with v=e}
  }
| int=INTCONSTANT { Constant {int with v=snd int.v} }
| f=FLOATCONSTANT { error (FloatUnsupported f);
		    Constant {f with v=int_of_float f.v} }
| op=WORD; macro=WORD? {
    match macro with
      | Some operand -> if op.v="defined"
	then Defined {(fuse_pptok [proj op; proj operand]) with v=operand}
	else Opaque {(fuse_pptok [proj op; proj operand])
		     with v=[Word {op with v=(op.v,Env.empty)};
			     Word {operand with v=(operand.v,Env.empty)}]}
      | None -> Opaque {(fuse_pptok [proj op])
			with v=[Word {op with v=(op.v,Env.empty)}]}
  }
| op=WORD; l=LEFT_PAREN; macro=WORD; r=RIGHT_PAREN {
    if op.v="defined"
    then Defined {(fuse_pptok [proj op; proj macro]) with v=macro}
    else Opaque {(fuse_pptok [proj op; proj l; proj macro; proj r])
		 with v=[Word {op with v=(op.v,Env.empty)};
			 Leftp l;
			 Word {macro with v=(macro.v,Env.empty)};
			 Rightp r]}
  }
| WORD; o=LEFT_BRACE; cond_expr; RIGHT_BRACE
| WORD; o=LEFT_BRACKET; cond_expr; RIGHT_BRACKET
| WORD; o=INC_OP
| WORD; o=DEC_OP
| o=INC_OP; WORD
| o=DEC_OP; WORD {
    error (UnsupportedPPOp o);
    Opaque {o with v=[Punc o]}
  }

cond_expr (* TODO: check prec *)
: a=cond_expr; o=STAR; b=cond_expr {
  Mul {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
}
| a=cond_expr; o=SLASH; b=cond_expr {
    Div {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=PERCENT; b=cond_expr {
    Mod {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=PLUS; b=cond_expr {
    Add {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=DASH; b=cond_expr {
    Sub {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=LEFT_OP; b=cond_expr {
    BitLeft {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=RIGHT_OP; b=cond_expr {
    BitRight {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=LEFT_ANGLE; b=cond_expr {
    Lt {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=RIGHT_ANGLE; b=cond_expr {
    Gt {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=LE_OP; b=cond_expr {
    Lte {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=GE_OP; b=cond_expr {
    Gte {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=EQ_OP; b=cond_expr {
    Eq {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=NE_OP; b=cond_expr {
    Neq {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=AMPERSAND; b=cond_expr {
    BitAnd {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=CARET; b=cond_expr {
    BitXor {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=VERTICAL_BAR; b=cond_expr {
    BitOr {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=AND_OP; b=cond_expr {
    And {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=OR_OP; b=cond_expr {
    Or {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b]) with v=(a,b)}
  }
| a=cond_expr; o=unused_binop; b=cond_expr {
    error (UnsupportedPPOp o);
    Opaque {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b])
	    with v=[Punc o]}
  }
| a=cond_expr; o=QUESTION; b=cond_expr; e=COLON; c=cond_expr {
    error (UnsupportedPPOp o);
    Opaque {(fuse_pptok [proj_cond_expr a; proj o; proj_cond_expr b;
			 proj e; proj_cond_expr c]) with v=[Punc o]}
  }
| u=cond_unop { u }

ce
: c=cond_expr EOF { c }

%%
