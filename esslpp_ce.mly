%{

%}

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

cond_unop
: l=LEFT_PAREN; e=cond_expr; r=RIGHT_PAREN {
  Group {(fuse_pptok [proj l;proj e;proj r]) with v=e}
}
| o=PLUS; e=cond_unop { Pos {(fuse_pptok [proj o; proj e]) with v=e} }
| o=DASH; e=cond_unop { Neg {(fuse_pptok [proj o; proj e]) with v=e} }
| o=TILDE; e=cond_unop { BitNot {(fuse_pptok [proj o; proj e]) with v=e} }
| o=BANG; e=cond_unop { Not {(fuse_pptok [proj o; proj e]) with v=e} }
| int=INTCONSTANT { Constant {int with v=snd int.v} }
| op=WORD macro=WORD {
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
