%{
type slprec = High | Medium | Low
type slfloat = [ `float of slprec ]
type slint = [ `int of slprec ]
type slnum = [ slfloat | slint ]
type slbool = [ `bool ]
type slprim = [ slnum | slbool ]
type sldim = [ `vec2 of slprim
	     | `vec3 of slprim
	     | `vec4 of slprim
	     | `mat2 of slprec
	     | `mat3 of slprec
	     | `mat4 of slprec
	     ]
type slsampler = [ `sampler2d | `samplerCube ]
type slstruct = [ `record of string * (string * sltype) list ]
and slarray = [ `array of int * slnonarray ]
and slnumish = [ slprim | sldim ]
and sleq = [ slnumish | slstruct ]
and slnonarray = [ slsampler | sleq ]
and sltype = [ slarray | slnonarray ]
type 'a slparam = In of 'a | Out of 'a | Inout of 'a
type slfun = [ `lam of sltype slparam list * sltype option ]
type sluniv = [ sltype | slfun ]

type 'a slval = Int of 'a * int
		| Float of 'a * float
		| Bool of 'a * bool

type slel = X | Y | Z | W
type slswizzle =
    Sub1 of slel
  | Sub2 of slel * slel
  | Sub3 of slel * slel * slel
  | Sub4 of slel * slel * slel * slel

type 'b slexpr =
    Var of (string * 'b) pptok
  | Attribute of (string * 'b) pptok
  | Uniform of (string * 'b) pptok
  | Varying of (bool * string * 'b) pptok
  | Constant of 'b slval pptok
  | Construct of ('b * string * slnonarray slexpr list) pptok
  | Group of 'b slexpr pptok
  | Subscript of ('b * slarray slexpr * slint slexpr) pptok
  | App of ('b * string * sltype slexpr list) pptok
  | Field of ('b * slstruct slexpr * string) pptok
  | Swizzle of ('b * sldim slexpr * slswizzle) pptok
  | PostInc of (slnumish as 'b) slexpr pptok
  | PostDec of (slnumish as 'b) slexpr pptok
  | PreInc of (slnumish as 'b) slexpr pptok
  | PreDec of (slnumish as 'b) slexpr pptok
  | Pos of (slnumish as 'b) slexpr pptok
  | Neg of (slnumish as 'b) slexpr pptok
  | Not of (slbool as 'b) slexpr pptok
  | Mul of ((slnumish as 'b) slexpr * (slnumish as 'b) slexpr) pptok
  | Div of ((slnumish as 'b) slexpr * (slnumish as 'b) slexpr) pptok
  | Add of ((slnumish as 'b) slexpr * (slnumish as 'b) slexpr) pptok
  | Sub of ((slnumish as 'b) slexpr * (slnumish as 'b) slexpr) pptok
  | Lt of ((slbool as 'b) * slnum slexpr * slnum slexpr) pptok
  | Gt of ((slbool as 'b) * slnum slexpr * slnum slexpr) pptok
  | Lte of ((slbool as 'b) * slnum slexpr * slnum slexpr) pptok
  | Gte of ((slbool as 'b) * slnum slexpr * slnum slexpr) pptok
  | Eq of ((slbool as 'b) * sleq slexpr * sleq slexpr) pptok
  | Neq of ((slbool as 'b) * sleq slexpr * sleq slexpr) pptok
  | And of ((slbool as 'b) * slbool slexpr * slbool slexpr) pptok
  | Xor of ((slbool as 'b) * slbool slexpr * slbool slexpr) pptok
  | Or of ((slbool as 'b) * slbool slexpr * slbool slexpr) pptok
  | Sel of (slbool slexpr * 'b slexpr * 'b slexpr) pptok
  | Set of ('b slexpr * 'b slexpr) pptok
  | AddSet of ((slnumish as 'b) slexpr * (slnumish as 'b) slexpr) pptok
  | SubSet of ((slnumish as 'b) slexpr * (slnumish as 'b) slexpr) pptok
  | MulSet of ((slnumish as 'b) slexpr * (slnumish as 'b) slexpr) pptok
  | DivSet of ((slnumish as 'b) slexpr * (slnumish as 'b) slexpr) pptok
  | Seq of (slnonarray slexpr list * 'b slexpr) pptok

type 'a slbind = { const: bool; v: 'a slexpr; name: string option }
type slstmt =
    Expr of sltype slexpr pptok
  | Cond of (slbool slexpr * slenv * slenv) pptok
  | For of (slstmt * (slbool slexpr option
		      * sltype slexpr option) pptok * slenv) pptok
  | While of (slstmt * slenv) pptok
  | DoWhile of (slstmt list * slbool slexpr) pptok
  | Return of (sltype slexpr option) pptok
  | Discard of unit pptok
  | Break of unit pptok
  | Continue of unit pptok
  | Precdecl of slnum pptok
  | Typedecl of slstruct slbind list pptok
  | Vardecl of sltype slbind list pptok
  | Fundecl of (slfun slbind * sltype slbind list * slenv option) pptok
and slenv = { ctxt : sluniv slbind SymMap.t; stmts : slstmt list }
%}

%token EOF

%token <string Pp_lib.pptok> IDENTIFIER

%token <float Pp_lib.pptok> FLOATCONSTANT
%token <int Pp_lib.pptok> INTCONSTANT
%token <bool Pp_lib.pptok> BOOLCONSTANT

%token <unit Pp_lib.pptok> HIGH_PRECISION MEDIUM_PRECISION LOW_PRECISION
%token <unit Pp_lib.pptok> PRECISION INVARIANT
%token <unit Pp_lib.pptok> ATTRIBUTE CONST BOOL FLOAT INT BREAK CONTINUE DO
%token <unit Pp_lib.pptok> ELSE FOR IF DISCARD
%token <unit Pp_lib.pptok> RETURN BVEC2 BVEC3 BVEC4 IVEC2 IVEC3 IVEC4 VEC2
%token <unit Pp_lib.pptok> VEC3 VEC4 MAT2 MAT3 MAT4 IN OUT INOUT UNIFORM
%token <unit Pp_lib.pptok> VARYING STRUCT VOID WHILE SAMPLER2D
%token <unit Pp_lib.pptok> SAMPLERCUBE
%token <Punc.tok Pp_lib.pptok> LEFT_OP RIGHT_OP INC_OP DEC_OP LE_OP GE_OP
%token <Punc.tok Pp_lib.pptok> EQ_OP NE_OP AND_OP OR_OP XOR_OP MUL_ASSIGN
%token <Punc.tok Pp_lib.pptok> DIV_ASSIGN ADD_ASSIGN MOD_ASSIGN
%token <Punc.tok Pp_lib.pptok> LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN XOR_ASSIGN
%token <Punc.tok Pp_lib.pptok> OR_ASSIGN SUB_ASSIGN LEFT_PAREN RIGHT_PAREN
%token <Punc.tok Pp_lib.pptok> LEFT_BRACKET RIGHT_BRACKET LEFT_BRACE
%token <Punc.tok Pp_lib.pptok> RIGHT_BRACE DOT COMMA COLON EQUAL SEMICOLON
%token <Punc.tok Pp_lib.pptok> BANG DASH TILDE PLUS STAR SLASH PERCENT
%token <Punc.tok Pp_lib.pptok> LEFT_ANGLE RIGHT_ANGLE VERTICAL_BAR CARET
%token <Punc.tok Pp_lib.pptok> AMPERSAND QUESTION

%type <slenv> translation_unit

%start translation_unit

%%

variable_identifier
: i=IDENTIFIER { 
  Var { i with v = (i.v,typeof ctxt i.v)}
}
;
primary_expression
: v=variable_identifier { v }
| i=INTCONSTANT { Constant { i with v = Int (`int (lookup_prec ctxt),i.v) } }
| f=FLOATCONSTANT { Constant { f with v = Float (`float (lookup_prec ctxt),f.v) } }
| b=BOOLCONSTANT { Constant { b with v = Bool (`bool,b.v) } }
| l=LEFT_PAREN; e=expression; r=RIGHT_PAREN {
    Group {(fuse_pptok [proj l; proj_slexpr e; proj r]) with v=e}
}
;
postfix_expression
: p=primary_expression { p }
| p=postfix_expression; l=LEFT_BRACKET; i=integer_expression; r=RIGHT_BRACKET {
    let t = fuse_pptok [proj_slexpr p; proj l; proj_slexpr i; proj r]
    in begin match typeof ctxt p with
      | `array (_,el) ->
	  Subscript {t with v = (el, p, i)}
      | `vec2 el | `vec3 el | `vec4 el ->
	  Swizzle {t with v = (el, p, Sub1 (swizzle_of_int i))}
      | `mat2 _ ->
	  Swizzle {t with v = (`vec2 `float (lookup_prec ctxt), p,
			       Sub1 (swizzle_of_int i))}
      | `mat3 _ ->
	  Swizzle {t with v = (`vec3 `float (lookup_prec ctxt), p,
			       Sub1 (swizzle_of_int i))}
      | `mat4 _ ->
	  Swizzle {t with v = (`vec4 `float (lookup_prec ctxt), p,
			       Sub1 (swizzle_of_int i))}
      end
  }
| f=function_call { f }
| p=postfix_expression; d=DOT; i=IDENTIFIER {
    let t = fuse_pptok [proj_slexpr p; proj d; proj i]
    in begin match typeof ctxt p with
      | `record (_,tl) ->
      | `vec2 t ->
      | `vec3 t ->
      | `vec4 t ->
      | `mat2 t ->
      | `mat3 t ->
      | `mat4 t ->
      end
  }
| p=postfix_expression; i=INC_OP {
    let t = fuse_pptok [proj_slexpr p; proj i]
    in PostInc {t with v=p}
  }
| p=postfix_expression; d=DEC_OP {
    let t = fuse_pptok [proj_slexpr p; proj d]
    in PostDec {t with v=p}
  }
;
integer_expression
: e=expression { e } (* TODO: check type is slint *)
;
(* TODO: rework function call productions *)
function_call
: f=function_call_generic { f }
| p=postfix_expression; d=DOT; f=function_call_generic {
    let t = fuse_pptok [proj_slexpr p; proj d; proj_slexpr f]
    in error (MethodsUnsupported t) (* TODO: dummy *)
}
;
function_call_generic
: f=function_call_header_with_parameters; r=RIGHT_PAREN {
  let t = fuse_pptok [proj_slexpr ] in g
}
| f=function_call_header_no_parameters; r=RIGHT_PAREN {
    
  }
;
function_call_header_no_parameters
: f=function_call_header; v=VOID {
  
}
| f=function_call_header {
    
  }
;
function_call_header_with_parameters
: f=function_call_header; a=assignment_expression {

}
| f=function_call_header_with_parameters; c=COMMA; a=assignment_expression {
    (* TODO: higher order *)
  }
;
function_call_header
: i=IDENTIFIER; l=LEFT_PAREN {

}
| c=constructor_identifier; l=LEFT_PAREN {
    
  }
;
constructor_identifier
: f=FLOAT {

}
| i=INT {

  }
| b=BOOL {

  }
| v=VEC2 {

  }
| v=VEC3 {

  }
| v=VEC4 {

  }
| v=BVEC2 {

  }
| v=BVEC3 {

  }
| v=BVEC4 {

  }
| v=IVEC2 {

  }
| v=IVEC3 {

  }
| v=IVEC4 {

  }
| m=MAT2 {

  }
| m=MAT3 {

  }
| m=MAT4 {

  }
;
unary_expression
: p=postfix_expression { p }
| i=INC_OP; u=unary_expression {

  }
| d=DEC_OP; u=unary_expression {

  }
| p=PLUS; u=unary_expression {

  }
| d=DASH; u=unary_expression {

  }
| b=BANG; u=unary_expression {

  }
;
multiplicative_expression
: u=unary_expression { u }
| m=multiplicative_expression; s=STAR; u=unary_expression {

  }
| m=multiplicative_expression; s=SLASH; u=unary_expression {

  }
;
additive_expression
: m=multiplicative_expression { m }
| a=additive_expression; p=PLUS; m=multiplicative_expression {

  }
| a=additive_expression; d=DASH; m=multiplicative_expression {

  }
;
relational_expression
: a=additive_expression { a }
| r=relational_expression; l=LEFT_ANGLE; a=additive_expression {

  }
| r=relational_expression; r=RIGHT_ANGLE; a=additive_expression {

  }
| r=relational_expression; l=LE_OP; a=additive_expression {

  }
| r=relational_expression; g=GE_OP; a=additive_expression {

  }
;
equality_expression
: r=relational_expression { r }
| e=equality_expression; eq=EQ_OP; r=relational_expression {

  }
| e=equality_expression; ne=NE_OP; r=relational_expression {

  }
;
logical_and_expression
: e=equality_expression { e }
| l=logical_and_expression; a=AND_OP; e=equality_expression {

  }
;
logical_xor_expression
: l=logical_and_expression { l }
| lx=logical_xor_expression; x=XOR_OP; la=logical_and_expression {

  }
;
logical_or_expression
: l=logical_xor_expression { l }
| lo=logical_or_expression; o=OR_OP; lx=logical_xor_expression {

  }
;
conditional_expression
: l=logical_op_expression { l }
| c=logical_or_expression; q=QUESTION; e=expression;
c=COLON; a=assignment_expression {

}
;
assignment_expression
: c=conditional_expression { c }
| u=unary_expression; o=assignment_operator; a=assignment_expression {

  }
;
assignment_operator
: e=EQUAL { }
| m=MUL_ASSIGN { }
| d=DIV_ASSIGN { }
| a=ADD_ASSIGN { }
| s=SUB_ASSIGN { }
;
expression
: a=assignment_expression { a }
| e=expression; c=COMMA; a=assignment_expression { }
;
constant_expression
: c=conditional_expression { c } (* TODO: check const *)
;
declaration
: f=function_prototype; s=SEMICOLON {

}
| i=init_declarator_list; s=SEMICOLON {

  }
| p=PRECISION; pq=precision_qualifier; t=type_specifier_no_prec; s=SEMICOLON {

  }
;
function_prototype
: t=fully_specified_type; i=IDENTIFIER;
l=LEFT_PAREN; param_decl_list; r=RIGHT_PAREN {

} t=fully_specific_type; i=IDENTIFIER; l=LEFT_PAREN; v=VOID; r=RIGHT_PAREN {

}
;
parameter_declarator
: t=type_specifier; i=IDENTIFIER {

}
| t=type_specifier; i=IDENTIFIER;
l=LEFT_BRACKET; c=constant_expression; r=RIGHT_BRACKET {

}
;
parameter_declaration
: t=type_qualifier?; q=parameter_qualifier?; d=parameter_declarator {

}
| t=type_qualifier?; q=parameter_qualifier?; s=parameter_type_specifier {

  }
;
parameter_qualifier
: i=IN { }
| o=OUT { }
| io=INOUT { }
;
parameter_type_specifier
: t=type_specifier { }
| t=type_specifier; l=LEFT_BRACKET; c=constant_expression; r=RIGHT_BRACKET {

  }
;
init_declarator_list
: s=single_declaration { }
| i=init_declarator_list; c=COMMA; i=IDENTIFIER { }
| i=init_declarator_list; c=COMMA; i=IDENTIFIER;
l=LEFT_BRACKET; c=constant_expression; r=RIGHT_BRACKET {

}
| idl=init_declarator_list; c=COMMA; i=IDENTIFIER; e=EQUAL; i=initializer_ {

  }
;
single_declaration
: t=fully_specified_type { }
| t=fully_specified_type; i=IDENTIFIER { }
| t=fully_specified_type; i=IDENTIFIER;
l=LEFT_BRACKET; c=constant_expression; r=RIGHT_BRACKET {

}
| t=fully_specified_type; i=IDENTIFIER; e=EQUAL; ini=initializer_ {

  }
| i=INVARIANT; id=IDENTIFIER {

  }
;
fully_specified_type (* TODO *)
: q=type_qualifier?; t=type_specifier {

}
;
type_qualifier (* TODO *)
: c=CONST { }
| a=ATTRIBUTE { }
| i=INVARIANT?; v=VARYING { }
| u=UNIFORM { }
;
type_specifier (* TODO *)
: p=precision_qualifier?; t=type_specifier_no_prec {

}
;
type_specifier_no_prec
: v=VOID { }
| f=FLOAT { }
| i=INT { }
| b=BOOL { }
| v=VEC2 { }
| v=VEC3 { }
| v=VEC4 { }
| v=BVEC2 { }
| v=BVEC3 { }
| v=BVEC4 { }
| v=IVEC2 { }
| v=IVEC3 { }
| v=IVEC4 { }
| m=MAT2 { }
| m=MAT3 { }
| m=MAT4 { }
| s=SAMPLER2D { }
| s=SAMPLERCUBE { }
| s=struct_specifier { }
| i=IDENTIFIER { }
;
precision_qualifier
: h=HIGH_PRECISION { High }
| m=MEDIUM_PRECISION { Medium }
| l=LOW_PRECISION { Low }
;
struct_specifier
: s=STRUCT; i=IDENTIFIER?;
l=LEFT_BRACE; dl=list(struct_declaration); r=RIGHT_BRACE {
  
}
;
struct_declaration
: t=type_specifier; dl=list(struct_declarator); s=SEMICOLON { (* TODO: COMMA *)

}
;
struct_declarator
: i=IDENTIFIER {

}
| i=IDENTIFIER; l=LEFT_BRACKET; c=constant_expression; r=RIGHT_BRACKET {

  }
;
initializer_
: a=assignment_expression { a } (* TODO: ? *)
;
declaration_statement
: d=declaration { d } (* TODO: ? *)
;
statement_no_new_scope
: c=compound_statement_with_scope { }
| s=simple_statement { }
;
simple_statement
: d=declaration_statement { }
| e=expression_statement { }
| s=selection_statement { }
| i=iteration_statement { }
| j=jump_statement { }
;
compound_statement_with_scope
: l=LEFT_BRACE; sl=list(statement_no_new_scope); r=RIGHT_BRACE {

}
;
statement_with_scope
: c=compound_statement_no_new_scope { }
| s=simple_statement { }
;
compound_statement_no_new_scope
: l=LEFT_BRACE; sl=list(statement_no_new_scope); r=RIGHT_BRACE {

}
;
expression_statement
: e=expression?; s=SEMICOLON {

}
;
selection_statement
: i=IF; l=LEFT_PAREN; e=expression; r=RIGHT_PAREN; r=selection_rest_statement {

}
;
selection_rest_statement
: tb=statement_with_scope; e=ELSE; fb=statement_with_scope {

}
| tb=statement_with_scope {

  }
;
condition
: e=expression { e }
| t=fully_specified_type; i=IDENTIFIER; e=EQUAL; ini=initializer_ {
    (* TODO *)
  }
;
iteration_statement (* TODO *)
: w=WHILE; l=LEFT_PAREN; c=condition; r=RIGHT_PAREN; s=statement_no_new_scope {

}
| d=DO; s=statement_with_scope; w=WHILE;
l=LEFT_PAREN; e=expression; r=RIGHT_PAREN; s=SEMICOLON {

}
| f=FOR; l=LEFT_PAREN; i=for_init_statement; r=for_rest_statement; r=RIGHT_PAREN;
s=statement_no_new_scope {

}
;
for_init_statement
: s=expression_statement | s=declaration_statement { s }
;
for_rest_statement
: c=condition; s=SEMICOLON; e=expression {
  {(fuse_pptok [proj_slexpr c; proj s; proj_slexpr e]) with v=(Some c, Some e)}
}
| c=condition; s=SEMICOLON {
    {(fuse_pptok [proj_slexpr c; proj s]) with v=(Some c, None)}
  }
| s=SEMICOLON; e=expression {
    {(fuse_pptok [proj s; proj_slexpr e]) with v=(None, Some e)}
  }
| s=SEMICOLON {
    { s with v=(None, None) }
  }
;
jump_statement
: c=CONTINUE; s=SEMICOLON { Continue (fuse_pptok [proj c; proj s]) }
| b=BREAK; s=SEMICOLON { Break (fuse_pptok [proj b; proj s]) }
| r=RETURN; e=expression?; s=SEMICOLON {
    Return {(fuse_pptok [proj r; proj_slexpr e; proj s]) with v=e}
  }
| d=DISCARD; s=SEMICOLON { Discard (fuse_pptok [proj d; proj s]) }
;
translation_unit
: dl=list(external_declaration) { { ctxt; stmts=dl } }
;
external_declaration
: f=function_definition { f }
| d=declaration { d }
;
function_definition (* TODO *)
: p=function_prototype; c=compound_statement_no_new_scope { }
;
%%
