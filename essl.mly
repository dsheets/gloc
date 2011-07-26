%{

type 'a slexpr =
    Variable of 'a slvar
  | Constant of 'a
  | Group of 'a slexpr
  | Subscript of 'a slarray * int slexpr
  | App of string * slexpr_type list
  | Field of slexpr_type * string
  | Swizzle of 'a slexpr (* TODO: type *)
  | PostInc of 'a slexpr (* closed *)
  | PostDec of 'a slexpr (* closed *)
  | PreInc of 'a slexpr (* closed *)
  | PreDec of 'a slexpr (* closed *)
  | Pos of 'a slexpr (* closed *)
  | Neg of 'a slexpr (* closed *)
  | Not of bool slexpr
  | Mul of 'a slexpr * 'a slexpr (* closed *)
  | Div of 'a slexpr * 'a slexpr (* closed *)
  | Add of 'a slexpr * 'a slexpr (* closed *)
  | Sub of 'a slexpr * 'a slexpr (* closed *)
  | Lt of slexpr_type * slexpr_type
  | Gt of slexpr_type * slexpr_type
  | Lte of slexpr_type * slexpr_type
  | Gte of slexpr_type * slexpr_type
  | Eq of slexpr_type * slexpr_type
  | Neq of slexpr_type * slexpr_type
  | And of bool slexpr * bool slexpr
  | Xor of bool slexpr * bool slexpr
  | Or of bool slexpr * bool slexpr
  | Sel of bool slexpr * 'a slexpr * 'a slexpr
  | Set of 'a slexpr * 'a slexpr (* closed *)
  | AddSet of 'a slexpr * 'a slexpr (* closed *)
  | SubSet of 'a slexpr * 'a slexpr (* closed *)
  | MulSet of 'a slexpr * 'a slexpr (* closed *)
  | DivSet of 'a slexpr * 'a slexpr (* closed *)
  | Seq of slexpr_type list * 'a slexpr

type slprec = High | Medium | Low
type slfloat = [ `float of slprec ]
type slint = [ `int of slprec ]
type slbool = [ `bool ]
type slprim = [ slfloat | slint | slbool ]
type sldim = [ slprim
	     | `vec2 of slprim
	     | `vec3 of slprim
	     | `vec4 of slprim
	     | `mat2 of slprec
	     | `mat3 of slprec
	     | `mat4 of slprec
	     ]
type slsampler = [ `sampler2d | `samplerCube ]
type slstruct = [ `record of string * (string * sltype) list ]
and slarray = [ `array of int * slnonarray ]
and slnonarray = [ sldim | slsampler | slstruct ]
and sltype = [ slarray | slnonarray ]
type 'a slparam = In of 'a | Out of 'a | Inout of 'a
type slfun = [ `lam of sltype slparam list * sltype option ]
type sluniv = [ sltype | slfun ]

type 'a slvec2 = 'a * 'a
type 'a slvec3 = 'a * 'a * 'a
type 'a slvec4 = 'a * 'a * 'a * 'a

type 'a slval = Int of 'a * int
		| Float of 'a * float
		| Bool of 'a * bool
		| Vec2 of 

type ('a,'b) slexpr =
    Var of string * 'b
  | Attribute of string * 'b
  | Uniform of string * 'b
  | Varying of bool * string * 'b
  | Constant of 'b slval

type slstmt =
    Assign of 

type sldecl =
    Function of slexpr_type list * slexpr_type
  | 

type translation_unit = {prog:global list; env:env}
%}

%token EOF

%token <string Pp_lib.pptok> IDENTIFIER
%token <string Pp_lib.pptok> TYPE_NAME FIELD_SELECTION

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

%type <Essl_lib.translation_unit> translation_unit

%start translation_unit

%%

variable_identifier
: IDENTIFIER { 
  try match SymMap.find (fst $1.v) (snd $1.v) with
    | {symType=Var; symQual} as symbol ->
      if List.mem Const symQual then Constant {symbol}
      else Variable {symbol}
    | symbol -> (error (UnexpectedNamespace ($1,Var));
		 Variable {symbol})
  with Not_found -> (error (UndeclaredIdentifier $1);
		     Variable {symbol={symType=Var; symQual=[]}})
}
;
primary_expression
  : variable_identifier { $1 }
  | INTCONSTANT {
    (if abs $1.v >= (1 lsl 16) (* TODO: VS only, FS is 10-bit *)
     then error (IntegerOverflow ($1,1 lsl 16)));
    Constant {symbol={symType=Int; symQual=[Const]}}
  }
  | FLOATCONSTANT {
    Constant {symbol={symType=Float; symQual=[Const]}}
  }
%%
