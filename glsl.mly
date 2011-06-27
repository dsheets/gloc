%{
open Glsl_type

type qual = Const

type 't tok = { loc: int * int * int; v: 't }
module SymMap = Map.Make(struct type t = string let compare = compare end)

type 'a scalar = 'a
type 'a vec2   = 'a * 'a
type 'a vec3   = 'a * 'a * 'a
type 'a vec4   = 'a * 'a * 'a * 'a

type 'a prim =
    Void
  | Float of 'a cell
  | Int of 'a cell
  | Bool of 'a cell

type symtype =
    Bool of bool prim
  | Int of int prim
  | Float of float prim
  | Fun of arrow
  | Struct of agg
  | Field of agg * string
  | Array of int * symtype
and arrow = symtype * symtype array
and agg = symtype SymMap.t

type 'a symbol = {symTok:'a tok; symType:symtype; symQual:qual list}

type 'a expr = Variable of 'a var_expr | Constant of 'a const_expr

type global = Function of | Global | Directive

type translation_unit = {prog:global list; env:env}
%}

%token EOF

%token <string list tok> COMMENT
%token <(Glsl_type.env * string) tok> IDENTIFIER
%token <string tok> TYPE_NAME FIELD_SELECTION

%token <float tok> FLOATCONSTANT
%token <int tok> INTCONSTANT
%token <bool tok> BOOLCONSTANT

%token <unit tok> HIGH_PRECISION MEDIUM_PRECISION LOW_PRECISION
%token <unit tok> PRECISION INVARIANT
%token ATTRIBUTE CONST BOOL FLOAT INT BREAK CONTINUE DO ELSE FOR IF DISCARD
%token RETURN BVEC2 BVEC3 BVEC4 IVEC2 IVEC3 IVEC4 VEC2 VEC3 VEC4 MAT2 MAT3
%token MAT4 IN OUT INOUT UNIFORM VARYING STRUCT VOID WHILE SAMPLER2D
%token SAMPLERCUBE LEFT_OP RIGHT_OP INC_OP DEC_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP XOR_OP MUL_ASSIGN DIV_ASSIGN ADD_ASSIGN MOD_ASSIGN
%token LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN SUB_ASSIGN
%token LEFT_PAREN RIGHT_PAREN LEFT_BRACKET RIGHT_BRACKET LEFT_BRACE
%token RIGHT_BRACE DOT COMMA COLON EQUAL SEMICOLON BANG DASH TILDE PLUS
%token STAR SLASH PERCENT LEFT_ANGLE RIGHT_ANGLE VERTICAL_BAR CARET
%token AMPERSAND QUESTION

%type <Glsl_type.translation_unit> translation_unit

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
