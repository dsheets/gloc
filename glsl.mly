%{
open Glsl_type

type 't tok = { loc: int * int * int; v: 't }
module SymMap = Map.Make(struct type t = symbol let compare = compare end)
%}

%token EOF

%token <string list tok> COMMENT
%token <(Glsl_type.env * string) tok> IDENTIFIER
%token <string tok> TYPE_NAME FIELD_SELECTION

%token <float tok> FLOATCONSTANT
%token <int tok> INTCONSTANT
%token <bool tok> BOOLCONSTANT

%token <unit tok> INVARIANT HIGH_PRECISION MEDIUM_PRECISION LOW_PRECISION PRECISION
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
  try match SymMap.find (fst $1) (snd $1) with
    | Var v ->
    | _ -> raise (VariableExpected
  with Not_found -> raise (UndeclaredIdentifier (snd $1))


%%
