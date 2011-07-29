%{
type 'a slval = Int of 'a * int
		| Float of 'a * float
		| Bool of 'a * bool

type slel = X | Y | Z | W
type slswizzle =
    Sub1 of slel
  | Sub2 of slel * slel
  | Sub3 of slel * slel * slel
  | Sub4 of slel * slel * slel * slel
type 'a slbind = { const: bool; name: string option; b: 'a }

type slprec = High | Medium | Low
type slprectype = Float | Int | Sampler2d | SamplerCube
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
type slsampler = [ `sampler2d of slprec | `samplerCube of slprec ]
type slprecable = [ slnum | slsampler ]
type 'a slparam = In of 'a | Out of 'a | Inout of 'a
type slvoid = [ `void ] (* Not a real type -- more like bottom/unit/falsity *)
type slstruct = [ `record of (string * sltype) list ]
and slarray = [ `array of slint slexpr * slnonarray ]
and slnumish = [ slprim | sldim ]
and sleq = [ slnumish | slstruct ]
and slnonarray = [ slsampler | sleq ]
and sltype = [ slarray | slnonarray ]
and slreturn = [ slvoid | sltype ]
and slfun = [ `lam of sltype slparam slbind list * slreturn ]
and sldecl = [ `custom of string * slstruct ]
and sluniv = [ sltype | slfun | sldecl ]
and 'b slexpr =
    Var of (string * slenv * 'b) pptok
  | Constant of 'b slval pptok
  | Construct of ('b * string * slenv * slnonarray slexpr list) pptok
  | Group of 'b slexpr pptok
  | Subscript of ('b * slarray slexpr * slint slexpr) pptok
  | App of ('b * string * slenv * sltype slexpr list) pptok
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
  | Seq of (slnonarray slexpr * 'b slexpr) pptok
and slstmt =
    Expr of sltype slexpr pptok
  | Select of (slbool slexpr pptok
	       * slstmt list pptok * slstmt list pptok) pptok
  | For of (slstmt * (slbool slexpr option
		      * sltype slexpr option) pptok * slstmt) pptok
  | While of (slstmt * slstmt) pptok
  | DoWhile of (slstmt list pptok * slbool slexpr) pptok
  | Return of (sltype slexpr option) pptok
  | Discard of unit pptok
  | Break of unit pptok
  | Continue of unit pptok
  | Scope of slstmt list pptok
  | Invariant of string list pptok
  | Attribute of (string * sltype) list pptok
  | Uniform of (string * sltype) list pptok
  | Varying of (bool * string * sltype) list pptok
  | Precdecl of slprecable pptok
  | Typedecl of (sldecl * slstruct slexpr option) slbind list pptok
  | Vardecl of (sltype * sltype slexpr option) slbind list pptok
  | Fundecl of (slfun slbind * slstmt list pptok option) pptok
and slenv = { ctxt : slstmt list slbind SymMap.t list; (* scope stack *)
	      opensyms : sluniv slbind list;
	      prec : slprec PrecMap.t list; (* precision stack *)
	      invariant : bool;
	      pragmas : string pptok pptok SymMap.t;
	      stmts : slstmt list }
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
%token <Punc.tok Pp_lib.pptok> INC_OP DEC_OP LE_OP GE_OP
%token <Punc.tok Pp_lib.pptok> EQ_OP NE_OP AND_OP OR_OP XOR_OP MUL_ASSIGN
%token <Punc.tok Pp_lib.pptok> DIV_ASSIGN ADD_ASSIGN
%token <Punc.tok Pp_lib.pptok> SUB_ASSIGN LEFT_PAREN RIGHT_PAREN
%token <Punc.tok Pp_lib.pptok> LEFT_BRACKET RIGHT_BRACKET LEFT_BRACE
%token <Punc.tok Pp_lib.pptok> RIGHT_BRACE DOT COMMA COLON EQUAL SEMICOLON
%token <Punc.tok Pp_lib.pptok> BANG DASH PLUS STAR SLASH
%token <Punc.tok Pp_lib.pptok> LEFT_ANGLE RIGHT_ANGLE
%token <Punc.tok Pp_lib.pptok> QUESTION

(* AMPERSAND AND_ASSIGN CARET LEFT_ASSIGN LEFT_OP MOD_ASSIGN OR_ASSIGN
 * PERCENT RIGHT_ASSIGN RIGHT_OP TILDE VERTICAL_BAR XOR_ASSIGN
 *)

%type <slenv> translation_unit

%start translation_unit

%%

variable_identifier
: i=IDENTIFIER { 
  Var { i with v = (i.v,lookup_type ctxt i.v)}
}
;
primary_expression
: v=variable_identifier { v }
| i=INTCONSTANT { Constant { i with v = Int (`int,i.v) } }
| f=FLOATCONSTANT { Constant { f with v = Float (`float,f.v) } }
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
	  Swizzle {t with v = (`vec2 `float, p, Sub1 (swizzle_of_int i))}
      | `mat3 _ ->
	  Swizzle {t with v = (`vec3 `float, p, Sub1 (swizzle_of_int i))}
      | `mat4 _ ->
	  Swizzle {t with v = (`vec4 `float, p, Sub1 (swizzle_of_int i))}
      end
  }
| f=function_call { f }
| p=postfix_expression; d=DOT; i=IDENTIFIER {
    let t = fuse_pptok [proj_slexpr p; proj d; proj i]
    in begin match typeof ctxt p with
      | `record (_,tl) ->
	  Field { t with v=(List.assoc i.v tl,p,i.v)} (* TODO: missing field *)
      | `vec2 t | `vec3 t | `vec4 t ->
	  let s = swizzle_of_string i.v in
	  let st = typeof_swizzle ctxt t s in
	    Swizzle { t with v=(st, p, s) } (* TODO: check widths *)
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
function_call
: f=function_call_generic { f }
| p=postfix_expression; d=DOT; f=function_call_generic {
    let t = fuse_pptok [proj_slexpr p; proj d; proj_slexpr f]
    in error (MethodsUnsupported t) (* TODO: dummy *)
}
;
function_call_generic
: i=IDENTIFIER; p=fuse_sep_list(LEFT_PAREN,assignment_expression,COMMA,RIGHT_PAREN) {
  let t = fuse_pptok [proj i; proj p]
  in begin match lookup_type ctxt i.v with
    | `lam (pl,rt) -> App {t with v=(rt,i.v,p.v)}
    | ctype -> Construct {t with v=(ctype,i.v,p.v)}
    end
}
| c=constructor; p=fuse_sep_list(LEFT_PAREN,assignment_expression,COMMA,RIGHT_PAREN) {
    Construct {(fuse_pptok [proj c; proj p]) with v=(fst c.v,snd c.v,p.v)}
  }
| i=IDENTIFIER; l=LEFT_PAREN; v=VOID; r=RIGHT_PAREN {
    let t = fuse_pptok [proj i; proj l; proj v; proj r]
    in begin match lookup_type ctxt i.v with
      | `lam (pl,rt) -> App {t with v=(rt,i.v,[])}
      | ctype -> Construct {t with v=(ctype,i.v,[])}
      end
  }
| c=constructor; l=LEFT_PAREN; v=VOID; r=RIGHT_PAREN {
    let t = fuse_pptok [proj c; proj l; proj v; proj r] in
      Construct { t with v=(fst c.v,snd c.v,[]) }
  }
;
constructor
: f=FLOAT { {f with v=(`float,"float")} }
| i=INT { {i with v=(`int,"int")} }
| b=BOOL { {b with v=(`bool,"bool")} }
| v=VEC2 { {v with v=(`vec2 `float,"vec2")} }
| v=VEC3 { {v with v=(`vec3 `float,"vec3")} }
| v=VEC4 { {v with v=(`vec4 `float,"vec4")} }
| v=BVEC2 { {v with v=(`vec2 `bool,"bvec2")} }
| v=BVEC3 { {v with v=(`vec3 `bool,"bvec3")} }
| v=BVEC4 { {v with v=(`vec4 `bool,"bvec4")} }
| v=IVEC2 { {v with v=(`vec2 `int,"ivec2")} }
| v=IVEC3 { {v with v=(`vec3 `int,"ivec3")} }
| v=IVEC4 { {v with v=(`vec4 `int,"ivec4")} }
| m=MAT2 { {m with v=(`mat2,"mat2")} }
| m=MAT3 { {m with v=(`mat3,"mat3")} }
| m=MAT4 { {m with v=(`mat4,"mat4")} }
;
unary_expression
: p=postfix_expression { p }
| i=INC_OP; u=unary_expression {
  PreInc {(fuse_pptok [proj i; proj_slexpr u]) with v=u}
  }
| d=DEC_OP; u=unary_expression {
  PreDec {(fuse_pptok [proj d; proj_slexpr u]) with v=u}
  }
| p=PLUS; u=unary_expression {
  Pos {(fuse_pptok [proj p; proj_slexpr u]) with v=u}
  }
| d=DASH; u=unary_expression {
  Neg {(fuse_pptok [proj d; proj_slexpr u]) with v=u}
  }
| b=BANG; u=unary_expression {
  Not {(fuse_pptok [proj b; proj_slexpr u]) with v=u}
  }
;
multiplicative_expression
: u=unary_expression { u }
| m=multiplicative_expression; s=STAR; u=unary_expression {
    Mul {(fuse_pptok [proj_slexpr m; proj s; proj_slexpr u]) with v=(m,u)}
  }
| m=multiplicative_expression; s=SLASH; u=unary_expression {
    Div {(fuse_pptok [proj_slexpr m; proj s; proj_slexpr u]) with v=(m,u)}
  }
;
additive_expression
: m=multiplicative_expression { m }
| a=additive_expression; p=PLUS; m=multiplicative_expression {
    Add {(fuse_pptok [proj_slexpr a; proj p; proj_slexpr m]) with v=(a,m)}
  }
| a=additive_expression; d=DASH; m=multiplicative_expression {
    Sub {(fuse_pptok [proj_slexpr a; proj d; proj_slexpr m]) with v=(a,m)}
  }
;
relational_expression
: a=additive_expression { a }
| r=relational_expression; l=LEFT_ANGLE; a=additive_expression {
    Lt {(fuse_pptok [proj_slexpr r; proj l; proj_slexpr a]) with v=(r,a)}
  }
| r=relational_expression; g=RIGHT_ANGLE; a=additive_expression {
    Gt {(fuse_pptok [proj_slexpr r; proj g; proj_slexpr a]) with v=(r,a)}
  }
| r=relational_expression; l=LE_OP; a=additive_expression {
    Lte {(fuse_pptok [proj_slexpr r; proj l; proj_slexpr a]) with v=(r,a)}
  }
| r=relational_expression; g=GE_OP; a=additive_expression {
    Gte {(fuse_pptok [proj_slexpr r; proj g; proj_slexpr a]) with v=(r,a)}
  }
;
equality_expression
: r=relational_expression { r }
| e=equality_expression; eq=EQ_OP; r=relational_expression {
  Eq {(fuse_pptok [proj_slexpr e; proj eq; proj_slexpr r])
      with v=(`bool,e,r)}
}
| e=equality_expression; ne=NE_OP; r=relational_expression {
  Neq {(fuse_pptok [proj_slexpr e; proj ne; proj_slexpr r])
       with v=(`bool,e,r)}
}
;
logical_and_expression
: e=equality_expression { e }
| l=logical_and_expression; a=AND_OP; e=equality_expression {
  And {(fuse_pptok [proj_slexpr l; proj a; proj_slexpr e])
       with v=(`bool,l,e)}
}
;
logical_xor_expression
: l=logical_and_expression { l }
| lx=logical_xor_expression; x=XOR_OP; la=logical_and_expression {
  Xor {(fuse_pptok [proj_slexpr lx; proj x; proj_slexpr la])
       with v=(`bool,lx,la)}
}
;
logical_or_expression
: l=logical_xor_expression { l }
| lo=logical_or_expression; o=OR_OP; lx=logical_xor_expression {
  Or {(fuse_pptok [proj_slexpr lo; proj o; proj_slexpr lx])
      with v=(`bool,lo,lx)}
}
;
conditional_expression
: l=logical_or_expression { l }
| o=logical_or_expression; q=QUESTION; e=expression;
c=COLON; a=assignment_expression {
  Sel {(fuse_pptok [proj_slexpr o; proj q; proj_slexpr e;
		    proj c; proj_slexpr a])
       with v=(c,e,a)}
}
;
assignment_expression
: c=conditional_expression { c }
| u=unary_expression; o=assignment_operator; a=assignment_expression {
  (* TODO: mutate *)
  let t = fuse_pptok [proj_slexpr u; proj o; proj_slexpr a] in
  begin match o.v with
    | EQUAL -> Set { t with v=(u,a) }
    | MUL_ASSIGN -> MulSet { t with v=(u,a) }
    | DIV_ASSIGN -> DivSet { t with v=(u,a) }
    | ADD_ASSIGN -> AddSet { t with v=(u,a) }
    | SUB_ASSIGN -> SubSet { t with v=(u,a) }
  end
}
;
assignment_operator
: o=EQUAL | o=MUL_ASSIGN | o=DIV_ASSIGN | o=ADD_ASSIGN | o=SUB_ASSIGN { o }
;
expression
  : a=assignment_expression { a }
| e=expression; c=COMMA; a=assignment_expression {
    Seq {(fuse_pptok [proj_slexpr e; proj c; proj_slexpr a]) with v=(e,a)}
  }
;
constant_expression
: c=conditional_expression { c } (* TODO: check const *)
;
declaration
: f=function_prototype; s=SEMICOLON {
  Fundecl {(fuse_pptok [proj f; proj s]) with v=f.v}
}
| t=type_specifier; s=SEMICOLON {
  Typedecl {(fuse_pptok [proj t; proj s]) with v=()}
}
| t=type_specifier; dl=fuse_sep_nonempty_list(declarator,COMMA); s=SEMICOLON {
    let v = List.map
      (fun d -> match d.v with
	 | n,None,ini ->
	     { const=false; name=Some n; b=(t.v,ini) }
	 | n,Some ie,ini ->
	     { const=false; name=Some n;
	       b=(`array (ie,t.v),ini) }
      ) dl
    in
      Vardecl {(fuse_pptok [proj t; proj dl; proj s]) with v}
  }
| c=CONST; t=type_specifier;
dl=fuse_sep_nonempty_list(declarator,COMMA); s=SEMICOLON {
  let v = List.map
    (fun d -> match d.v with
       | n,None,ini ->
	   { const=true; name=Some n; b=(t.v,ini) }
       | n,Some ie,ini ->
	   { const=true; name=Some n;
	     b=(`array (ie,t.v),ini) }
    ) dl
  in
    Vardecl {(fuse_pptok [proj c; proj t; proj dl; proj s]) with v}
}
| a=ATTRIBUTE; t=type_specifier;
dl=fuse_sep_nonempty_list(declarator,COMMA); s=SEMICOLON {
  let v = List.map
    (fun d -> match d.v with
      | n,None,None -> (n,t.v)
      | n,Some ie,None -> (n,`array (ie,t.v))
      | n,None,Some _ -> error (CannotInitializeAttribute (proj d));
	(n,t.v)
      | n,Some ie,Some _ -> error (CannotInitializeAttribute (proj d));
	(n,`array (ie,t.v))
    ) dl
  in
  Attribute {(fuse_pptok [proj a; proj t; proj dl; proj s]) with v}
}
| i=INVARIANT?; q=VARYING; t=type_specifier;
dl=fuse_sep_nonempty_list(declarator,COMMA); s=SEMICOLON {
  let inv,i = match i with Some t -> true,[t] | None -> false,[] in
  let v = List.map
    (fun d -> match d.v with
      | n,None,None -> (inv,n,t.v)
      | n,Some ie,None -> (inv,n,`array (ie,t.v))
      | n,None,Some _ -> error (CannotInitializeVarying (proj d));
	(inv,n,t.v)
      | n,Some ie,Some _ -> error (CannotInitializeVarying (proj d));
	(inv,n,`array (ie,t.v))
    ) dl
  in Varying {(fuse_pptok (i@[proj q; proj t; proj dl; proj s])) with v}
  }
| u=UNIFORM; t=type_specifier;
dl=fuse_sep_nonempty_list(declarator,COMMA); s=SEMICOLON {
  let v = List.map
    (fun d -> match d.v with
      | n,None,None -> (n,t.v)
      | n,Some ie,None -> (n,`array (ie,t.v))
      | n,None,Some _ -> error (CannotInitializeUniform (proj d));
	(n,t.v)
      | n,Some ie,Some _ -> error (CannotInitializeUniform (projd));
	(n,`array (ie,t.v))
    ) dl
  in Uniform {(fuse_pptok [proj u; proj t; proj dl; proj s]) with v}
  }
| inv=INVARIANT; dl=fuse_sep_nonempty_list(IDENTIFIER,COMMA); s=SEMICOLON {
  let v = List.map (fun d -> d.v) dl in
  Invariant {(fuse_pptok [proj inv; proj dl; proj s]) with v}
}
| p=PRECISION; t=precision_type; s=SEMICOLON {
   Precdecl {(fuse_pptok [proj p; proj t; proj s]) with v=t.v}
  }
(* TODO: catch bad precision type *)
;
declarator
: i=IDENTIFIER { { i with v=(i.v,None,None) } }
| i=IDENTIFIER; a=array_type_annot {
    {(fuse_pptok [proj i; proj a]) with v=(i.v,Some a.v,None) }
  }
| i=IDENTIFIER; e=EQUAL; ini=initializer_ {
    {(fuse_pptok [proj i; proj e; proj_slexpr ini]) with v=(i.v,None,Some ini)}
  }
;
array_type_annot
: l=LEFT_BRACKET; c=constant_expression; r=RIGHT_BRACKET {
  {(fuse_pptok [proj l; proj_slexpr c; proj r]) with v=c}
}
precision_type
  : pq=precision_qualifier; f=FLOAT {
    {(fuse_pptok [proj pq; proj f]) with v=`float pq.v}
  }
| pq=precision_qualifier; i=INT {
    {(fuse_pptok [proj pq; proj i]) with v=`int pq.v}
  }
| pq=precision_qualifier; s=SAMPLER2D {
    {(fuse_pptok [proj pq; proj s]) with v=`sampler2d pq.v}
  }
| pq=precision_qualifier; s=SAMPLERCUBE {
    {(fuse_pptok [proj pq; proj s]) with v=`samplerCube pq.v}
  }
function_prototype
: t=type_specifier; i=IDENTIFIER;
p=fuse_sep_list(LEFT_PAREN,param_declaration,COMMA,RIGHT_PAREN) {
  {(fuse_pptok [proj t; proj i; proj p])
   with v=({const=false; name=Some i.v;
	    b=`lam (p.v, t.v)
	   }, None)}
}
| t=type_specifier; i=IDENTIFIER; l=LEFT_PAREN; v=VOID; r=RIGHT_PAREN {
    {(fuse_pptok [proj t; proj i; proj l; proj v; proj r])
     with v=({const=false; name=Some i.v;
	      b=`lam ([],t.v)
	     }, None)}
  }
| v=VOID; i=IDENTIFIER;
p=fuse_sep_list(LEFT_PAREN,param_declaration,COMMA,RIGHT_PAREN) {
    {(fuse_pptok [proj v; proj i; proj p])
     with v=({const=false; name=Some i.v;
	      b=`lam (p.v,`slvoid)
	     }, None)}
  }
| n=VOID; i=IDENTIFIER; l=LEFT_PAREN; v=VOID; r=RIGHT_PAREN {
    {(fuse_pptok [proj v; proj i; proj l; proj v; proj r])
     with v=({const=false; name=Some i.v;
	      b=`lam ([],`slvoid)
	     }, None)}
  }
;
param_declarator
  : t=type_specifier; i=IDENTIFIER {
    {(fuse_pptok [proj t; proj i]) with v=(t.v,i.v)}
  }
| t=type_specifier; i=IDENTIFIER; a=array_type_annot {
    {(fuse_pptok [proj t; proj i; proj a]) with v=(`array (a.v,t.v),i.v)}
  }
;
param_declaration
: t=CONST?; d=param_declarator
| t=CONST?; d=param_type_specifier {
  let ct,const = match t with Some t -> [t],true | None -> [],false in
  {(fuse_pptok (ct@[proj d]))
   with v={const; name=(snd d.v); b=In (fst d.v)}}
  }
| t=CONST?; i=IN; d=param_declarator
| t=CONST?; i=IN; d=param_type_specifier {
  let ct,const = match t with Some t -> [t],true | None -> [],false in
  {(fuse_pptok (ct@[proj i; proj d]))
   with v={const; name=(snd d.v); b=In (fst d.v)}}
}
| t=CONST?; o=OUT; d=param_declarator
| t=CONST?; o=OUT; d=param_type_specifier {
  let ct,const = match t with Some t -> [t],true | None -> [],false in
  {(fuse_pptok (ct@[proj o; proj d]))
   with v={const; name=(snd d.v); b=Out (fst d.v)}}
}
| t=CONST?; io=INOUT; d=param_declarator
| t=CONST?; io=INOUT; d=param_type_specifier {
  let ct,const = match t with Some t -> [t],true | None -> [],false in
  {(fuse_pptok (ct@[proj io; proj d]))
   with v={const; name=(snd d.v); b=Inout (fst d.v)}}
  }
;
param_type_specifier
: t=type_specifier { { t with v=(t.v,None) } }
| t=type_specifier; a=array_type_annot {
    {(fuse_pptok [proj t; proj a]) with v=(`array (a.v,t.v),None)}
  }
;
type_specifier
  : p=precision_type { p }
| f=FLOAT { {f with v=`float (lookup_prec ctxt Float)} }
| i=INT { {i with v=`int (lookup_prec ctxt Int)} }
| b=BOOL { {b with v=`bool} }
| v=VEC2 { {v with v=`vec2 (`float (lookup_prec ctxt Float))} }
| v=VEC3 { {v with v=`vec3 (`float (lookup_prec ctxt Float))} }
| v=VEC4 { {v with v=`vec4 (`float (lookup_prec ctxt Float))} }
| v=BVEC2 { {v with v=`vec2 `bool} }
| v=BVEC3 { {v with v=`vec3 `bool} }
| v=BVEC4 { {v with v=`vec4 `bool} }
| v=IVEC2 { {v with v=`vec2 (`int (lookup_prec ctxt Int))} }
| v=IVEC3 { {v with v=`vec3 (`int (lookup_prec ctxt Int))} }
| v=IVEC4 { {v with v=`vec4 (`int (lookup_prec ctxt Int))} }
| m=MAT2 { {m with v=`mat2 (lookup_prec ctxt Float)} }
| m=MAT3 { {m with v=`mat3 (lookup_prec ctxt Float)} }
| m=MAT4 { {m with v=`mat4 (lookup_prec ctxt Float)} }
| s=SAMPLER2D { {s with v=`sampler2d (lookup_prec ctxt Sampler2d)} }
| s=SAMPLERCUBE { {s with v=`samplercube (lookup_prec ctxt SamplerCube)} }
| s=struct_specifier { s }
| i=IDENTIFIER { {i with v=lookup_type ctxt i.v} }
;
precision_qualifier
: h=HIGH_PRECISION { {h with v=High} }
| m=MEDIUM_PRECISION { {m with v=Medium} }
| l=LOW_PRECISION { {l with v=Low} }
;
struct_specifier
: s=STRUCT; i=IDENTIFIER?;
l=LEFT_BRACE; dl=list(struct_declaration); r=RIGHT_BRACE {
  match i with
    | None ->
      {(fuse_pptok [proj s; proj l; proj dl; proj r])
       with v=`record (List.flatten dl)}
    | Some i ->
      {(fuse_pptok [proj s; proj i; proj l proj dl; proj r])
       with v=`custom (i.v,List.flatten dl)}
}
;
struct_declaration
: t=type_specifier; dl=fuse_sep_nonempty_list(struct_declarator,COMMA); s=SEMICOLON {
  let v = List.map
    (fun d -> match d.v with
      | None,n -> (n,t.v)
      | Some ie,n -> (n,`array (ie,t.v))
    ) dl
  in {(fuse_pptok [proj t; proj dl; proj s]) with v}
}
;
struct_declarator
: i=IDENTIFIER { { i with v=(None,i.v) } }
| i=IDENTIFIER; a=array_type_annot {
    {(fuse_pptok [proj i; proj a]) with v=(Some a.v,i.v)}
  }
;
initializer_
: a=assignment_expression { a } (* TODO: ? *)
;
declaration_statement
: d=declaration { d } (* TODO: ? *)
;
statement_no_new_scope
: s=compound_statement | s=simple_statement { s }
;
simple_statement
: d=declaration_statement { d }
| e=expression_statement { e }
| s=selection_statement { s }
| i=iteration_statement { i }
| j=jump_statement { j }
;
compound_statement (* TODO: scope sooner? *)
: l=LEFT_BRACE; sl=list(statement_no_new_scope); r=RIGHT_BRACE {
  let env = push_new_env sl in
  Scope {env
	 with stmts={(fuse_pptok (proj l)::(List.map proj_slstmt sl)@[proj r])
                     with v=sl}}
}
;
statement
: c=compound_statement_no_new_scope { c }
| s=simple_statement { {(proj_slstmt s) with v=[s]} }
;
compound_statement_no_new_scope
: l=LEFT_BRACE; sl=list(statement_no_new_scope); r=RIGHT_BRACE {
  {(fuse_pptok (proj l)::(List.map proj_slstmt sl)@[proj r]) with v=sl}
}
;
expression_statement
: e=expression; s=SEMICOLON {
  Expr {(fuse_pptok [proj_slexpr e; proj s]) with v=e}
}
| s=SEMICOLON {
  Expr {s with v=Constant {s with v=Bool (`bool,false)}}
}
;
selection_statement
: i=IF; l=LEFT_PAREN; e=expression; r=RIGHT_PAREN; tb=statement {
  Select {(fuse_pptok [proj i; proj l; proj_slexpr e; proj r;
		       proj_slstmt tb])
          with v=(e, tb, [])}
}
| i=IF; l=LEFT_PAREN; bex=expression; r=RIGHT_PAREN;
tb=statement; e=ELSE; fb=statement {
  Select {(fuse_pptok [proj i; proj l; proj_slexpr bex; proj r;
		       proj_slstmt tb; proj e; proj_slstmt fb])
          with v=(bex, tb, fb)}
}
;
condition
: e=expression { Expr e } (* TODO: check bool *)
| t=type_specifier; i=IDENTIFIER; e=EQUAL; ini=initializer_ {
    Vardecl {(fuse_pptok [proj t; proj i; proj e; proj_slexpr ini])
             with v=[{const=false; name=Some i.v;
		      b=(t.v,Some ini)}]}
  }
;
iteration_statement
: w=WHILE; l=LEFT_PAREN; c=condition; r=RIGHT_PAREN; s=statement_no_new_scope {
  While {(fuse_pptok [proj w; proj l; proj_slstmt c; proj r; proj_slstmt s])
         with v=(c,s)}
}
| d=DO; st=statement; w=WHILE;
l=LEFT_PAREN; e=expression; r=RIGHT_PAREN; s=SEMICOLON {
  DoWhile {(fuse_pptok [proj d; proj st; proj w;
			proj l; proj_slexpr e; proj r; proj s])
           with v=(st,e)}
}
| f=FOR; l=LEFT_PAREN; i=for_init_statement; j=for_rest_statement; r=RIGHT_PAREN;
s=statement_no_new_scope {
  For {(fuse_pptok [proj f; proj l; proj_slstmt i; proj j; proj r;
		    proj_slstmt s])
       with v=(i,j,s)}
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
translation_unit (* TODO: expand *)
: dl=list(external_declaration); EOF { { ctxt; stmts=dl } }
;
external_declaration
: f=function_definition { f }
| d=declaration { d }
;
function_definition (* TODO: scopes? *)
: p=function_prototype; c=compound_statement_no_new_scope {
  let rt,al,b = p.v in
  let env = push_new_env c in (* TODO: sooner? *)
  Fundecl {(fuse_pptok [proj p; proj c])
           with v=(rt,al,Some env)}
}
;
%%
