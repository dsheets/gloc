%{
(* Copyright (c) 2012 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

open Pp_lib
open Sl_lib
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

(*%type <Sl_lib.sltype Sl_lib.slexpr> assignment_expression*)

%nonassoc LESS_THAN_ELSE
%nonassoc ELSE

%type <Sl_lib.slenv> translation_unit

%start translation_unit

%%

variable_identifier
: i=IDENTIFIER {
  let t = lookup_type ctxt i.v in
    Var { i with v = (i.v,!ctxt,t)}
}
;
primary_expression
: v=variable_identifier { v }
| i=INTCONSTANT { Constant { i with v = I (`int (lookup_prec ctxt Int),i.v) } }
| b=BOOLCONSTANT { Constant { b with v = B (`bool,b.v) } }
| f=FLOATCONSTANT {
    Constant { f with v = F (`float (lookup_prec ctxt Float),f.v) }
}
| l=LEFT_PAREN; e=expression; r=RIGHT_PAREN {
    Group {(fuse_pptok [proj l; proj_slexpr e; proj r]) with v=e}
}
;
postfix_expression
: p=primary_expression { p }
| p=postfix_expression; l=LEFT_BRACKET; i=integer_expression; r=RIGHT_BRACKET {
    let t = fuse_pptok [proj_slexpr p; proj l; proj_slexpr i; proj r]
    in begin match typeof p with
      | `array (_,el) ->
        Subscript {t with v = (el, p, i)}
      | `vec2 el | `vec3 el | `vec4 el ->
        Swizzle {t with v = (inj_prim el, p, Sub1 X)} (*(swizzle_of_int i))}*)
      | `mat2 prec ->
        Swizzle {t with v = (`vec2 (`float prec), p,
                             Sub1 X)} (*(swizzle_of_int i))}*)
      | `mat3 prec ->
        Swizzle {t with v = (`vec3 (`float prec), p,
                             Sub1 X)} (*(swizzle_of_int i))}*)
      | `mat4 prec ->
        Swizzle {t with v = (`vec4 (`float prec), p,
                             Sub1 X)} (*(swizzle_of_int i))}*)
      | `univ -> Subscript {t with v = (`univ,p,i)}
      | v -> error (BadSubscript {(proj_slexpr p) with v});
             (* TODO: enforce subscriptability *)
             Swizzle {t with v=(`univ,p,Sub1 X)}
      end
  }
| f=function_call { f }
| p=postfix_expression; d=DOT; i=IDENTIFIER {
    let t = fuse_pptok [proj_slexpr p; proj d; proj i]
    in begin match typeof p with
      | `record (name, fs) ->
          Field { t with v=(List.assoc i.v fs,p,i.v)} (* TODO: missing field *)
      | `vec2 elt | `vec3 elt | `vec4 elt ->
          let s = swizzle_of_identifier i in
          let st = typeof_swizzle elt s in
            Swizzle { t with v=(st, p, s) } (* TODO: check widths *)
      | `univ -> Field { t with v=(`univ,p,i.v)}
      | v -> error (BadField {(proj_slexpr p) with v});
             (* TODO: enforce operand slstruct *)
             Field { t with v=(`univ,p,i.v)}
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
(*| p=postfix_expression; d=DOT; f=function_call_generic {
    let t = fuse_pptok [proj_slexpr p; proj d; proj_slexpr f]
    in error (MethodsUnsupported t) (* TODO: dummy *)
}*)
;
function_call_generic
: i=IDENTIFIER;
p=fuse_sep_slexpr_list(LEFT_PAREN,assignment_expression,COMMA,RIGHT_PAREN) {
  let t = fuse_pptok [proj i; proj p]
  in begin match lookup_type ctxt i.v with
    | `lam (pl,rt) ->
      App {t with v=(`univ (* TODO: rt *),i.v,!ctxt,p.v)}
    | ctype ->
      Construct {t with v=(`univ (* TODO: ctype *),i.v,!ctxt,p.v)}
    end
}
| c=constructor;
p=fuse_sep_slexpr_list(LEFT_PAREN,assignment_expression,COMMA,RIGHT_PAREN) {
    Construct {(fuse_pptok [proj c; proj p])
               with v=(fst c.v,snd c.v,!ctxt,p.v)}
  }
| i=IDENTIFIER; l=LEFT_PAREN; v=VOID; r=RIGHT_PAREN {
    let t = fuse_pptok [proj i; proj l; proj v; proj r]
    in begin match lookup_type ctxt i.v with
      | `lam (pl,rt) -> App {t with v=(`univ (* TODO: rt *),i.v,!ctxt,[])}
      | ctype -> Construct {t with v=(`univ (* TODO: ctype*),i.v,!ctxt,[])}
      end
  }
| c=constructor; l=LEFT_PAREN; v=VOID; r=RIGHT_PAREN {
    let t = fuse_pptok [proj c; proj l; proj v; proj r] in
      Construct { t with v=(fst c.v,snd c.v,!ctxt,[]) }
  }
;
constructor (* TODO: correct precision tracking (p.34 1.0r17) *)
: f=FLOAT { {f with v=(`float (lookup_prec ctxt Float),"float")} }
| i=INT { {i with v=(`int (lookup_prec ctxt Int),"int")} }
| b=BOOL { {b with v=(`bool,"bool")} }
| v=VEC2 { {v with v=(`vec2 (`float (lookup_prec ctxt Float)),"vec2")} }
| v=VEC3 { {v with v=(`vec3 (`float (lookup_prec ctxt Float)),"vec3")} }
| v=VEC4 { {v with v=(`vec4 (`float (lookup_prec ctxt Float)),"vec4")} }
| v=BVEC2 { {v with v=(`vec2 `bool,"bvec2")} }
| v=BVEC3 { {v with v=(`vec3 `bool,"bvec3")} }
| v=BVEC4 { {v with v=(`vec4 `bool,"bvec4")} }
| v=IVEC2 { {v with v=(`vec2 (`int (lookup_prec ctxt Int)),"ivec2")} }
| v=IVEC3 { {v with v=(`vec3 (`int (lookup_prec ctxt Int)),"ivec3")} }
| v=IVEC4 { {v with v=(`vec4 (`int (lookup_prec ctxt Int)),"ivec4")} }
| m=MAT2 { {m with v=(`mat2 (lookup_prec ctxt Float),"mat2")} }
| m=MAT3 { {m with v=(`mat3 (lookup_prec ctxt Float),"mat3")} }
| m=MAT4 { {m with v=(`mat4 (lookup_prec ctxt Float),"mat4")} }
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
| lh=additive_expression; l=LEFT_ANGLE; rh=additive_expression {
    Lt {(fuse_pptok [proj_slexpr lh; proj l; proj_slexpr rh])
        with v=(`bool,lh,rh)}
  }
| lh=additive_expression; g=RIGHT_ANGLE; rh=additive_expression {
    Gt {(fuse_pptok [proj_slexpr lh; proj g; proj_slexpr rh])
        with v=(`bool,lh,rh)}
  }
| lh=additive_expression; l=LE_OP; rh=additive_expression {
    Lte {(fuse_pptok [proj_slexpr lh; proj l; proj_slexpr rh])
         with v=(`bool,lh,rh)}
  }
| lh=additive_expression; g=GE_OP; rh=additive_expression {
    Gte {(fuse_pptok [proj_slexpr lh; proj g; proj_slexpr rh])
         with v=(`bool,lh,rh)}
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
       with v=(o,e,a)}
}
;
assignment_expression
: c=conditional_expression { c }
| u=unary_expression; o=assignment_operator; a=assignment_expression {
  (* TODO: mutate *)
  let t = fuse_pptok [proj_slexpr u; proj o; proj_slexpr a] in
  begin match o.v with
    | Punc.EQUAL -> Set { t with v=(u,a) }
    | Punc.MUL_ASSIGN -> MulSet { t with v=(u,a) }
    | Punc.DIV_ASSIGN -> DivSet { t with v=(u,a) }
    | Punc.ADD_ASSIGN -> AddSet { t with v=(u,a) }
    | Punc.SUB_ASSIGN -> SubSet { t with v=(u,a) }
    | _ -> Set { t with v=(u,a) } (* TODO: ERROR *)
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
%inline begin_declaration : t=type_specifier {
  fun ctxt ->
    open_decl_bind ctxt (make_ref false t) (empty_pptok t.span.a) (proj t);
    (empty_pptok t.span.a, t)
}
| c=CONST; t=type_specifier {
  fun ctxt ->
    open_decl_bind ctxt (make_ref true t) c (proj t);
    (c, t)
}
| a=ATTRIBUTE; t=type_specifier {
  fun ctxt ->
    open_decl_bind ctxt (make_attribute t) a (proj t);
    (match t.v with `record (_,_) -> error (InvalidAttributeStruct (proj t));
      | _ -> ());
    (a, t)
}
| i=INVARIANT?; q=VARYING; t=type_specifier {
  fun ctxt ->
    let inv,i = match i with Some i -> true,[i] | None -> false,[] in
    let qt = fuse_pptok (i@[q]) in
    open_decl_bind ctxt (make_varying inv t) qt (proj t);
    (match t.v with `record (_,_) -> error (InvalidVaryingStruct (proj t))
      | _ -> ());
    (qt, t)
}
| u=UNIFORM; t=type_specifier {
  fun ctxt ->
    open_decl_bind ctxt (make_uniform t) u (proj t);
    (u, t)
};
declaration
: t=struct_specifier; s=SEMICOLON {
  let decl = register ctxt
    { qualt=(empty_pptok t.span.a);
      typet=(empty_pptok t.span.a);
      symt={t with v=Type t.v} }
  in Bind {(fuse_pptok [proj t; proj s]) with v=[decl]}
}
| b=begin_declaration;
dl=fuse_sep_nonempty_list(declarator,COMMA); s=SEMICOLON {
  let q, t = b ctxt in
  let decls = List.map (fun t -> (t.v ctxt).v) dl.v in
  close_decl_bind ctxt;
  let tok = fuse_pptok [proj q; proj t; proj dl; proj s] in
  match t.v with
    | `record (_,_) ->
      let t = { qualt=(empty_pptok t.span.a);
                typet=(empty_pptok t.span.a);
                symt={t with v=Type t.v }} in
      Bind {tok with v=t::decls}
    | _ -> Bind {tok with v=decls}
  }
| inv=INVARIANT; dl=fuse_sep_nonempty_list(IDENTIFIER,COMMA); s=SEMICOLON {
  let v = List.map (fun d -> d.v) dl.v in
  Invariant {(fuse_pptok [proj inv; proj dl; proj s]) with v}
  }
| p=PRECISION; t=precision_type; s=SEMICOLON {
  Precdecl {(fuse_pptok [proj p; proj t; proj s]) with v=t.v}
  }
(* TODO: catch bad precision type *)
;
declarator
: i=IDENTIFIER {
  { i with v=fun ctxt ->
    let decl = bind_decl ctxt { i with v=(i.v,None,None) } in
    {decl.symt with v=decl}
  }
  }
| i=IDENTIFIER; a=array_type_annot {
  let t = fuse_pptok [proj i; proj a] in
  { t with v=fun ctxt ->
    let decl = bind_decl ctxt { t with v=(i.v,Some a.v,None) }
    in {decl.symt with v=decl}
  }
  }
| i=IDENTIFIER; e=EQUAL; ini=initializer_ {
  let t = fuse_pptok [proj i; proj e; proj_slexpr ini] in
  { t with v=fun ctxt ->
    let decl = bind_decl ctxt { t with v=(i.v,None,Some ini)}
    in {decl.symt with v=decl}
  }
}
;
array_type_annot
: l=LEFT_BRACKET; c=constant_expression; r=RIGHT_BRACKET {
  {(fuse_pptok [proj l; proj_slexpr c; proj r]) with v=c}
}
;
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
;
begin_param_list
: l=LEFT_PAREN {
  push_scope ctxt; l
}
;
function_prototype
: t=type_specifier; i=IDENTIFIER;
p=fuse_sep_list(begin_param_list,param_declaration,COMMA,RIGHT_PAREN) {
  let params = List.map (fun p -> p.v) p.v in
  push_scope ctxt;
  (t, {(fuse_pptok [proj i; proj p]) with v=(`lam (params, t.v), i.v)})
}
| t=type_specifier; i=IDENTIFIER; l=LEFT_PAREN; v=VOID; r=RIGHT_PAREN {
  push_scope ctxt; push_scope ctxt;
  (t, {(fuse_pptok [proj i; proj l; proj v; proj r]) with v=(`lam ([],t.v), i.v)})
}
| v=VOID; i=IDENTIFIER;
  p=fuse_sep_list(begin_param_list,param_declaration,COMMA,RIGHT_PAREN) {
  let params = List.map (fun p -> p.v) p.v in
  push_scope ctxt;
  ({v with v=`void},{(fuse_pptok [proj i; proj p])
   with v=(`lam (params,`void), i.v)})
}
| n=VOID; i=IDENTIFIER; l=LEFT_PAREN; v=VOID; r=RIGHT_PAREN {
  push_scope ctxt; push_scope ctxt;
  ({v with v=`void},{(fuse_pptok [proj i; proj l; proj v; proj r])
   with v=(`lam ([],`void), i.v)})
  }
;
param_declarator
: t=type_specifier; i=IDENTIFIER {
    (t, {i with v=(t.v,i.v)})
  }
| t=type_specifier; i=IDENTIFIER; a=array_type_annot {
    (t, {(fuse_pptok [proj i; proj a]) with v=(`array (a.v,t.v),i.v)})
  }
;
param_declaration
: t=CONST?; d=param_declarator {
    let qualt,const = match t with
      | Some t -> t, true
      | None -> (empty_pptok (fst d).span.a), false in
    let bind = Param (const, In (fst d).v, Some (snd (snd d).v)) in
    let decl = register ctxt
      {qualt; typet=(proj (fst d)); symt={(snd d) with v=bind}}
    in {(fuse_pptok [decl.qualt;decl.typet;proj decl.symt]) with v=bind}
  }
| t=CONST?; d=param_type_specifier {
    let qualt,const = match t with
      | Some t -> t, true
      | None -> (empty_pptok d.span.a), false in
    let bind = Param (const, In d.v, None) in
    let decl = register ctxt
      {qualt; typet=proj d;
       symt={(empty_pptok d.span.z) with v=bind}}
    in {(fuse_pptok [decl.qualt;decl.typet;proj decl.symt]) with v=bind}
  }
| t=CONST?; i=IN; d=param_declarator {
    let qualt,const = match t with
      | Some t -> fuse_pptok [t;i], true
      | None -> i, false in
    let bind = Param (const, In (fst d).v, Some (snd (snd d).v)) in
    let decl = register ctxt
      {qualt; typet=(proj (fst d)); symt={(snd d) with v=bind}}
    in {(fuse_pptok [decl.qualt;decl.typet;proj decl.symt]) with v=bind}
  }
| t=CONST?; i=IN; d=param_type_specifier {
    let qualt,const = match t with
      | Some t -> fuse_pptok [t;i], true
      | None -> i, false in
    let bind = Param (const, In d.v, None) in
    let decl = register ctxt
      {qualt; typet=proj d;
       symt={(empty_pptok d.span.z) with v=bind}}
    in {(fuse_pptok [decl.qualt;decl.typet;proj decl.symt]) with v=bind}
  }
| t=CONST?; o=OUT; d=param_declarator {
    let qualt,const = match t with
      | Some t -> fuse_pptok [t;o], true
      | None -> o, false in
    let bind = Param (const, Out (fst d).v, Some (snd (snd d).v)) in
    let decl = register ctxt
      {qualt; typet=(proj (fst d)); symt={(snd d) with v=bind}}
    in {(fuse_pptok [decl.qualt;decl.typet;proj decl.symt]) with v=bind}
  }
| t=CONST?; o=OUT; d=param_type_specifier {
    let qualt,const = match t with
      | Some t -> fuse_pptok [t;o], true
      | None -> o, false in
    let bind = Param (const, Out d.v, None) in
    let decl = register ctxt
      {qualt; typet=proj d;
       symt={(empty_pptok d.span.z) with v=bind}}
    in {(fuse_pptok [decl.qualt;decl.typet;proj decl.symt]) with v=bind}
  }
| t=CONST?; io=INOUT; d=param_declarator {
    let qualt,const = match t with
      | Some t -> fuse_pptok [t;io], true
      | None -> io, false in
    let bind = Param (const, Inout (fst d).v, Some (snd (snd d).v)) in
    let decl = register ctxt
      {qualt; typet=(proj (fst d)); symt={(snd d) with v=bind}}
    in {(fuse_pptok [decl.qualt;decl.typet;proj decl.symt]) with v=bind}
  }
| t=CONST?; io=INOUT; d=param_type_specifier {
    let qualt,const = match t with
      | Some t -> fuse_pptok [t;io], true
      | None -> io, false in
    let bind = Param (const, Inout d.v, None) in
    let decl = register ctxt
      {qualt; typet=proj d; symt={(empty_pptok d.span.z) with v=bind}}
    in {(fuse_pptok [decl.qualt;decl.typet;proj decl.symt]) with v=bind}
  }
;
param_type_specifier
: t=type_specifier { t }
| t=type_specifier; a=array_type_annot {
    {(fuse_pptok [proj t; proj a]) with v=(`array (a.v,t.v))}
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
| s=SAMPLERCUBE { {s with v=`samplerCube (lookup_prec ctxt SamplerCube)} }
| s=struct_specifier {
  ignore (register ctxt {qualt=empty_pptok s.span.a;
                         typet=empty_pptok s.span.a;
                         symt={s with v=Type s.v}});
  s
}
| i=IDENTIFIER { match lookup_type ctxt i.v with
                   | `univ -> {i with v=`record (Some i.v,[])}
                   | v -> {i with v}
               }
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
        let t = `record (None, List.flatten (List.map (fun d -> d.v) dl)) in
          {(fuse_pptok ([proj s; proj l]@(List.map proj dl)@[proj r]))
           with v=t}
    | Some i ->
        let t = `record (Some i.v, List.flatten (List.map (fun d -> d.v) dl)) in
          {(fuse_pptok ([proj s; proj i; proj l]@(List.map proj dl)@[proj r]))
           with v=t}
}
;
struct_declaration (* TODO: nested struct error *)
: t=type_specifier; dl=fuse_sep_nonempty_list(struct_declarator,COMMA); s=SEMICOLON {
  let v = List.map
    (fun d -> match d.v with
      | None,n -> (n,t.v)
      | Some ie,n -> (n,`array (ie,t.v))
    ) dl.v
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
begin_compound_scope : l=LEFT_BRACE { push_scope ctxt; l }
compound_statement
: l=begin_compound_scope; sl=list(statement_no_new_scope); r=RIGHT_BRACE {
  pop_scope ctxt 1;
  Scope {(fuse_pptok ((proj l)
                      ::(List.map proj_slstmt sl)@[proj r]))
         with v=sl}
}
;
statement
: c=compound_statement_no_new_scope { c }
| s=simple_statement { {(proj_slstmt s) with v=[s]} }
;
compound_statement_no_new_scope
: l=LEFT_BRACE; sl=list(statement_no_new_scope); r=RIGHT_BRACE {
  {(fuse_pptok ((proj l)::(List.map proj_slstmt sl)@[proj r])) with v=sl}
}
;
expression_statement
: e=expression; s=SEMICOLON {
  Expr {(fuse_pptok [proj_slexpr e; proj s]) with v=e}
}
| s=SEMICOLON {
  Expr {s with v=Constant {s with v=B (`bool,false)}}
}
;
selection_intro
: i=IF; l=LEFT_PAREN; e=expression; r=RIGHT_PAREN {
  push_scope ctxt;
  {(fuse_pptok [proj i; proj l; proj_slexpr e; proj r]) with v=e}
}
;
else_intro
: e=ELSE { pop_scope ctxt 1; push_scope ctxt; e }
;
selection_statement
: s=selection_intro; tb=statement {
  pop_scope ctxt 1;
  Select {(fuse_pptok [proj s; proj tb]) with v=(s.v, tb, None)}
} %prec LESS_THAN_ELSE
| s=selection_intro; tb=statement; e=else_intro; fb=statement {
  pop_scope ctxt 1;
  Select {(fuse_pptok [proj s; proj tb; proj e; proj fb])
          with v=(s.v, tb, Some fb)}
}
;
condition
: e=expression {
  push_scope ctxt; (* symm with next other production *)
  Expr {(proj_slexpr e) with v=e} (* TODO: check bool *)
}
| t=type_specifier; i=IDENTIFIER; e=EQUAL; ini=initializer_ {
    push_scope ctxt;
    let r = Ref (false, t.v, i.v,None,Some ini) in
    let symt = {(fuse_pptok [proj i; proj e; proj_slexpr ini]) with v=r} in
      match t.v with
        | `record (Some tyname, fields) ->
          Bind {(fuse_pptok [proj t; proj symt])
          with v=[{qualt=(empty_pptok t.span.a);
                   typet=(empty_pptok t.span.a);
                   symt={t with v=Type t.v}};
                  register ctxt {qualt=(empty_pptok t.span.a);
                                 typet=proj t;
                                 symt}]}
        | _ -> Bind {(fuse_pptok [proj t; proj symt])
        with v=[register ctxt {qualt=(empty_pptok t.span.a);
                               typet=proj t; symt}]}
  }
;
do_intro : d=DO { push_scope ctxt; d };
do_outro : w=WHILE { pop_scope ctxt 1; w };
iteration_statement
: w=WHILE; l=LEFT_PAREN; c=condition; r=RIGHT_PAREN; s=statement_no_new_scope {
  pop_scope ctxt 1; (* condition introduces scope *)
  While {(fuse_pptok [proj w; proj l; proj_slstmt c; proj r; proj_slstmt s])
         with v=(c,s)}
}
| d=do_intro; st=statement; w=do_outro;
l=LEFT_PAREN; e=expression; r=RIGHT_PAREN; s=SEMICOLON {
  DoWhile {(fuse_pptok [proj d; proj st; proj w;
                        proj l; proj_slexpr e; proj r; proj s])
           with v=(st,e)}
}
| f=FOR; l=LEFT_PAREN; i=for_init_statement; j=for_rest_statement; r=RIGHT_PAREN;
s=statement_no_new_scope {
  pop_scope ctxt 1; (* for_rest_statement introduces scope *)
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
    {(fuse_pptok [proj_slstmt c; proj s; proj_slexpr e])
     with v=(Some c, Some e)}
  }
| c=condition; s=SEMICOLON {
    {(fuse_pptok [proj_slstmt c; proj s]) with v=(Some c, None)}
  }
| s=SEMICOLON; e=expression {
    push_scope ctxt; (* symm with condition *)
    {(fuse_pptok [proj s; proj_slexpr e]) with v=(None, Some e)}
  }
| s=SEMICOLON {
    push_scope ctxt; (* symm with condition *)
    { s with v=(None, None) }
  }
;
jump_statement
: c=CONTINUE; s=SEMICOLON { Continue (fuse_pptok [proj c; proj s]) }
| b=BREAK; s=SEMICOLON { Break (fuse_pptok [proj b; proj s]) }
| r=RETURN; e=expression?; s=SEMICOLON {
    let etl = match e with Some e -> [proj_slexpr e] | None -> [] in
    Return {(fuse_pptok ((proj r)::etl@[proj s])) with v=e}
  }
| d=DISCARD; s=SEMICOLON { Discard (fuse_pptok [proj d; proj s]) }
;
translation_unit
: dl=list(external_declaration); EOF {
  ctxt := {!ctxt with externals=List.fold_left
      (fun l -> function Bind d -> d::l | _ -> l) [] dl};
  !ctxt
}
;
external_declaration
: d=declaration { d }
| f=function_prototype; s=SEMICOLON {
  pop_scope ctxt 2;
  let bind = Fun (fst (snd f).v, snd (snd f).v, None) in
  let decl = {qualt=(empty_pptok (fst f).span.a); typet=(proj (fst f));
              symt={(fuse_pptok [proj (snd f); proj s]) with v=bind}} in
  Bind {(fuse_pptok [decl.typet; proj decl.symt]) with v=[register ctxt decl]}
}
| p=function_prototype; c=compound_statement_no_new_scope {
  pop_scope ctxt 2;
  let bind = Fun (fst (snd p).v, snd (snd p).v, Some c.v) in
  let decl = {qualt=(empty_pptok (fst p).span.a); typet=(proj (fst p));
              symt={(fuse_pptok [proj (snd p); proj c])
              with v=bind}} in
  Bind {(fuse_pptok [decl.typet; proj decl.symt]) with v=[register ctxt decl]}
}
;
%%
