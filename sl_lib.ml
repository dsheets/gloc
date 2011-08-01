open Pp_lib
module SymMap = Map.Make(struct type t=string let compare = compare end)

type 'a slval = I of 'a * int
                | F of 'a * float
                | B of 'a * bool

type slel = X | Y | Z | W
type slswizzle =
    Sub1 of slel
  | Sub2 of slel * slel
  | Sub3 of slel * slel * slel
  | Sub4 of slel * slel * slel * slel

exception BadSwizzleField of char
exception BadSwizzle of string pptok

type 'a slbind = { const: bool; name: string option; b: 'a }

type slprec = High | Medium | Low | Default
type slprectype = Float | Int | Sampler2d | SamplerCube
module PrecMap = Map.Make(struct type t=slprectype let compare = compare end)

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
type slnumish = [ slprim | sldim ]

type slsampler = [ `sampler2d of slprec | `samplerCube of slprec ]
type slprecable = [ slnum | slsampler ]
type 'a slparam = In of 'a | Out of 'a | Inout of 'a
type slvoid = [ `void ] (* Not a real type -- more like bottom/unit/falsity *)
type sltop = [ `univ ]
type 'a slstruct = [ `record of (string * 'a) list | sltop ]
type 'a sldecl = [ `custom of string * 'a slstruct ]
type ('a,'b) slfun = [ `lam of 'a slparam slbind list * 'b ]
type ('a,'b) slarray = [ `array of 'a * 'b ]
type sltype = [ (sltype (*slint*) slexpr, sltype) slarray
              | slsampler | slnumish
              | sltype slstruct
	      | slvoid ] (* TODO: type shouldn't have void -- error *)
and slreturn = [ slvoid
               | (sltype (*slint*) slexpr, sltype) slarray
               | slsampler | slnumish
               | sltype slstruct ]
and sluniv = [ (sltype, sltype (*slreturn*)) slfun
             | sltype sldecl
             | (sltype (*slint*) slexpr, sltype) slarray
             | slsampler | slnumish
             | sltype slstruct ]
and 'b slexpr =
    Var of (string * slenv * 'b) pptok
  | Constant of 'b slval pptok
      (* TODO: restrict constructor args to nonarrays *)
  | Construct of ('b * string * slenv * sltype slexpr list) pptok
  | Group of 'b slexpr pptok
      (* TODO: restrict subscript index and array def size to slint *)
      (* TODO: restruct subscript operand to (sltype slexpr, sltype) slarray *)
  | Subscript of ('b * sltype slexpr * sltype slexpr) pptok
  | App of ('b * string * slenv * sltype slexpr list) pptok
      (* TODO: restrict field operand to slstruct *)
  | Field of ('b * sltype slexpr * string) pptok
      (* TODO: restrict swizzle operand to sldim *)
  | Swizzle of ('b * sltype slexpr * slswizzle) pptok
  | PostInc of 'b slexpr pptok (* slnumish -> slnumish *)
  | PostDec of 'b slexpr pptok (* slnumish -> slnumish *)
  | PreInc of 'b slexpr pptok (* slnumish -> slnumish *)
  | PreDec of 'b slexpr pptok (* slnumish -> slnumish *)
  | Pos of 'b slexpr pptok (* slnumish -> slnumish *)
  | Neg of 'b slexpr pptok (* slnumish -> slnumish *)
  | Not of 'b slexpr pptok (* slbool -> slbool *)
  | Mul of ('b slexpr * 'b slexpr) pptok (* slnumish * slnumish -> slnumish *)
  | Div of ('b slexpr * 'b slexpr) pptok (* slnumish * slnumish -> slnumish *)
  | Add of ('b slexpr * 'b slexpr) pptok (* slnumish * slnumish -> slnumish *)
  | Sub of ('b slexpr * 'b slexpr) pptok (* slnumish * slnumish -> slnumish *)
      (* TODO: restrict relational ops to slnum operands *)
  | Lt of ('b * sltype slexpr * sltype slexpr) pptok (* -> slbool *)
  | Gt of ('b * sltype slexpr * sltype slexpr) pptok (* -> slbool *)
  | Lte of ('b * sltype slexpr * sltype slexpr) pptok (* -> slbool *)
  | Gte of ('b * sltype slexpr * sltype slexpr) pptok (* -> slbool *)
      (* TODO: no samplers, no arrays in equality tests *)
  | Eq of ('b * sltype slexpr * sltype slexpr) pptok (* -> slbool *)
  | Neq of ('b * sltype slexpr * sltype slexpr) pptok (* -> slbool *)
      (* TODO: restrict boolean ops to slbool operands *)
  | And of ('b * sltype slexpr * sltype slexpr) pptok (* -> slbool *)
  | Xor of ('b * sltype slexpr * sltype slexpr) pptok (* -> slbool *)
  | Or of ('b * sltype slexpr * sltype slexpr) pptok (* -> slbool *)
      (* TODO: restrict selection condition to slbool *)
  | Sel of (sltype slexpr * 'b slexpr * 'b slexpr) pptok
  | Set of ('b slexpr * 'b slexpr) pptok
  | AddSet of ('b slexpr * 'b slexpr) pptok (* slnumish * slnumish -> slnumish *)
  | SubSet of ('b slexpr * 'b slexpr) pptok (* slnumish * slnumish -> slnumish *)
  | MulSet of ('b slexpr * 'b slexpr) pptok (* slnumish * slnumish -> slnumish *)
  | DivSet of ('b slexpr * 'b slexpr) pptok (* slnumish * slnumish -> slnumish *)
  | Seq of (sltype slexpr * 'b slexpr) pptok
and slstmt =
    Expr of sltype slexpr pptok
	(* TODO: select condition only slbool *)
  | Select of (sltype slexpr
	       * slstmt list pptok * slstmt list pptok option) pptok
      (* TODO: for condition only slbool *)
  | For of (slstmt * (slstmt option * sltype slexpr option) pptok
	    * slstmt) pptok
  | While of (slstmt * slstmt) pptok
      (* TODO: dowhile condition only slbool *)
  | DoWhile of (slstmt list pptok * sltype slexpr) pptok
  | Return of (sltype slexpr option) pptok
  | Discard of unit pptok
  | Break of unit pptok
  | Continue of unit pptok
  | Scope of slstmt list pptok (* slenv? *)
  | Invariant of string list pptok
  | Attribute of (string * sltype) list pptok
  | Uniform of (string * sltype) list pptok
  | Varying of (bool * string * sltype) list pptok
  | Precdecl of sltype (*slprecable*) pptok
  | Typedecl of (sltype sldecl
                 * sltype slstruct slexpr option) slbind list pptok
  | Vardecl of (sltype * sltype slexpr option) slbind list pptok
  | Fundecl of ((sltype, sltype (*slreturn*)) slfun slbind
                * slstmt list pptok option) pptok
and slenv = { ctxt : slstmt list slbind SymMap.t list; (* scope stack *)
              opensyms : sluniv slbind list;
              prec : slprec PrecMap.t list; (* precision stack *)
              invariant : bool;
              pragmas : string pptok pptok SymMap.t;
              stmts : slstmt list }

exception BadField of sltype pptok
exception BadDeclStatement of unit pptok
exception BadSubscript of sltype pptok

exception CannotInitializeAttribute of unit pptok
exception CannotInitializeVarying of unit pptok
exception CannotInitializeUniform of unit pptok

let ctxt = ref { ctxt=[SymMap.empty]; opensyms=[]; prec=[PrecMap.empty];
                 invariant=false; pragmas=SymMap.empty; stmts=[] }
(*let typeof_stmt ctxt = function
  | Expr t -> error (BadDeclStatement (proj t)); `univ
  | Select t -> error (BadDeclStatement (proj t)); `univ
  | For t -> error (BadDeclStatement (proj t)); `univ
  | While t -> error (BadDeclStatement (proj t)); `univ
  | DoWhile t -> error (BadDeclStatement (proj t)); `univ
  | Invariant t -> error (BadDeclStatement (proj t)); `univ
  | Discard t -> error (BadDeclStatement (proj t)); `univ
  | Break t -> error (BadDeclStatement (proj t)); `univ
  | Continue t -> error (BadDeclStatement (proj t)); `univ
  | Return t -> error (BadDeclStatement (proj t)); `univ
  | Scope t -> error (BadDeclStatement (proj t)); `univ
  | Attribute ({v=[]} as t) -> error (BadDeclStatement (proj t)); `univ
  | Uniform ({v=[]} as t) -> error (BadDeclStatement (proj t)); `univ
  | Varying ({v=[]} as t) -> error (BadDeclStatement (proj t)); `univ
  | Precdecl t -> error (BadDeclStatement (proj t)); `univ
  | Typedecl ({v=[]} as t) -> error (BadDeclStatement (proj t)); `univ
  | Vardecl ({v=[]} as t) -> error (BadDeclStatement (proj t)); `univ
  | Attribute {v=(_,t)::_} -> t
  | Uniform {v=(_,t)::_} -> t
  | Varying {v=(_,_,t)::_} -> t
  | Typedecl {v={b=(`custom (_,r),_)}::_} -> r
  | Vardecl {v={b=(t,_)}::_} -> t
  | Fundecl {v=({b},_)} -> b*)

let lookup_type envr sym = let env = !envr in (* TODO: overloads *)
  try let h = List.hd (SymMap.find sym (List.hd env.ctxt)).b in
    (* TODO: scope popping *)
    (*typeof_stmt env h*) `univ (* TODO: type propagation *)
  with Not_found ->
    envr := {env with opensyms={const=false;
                                name=Some sym; b=`univ}::env.opensyms};
    `univ

let lookup_prec envr pt =
  let env = !envr in
  try PrecMap.find pt (List.hd env.prec) (* TODO: scope popping *)
  with Not_found -> Default

let rec typeof expr : sltype = match expr with
  | Var {v=(_,_,t)} -> t
  | Constant {v=I (t,_)}
  | Constant {v=F (t,_)}
  | Constant {v=B (t,_)} -> t
  (* TODO: restrict constructor args to nonarrays *)
  | Construct {v=(t,_,_,_)} -> t
  | Group {v} -> typeof v
  (* TODO: restrict subscript index and array def size to slint *)
  | Subscript {v=(t,_,_)} -> t
  | App {v=(t,_,_,_)} -> t
  (* TODO: restrict field operand to slstruct *)
  | Field {v=(t,_,_)} -> t
  (* TODO: restrict swizzle operand to sldim *)
  | Swizzle {v=(t,_,_)} -> t
  | PostInc {v} -> typeof v
  (* slnumish -> slnumish *)
  | PostDec {v} -> typeof v
  (* slnumish -> slnumish *)
  | PreInc {v} -> typeof v
  (* slnumish -> slnumish *)
  | PreDec {v} -> typeof v
  (* slnumish -> slnumish *)
  | Pos {v} -> typeof v
  (* slnumish -> slnumish *)
  | Neg {v} -> typeof v
  (* slnumish -> slnumish *)
  | Not {v} -> typeof v
  (* slbool -> slbool *)
  | Mul {v=(v,_)} -> typeof v
  (* slnumish * slnumish -> slnumish *)
  | Div {v=(v,_)} -> typeof v
  (* slnumish * slnumish -> slnumish *)
  | Add {v=(v,_)} -> typeof v
  (* slnumish * slnumish -> slnumish *)
  | Sub {v=(v,_)} -> typeof v
  (* slnumish * slnumish -> slnumish *)
  (* TODO: restrict relational ops to slnum operands *)
  | Lt {v=(t,_,_)} -> t
  (* -> slbool *)
  | Gt {v=(t,_,_)} -> t
  (* -> slbool *)
  | Lte {v=(t,_,_)} -> t
  (* -> slbool *)
  | Gte {v=(t,_,_)} -> t
  (* -> slbool *)
  (* TODO: no samplers, no arrays in equality tests *)
  | Eq {v=(t,_,_)} -> t
  (* -> slbool *)
  | Neq {v=(t,_,_)} -> t
  (* -> slbool *)
  (* TODO: restrict boolean ops to slbool operands *)
  | And {v=(t,_,_)} -> t
  (* -> slbool *)
  | Xor {v=(t,_,_)} -> t
  (* -> slbool *)
  | Or {v=(t,_,_)} -> t
  (* -> slbool *)
  (* TODO: restrict selection condition to slbool *)
  | Sel {v=(_,tb,_)} -> typeof tb
  | Set {v=(lhs,_)} -> typeof lhs (* 'a -> 'a *)
  | AddSet {v=(lhs,_)} -> typeof lhs
  (* slnumish -> slnumish *)
  | SubSet {v=(lhs,_)} -> typeof lhs
  (* slnumish -> slnumish *)
  | MulSet {v=(lhs,_)} -> typeof lhs
  (* slnumish -> slnumish *)
  | DivSet {v=(lhs,_)} -> typeof lhs
  (* slnumish -> slnumish *)
  | Seq {v=(_,rhs)} -> typeof rhs

let el_of_char = function
  | 'x' | 'r' | 'u' -> X
  | 'y' | 'g' | 'v' -> Y
  | 'z' | 'b' | 's' -> Z
  | 'w' | 'a' | 't' -> W
  | c -> raise (BadSwizzleField c)

let swizzle_of_identifier id = let s = id.v in try match String.length s with
  | 1 -> Sub1 (el_of_char s.[0])
  | 2 -> Sub2 (el_of_char s.[0],
	       el_of_char s.[1])
  | 3 -> Sub3 (el_of_char s.[0],
	       el_of_char s.[1],
	       el_of_char s.[2])
  | 4 -> Sub4 (el_of_char s.[0],
	       el_of_char s.[1],
	       el_of_char s.[2],
	       el_of_char s.[3])
  | _ -> error (BadSwizzle id); Sub1 X
  with BadSwizzleField c -> error (BadSwizzle id); Sub1 X (* care? *)

let inj_prim : slprim -> sltype = function
  | `bool -> `bool
  | `int p -> `int p
  | `float p -> `float p

let typeof_swizzle : slprim ->  slswizzle -> sltype = fun elt -> function
  | Sub1 _ -> inj_prim elt
  | Sub2 _ -> `vec2 elt
  | Sub3 _ -> `vec3 elt
  | Sub4 _ -> `vec4 elt

let proj_slexpr = function
    Var t -> proj t
  | Constant t -> proj t
  | Construct t -> proj t
  | Group t -> proj t
  | Subscript t -> proj t
  | App t -> proj t
  | Field t -> proj t
  | Swizzle t -> proj t
  | PostInc t -> proj t
  | PostDec t -> proj t
  | PreInc t -> proj t
  | PreDec t -> proj t
  | Pos t -> proj t
  | Neg t -> proj t
  | Not t -> proj t
  | Mul t -> proj t
  | Div t -> proj t
  | Add t -> proj t
  | Sub t -> proj t
  | Lt t -> proj t
  | Gt t -> proj t
  | Lte t -> proj t
  | Gte t -> proj t
  | Eq t -> proj t
  | Neq t -> proj t
  | And t -> proj t
  | Xor t -> proj t
  | Or t -> proj t
  | Sel t -> proj t
  | Set t -> proj t
  | AddSet t -> proj t
  | SubSet t -> proj t
  | MulSet t -> proj t
  | DivSet t -> proj t
  | Seq t -> proj t

let proj_slstmt = function
    Expr t -> proj t
  | Select t -> proj t
  | For t -> proj t
  | While t -> proj t
  | DoWhile t -> proj t
  | Return t -> proj t
  | Discard t -> proj t
  | Break t -> proj t
  | Continue t -> proj t
  | Scope t -> proj t
  | Invariant t -> proj t
  | Attribute t -> proj t
  | Uniform t -> proj t
  | Varying t -> proj t
  | Precdecl t -> proj t
  | Typedecl t -> proj t
  | Vardecl t -> proj t
  | Fundecl t -> proj t
