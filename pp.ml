module Macros = Map.Make(struct type t = string let compare = compare end)

type base = Oct | Dec | Hex
type behavior = Require | Enable | Warn | Disable

type src_loc = { src: int; input: int }
type loc = { file : src_loc; line: src_loc; col: int }
type span = { a: loc; z: loc }
type 't pptok = { span: span;
		  macros: int Macros.t;
		  scan: loc -> loc * string;
                  comments: comments * comments ref;
                  v: 't }
and comments = string list pptok list

exception ParserError of string
exception UnterminatedConditional of unit pptok
exception UnknownBehavior of string pptok

type pptok_type =
  | Int of (base * int) pptok
  | Float of float pptok
  | Word of string pptok
  | Call of string pptok
  | Punc of Punc.tok pptok
  | Comma of Punc.tok pptok
  | Leftp of Punc.tok pptok
  | Rightp of Punc.tok pptok
  | Tokens of pptok_type list pptok

type cond_expr =
  | Group of cond_expr pptok
  | Opaque of pptok_type list pptok
  | Constant of int pptok
  | Defined of string pptok pptok
  | Pos of cond_expr pptok
  | Neg of cond_expr pptok
  | BitNot of cond_expr pptok
  | Not of cond_expr pptok
  | Mul of (cond_expr * cond_expr) pptok
  | Div of (cond_expr * cond_expr) pptok
  | Mod of (cond_expr * cond_expr) pptok
  | Add of (cond_expr * cond_expr) pptok
  | Sub of (cond_expr * cond_expr) pptok
  | BitLeft of (cond_expr * cond_expr) pptok
  | BitRight of (cond_expr * cond_expr) pptok
  | Lt of (cond_expr * cond_expr) pptok
  | Gt of (cond_expr * cond_expr) pptok
  | Lte of (cond_expr * cond_expr) pptok
  | Gte of (cond_expr * cond_expr) pptok
  | Eq of (cond_expr * cond_expr) pptok
  | Neq of (cond_expr * cond_expr) pptok
  | BitAnd of (cond_expr * cond_expr) pptok
  | BitXor of (cond_expr * cond_expr) pptok
  | BitOr of (cond_expr * cond_expr) pptok
  | And of (cond_expr * cond_expr) pptok
  | Or of (cond_expr * cond_expr) pptok

type pptok_expr =
  | Comments of comments pptok
  | Chunk of pptok_type list pptok
  | If of (cond_expr * pptok_expr * (pptok_expr option)) pptok
  | Def of (string pptok * pptok_type list) pptok
  | Fun of (string pptok * (string pptok list) * pptok_type list) pptok
  | Undef of string pptok pptok
  | Err of pptok_type list pptok
  | Pragma of pptok_type list pptok
  | Version of int pptok pptok
  | Extension of (string pptok * behavior pptok) pptok
  | Line of (int pptok option * int pptok) pptok
  | List of pptok_expr list pptok

let proj : 'a pptok -> unit pptok = fun t -> { t with v=() }

let proj_pptok_type = function
  | Int t -> proj t
  | Float t -> proj t
  | Word t -> proj t
  | Call t -> proj t
  | Punc t -> proj t
  | Comma t -> proj t
  | Leftp t -> proj t
  | Rightp t -> proj t
  | Tokens t -> proj t

let proj_cond_expr = function
  | Group t -> proj t
  | Opaque t -> proj t
  | Constant t -> proj t
  | Defined t -> proj t
  | Pos t -> proj t
  | Neg t -> proj t
  | BitNot t -> proj t
  | Not t -> proj t
  | Mul t -> proj t
  | Div t -> proj t
  | Mod t -> proj t
  | Add t -> proj t
  | Sub t -> proj t
  | BitLeft t -> proj t
  | BitRight t -> proj t
  | Lt t -> proj t
  | Gt t -> proj t
  | Lte t -> proj t
  | Gte t -> proj t
  | Eq t -> proj t
  | Neq t -> proj t
  | BitAnd t -> proj t
  | BitXor t -> proj t
  | BitOr t -> proj t
  | And t -> proj t
  | Or t -> proj t

let proj_pptok_expr = function
  | Comments t -> proj t
  | Chunk t -> proj t
  | If t -> proj t
  | Def t -> proj t
  | Fun t -> proj t
  | Undef t -> proj t
  | Err t -> proj t
  | Pragma t -> proj t
  | Version t -> proj t
  | Extension t -> proj t
  | Line t -> proj t
  | List t -> proj t

let span_of_list : 'a pptok list -> span = function
  | [] -> raise (ParserError "spanning empty pptok list")
  | (h::_) as ptl -> {a=(proj h).span.a;
		      z=(proj (List.hd (List.rev ptl))).span.z}

let file = ref {src=0; input=0}
let line = ref {src=1; input=1}
let errors : exn list ref = ref []
let error exc = errors := exc::!errors
let macros = Macros.empty

let fuse_pptok = function
  | [] -> raise (ParserError "fusing empty pptok list")
  | (h::_) as tokl ->
      {span=span_of_list tokl;
       macros;
       scan=(fun start -> List.fold_left
	       (fun (loc,str) tok ->
		  let nloc,nstr = tok.scan loc in
		    (nloc,str^nstr))
	       (start,"") tokl);
       comments=(fst h.comments,snd (List.hd (List.rev tokl)).comments);
       v=()}

let fuse_pptok_expr = function
  | [] -> raise (ParserError "fusing empty pptok_expr list")
  | (h::_) as el ->
    List {(fuse_pptok (List.map proj_pptok_expr el)) with v=el}

let cexpr cs = Comments {(fuse_pptok cs) with v=cs}

let pptok_expr_of_body bl def = match bl with
  | [] -> List { def with v=[] }
  | l -> fuse_pptok_expr l
