module Macros = Map.Make(struct type t = string let compare = compare end)

type base = Oct | Dec | Hex
type behavior = Require | Enable | Warn | Disable

type src_loc = { src: int; input: int }
type loc = { file : src_loc; line: src_loc; col: int }
type span = { a: loc; z: loc }
type 't pptok = { span: span;
		  macros: unit Macros.t;
		  scan: loc -> loc * string;
                  comments: comments * comments ref;
                  v: 't }
and comments = string list pptok list

exception ParserError of string
exception UnterminatedConditional of unit pptok
exception UnknownBehavior of string pptok

type cond_expr =
    Group of cond_expr pptok
  | Constant of int pptok
  | Omacros of string pptok list pptok
  | Fmacro of (string pptok * (string pptok list)) pptok
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

type pptok_type =
    Int of (base * int) pptok
  | Float of float pptok
  | Word of string pptok
  | Call of string pptok
  | Punc of Punc.tok pptok
  | Tokens of pptok_type list pptok

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
  | Concat of (pptok_expr * pptok_expr) pptok

let proj : 'a pptok -> unit pptok = fun t -> { t with v=() }

let rec first_of_pptok_type = function
  | Int t -> t.span.a
  | Float t -> t.span.a
  | Word t -> t.span.a
  | Call t -> t.span.a
  | Punc t -> t.span.a
  | Tokens t -> t.span.a

let rec last_of_pptok_type = function
  | Int t -> t.span.z
  | Float t -> t.span.z
  | Word t -> t.span.z
  | Call t -> t.span.z
  | Punc t -> t.span.z
  | Tokens t -> t.span.z

let rec span_of_pptok_types = function
  | [] -> None
  | (h::_) as ptl ->
      Some {a=first_of_pptok_type h;
	    z=last_of_pptok_type (List.hd (List.rev ptl))}

let span_of_comments = function [] -> None
  | (h::_) as cl ->
      Some {a=h.span.a; z=(List.hd (List.rev cl)).span.z}

let file = ref {src=0; input=0}
let line = ref {src=1; input=1}
let errors : exn list ref = ref []
let error exc = errors := exc::!errors
let macros = Macros.empty

let fuse_pptok = function
  | [] -> raise (ParserError "fusing empty pptok list")
  | (h::_) as tokl ->
      {span={a=h.span.a;z=(List.hd (List.rev tokl)).span.z};
       macros;
       scan=(fun start -> List.fold_left
	       (fun (loc,str) tok ->
		  let nloc,nstr = tok.scan loc in
		    (nloc,str^nstr))
	       (start,"") tokl);
       comments=(fst h.comments,snd (List.hd (List.rev tokl)).comments);
       v=()}
