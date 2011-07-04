module Macros = Map.Make(struct type t = string let compare = compare end)

type base = Oct | Dec | Hex
type behavior = Require | Enable | Warn | Disable

type src_loc = { src: int; input: int }
type loc = { file : src_loc; line: src_loc; col: int }
type 't pptok = { loc: loc; scan: string;
                  comments: comments * comments ref;
                  v: 't }
and comments = string list pptok list

exception ParserError of int
exception UnknownBehavior of string pptok

type cond_expr =
    Group of cond_expr
  | Constant of int pptok
  | Omacros of string pptok list
  | Fmacro of string pptok * (string pptok list)
  | Defined of string pptok
  | Pos of cond_expr
  | Neg of cond_expr
  | BitNot of cond_expr
  | Not of cond_expr
  | Mul of cond_expr * cond_expr
  | Div of cond_expr * cond_expr
  | Mod of cond_expr * cond_expr
  | Add of cond_expr * cond_expr
  | Sub of cond_expr * cond_expr
  | BitLeft of cond_expr * cond_expr
  | BitRight of cond_expr * cond_expr
  | Lt of cond_expr * cond_expr
  | Gt of cond_expr * cond_expr
  | Lte of cond_expr * cond_expr
  | Gte of cond_expr * cond_expr
  | Eq of cond_expr * cond_expr
  | Neq of cond_expr * cond_expr
  | BitAnd of cond_expr * cond_expr
  | BitXor of cond_expr * cond_expr
  | BitOr of cond_expr * cond_expr
  | And of cond_expr * cond_expr
  | Or of cond_expr * cond_expr

type pptok_type =
    Int of (base * int) pptok
  | Float of float pptok
  | Word of string pptok
  | Call of string pptok
  | Punc of Punc.tok pptok
  | Tokens of chunk
and chunk = { macros: unit Macros.t; stream: pptok_type list }

type span = { first: unit pptok; last: unit pptok }

type pptok_expr =
  | Comments of span * comments
  | Chunk of span * chunk
  | If of span * cond_expr * pptok_expr * (pptok_expr option)
  | Def of span * string pptok * chunk
  | Fun of span * string pptok * (string pptok list) * chunk
  | Undef of span * string pptok
  | Err of span * pptok_type list
  | Pragma of span * pptok_type list
  | Version of span * int
  | Extension of span * string pptok * behavior pptok
  | Line of span * int * int
  | Concat of span * pptok_expr * pptok_expr

let fake_loc = {file={src=(-1);input=(-1)};line={src=(-1);input=(-1)};col=(-1)}
let fake_span = {first={loc=fake_loc; scan=""; comments=([],ref []); v=()};
		 last={loc=fake_loc; scan=""; comments=([],ref []); v=()}}

let proj : 'a pptok -> unit pptok = fun t -> { t with v=() }

let rec fst_of_pptok_type = function
  | Int t -> Some (proj t)
  | Float t -> Some (proj t)
  | Word t -> Some (proj t)
  | Call t -> Some (proj t)
  | Punc t -> Some (proj t)
  | Tokens { stream=h::r } ->
    begin match fst_of_pptok_type h with
      | (Some _) as proj -> proj
      | None -> fst_of_pptok_type (Tokens {macros=Macros.empty; stream=r})
    end
  | Tokens { stream=[] } -> None

let rec lst_of_pptok_type = function
  | Int t -> Some (proj t)
  | Float t -> Some (proj t)
  | Word t -> Some (proj t)
  | Call t -> Some (proj t)
  | Punc t -> Some (proj t)
  | Tokens { stream } ->
    begin match List.rev stream with
      | h::r -> begin match lst_of_pptok_type h with
          | (Some _) as proj -> proj
          | None -> lst_of_pptok_type (Tokens {macros=Macros.empty; stream=List.rev r})
      end
      | [] -> None
    end

let rec span_of_pptok_types = function [] -> None
  | (h::r) as ptl ->
    begin match fst_of_pptok_type h with
      | None -> span_of_pptok_types r
      | Some first ->
        begin match List.rev ptl with
          | [] -> None
          | (rh::rr) ->
            begin match lst_of_pptok_type rh with
              | None -> span_of_pptok_types (List.rev rr)
              | Some last -> Some {first; last}
            end
        end
    end

let span_of_comments = function [] -> None
  | (first::_) as cl ->
    begin match List.rev cl with
      | [] -> None
      | (last::_) -> Some {first=proj first; last=proj last}
    end

let span_of_pptok_expr = function
  | Chunk (s,_) | If (s,_,_,_) | Def (s,_,_) | Fun (s,_,_,_) | Undef (s,_)
  | Err (s,_) | Pragma (s,_) | Version (s,_) | Extension (s,_,_)
  | Line (s,_,_) | Concat (s,_,_) | Comments (s,_) -> s

let span_concat e1 e2 = { first=(span_of_pptok_expr e1).first;
                          last=(span_of_pptok_expr e2).last }

let last_pptok default = function
  | [] -> default
  | q -> begin match span_of_pptok_types q with
      | None -> default
      | Some span -> span.last
  end

let file = ref {src=0; input=0}
let line = ref {src=1; input=1}
let errors : exn list ref = ref []
let error exc = errors := exc::!errors
