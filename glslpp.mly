%{
 module Macros = Map.Make(struct type t = string let compare = compare end)

 type base = Oct | Dec | Hex
 type behavior = Require | Enable | Warn | Disable
 type src_loc = { src: int; input: int }
 type loc = { file : src_loc; line: src_loc; col: int }
 type 't pptok = { loc: loc; scan: string;
		   mutable comments: comments * comments;
		   v: 't }
 and comments = string list pptok list

 type pptok_type =
     Int of (base * int) pptok
   | Float of float pptok
   | Word of string pptok
   | Call of string pptok
   | Punc of Punc.tok pptok
   | Tokens of chunk
   | Comments of comments
 and chunk = { macros: unit Macros.t; stream: pptok_type list }
 type span = { first: unit pptok; last: unit pptok }
 type pptok_expr =
   | Chunk of span * chunk
   | If of span * chunk * pptok_expr * (pptok_expr option)
   | Def of span * string pptok * chunk
   | Fun of span * string pptok * (string pptok list) * chunk
   | Undef of span * string pptok
   | Error of span * pptok_type list
   | Pragma of span * pptok_type list
   | Version of span * int pptok
   | Extension of span * string pptok * behavior pptok
   | Line of span * int pptok * int pptok
   | Concat of span * pptok_expr * pptok_expr

 let rec fst_of_pptok_type = function
   | Int t | Float t | Word t | Call t | Punc t -> Some { t with v=() }
   | Tokens { stream=h::r } ->
     begin match fst_of_pptok_type h with
       | (Some _) as proj -> proj
       | None -> fst_of_pptok_type (Tokens {stream=r})
     end
   | Tokens { stream=[] } -> None
   | Comments h::_ -> { h with v=() }
   | Comments [] -> None

 let rec lst_of_pptok_type = function
   | Int t | Float t | Word t | Call t | Punc t -> Some { t with v=() }
   | Tokens { stream } ->
     begin match List.rev stream with
       | h::r -> begin match lst_of_pptok_type h with
	   | (Some _) as proj -> proj
	   | None -> lst_of_pptok_type (Tokens {stream=List.rev r})
       end
       | [] -> None
     end
   | Comments ctl ->
     begin match List.rev ctl with
       | h::r -> { h with v=() }
       | [] -> None
     end

 let span_of_pptok_types = function [] -> None
   | (h::_) as ptl ->
     { first=fst_of_pptok_type h;
       last=lst_of_pptok_type (List.hd (List.rev ptl)) }

 let span_of_pptok_expr = function
   | Chunk (s,_) | If (s,_,_,_) | Def (s,_,_) | Fun (s,_,_,_) | Undef (s,_)
   | Error (s,_) | Pragma (s,_) | Version (s,_) | Extension (s,_,_)
   | Line (s,_,_) | Concat (s,_,_) -> s

 let span_concat e1 e2 = { first=(span_of_pptok_expr e1).first;
			   last=(span_of_pptok_expr e2).last }
%}

%token <unit pptok> BOF EOF
%token <unit pptok> PPDIRECTIVE ENDPPDIRECTIVE EXTENSION VERSION LINE
%token <unit pptok> DEFINE UNDEF IF IFDEF IFNDEF ELSE ELIF ENDIF ERROR PRAGMA
%token <string pptok> WORD CALL
%token <float pptok> FLOATCONSTANT
%token <(base * int) pptok> INTCONSTANT

%token <Punc.tok pptok> LEFT_OP RIGHT_OP INC_OP DEC_OP LE_OP GE_OP EQ_OP NE_OP
%token <Punc.tok pptok> AND_OP OR_OP XOR_OP MUL_ASSIGN DIV_ASSIGN ADD_ASSIGN
%token <Punc.tok pptok> MOD_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN
%token <Punc.tok pptok> AND_ASSIGN XOR_ASSIGN OR_ASSIGN SUB_ASSIGN
%token <Punc.tok pptok> LEFT_PAREN RIGHT_PAREN LEFT_BRACKET RIGHT_BRACKET
%token <Punc.tok pptok> LEFT_BRACE RIGHT_BRACE DOT COMMA COLON EQUAL SEMICOLON
%token <Punc.tok pptok> BANG DASH TILDE PLUS STAR SLASH PERCENT LEFT_ANGLE
%token <Punc.tok pptok> RIGHT_ANGLE VERTICAL_BAR CARET AMPERSAND QUESTION

%type <pptok_expr> translation_unit

%start translation_unit

%%
punc
: p=LEFT_OP | p=RIGHT_OP | p=INC_OP | p=DEC_OP | p=LE_OP | p=GE_OP | p=EQ_OP | p=NE_OP
| p=AND_OP | p=OR_OP | p=XOR_OP | p=MUL_ASSIGN | p=DIV_ASSIGN | p=ADD_ASSIGN
| p=MOD_ASSIGN | p=LEFT_ASSIGN | p=RIGHT_ASSIGN
| p=AND_ASSIGN | p=XOR_ASSIGN | p=OR_ASSIGN | p=SUB_ASSIGN
| p=LEFT_PAREN | p=RIGHT_PAREN | p=LEFT_BRACKET | p=RIGHT_BRACKET
| p=LEFT_BRACE | p=RIGHT_BRACE | p=DOT | p=COMMA | p=COLON | p=EQUAL | p=SEMICOLON
| p=BANG | p=DASH | p=TILDE | p=PLUS | p=STAR | p=SLASH | p=PERCENT | p=LEFT_ANGLE
| p=RIGHT_ANGLE | p=VERTICAL_BAR | p=CARET | p=AMPERSAND | p=QUESTION { p }
    
source
: w=WORD { Word w }
| c=CALL { Call c }
| f=FLOATCONSTANT { Float f }
| i=INTCONSTANT { Int i }
| p=punc { Punc p }
| e=EOF { match fst e.comments with [] -> Tokens {macros=Macros.empty; stream=[]}
    | cl -> Comments cl }
| b=BOF { match snd e.comments with [] -> Tokens {macros=Macros.empty; stream=[]}
    | cl -> Comments cl }

cond_expr
:

cond_body
:

behavior
:

directive
: IF cond_expr ENDPPDIRECTIVE cond_body PPDIRECTIVE ENDIF {}
| IFDEF WORD ENDPPDIRECTIVE cond_body PPDIRECTIVE ENDIF {}
| IFNDEF WORD ENDPPDIRECTIVE cond_body PPDIRECTIVE ENDIF {}
| DEFINE WORD source* {}
| DEFINE CALL separated_list(COMMA,WORD) RIGHT_PAREN source* {}
| UNDEF WORD {}
| ERROR source* {}
| PRAGMA source* {}
| EXTENSION WORD COLON behavior {}
| VERSION INTCONSTANT {}
| LINE INTCONSTANT {}
| LINE INTCONSTANT INTCONSTANT {}

translation_unit
: first=PPDIRECTIVE; dir=directive; last=ENDPPDIRECTIVE; rest=translation_unit? {
  match rest with None -> put_span dir {first;last}
    | Some expr ->
      Concat (span_concat
		(Chunk ({first;last},{macros=Macros.empty; stream=[]}))
		expr, dir, expr)
}
| first=PPDIRECTIVE; last=ENDPPDIRECTIVE; rest=translation_unit? {
  match rest with None -> Chunk ({first; last}, {macros=Macros.empty; stream=[]})
    | Some expr -> expr
}
| chunk=source+; rest=translation_unit? {
  let chunk = Chunk (span_of_pptok_types chunk,
		     {macros=Macros.empty; stream=chunk})
  in match rest with None -> chunk
    | Some expr -> Concat (span_concat chunk expr, chunk, expr)
}
%%
