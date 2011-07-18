module Env = Map.Make(struct type t = string let compare = compare end)

type base = Oct | Dec | Hex
type behavior = Require | Enable | Warn | Disable

type src_loc = { src: int; input: int }
type loc = { file : src_loc; line: src_loc; col: int }
type span = { a: loc; z: loc }
type 't pptok = { span: span;
		  scan: loc -> loc * string;
                  comments: comments * comments ref;
                  v: 't }
and comments = string pptok list pptok list
and pptok_type =
  | Int of (base * int) pptok
  | Float of float pptok
  | Word of (string * unit Env.t) pptok
  | Call of string pptok
  | Punc of Punc.tok pptok
  | Comma of Punc.tok pptok
  | Leftp of Punc.tok pptok
  | Rightp of Punc.tok pptok
and stream = pptok_type list
and macro = { name: string option;
	      arity: (int * string list) option;
	      stream: stream }

exception ParserError of string
exception UnterminatedConditional of unit pptok
exception UnknownBehavior of string pptok
exception HolyVersion of unit pptok
exception UnsupportedVersion of int pptok
exception InvalidVersionBase of base pptok
exception InvalidVersionArg of unit pptok
exception InvalidLineBase of base pptok
exception InvalidLineArg of unit pptok

exception MacroArgUnclosed of unit pptok
exception MacroArgInnerParenUnclosed of unit pptok
exception MacroArgTooFew of unit pptok * int * int
exception MacroArgTooMany of unit pptok * int * int

type cond_expr =
  | Group of cond_expr pptok
  | Opaque of stream pptok
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

type line_dir = Stream of stream | Loc of int pptok option * int pptok

type pptok_expr =
  | Comments of comments pptok
  | Chunk of stream pptok
  | If of (cond_expr * pptok_expr * (pptok_expr option)) pptok
  | Def of (string pptok * stream) pptok
  | Fun of (string pptok * (string pptok list) * stream) pptok
  | Undef of string pptok pptok
  | Err of stream pptok
  | Pragma of stream pptok
  | Version of int pptok pptok
  | Extension of (string pptok * behavior pptok) pptok
  | Line of line_dir pptok
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

let check_version_base t =
  match fst t.v with
    | Hex | Oct -> error (InvalidVersionBase {t with v=fst t.v})
    | Dec -> ()

let check_line_base t =
  match fst t.v with
    | Hex | Oct -> error (InvalidLineBase {t with v=fst t.v})
    | Dec -> ()

let fuse_pptok = function
  | [] -> raise (ParserError "fusing empty pptok list")
  | (h::_) as tokl ->
      {span=span_of_list tokl;
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

let cexpr cs = let t = {(fuse_pptok cs) with v=cs} in
  Comments {t with scan=fun loc ->
            let loc,s = t.scan {loc with col=loc.col+2} in
	      {loc with col=loc.col+2}, "/*"^s^"*/"}
    
let pptok_expr_of_body bl def = match bl with
  | [] -> List { def with v=[] }
  | l -> fuse_pptok_expr l

let normalize_ppexpr e = (* TODO: remove Call tokens *)
  let rec loop ini prev = function
    | (Version v)::r ->
	(if not ini then error (HolyVersion (proj v)));
	(if v.v.v <> 100 then error (UnsupportedVersion v.v));
	loop false ((Version v)::prev) r
    | (List a)::(List b)::r ->
	loop ini prev ((fuse_pptok_expr (a.v@b.v))::r)
    | (List l)::r -> let ini,l = loop ini [] l.v in
	if List.length l = 1
	then match List.hd l with
	  | (Version _) as v -> loop ini (v::prev) r
	  | d -> loop ini prev (d::r)
	else loop ini ((fuse_pptok_expr l)::prev) r
    | (Chunk a)::(Chunk b)::r ->
	loop false prev
	  ((Chunk {(fuse_pptok [proj a; proj b]) with v=a.v@b.v})::r)
    | ((Comments _) as c)::r -> loop ini (c::prev) r
    | d::r -> loop false (d::prev) r
    | [] -> (ini,List.rev prev)
  in List.hd (snd (loop true [] [e]))

(*type pptok_expr =
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

type pptok_type =
  | Int of (base * int) pptok
  | Float of float pptok
  | Word of string pptok
  | Call of string pptok
  | Punc of Punc.tok pptok
  | Comma of Punc.tok pptok
  | Leftp of Punc.tok pptok
  | Rightp of Punc.tok pptok
*)
let lookup env (name,ms) =
  if Env.mem name ms then None
  else
    try
      let m = Env.find name env in
      Some { m with stream=List.map
	  (function
	    | Word w ->
	      let me = if m.name=None
		then Env.fold Env.add ms (snd w.v)
		else Env.fold Env.add ms (Env.add name () (snd w.v))
	      in Word {w with v=(fst w.v,me)}
	    | x -> x)
	  m.stream
	   }
    with Not_found -> None

let macro_arg_collect start stream =
  let rec outer opa args = function
    | (Rightp p)::r -> (args, r)
    | (Comma c)::r -> outer [] ((List.rev opa)::args) r
    | (Leftp p)::r ->
      let e, r = inner [Leftp p] r in
      outer (e@opa) args r
    | t::r -> outer (t::opa) args r
    | [] ->
      error (MacroArgUnclosed start);
      ((List.rev opa)::args,[])
  and inner opp = function
    | (Leftp p)::r ->
      let e, r = inner [Leftp p] r in
      inner (e@opp) r
    | (Rightp p)::r -> ((Rightp p)::opp, r)
    | t::r -> inner (t::opp) r
    | [] ->
      error (MacroArgInnerParenUnclosed
	       (proj_pptok_type (List.hd (List.rev opp))));
      (opp, [])
  in let args, r = outer [] [] stream in (List.rev args, r)

let defarg env name stream = Env.add name {name=None; arity=None; stream} env

let macro_expand ?(cond=false) env ptl =
  let extend_list fill lst len =
    lst@(Array.to_list (Array.make (len - (List.length lst)) fill))
  in
  let rec drop_head_list lst len = match len, lst with
    | 0,_ -> lst
    | x,_::r -> drop_head_list r (x-1)
    | _,[] -> raise (ParserError "drop_head_list empty list arg")
  in
  let rec loop env prev = function
    | (Int i)::r -> loop env ((Int i)::prev) r
    | (Float f)::r -> loop env ((Float f)::prev) r
    | (Word w)::r ->
      begin match cond, prev with
	| true,(Word {v=("defined",_)})::_
	| true,(Leftp _)::(Word {v=("defined",_)})::_ ->
	  loop env ((Word w)::prev) r (* defined is soooo "special" *)
	| _,_ -> begin match r with
	    | (Leftp p)::r -> begin match lookup env w.v with
		| Some ({arity=None; stream}) ->
		  loop env prev (stream@[Leftp p]@r)
		| Some ({arity=Some (a,binders); stream}) ->
		  let args, r = macro_arg_collect (proj p) r in
		  let arglen = List.length args in
		  let args = if arglen < a
		    then (error (MacroArgTooFew ((proj w),arglen,a));
			  extend_list [] args a)
		    else if arglen > a
		    then (error (MacroArgTooMany ((proj w),arglen,a));
			  List.rev (drop_head_list
				      (List.rev args)
				      ((List.length args) - a)))
		    else args
		  in
		  let appenv = List.fold_left2
		    (fun appenv binder arg ->
		      defarg appenv binder (loop env [] arg)) (* "prescan" *)
		    Env.empty binders args
		  in loop env prev ((loop appenv [] stream)@r)
		| None -> loop env ((Leftp p)::(Word w)::prev) r
	    end
	    | r -> begin match lookup env w.v with
		| Some ({arity=None; stream}) -> loop env prev (stream@r)
		| Some ({arity=Some _}) | None -> loop env ((Word w)::prev) r
	    end
	end
      end
    | (Call c)::r -> raise (ParserError "Call token in normalized stream")
    | (Punc p)::r -> loop env ((Punc p)::prev) r
    | (Comma c)::r -> loop env ((Comma c)::prev) r
    | (Leftp p)::r -> loop env ((Leftp p)::prev) r
    | (Rightp p)::r -> loop env ((Rightp p)::prev) r
    | [] -> List.rev prev
  in loop env [] ptl

let macro_expand_ppexpr env ppexpr =
  let rec loop env prev = function
    | (Comments c)::r -> loop env ((Comments c)::prev) r
    | (Chunk c)::r ->
      let s = macro_expand env c.v in
      loop env ((Chunk {(fuse_pptok
			   (List.map proj_pptok_type s)) with v=s})::prev) r
    | (If i)::r ->
      let cond,tb,fb = i.v in
      let cond = match cond with
	| Opaque ptlt ->
	  let s = macro_expand ~cond:true env ptlt.v in
	  Opaque {(fuse_pptok ~nl:false
		     (List.map proj_pptok_type s)) with v=s}
	| _ -> cond
      in match cond_eval env cond with
	| True ->
	| False ->
	| Deferred o ->
	  let tb = loop (guard env) [] tb in
	  let fb = match fb with
	    | Some fb -> loop (guard env) [] fb
	    | None -> None
	  in loop env ((If i)::prev) r (* TODO: rebuild If *)
    | (Def f)::r -> let env = define env (fst f.v).v (snd f.v) in
		    loop env ((Def f)::prev) r
    | (Fun f)::r -> let name,args,body = f.v in
		    let env = defun env name.v
		      (List.map (fun a -> a.v) args)
		      body
		    in loop env ((Fun f)::prev) r
    | (Undef u)::r -> let env = undef env u.v.v in
		      loop env ((Undef u)::prev) r
    | (Err e)::r -> loop env ((Err e)::prev) r
    | (Pragma p)::r -> loop env ((Pragma p)::prev) r
    | (Version v)::r -> loop env ((Version v)::prev) r
    | (Extension x)::r -> loop env ((Extension x)::prev) r
    | (Line l)::r -> loop env ((Line l)::prev) r
    | (List l)::r -> let env,l = loop env [] l in
		     loop env ((List l)::prev) r
    | [] -> cleanup_macros env (List.rev prev)
  in loop env [] ppexpr

let string_of_ppexpr_tree e =
  let rec loop indent p = function
    | (Comments _)::r -> loop indent (p^indent^"comments\n") r
    | (Chunk c)::r ->
	loop indent
	  (p^indent^"chunk of "^(string_of_int (List.length c.v))^"\n") r
    | (If i)::r ->
	let _,tb,fbo = i.v in
	let newind = indent^"  " in
	  loop indent (p^indent^"if\n"
		       ^(loop newind "" [tb])
		       ^(match fbo with
			   | None -> ""
			   | Some fb ->
			       indent^"else\n"
			       ^(loop newind "" [fb]))) r
    | (Def _)::r -> loop indent (p^indent^"define\n") r
    | (Fun _)::r -> loop indent (p^indent^"defun\n") r
    | (Undef _)::r -> loop indent (p^indent^"undef\n") r
    | (Err _)::r -> loop indent (p^indent^"error\n") r
    | (Pragma _)::r -> loop indent (p^indent^"pragma\n") r
    | (Version _)::r -> loop indent (p^indent^"version\n") r
    | (Extension _)::r -> loop indent (p^indent^"extension\n") r
    | (Line _)::r -> loop indent (p^indent^"line\n") r
    | (List l)::r -> loop indent (p^indent^"list\n"
				  ^(loop (indent^"  ") "" l.v)) r
    | [] -> p
  in loop "" "" [e]
