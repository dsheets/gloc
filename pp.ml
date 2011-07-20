open Printf

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
exception ReservedKeyword of string pptok

type cond_expr_result = Deferred | Result of Int32.t

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

let emit_newline = ref true
let file = ref {src=0; input=0}
let line = ref {src=1; input=1}
let errors : exn list ref = ref []
let error exc = errors := exc::!errors

let scan_of_comments cs start =
  let ({a}) = (List.hd cs).span in
  let start, pre = if (a.line.src - start.line.src) > 0 && !emit_newline
  then {start with line=a.line; col=0},
    String.make (a.line.src - start.line.src) '\n'
  else start,"" in
  let start, pre = List.fold_left
    (fun (start,pre) t ->
       let loc,s = t.scan start in
	 (loc,pre^s)
    ) ({start with col=start.col+2},pre^"/*") cs
  in ({start with col=start.col+2},pre^"*/")

let scan_of_string ({a;z}) (prec,postc) s = fun start ->
  let start,pre = match prec with
    | [] ->
      if !emit_newline
	&& (a.file.src <> start.file.src || (a.line.src - start.line.src) < 0)
      then {a with col=0},
	sprintf "\n#line %d %d\n" a.line.src a.file.src
      else if a.line.src - start.line.src > 0 && !emit_newline
      then {start with line=a.line; col=0},
	String.make (a.line.src - start.line.src) '\n'
      else start,""
    | cs -> scan_of_comments cs start
  in
  let cerr,cfix =
    if start.col <= a.col
    then 0,String.make (a.col - start.col) ' '
    else (start.col - a.col + 1)," "
  in let fin = {z with col=z.col+cerr} in
  let fin,post = match List.rev !postc with
    | [] -> fin,""
    | cs -> scan_of_comments cs fin
  in (fin, sprintf "%s%s%s%s" pre cfix s post)

let check_version_base t =
  match fst t.v with
    | Hex | Oct -> error (InvalidVersionBase {t with v=fst t.v})
    | Dec -> ()

let check_line_base t =
  match fst t.v with
    | Hex | Oct -> error (InvalidLineBase {t with v=fst t.v})
    | Dec -> ()

let check_reserved st =
  let reserved_keywords =
    ["asm"; "class"; "union"; "enum"; "typedef"; "template"; "this"; "packed";
     "goto"; "switch"; "default"; "inline"; "noinline"; "volatile"; "public";
     "static"; "extern"; "external"; "interface"; "flat"; "long"; "short";
     "double"; "half"; "fixed"; "unsigned"; "superp"; "input"; "output";
     "hvec2"; "hvec3"; "hvec4"; "dvec2"; "dvec3"; "dvec4"; "fvec2"; "fvec3";
     "fvec4"; "sampler1D"; "sampler3D"; "sampler1DShadow"; "sampler2DShadow";
     "sampler2DRect"; "sampler3DRect"; "sampler2DRectShadow"; "sizeof"; "cast";
     "namespace"; "using"]
  in if List.mem st.v reserved_keywords then error (ReservedKeyword st)

let fuse_pptok ?(nl=true) = function
  | [] -> raise (ParserError "fusing empty pptok list")
  | (h::_) as tokl ->
      {span=span_of_list tokl;
       scan=(fun start ->
	       let oenl = !emit_newline in
	       let () = emit_newline := nl in
	       let loc,rs = List.fold_left
		 (fun (loc,str) tok ->
		    let nloc,nstr = tok.scan loc in
		      (nloc,str^nstr))
		 (start,"") tokl
	       in emit_newline := oenl; (loc,rs)
	    );
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

let int_replace_word w i =
  let s = string_of_int i in
  let sl = String.length s in
  let span = {w.span with z={w.span.a with col=w.span.a.col+sl}} in
  let comments = ([], ref []) in
    Int { span; scan=scan_of_string span comments s; comments; v=(Dec,i) }

let lookup env w =
  let (name,ms) = w.v in
    if Env.mem name ms then None
    else if name = "__LINE__" then
      Some {name=None; arity=None;
	    stream=[int_replace_word w w.span.a.line.src]}
    else if name = "__FILE__" then
      Some {name=None; arity=None;
	    stream=[int_replace_word w w.span.a.file.src]}
    else try
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
	      | (Leftp p)::r -> begin match lookup env w with
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
		  | None ->
		      check_reserved {w with v=(fst w.v)};
		      loop env ((Leftp p)::(Word w)::prev) r
		end
	      | r -> begin match lookup env w with
		  | Some ({arity=None; stream}) -> loop env prev (stream@r)
		  | Some ({arity=Some _}) | None ->
		      check_reserved {w with v=(fst w.v)};
		      loop env ((Word w)::prev) r
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

let coerce_binop fn x y = if fn x y then Int32.one else Int32.zero

let rec cond_eval env = function
  | Group p -> cond_eval env p.v
  | Opaque o -> Deferred
  | Constant c -> Result (Int32.of_int c.v)
  | Defined d -> if Env.mem d.v.v env || d.v.v="__LINE__" || d.v.v="__FILE__"
    then Result Int32.one else Result Int32.zero
  | Pos p -> cond_eval env p.v
  | Neg n -> cond_unop env Int32.neg n.v
  | BitNot b -> cond_unop env Int32.lognot b.v
  | Not n -> cond_unop env
      (function x when x=Int32.zero -> Int32.one | _ -> Int32.zero) n.v
  | Mul m -> cond_binop env Int32.mul m.v
  | Div d -> cond_binop env Int32.div d.v
  | Mod m -> cond_binop env Int32.rem m.v
  | Add a -> cond_binop env Int32.add a.v
  | Sub s -> cond_binop env Int32.sub s.v
  | BitLeft b -> cond_binop env
      (fun x y -> Int32.shift_left x (Int32.to_int y)) b.v
  | BitRight b -> cond_binop env
      (fun x y -> Int32.shift_right x (Int32.to_int y)) b.v
  | Lt l -> cond_binop env (coerce_binop (<)) l.v
  | Gt g -> cond_binop env (coerce_binop (>)) g.v
  | Lte l -> cond_binop env (coerce_binop (<=)) l.v
  | Gte g -> cond_binop env (coerce_binop (>=)) g.v
  | Eq e -> cond_binop env (coerce_binop (=)) e.v
  | Neq n -> cond_binop env (coerce_binop (<>)) n.v
  | BitAnd b -> cond_binop env Int32.logand b.v
  | BitXor b -> cond_binop env Int32.logxor b.v
  | BitOr b -> cond_binop env Int32.logor b.v
  | And c -> let a,b = c.v in begin match cond_eval env a with
      | Result x when x=Int32.zero -> Result Int32.zero
      | Deferred -> Deferred
      | Result _ -> begin match cond_eval env b with
	  | Result x when x=Int32.zero -> Result Int32.zero
	  | Deferred -> Deferred
	  | Result _ -> Result Int32.one
	end
    end
  | Or d -> let a,b = d.v in begin match cond_eval env a with
      | Result x when x=Int32.zero -> begin match cond_eval env b with
	  | Result x when x=Int32.zero -> Result Int32.zero
	  | Result _ -> Result Int32.one
	  | Deferred -> Deferred
	end
      | Result _ -> Result Int32.one
      | Deferred -> Deferred
    end
and cond_unop env f a =
  match cond_eval env a with
    | Result i -> Result (f i)
    | _ -> Deferred
and cond_binop env f (a,b) =
  match (cond_eval env a),(cond_eval env b) with
    | Result x, Result y -> Result (f x y)
    | _, _ -> Deferred

let preprocess_ppexpr env ppexpr =
  let rec loop env prev = function
    | (Comments c)::r -> loop env prev r
    | (Chunk c)::r -> let s = macro_expand env c.v in
	loop env ((Chunk {(fuse_pptok
			     (List.map proj_pptok_type s)) with v=s})::prev) r
    | (If i)::r ->
      let cond,tb,fb = i.v in
      let cond = match cond with
	| Opaque ptlt ->
	  let s = macro_expand ~cond:true env ptlt.v in
	  let ts = tokenize s in
	    parse_cond_expr {(fuse_pptok ~nl:false
				(List.map proj_pptok_type s)) with v=s}
	| _ -> cond
      in begin match cond_eval env cond with
	| Result x when x=Int32.zero ->
	    (match fb with
	       | Some fb -> loop env prev (fb::r)
	       | None -> loop env prev r)
	| Result _ -> loop env prev (tb::r)
	| Deferred ->
	    List.append
	      (loop env prev (tb::r))
	      (match fb with
		 | Some fb -> loop env prev (fb::r)
		 | None -> [])
	end
    | (Def f)::r -> let env = define env (fst f.v).v (snd f.v) in
	check_reserved_macro (fst f.v).v;
	loop env prev r
    | (Fun f)::r -> let name,args,body = f.v in
      let env = defun env name.v
	(List.map (fun a -> a.v) args)
	body
      in check_reserved_macro name.v;
	loop env prev r
    | (Undef u)::r -> let env = undef env u.v.v in
	loop env prev r
    | (Err e)::r -> error (ErrorDirective e); loop env prev r
    | (Pragma p)::r -> loop (register_pragma env p) prev r
    | (Version v)::r -> loop env prev r
    | (Extension x)::r -> loop (register_extension env x) prev r
    | (Line l)::r -> loop env prev r
    | (List l)::r -> loop env prev (l::r)
    | [] -> env, (List.rev prev)
  in loop env [] [ppexpr]

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
