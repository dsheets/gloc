(* Copyright (c) 2011 David Sheets, Ashima Arts.
 * All rights reserved.
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

open Printf
open Pp_lib
open Ce_lib

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

let check_reserved_macro_redefine m =
  if String.length m.v > 2 && String.sub m.v 0 3 = "GL_"
  then error (RedefineReservedMacro m)
  else if Str.string_match (Str.regexp ".*__") m.v 0
  then error (RedefineReservedMacro m)

let check_reserved_macro_undefine m =
  if String.length m.v > 2 && String.sub m.v 0 3 = "GL_"
  then error (UndefineReservedMacro m)
  else if Str.string_match (Str.regexp ".*__") m.v 0
  then error (UndefineReservedMacro m)

let rec map_cond_expr_stream f = function
  | Group g -> Group { g with v=map_cond_expr_stream f g.v }
  | Opaque o -> Opaque { o with v=f o.v }
  | Constant c -> Constant c
  | Defined d -> Defined d
  | Pos p -> Pos (map_unop_stream f p)
  | Neg n -> Neg (map_unop_stream f n)
  | BitNot b -> BitNot (map_unop_stream f b)
  | Not n -> Not (map_unop_stream f n)
  | Mul m -> Mul (map_binop_stream f m)
  | Div d -> Div (map_binop_stream f d)
  | Mod m -> Mod (map_binop_stream f m)
  | Add a -> Add (map_binop_stream f a)
  | Sub s -> Sub (map_binop_stream f s)
  | BitLeft b -> BitLeft (map_binop_stream f b)
  | BitRight b -> BitRight (map_binop_stream f b)
  | Lt l -> Lt (map_binop_stream f l)
  | Gt g -> Gt (map_binop_stream f g)
  | Lte l -> Lte (map_binop_stream f l)
  | Gte g -> Gte (map_binop_stream f g)
  | Eq e -> Eq (map_binop_stream f e)
  | Neq n -> Neq (map_binop_stream f n)
  | BitAnd b -> BitAnd (map_binop_stream f b)
  | BitXor b -> BitXor (map_binop_stream f b)
  | BitOr b -> BitOr (map_binop_stream f b)
  | And a -> And (map_binop_stream f a)
  | Or o -> Or (map_binop_stream f o)
and map_unop_stream f q = { q with v=map_cond_expr_stream f q.v }
and map_binop_stream f q = let l,r = q.v in
  { q with v=(map_cond_expr_stream f l, map_cond_expr_stream f r) }

let normalize_calls s =
  let split_call c =
    let ppos = { c.span.z with col=c.span.z.col-1 } in
    let wspan = { c.span with z=ppos } in
      (Word { c with span=wspan;
		scan=scan_of_string wspan c.comments c.v;
		v=(c.v,Env.empty) },
       Leftp { span={ c.span with a=ppos };
	       scan=scan_of_string { c.span with a=ppos } ([],ref []) "(";
	       comments=([],ref []); v=Punc.LEFT_PAREN })
  in
  let rec loop prev = function
    | (Call c)::r -> let w,p = split_call c in loop (p::w::prev) r
    | t::r -> loop (t::prev) r
    | [] -> List.rev prev
  in loop [] s

let normalize_ppexpr e =
  let rec loop ini prev = function
    | (Version v)::r ->
	(if not ini then error (HolyVersion (proj v)));
	(if v.v.v <> 100 then error (UnsupportedVersion v.v));
	loop false ((Version v)::prev) r
    | (List l)::r -> loop ini prev (l.v@r)
    | (Chunk a)::(Chunk b)::r ->
	loop false prev
	  ((Chunk {(fuse_pptok [proj a; proj b]) with v=a.v@b.v})::r)
    | (Chunk c)::r ->
	loop false ((Chunk { c with v=normalize_calls c.v})::prev) r
    | ((Comments _) as c)::r -> loop ini (c::prev) r
    | (If i)::r -> let c,t,f = i.v in
      let tb = snd (loop false [] [t]) in
	loop false
	  ((If { i with
		   v=(map_cond_expr_stream normalize_calls c,
		      (if tb=[] then t else fuse_pptok_expr tb),
		      match f with
			| Some f -> let fb = snd (loop false [] [f]) in
			    Some (if fb=[] then f else fuse_pptok_expr fb)
			| None -> None)
	       })::prev) r
    | (Def d)::r -> let n,s = d.v in
	loop false ((Def {d with v=(n,normalize_calls s)})::prev) r
    | (Fun f)::r -> let n,a,s = f.v in
	loop false ((Fun {f with v=(n,a,normalize_calls s)})::prev) r
    | (Pragma p)::r ->
	loop false ((Pragma {p with v=normalize_calls p.v})::prev) r
    | d::r -> loop false (d::prev) r
    | [] -> (ini,List.rev prev)
  in let ne = snd (loop true [] [e]) in
    if ne=[] then e else fuse_pptok_expr ne

let int_replace_word w i =
  let s = string_of_int i in
  let sl = String.length s in
  let span = {w.span with z={w.span.a with col=w.span.a.col+sl}} in
  let comments = ([], ref []) in
    Int { span; scan=scan_of_string span comments s; comments; v=(Dec,i) }

let omacro name stream = { name=Some name; args=None; stream }
let defarg env name stream =
  let stream = fun _ -> stream in
    { env with macros=Env.add name {name=None; args=None; stream} env.macros }
let define env name stream =
  let stream = fun _ -> stream in
    { env with macros=Env.add name {name=Some name; args=None; stream}
	env.macros }
let defun env name args stream =
  let stream = fun _ -> stream in
    { env with macros=Env.add name {name=Some name; args=Some args; stream}
	env.macros }
let undef env name = { env with macros=Env.remove name env.macros }
let register_pragma env p = env (* TODO: care *)
let register_extension env x =
  { env with extensions=Env.add (fst x.v).v (snd x.v).v env.extensions }

let lookup env w =
  let (name,ms) = w.v in
    if Env.mem name ms then None
    else if name = "__LINE__" then
      Some {name=None; args=None;
	    stream=fun _ -> [int_replace_word w w.span.a.line.src]}
    else if name = "__FILE__" then
      Some {name=None; args=None;
	    stream=fun _ -> [int_replace_word w w.span.a.file.src]}
    else try
      let m = Env.find name env.macros in
	Some { m with stream=fun _ -> List.map
		 (function
		    | Word w ->
			let me = if m.name=None
			then Env.fold Env.add ms (snd w.v)
			else Env.fold Env.add ms (Env.add name () (snd w.v))
			in Word {w with v=(fst w.v,me)}
		    | x -> x)
		 (m.stream w.span.a)
	     }
    with Not_found -> None

let macro_arg_collect start stream =
  let rec outer opa args = function
    | (Rightp p)::r -> ((List.rev opa)::args, r)
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
		  | Some ({args=None; stream}) ->
		      loop env prev ((stream w.span.a)@[Leftp p]@r)
		  | Some ({args=Some binders; stream}) ->
		      let blen = List.length binders in
		      let args, r = macro_arg_collect (proj p) r in
			(* nullary macro fns are a special case of unary *)
		      let args = if blen=0 && args=[[]] then [] else args in
		      let arglen = List.length args in
		      let args = if arglen < blen
		      then (error (MacroArgTooFew ((proj w),arglen,blen));
			    extend_list [] args blen)
		      else if arglen > blen
		      then (error (MacroArgTooMany ((proj w),arglen,blen));
			    List.rev (drop_head_list
					(List.rev args)
					(arglen - blen)))
		      else args
		      in
		      let appenv = List.fold_left2
			(fun appenv binder arg ->
			   defarg appenv binder (loop env [] arg)) (* "prescan" *)
			{macros=Env.empty;
			 extensions=Env.empty;
			 inmacros=[]}
			binders args
		      in loop env prev ((loop appenv [] (stream w.span.a))@r)
		  | None ->
		      check_reserved {w with v=(fst w.v)};
		      loop env ((Leftp p)::(Word w)::prev) r
		end
	      | r -> begin match lookup env w with
		  | Some ({args=None; stream}) ->
		      loop env prev ((stream w.span.a)@r)
		  | Some ({args=Some _}) | None ->
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
  | Opaque o -> Deferred [{ o with v=snd (o.scan o.span.a) }]
  | Constant c -> Result (Int32.of_int c.v)
  | Defined d ->
      if Env.mem d.v.v env.macros
	|| d.v.v="__LINE__" || d.v.v="__FILE__"
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
      | Deferred i -> Deferred i
      | Result _ -> begin match cond_eval env b with
	  | Result x when x=Int32.zero -> Result Int32.zero
	  | Deferred i -> Deferred i
	  | Result _ -> Result Int32.one
	end
    end
  | Or d -> let a,b = d.v in begin match cond_eval env a with
      | Result x when x=Int32.zero -> begin match cond_eval env b with
	  | Result x when x=Int32.zero -> Result Int32.zero
	  | Result _ -> Result Int32.one
	  | Deferred i -> Deferred i
	end
      | Result _ -> Result Int32.one
      | Deferred i -> Deferred i
    end
and cond_unop env f a =
  match cond_eval env a with
    | Result i -> Result (f i)
    | Deferred i -> Deferred i
and cond_binop env f (a,b) =
  match (cond_eval env a),(cond_eval env b) with
    | Result x, Result y -> Result (f x y)
    | Deferred i, Result _ -> Deferred i
    | Result _, Deferred i -> Deferred i
    | Deferred i, Deferred j -> Deferred (j@i)

let preprocess_ppexpr env ppexpr =
  let rec loop env prev = function
    | (Comments c)::r -> loop env prev r
    | (Chunk c)::r -> let s = macro_expand env c.v in
	if s=[] then loop env prev r
	else loop env
	  ((Chunk {(fuse_pptok
		      (List.map proj_pptok_type s)) with v=s})::prev) r
    | (If i)::r ->
	let cond,tb,fb = i.v in
	let cond = match cond with
	  | Opaque ptlt ->
	      let s = macro_expand ~cond:true env ptlt.v in
	      let ts = ce_tokenize s in
		begin try parse_cond_expr (ce_lexerfn ts)
		with Esslpp_ce.Error ->
		  let t = {(fuse_pptok ~nl:false
			      (List.map proj_pptok_type s)) with v=s}
		  in error (PPCondExprParseError t); Opaque t
		end
	  | _ -> cond
	in begin match cond_eval env cond with
	  | Result x when x=Int32.zero ->
	    (match fb with
	      | Some fb -> loop env prev (fb::r)
	      | None -> loop env prev r)
	  | Result _ -> loop env prev (tb::r)
	  | Deferred i ->
	    let env = { env with inmacros=i@env.inmacros } in
	    List.append
	      (loop env prev (tb::r))
	      (match fb with
		| Some fb -> loop env prev (fb::r)
		| None -> loop env prev r)
	end
    | (Def f)::r -> let env = define env (fst f.v).v (snd f.v) in
	check_reserved_macro_redefine (fst f.v);
	loop env prev r
    | (Fun f)::r -> let name,args,body = f.v in
      let env = defun env name.v
	(List.map (fun a -> a.v) args)
	body
      in check_reserved_macro_redefine name;
	loop env prev r
    | (Undef u)::r -> let env = undef env u.v.v in
	check_reserved_macro_undefine u.v;
	loop env prev r
    | (Err e)::r -> error (ErrorDirective e); loop env prev r
    | (Pragma p)::r -> loop (register_pragma env p) prev r
    | (Version v)::r -> loop env prev r
    | (Extension x)::r -> loop (register_extension env x) prev r
    | (Line l)::r -> loop env prev r
    | (List l)::r -> loop env prev (l.v@r)
    | [] -> if prev=[] then []
      else [env, fuse_pptok_expr (List.rev prev)]
  in loop env [] [ppexpr]

let string_of_ppexpr start e = snd ((proj_pptok_expr e).scan start)

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
