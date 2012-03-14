(* Copyright (c) 2012 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

open Printf
open Pp_lib
open Essl_lib
open Glo_lib

type require = Pdir of string | Edir of string * behavior | Vdir of int
type filenum = Num of int | Ref of int * string * string

exception DissolveError of string * unit pptok

(*
let rec print_stream = function
  | [] -> ()
  | (Int _)::r -> print_endline "Int"; print_stream r
  | (Float _)::r -> print_endline "Float"; print_stream r
  | (Word _)::r -> print_endline "Word"; print_stream r
  | (Call _)::r -> print_endline "Call"; print_stream r
  | (Punc _)::r -> print_endline "Punc"; print_stream r
  | (Comma _)::r -> print_endline "Comma"; print_stream r
  | (Leftp _)::r -> print_endline "Leftp"; print_stream r
  | (Rightp _)::r -> print_endline "Rightp"; print_stream r
*)

let apply_to_pp_if fn ({v=(ce,tb,ofb)} as t) =
  let tm, tb = match fn tb with
    | tm, Some tb -> tm, tb
    | tm, None -> tm, empty_pptok_expr tb
  in
  let fm, ofb = match ofb with
    | None -> [], None
    | Some fb -> fn fb
  in tm@fm, Some (synth_pp_if { t with v=(ce,tb,ofb) })

let apply_to_pp_list fn ({v=ppel}) =
  let ml, ppel = List.fold_left
    (fun (ml,ppel) ppe -> match fn ppe with
      | sml, None -> (sml@ml,ppel)
      | sml, Some ppe -> (sml@ml,ppe::ppel)
    )
    ([],[]) ppel
  in ml, Some (fuse_pptok_expr (List.rev ppel))

let rec process_requires e = match e with
  | Comments _ | Chunk _ | Def _ | Fun _ | Undef _
  | Err _ | Line _ -> [], Some e
  | Extension {v=({v=name},{v=behavior})} ->
    [Edir (name, behavior)], None
  | Pragma ({v=[Word {v=("STDGL",_)};
                Word {v=("invariant",_)};
                Leftp _;
                Word {v=("all",_)};
                Rightp _;
               ]} as t) -> [Pdir (snd (t.scan t.span.a))], None
  | Pragma {v} -> [], Some e
  | Version itt -> [Vdir itt.v.v], None
  | If t -> apply_to_pp_if process_requires t
  | List t -> apply_to_pp_list process_requires t

let line_ref_re = Re_str.regexp "\\([^ ]*\\)#n=\\([^ ]+\\)"

let extract_name_frag fn = function
  | [] -> Num fn
  | h::_ -> begin
    try ignore (Re_str.search_forward line_ref_re h 0);
        Ref (fn, Re_str.matched_group 1 h, Re_str.matched_group 2 h)
    with Not_found -> Num fn
  end

let rec process_line e = match e with
  | Comments _ | Chunk _ | Def _ | Fun _ | Undef _
  | Err _ | Pragma _ | Extension _ | Version _ -> [], Some e
  | Line ({ v=(Some ft,_) } as t) ->
    let _, postc = ft.comments in
    let postc = List.flatten
      (List.map (fun t -> List.map (fun t -> t.v) t.v) !postc)
    in [extract_name_frag ft.v postc], Some (synth_pp_line_armored t)
  | Line _ -> [], Some e
  | If t -> apply_to_pp_if process_line t
  | List t -> apply_to_pp_list process_line t

let directives_of_requires requires =
  List.fold_left
    (fun (pl,el,vo) -> function
      | Pdir s -> (s::pl,el,vo)
      | Edir (n,b) -> (pl,(n,string_of_behavior b)::el,vo)
      | Vdir n -> (pl,el,Some n)
    ) ([],[],None) requires

(* TODO: enforce ESSL 1-declare, 1-define rule *)
let prototypes slenv = Sl_lib.SymMap.fold
  (fun k bindings l ->
    if List.for_all (fun b -> not (Sl_lib.definitionp b)) bindings
    then k::l else l
  ) (List.hd (List.rev slenv.Sl_lib.ctxt)) []

let get_insyms envs =
  let protos = List.fold_left (fun l e -> (prototypes e)@l) [] envs in
  List.fold_left
    (fun l e -> List.fold_left
      (fun l s -> (* TODO: inference *)
        if List.mem s l then l
        else if List.mem_assoc s builtins then l
        else s::l)
      l e.Sl_lib.opensyms)
    protos envs

let get_outsyms envs =
  List.fold_left
    (fun l e -> Sl_lib.SymMap.fold
      (fun k bindings l -> (* TODO: inference *)
        if (List.mem k l) or not (List.exists Sl_lib.definitionp bindings)
        then l else k::l)
      (List.hd (List.rev e.Sl_lib.ctxt)) l)
    [] envs

(* maintain order *)
let unique idx l = List.rev
  (snd (List.fold_left
          (fun (m,l) v ->
            if Sl_lib.SymMap.mem (idx v) m then (m,l)
            else (Sl_lib.SymMap.add (idx v) v m, v::l)
          ) (Sl_lib.SymMap.empty,[]) l))

let env_map idx f ppl =
  unique idx (List.flatten (List.map (fun (env,_) -> f env) ppl))
let get_inmac env = List.map (fun t -> t.v) env.inmacros
let get_opmac env = Env.fold (fun _ mac l -> mac::l) env.macros []

let slenv_of_stream s =
  let hot = ref None in
  try parse_essl (essl_lexerfn hot s)
  with Essl.Error ->
    let span, scan = match !hot with Some t -> t.span, snd (t.scan t.span.a)
      | None -> {a=start_loc;z=start_loc}, "__BOF__" in
    raise (Essl_lib.EsslParseError ("'"^scan^"'",span))

let create_unit expr ppl =
  let id x = x in
  let inmac = env_map id get_inmac ppl in
  let opmac = List.fold_right
    (function
      | {name=None} -> id
      | {name=Some n} -> fun l -> n::l)
    (env_map (fun {name} -> match name with None -> "" | Some s -> s)
       get_opmac ppl) [] in
  let envs = List.map
    (fun (_,ppexpr) -> slenv_of_stream (stream_of_pptok_expr ppexpr))
    ppl in
  let file_nums, expr = match process_line expr with
    | file_nums, Some e -> file_nums, e
    | file_nums, None -> file_nums, empty_pptok_expr expr
  in
  let file_nums, start = match expr with
    | List {v=(Line {v=(Some _,_)})::_}
    | Line {v=(Some _,_)} ->
      file_nums, {file={src=0;input=0}; line={src=1;input=1}; col=0}
    | _ ->
      ((Num 0)::file_nums), {file={src=(-1);input=(-1)}; line={src=1;input=1}; col=0}
  in
  let (pdir,edir,vdir),expr = match process_requires expr with
    | requires, Some e -> directives_of_requires requires, e
    | requires, None -> directives_of_requires requires, empty_pptok_expr expr
  in (* TODO: rename GLOC_* to GLOC_GLOC_* *)
  ({pdir; edir; vdir;
    insym=get_insyms envs; outsym=get_outsyms envs;
    inmac; opmac; outmac=[];
    source=(snd ((proj_pptok_expr expr).scan start))},
   List.rev
     (unique (function Num fn | Ref (fn,_,_) -> string_of_int fn) file_nums))

let empty_ppenv lang =
  {macros=Env.empty;
   builtin_macros=builtin_macros_of_language lang;
   extensions=Env.empty;
   inmacros=[]}

let link_of_filenum = function
  | Num n -> (string_of_int n, sprintf "#n=%d" n)
  | Ref (n, fn, frag) -> (string_of_int n, sprintf "#n=%s" frag)

let compile ?meta lang fn origexpr ppl =
  let target = Language.target_of_language lang in
  let body_unit, file_nums = create_unit origexpr ppl in
  let linkmap = List.map link_of_filenum file_nums in
  Glo {glo=glo_version; target; meta; units=[|body_unit|]; linkmap}

let input_decl_a x =
  (x.Sl_lib.qualt.span.a.line.input,x.Sl_lib.qualt.span.a.col)
let input_decl_z x =
  (x.Sl_lib.symt.span.z.line.input,x.Sl_lib.symt.span.z.col)

let input_pptok_type_a pt =
  let t = proj_pptok_type pt in
  (t.span.a.line.input,t.span.a.col)

let input_pptok_type_z pt =
  let t = proj_pptok_type pt in
  (t.span.z.line.input,t.span.z.col)

let input_decl_cmp x y =
  let x_a = input_decl_a x in
  let x_z = input_decl_z x in
  let y_a = input_decl_a y in
  let y_z = input_decl_z y in
  let a = compare x_a y_a in
  let z = compare x_z y_z in
  if a>(-1) && z=(-1) || a<1 && z=1 then z else a

let unit_of_binding (n, bs) =
  let start = {file={src=(-1);input=(-1)}; line={src=1;input=1}; col=0} in
  let _, source = List.fold_left
    (fun (loc,s) {Sl_lib.qualt; Sl_lib.typet; Sl_lib.symt} ->
      let loc,qs = qualt.scan loc in
      let (loc,ts) = match Sl_lib.usertype_of_bind symt.v with
        | None -> (typet.scan loc)
        | Some ut ->
          (scan_of_string
             {typet.span with
               z={typet.span.a with
                 col=typet.span.a.col+(String.length ut)}}
             ([],ref []) ut loc)
      in
      let loc,ss = symt.scan loc in
      match symt.v with
        | Sl_lib.Fun (_,_,_) -> (loc,s^qs^ts^ss)
        | _ -> (loc,s^qs^ts^ss^";")
    ) (start,"") (List.rev bs)
  in {pdir=[]; edir=[]; vdir=None;
      insym=[]; outsym=[]; inmac=[]; outmac=[]; opmac=[];
      source}

(*
            let streams = List.map (fun b -> match b with
              | (_,[]) -> (b,[])
              | (n,bs) ->
                let s = input_decl_a (List.hd bs) in
                let e = input_decl_z (List.hd (List.rev bs)) in
                (b,List.fold_right
                  (fun pt l ->
                    let ta = input_pptok_type_a pt in
                    let tz = input_pptok_type_z pt in
                    if ta >= s && tz <= e then pt::l else l
                  ) c.v [])
            ) bindings in
*)

let dissolve ?meta lang fn origexpr ppl =
  let target = Language.target_of_language lang in
  let pglo = {glo=glo_version; target; meta; units=[||]; linkmap=[]} in
  let append_unit file_nums requires oglo u =
    let (pdir,edir,vdir) = directives_of_requires requires in
    {oglo with units=Array.append oglo.units [|{u with vdir; edir; pdir}|];
      linkmap=(List.map link_of_filenum file_nums)@oglo.linkmap}
  in
  let rec loop env (dfn,oglo) glom requires = function
    | (Comments c)::r -> comments env (dfn,oglo) glom c.v requires r
    | ((Chunk c) as pp)::r ->
      let ul, linkmap = begin
        try let slenv = slenv_of_stream (stream_of_pptok_expr pp) in
            let bindings = Sl_lib.SymMap.bindings
              (List.hd (List.rev slenv.Sl_lib.ctxt)) in
            List.map (fun b ->
              let u = unit_of_binding b in
              let ppexpr = parse lang u.source in
              let slenv = slenv_of_stream (stream_of_pptok_expr ppexpr) in
              {u with insym=get_insyms [slenv]; outsym=get_outsyms [slenv]}
            ) (List.sort (* TODO: interleaved overloads *)
                 (fun (_,a) (_,b) ->
                   input_decl_cmp (List.hd a) (List.hd b))
                 bindings),
            List.fold_left (fun l -> function
              | (_,b::_) -> (Num b.Sl_lib.symt.span.a.file.src)::l
              | (_,[]) -> l
            ) [] bindings
        with Essl_lib.EsslParseError _ -> (* TODO: interleaved ppdir *)
          let macros, _ = Pp.macro_expand env c.v in
          let ppl = Pp.preprocess_ppexpr env pp in
          (*maybe_fatal_error PPError;*)
          let u,linkmap = create_unit pp ppl in
          [{u with opmac=[];
            inmac=List.map (function
              | {name=None} -> ""
              | {name=Some s} -> s)
              (unique (function {name=None} -> "" | {name=Some n} -> n) macros)}],
          linkmap
      end in
      loop env (dfn,List.fold_left
        (fun glo u -> append_unit linkmap requires glo u)
        oglo ul) glom requires r
    | ((If _) as pp)::r ->
      let ppl = Pp.preprocess_ppexpr (empty_ppenv lang) pp in
      (*maybe_fatal_error PPError;*)
      let u, file_nums = create_unit pp ppl in
      let u = {u with outmac=u.opmac; opmac=[]} in
      let o = env_map (fun {name} -> match name with None -> "" | Some s -> s)
        get_opmac ppl in
      loop {env with macros=List.fold_left
          (fun e m -> Env.add (match m.name with None -> "" | Some n -> n) m e)
          env.macros o}
        (dfn,append_unit file_nums requires oglo u)
        glom requires r
    | ((Def pptok) as pp)::r ->
      let env = Pp.define env pptok in
      let u, file_nums = create_unit pp
        [Pp.define (empty_ppenv lang) pptok,pp] in
      let u = {u with outmac=u.opmac; opmac=[]} in
      loop env (dfn,append_unit file_nums requires oglo u) glom requires r
    | ((Fun pptok) as pp)::r ->
      let env = Pp.defun env pptok in
      let u, file_nums = create_unit pp
        [Pp.defun (empty_ppenv lang) pptok,pp] in
      let u = {u with outmac=u.opmac; opmac=[]} in
      loop env (dfn,append_unit file_nums requires oglo u) glom requires r
    | (Undef {v=m})::r -> loop (Pp.undef env m.v) (dfn,oglo) glom requires r
    | (Err _)::r -> loop env (dfn,oglo) glom requires r
    | (Pragma t)::r ->
      loop env (dfn,oglo) glom ((Pdir (snd (t.scan t.span.a)))::requires) r
    | ((Version _) as pp)::r
    | ((Extension _) as pp)::r ->
      let req, _ = process_requires pp in
      loop env (dfn,oglo) glom (req@requires) r
    | ((Line ldt) as pp)::r ->
      let c, _ = ldt.comments in
      if c=[] then
        let file_num = List.hd (fst (process_line pp)) in (* Line is safe *)
        let linkmap = (link_of_filenum file_num)::oglo.linkmap in
        let dfn = match file_num with
          | Num _ | Ref (_,"",_) -> dfn | Ref (_,fn,_) -> fn
        in loop env (dfn,{oglo with linkmap}) glom requires r
      else
        comments env (dfn,oglo) glom c requires
          ((Line {ldt with comments=([],snd ldt.comments)})::r)
    | (List {v})::r -> loop env (dfn,oglo) glom requires (v@r)
    | [] -> List.rev ((dfn,Glo {oglo with linkmap=unique fst oglo.linkmap})::glom)
  and comments env (dfn,oglo) glom c =
    let glom = if 0=(Array.length oglo.units) then glom
      else ((dfn,Glo {oglo with linkmap=unique fst oglo.linkmap})::glom)
    in match extract_meta c with
      | NoMeta -> loop env (dfn,oglo) glom
      | EndMeta -> loop env (dfn,pglo) glom
      | NewMeta meta -> loop env (dfn,{pglo with meta=Some meta}) glom
  in let glo_alist = loop (empty_ppenv lang) (fn,pglo) [] [] [origexpr] in
     if 1=(List.length glo_alist) then snd (List.hd glo_alist)
     else Glom glo_alist

