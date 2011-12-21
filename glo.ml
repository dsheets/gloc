open Printf
open Pp_lib
open Essl_lib
open Glo_lib

let unique lst =
  let rec dedupe prev = function
    | x::y::r when x=y -> dedupe prev (x::r)
    | x::r -> dedupe (x::prev) r
    | [] -> prev
  in dedupe [] (List.sort compare lst)

let create_header expr =
  (* TODO: lift extension exprs *)
  (* TODO: lift pragma invariant *)
  {insym=[]; outsym=[]; inmac=[]; opmac=[]; outmac=[]; source=""}
let create_body expr envs =
  (* TODO: abstract *)
  let apply_to_if fn ({v=(ce,tb,ofb)} as t) =
    let tm, tb = match fn tb with
      | tm, Some tb -> tm, tb
      | tm, None -> tm, empty_pptok_expr tb
    in
    let fm, ofb = match ofb with
      | None -> [], None
      | Some fb -> fn fb
    in tm@fm, Some (synth_pp_if { t with v=(ce,tb,ofb) })
  in
  let apply_to_list fn ({v=ppel}) =
    let ml, ppel = List.fold_left
      (fun (ml,ppel) ppe -> match fn ppe with
	 | sml, None -> (sml@ml,ppel)
	 | sml, Some ppe -> (sml@ml,ppe::ppel)
      )
      ([],[]) ppel
    in ml, Some (fuse_pptok_expr (List.rev ppel))
  in
  let rec process_version e = match e with
    | Comments _ | Chunk _ | Def _ | Fun _ | Undef _
    | Err _ | Pragma _ | Extension _ | Line _ -> [], Some e
    | Version itt -> ["GLOC_VERSION_"^(string_of_int itt.v.v)], None
    | If t -> apply_to_if process_version t
    | List t -> apply_to_list process_version t
  in
  let rec process_line prefix e = let prefixl = String.length prefix in
    match e with
      | Comments _ | Chunk _ | Def _ | Fun _ | Undef _
      | Err _ | Pragma _ | Extension _ | Version _ -> [], Some e
      | Line ({ v=(Some ft,_) } as t) -> let linedir =
	  Line {t with
		  span={t.span with
			  z={t.span.z with col=t.span.z.col+prefixl}};
		  scan=fun loc ->
		    let loc, s = t.scan loc in
		    let ll = String.length s in
		    let fns = string_of_int ft.v in
		    let fnl = String.length fns in
		      ({loc with col=loc.col+prefixl},
		       (String.sub s 0 (ll-fnl))^prefix^fns)
	       }
	in [ft.v], Some linedir
      | Line _ -> [], Some e
      | If t -> apply_to_if (process_line prefix) t
      | List t -> apply_to_list (process_line prefix) t
  in
  let syml, expr = match process_version expr with
    | syml, Some e -> syml, e
    | syml, None -> syml, empty_pptok_expr expr
  in
  let file_nums, expr = match process_line !file_prefix expr with
    | file_nums, Some e -> file_nums, e
    | file_nums, None -> file_nums, empty_pptok_expr expr
  in
    (* TODO: enforce ESSL 1-declare, 1-define rule *)
  let prototypes = List.fold_left
    (fun l e -> Sl_lib.SymMap.fold
       (fun k bindings l ->
	  if List.for_all (fun b -> not (Sl_lib.definitionp b)) bindings
	  then k::l else l
       ) (List.hd (List.rev e.Sl_lib.ctxt)) l)
    [] envs
  in
  let file_nums, start = match expr with
    | List {v=(Line {v=(Some _,_)})::_}
    | Line {v=(Some _,_)} ->
	file_nums, {file={src=0;input=0}; line={src=1;input=1}; col=0}
    | _ ->
	(0::file_nums), {file={src=(-1);input=(-1)}; line={src=1;input=1}; col=0}
  in	
  (* TODO: rename GLOC_* to GLOC_GLOC_* *)
  (* TODO: remove extension *)
    ({insym=List.fold_left
	 (fun l e -> List.fold_left
	    (fun l s -> (* TODO: inference *)
	       if List.mem s l then l
	       else if List.mem_assoc s builtins then l
	       else s::l)
	    l e.Sl_lib.opensyms)
	 (syml@prototypes) envs;
      outsym=List.fold_left
	 (fun l e -> Sl_lib.SymMap.fold
	    (fun k bindings l -> (* TODO: inference *)
	       if (List.mem k l) or not (List.exists Sl_lib.definitionp bindings)
	       then l else k::l)
	    (List.hd (List.rev e.Sl_lib.ctxt)) l)
	 [] envs;
      inmac=[]; opmac=[]; outmac=[];
      source=(snd ((proj_pptok_expr expr).scan start))},
     unique file_nums)

let env_of_ppexpr ppexpr =
  let s = stream_of_pptok_expr ppexpr in
  let ts = essl_tokenize s in
    parse_essl (essl_lexerfn ts)

let compile ?meta target fn origexpr ~inmac ~opmac tokslst =
  let envs = List.map env_of_ppexpr tokslst in
  let body_unit, file_nums = create_body origexpr envs in
  let linkmap = Hashtbl.create (List.length file_nums) in
    List.iter
      (fun n -> Hashtbl.add linkmap (string_of_int n) (sprintf "%s#%d" fn n))
      file_nums;
    {glo=glo_version; target; meta;
     units=[|(*create_header origexpr;*)
       {body_unit with inmac; opmac}|];
     linkmap}
