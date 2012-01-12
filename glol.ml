(* Let's link some glo! *)
open Glo_lib

(* We'll need some maps from strings to structures. *)
module M = Map.Make(String)
  
(* Each unit of SL source belongs to a labeled glo and has a (mostly)
   meaningless glo index that determines link precedence. *)
type unit_addr = string * int
    
(* To satisfy the various symbol dependency constraints, we zip through the
   link list tracking Required symbols, Top symbols satisfied, and Bottom symbols
   exposed by the above teeth. *)
type tooth = { rsym : string list;   rmac : string list;
               tsym : unit_addr M.t; tmac : unit_addr M.t;
	       bsym : unit_addr M.t; bmac : unit_addr M.t;
	       addr : unit_addr }
    (* Bottom * Top *)
type zipper = tooth list * tooth list

exception MissingSymbol of unit_addr * string
exception MissingMacro of unit_addr * string
exception CircularDependency of unit_addr list
exception SymbolConflict of string * string * unit_addr * unit_addr
exception UnknownBehavior of unit_addr * string
exception UnknownGloVersion of string * version

(* Prepare the packaged source for string concatenation. *)
let armor meta (linkmap,fs_ct) opmac s =
  (* Replace special symbols in line directives to satisfy linkmap *)
  (*required regexp (greetz jwz) instead of macros due to ANGLE bug 183*)
  let intpatt = Str.regexp "GLOC_\\([0-9]+\\)" in
  let offset s =
    let fn = int_of_string (Str.string_after (Str.matched_string s) 5) in
    let anno = try "/* "^(Hashtbl.find linkmap (string_of_int fn))^" */"
    with Not_found -> ""
    in (string_of_int (fn + fs_ct))^anno
  in
  let s = Str.global_substitute intpatt offset s in
  let head = match meta with None -> "" | Some meta ->
    let field name (entity,href) = "// "^name^": "^entity^" <"^href^">\n" in
    let authors = List.fold_left
      (fun s link -> s^(field "Author" link))
      "" meta.author in
    let license = match meta.license with None -> ""
      | Some link -> field "License" link in
    let library = match meta.library with None -> ""
      | Some link -> field "Library" link in
    let version = match meta.version with None -> ""
      | Some (maj,min,rev) ->
	  "// Version: "^(string_of_int maj)^"."^(string_of_int min)^"."
	  ^(string_of_int rev)^"\n" in
    let build = match meta.build with None -> ""
      | Some buildstring -> "// Build: "^buildstring^"\n" in
    let (year,(holder,url)) = meta.copyright in
      "// Copyright "^(string_of_int year)^" "^holder^" <"^url^"> "
      ^"All rights reserved.\n"^license^authors^library^version^build
  in let s = head^s in
    (* Undefine local open macros so they do not leak *)
    List.fold_left (fun s mac -> s^"\n#undef "^mac) s opmac

(* Search a glo for a unit satisfying the supplied predicate p. *)
let search p glo =
  let rec loop = function
    | [] -> None
    | (i,u)::r -> if p u then Some i else loop r
  in loop (Array.to_list (Array.mapi (fun i u -> (i,u)) glo.units))

(* Linearly search for an exported macro. *)
let rec satisfy_mac addr macro = function
  | (name,glo)::rest ->
      begin match search (fun u -> List.mem macro u.outmac) glo with
	| Some i -> (name,i)
	| None -> satisfy_mac addr macro rest
      end
  | [] -> raise (MissingMacro (addr, macro))

(* Linearly search for an exported symbol or macro. *)
let rec satisfy_sym addr sym = function
  | (name,glo)::rest ->
      begin match search
	(fun u -> (List.mem sym u.outmac) || (List.mem sym u.outsym)) glo
      with
	| Some i -> (name,i)
	| None -> satisfy_sym addr sym rest
      end
  | [] -> raise (MissingSymbol (addr, sym))

let map_of_list v = List.fold_left (fun m n -> M.add n v m) M.empty

(* Construct the constraint data structure from a glo unit. *)
let tooth addr u =
  { rsym=u.insym; (* to be satisfied *)
    rmac=u.inmac;
    tsym=M.empty; (* locally satisfied *)
    tmac=M.empty;
    bsym=map_of_list addr u.outsym; (* cumulatively satisfies *)
    bmac=map_of_list addr u.outmac;
    addr }

let lookup glo_alist (n,u) = (List.assoc n glo_alist).units.(u)
let tooth_of_addr glo_alist addr = tooth addr (lookup glo_alist addr)
let has_addr addr_a ({addr=addr_b}) = addr_a = addr_b
(* Advertize prior units to later units. *)
let mergeb b = function [] -> b
  | {bsym; bmac}::r -> {b with bsym=M.fold M.add b.bsym bsym;
			  bmac=M.fold M.add b.bmac bmac}
(* Satisfy a macro dependency with an already included dependency. *)
let connect_mac b n addr =
  {b with rmac=List.filter (fun m -> not (m=n)) b.rmac; tmac=M.add n addr b.tmac}
(* Satisfy a symbol dependency with an already included dependency. *)
let connect_sym b n addr =
  {b with rsym=List.filter (fun m -> not (m=n)) b.rsym; tsym=M.add n addr b.tsym}
(* Search for an already included macro. *)
let provided_mac n = function [] -> None
  | t::r -> try let addr = M.find n t.bmac in Some addr
    with Not_found -> None
(* Search for an already included symbol. *)
let provided_sym n = function [] -> None
  | t::r -> try let addr = M.find n t.bmac in Some addr
    with Not_found -> try let addr = M.find n t.bsym in Some addr
    with Not_found -> None
(* Given a tooth and a prefix tooth list, find conflicts. *)
let conflicted a = function [] -> None
  | t::r -> let keys = List.map fst ((M.bindings a.bsym)@(M.bindings a.bmac)) in
      begin match List.filter (fun (sym,addr) -> List.mem sym keys)
	((M.bindings t.bsym)@(M.bindings t.bmac))
      with [] -> None | b::_ -> Some b end

(* Check for circular dependency *)
let check_circdep addr (b,t) =
  if List.exists (has_addr addr) b
  then raise (CircularDependency (List.map (fun {addr} -> addr) b))

(* Check for symbol conflicts *)
let check_conflicts n tooth (b,t) = match conflicted tooth t with
  | Some (sym, caddr) -> raise (SymbolConflict (n,sym,tooth.addr,caddr))
  | None -> ()

(* Build a list of units with internal requirements satisfied. *)
let rec satisfy_zipper glo_alist = function
    (* At the bottom of the zipper, we must be done. *)
  | ([],t) -> ([],t)
    (* Without further requirements from below, we must be ready to descend. *)
  | (({rmac=[]; rsym=[]} as b)::r,t) ->
      satisfy_zipper glo_alist (r,(mergeb b t)::t)
    (* Subsequent units require a macro. *)
  | (({rmac=n::_} as b)::r,t) ->
      begin match provided_mac n t with
	| Some addr -> satisfy_zipper glo_alist ((connect_mac b n addr)::r,t)
	| None -> let addr = satisfy_mac b.addr n glo_alist in
	    check_circdep addr (b::r,t);
	    let tooth = tooth_of_addr glo_alist addr in
	      check_conflicts n tooth (b::r,t);
	      satisfy_zipper glo_alist(tooth::(connect_mac b n addr)::r,t)
      end
    (* Subsequent units require a symbol. *)
  | (({rmac=[]; rsym=n::_} as b)::r,t) ->
      begin match provided_sym n t with
	| Some addr -> satisfy_zipper glo_alist ((connect_sym b n addr)::r,t)
	| None -> let addr = satisfy_sym b.addr n glo_alist in
	    check_circdep addr (b::r,t);
	    let tooth = tooth_of_addr glo_alist addr in
	      check_conflicts n tooth (b::r,t);
	      satisfy_zipper glo_alist (tooth::(connect_sym b n addr)::r,t)
      end

(* Generate a list of unit addresses from a list of required symbols and
   a search list. *)
let sort required glo_alist =
  let addrs = List.fold_left
    (fun al sym -> let addr = satisfy_sym ("<-u "^sym^">",0) sym glo_alist in
       if List.mem addr al then al else addr::al)
    [] required in
  let z = satisfy_zipper glo_alist
    (List.rev_map (tooth_of_addr glo_alist) addrs,[])
  in List.rev_map (fun tooth -> tooth.addr) (snd z)

(* Generate the shader preamble. *)
let preamble glol =
  (* Precedence for conflicting behaviors *)
  let b_order = ["require"; "warn"; "enable"; "disable"] in
  let rec b_max x y = function [] -> x
    | b::r when b=x -> x
    | b::r when b=y -> y
    | _::r -> b_max x y r
  in
  let ext_merge addr m (ext,b) =
    match (try Some (M.find ext m) with Not_found -> None) with
      | None -> if List.mem b b_order
	then M.add ext b m
	else raise (UnknownBehavior (addr, b))
      | Some pb -> M.add ext (b_max b pb b_order) m
  in
  let ext_decl ext b = "#extension "^ext^" : "^b^"\n" in
  let ext_segment m =
    (try ext_decl "all" (M.find "all" m) with Not_found -> "")
    ^(M.fold (fun ext b s -> if ext="all" then s else s^(ext_decl ext b)) m "")
  in
  let (version,pragmas,exts) = List.fold_left
    (fun (version,pragmas,exts) ((name,i),glo) -> let u = glo.units.(i) in
       (begin match u.vdir,version with
	  | None,v -> v
	  | Some uv,None -> Some uv
	  | Some uv,Some pv -> Some (max uv pv)
	end,
	List.fold_left (fun p s -> p^s^"\n") pragmas u.pdir,
	List.fold_left (ext_merge (name,i)) exts u.edir
       )
    ) (None,"",M.empty) glol
  in (match version with Some v -> "#version "^(string_of_int v)^"\n"
	| None -> "")^pragmas^(ext_segment exts)

(* Produce a string representing a valid SL program given a list of required
   symbols and a search list. *)
let link prologue required glo_alist =
  let support = [|[|false;true|]|] in
  let () = List.iter
    (fun (name,glo) -> let (maj,min,_) = glo.glo in
       if try not support.(maj).(min) with Invalid_argument _ -> true
       then raise (UnknownGloVersion (name,glo.glo))) glo_alist in
  let glol = List.map
    (fun (name,u) -> ((name,u),List.assoc name glo_alist))
    (sort required glo_alist)
  in fst begin List.fold_left
	begin fun (src,(pname,o)) ((name,u),glo) ->
	  let sup = Hashtbl.fold
	    (fun is _ sup -> max sup (int_of_string is))
	    glo.linkmap 0
	  in
	  let meta = if name=pname then None else glo.meta in
	  let u = glo.units.(u) in
	    (src^(armor meta (glo.linkmap, o) u.opmac u.source)^"\n",
	     (name,o+sup+1))
	end ((preamble glol)^prologue,("",0)) glol
    end
