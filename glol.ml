(* Let's link some glo! *)
open Glo_lib

(* We'll need some maps from strings to structures. *)
module M = Map.Make(String)
  
(* Each unit of SL source belongs to a labeled glo and has a (mostly)
   meaningless glo index. *)
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

(* Prepare the packaged source for concatenation. *)
let armor fs_ct opmac s =
  (*required regexp instead of macros due to ANGLE bug 183*)
  let intpatt = Str.regexp "GLOC_\\([0-9]+\\)" in
  let offset s = let s = Str.matched_string s in
    (string_of_int ((int_of_string (Str.string_after s 5))+fs_ct))
  in
  let s = Str.global_substitute intpatt offset s in
    List.fold_left (fun s mac -> s^"\n#undef "^mac) s opmac

let rec satisfy_macro addr macro = function
  | (name,glo)::rest -> begin match Array.fold_left
      (fun uo (i,u) -> match uo with Some u -> Some u
	 | None -> if List.mem macro u.outmac
	   then Some i else None
      ) None (Array.mapi (fun i u -> (i,u)) glo.units) with
	| Some u -> (name,u)
	| None -> satisfy_macro addr macro rest
    end
  | [] -> raise (MissingMacro (addr, macro))

let rec satisfy_sym addr sym = function
  | (name,glo)::rest -> begin match Array.fold_left
      (fun uo (i,u) -> match uo with Some u -> Some u
	 | None -> if List.mem sym u.outmac
	   then Some i else if List.mem sym u.outsym
	   then Some i else None
      ) None (Array.mapi (fun i u -> (i,u)) glo.units) with
	| Some u -> (name,u)
	| None -> satisfy_sym addr sym rest
    end
  | [] -> raise (MissingSymbol (addr, sym))

let map_of_list v = List.fold_left (fun m n -> M.add n v m) M.empty

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
let mergeb b = function [] -> b
  | {bsym; bmac}::r -> {b with bsym=M.fold M.add b.bsym bsym;
			  bmac=M.fold M.add b.bmac bmac}
let connect_mac b n addr =
  {b with rmac=List.filter (fun m -> not (m=n)) b.rmac; tmac=M.add n addr b.tmac}
let connect_sym b n addr =
  {b with rsym=List.filter (fun m -> not (m=n)) b.rsym; tsym=M.add n addr b.tsym}
let provided_mac n = function [] -> None
  | t::r -> try let addr = M.find n t.bmac in Some addr
    with Not_found -> None
let provided_sym n = function [] -> None
  | t::r -> try let addr = M.find n t.bmac in Some addr
    with Not_found -> try let addr = M.find n t.bsym in Some addr
    with Not_found -> None
let conflicted a = function [] -> None
  | t::r -> let keys = List.map fst ((M.bindings a.bsym)@(M.bindings a.bmac)) in
      begin match List.filter (fun (sym,addr) -> List.mem sym keys)
	((M.bindings t.bsym)@(M.bindings t.bmac))
      with [] -> None | b::_ -> Some b end

let rec satisfy_zipper glo_alist = function
  | ([],t) -> ([],t)
  | (({rmac=[]; rsym=[]} as b)::r,t) ->
      satisfy_zipper glo_alist (r,(mergeb b t)::t)
  | (({rmac=n::_} as b)::r,t) ->
      begin match provided_mac n t with
	| Some addr -> satisfy_zipper glo_alist ((connect_mac b n addr)::r,t)
	| None -> let addr = satisfy_macro b.addr n glo_alist in
	    if List.exists (has_addr addr) (b::r)
	    then raise (CircularDependency
			  (List.map (fun {addr} -> addr) (b::r)))
	    else let tooth = tooth_of_addr glo_alist addr in
	      begin match conflicted tooth t with
		| Some (sym, caddr) -> raise (SymbolConflict (sym,n,addr,caddr))
		| None -> satisfy_zipper glo_alist
		    (tooth::(connect_mac b n addr)::r,t)
	      end
      end
  | (({rmac=[]; rsym=n::_} as b)::r,t) ->
      begin match provided_sym n t with
	| Some addr -> satisfy_zipper glo_alist ((connect_sym b n addr)::r,t)
	| None -> let addr = satisfy_sym b.addr n glo_alist in
	    if List.exists (has_addr addr) (b::r)
	    then raise (CircularDependency
			  (List.map (fun {addr} -> addr) (b::r)))
	    else let tooth = tooth_of_addr glo_alist addr in
	      begin match conflicted tooth t with
		| Some (sym, caddr) -> raise (SymbolConflict (sym,n,addr,caddr))
		| None -> satisfy_zipper glo_alist
		    (tooth::(connect_sym b n addr)::r,t)
	      end
      end

let sort required glo_alist =
  let addrs = List.fold_left
    (fun al sym -> let addr = satisfy_sym ("<-u>",0) sym glo_alist in
       if List.mem addr al then al else addr::al)
    [] required in
  let z = satisfy_zipper glo_alist (List.map (tooth_of_addr glo_alist) addrs,[]) in
    List.rev_map (fun tooth -> tooth.addr) (snd z)

(* TODO: compact linkmap index space *)
let link required glo_alist =
  fst begin List.fold_left
      begin fun (src,o) (name,u) ->
	let glo = List.assoc name glo_alist in
	let sup = Hashtbl.fold
	  (fun is _ sup -> max sup (int_of_string is))
	  glo.linkmap 0
	in
	let u = glo.units.(u) in
	  (src^(armor o u.opmac u.source),
	   o+sup+1)
      end ("",0) (sort required glo_alist)
  end
