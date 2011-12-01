open Glo_lib

module M = Map.Make(String)

type unit_addr = string * int
type tooth = { rsym : unit M.t;      rmac : unit M.t;
               tsym : unit_addr M.t; tmac : unit_addr M.t;
	       bsym : unit_addr M.t; bmac : unit_addr M.t;
	       addr : unit_addr }
type zipper = tooth list * tooth list

exception MissingSymbol of string
exception MissingMacro of string
exception CircularDependency of unit_addr list
exception SymbolConflict of string * string * unit_addr * unit_addr

let armor s = s (* TODO: do *)

let rec satisfy_macro macro = function
  | (name,glo)::rest -> begin match Array.fold_left
      (fun uo (i,u) -> match uo with Some u -> Some u
	 | None -> if List.mem macro u.outmac
	   then Some i else None
      ) None (Array.mapi (fun i u -> (i,u)) glo.units) with
	| Some u -> (name,u)
	| None -> satisfy_macro macro rest
    end
  | [] -> raise (MissingMacro macro)

let rec satisfy_sym sym = function
  | (name,glo)::rest -> begin match Array.fold_left
      (fun uo (i,u) -> match uo with Some u -> Some u
	 | None -> if List.mem sym u.outmac
	   then Some i else if List.mem sym u.outsym
	   then Some i else None
      ) None (Array.mapi (fun i u -> (i,u)) glo.units) with
	| Some u -> (name,u)
	| None -> satisfy_sym sym rest
    end
  | [] -> raise (MissingSymbol sym)

let map_of_list v = List.fold_left (fun m n -> M.add n v m) M.empty

let tooth addr u =
  { rsym=map_of_list () u.insym;
    rmac=map_of_list () u.inmac;
    tsym=M.empty;
    tmac=M.empty;
    bsym=map_of_list addr u.outsym;
    bmac=map_of_list addr u.outmac;
    addr }

let lookup glo_alist (n,u) = (List.assoc n glo_alist).units.(u)
let tooth_of_addr glo_alist addr = tooth addr (lookup glo_alist addr)
let has_addr addr_a ({addr=addr_b}) = addr_a = addr_b
let mergeb b = function [] -> b
  | {bsym; bmac}::r -> {b with bsym=M.fold M.add b.bsym bsym;
			  bmac=M.fold M.add b.bmac bmac}
let connect_mac b n addr =
  {b with rmac=M.remove n b.rmac; tmac=M.add n addr b.tmac}
let connect_sym b n addr =
  {b with rsym=M.remove n b.rsym; tsym=M.add n addr b.tsym}
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
  | (b::r,t) -> if M.is_empty b.rmac
    then if M.is_empty b.rsym
    then satisfy_zipper glo_alist (r,(mergeb b t)::t)
    else let (n,()) = M.min_binding b.rsym in
      begin match provided_sym n t with
	| Some addr -> satisfy_zipper glo_alist ((connect_sym b n addr)::r,t)
	| None -> let addr = satisfy_sym n glo_alist in
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
    else let (n,()) = M.min_binding b.rmac in
      begin match provided_mac n t with
	| Some addr -> satisfy_zipper glo_alist ((connect_mac b n addr)::r,t)
	| None -> let addr = satisfy_macro n glo_alist in
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

let sort start glo_alist =
  let addr = satisfy_sym start glo_alist in
    List.rev_map (fun tooth -> tooth.addr)
      (snd (satisfy_zipper glo_alist ([tooth_of_addr glo_alist addr],[])))

let link start glo_alist = List.fold_left
  (fun src (name,u) -> src^(armor (List.assoc name glo_alist).units.(u).source))
  "" (sort start glo_alist)
