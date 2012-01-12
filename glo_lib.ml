open Json_type

type glo = {
  glo:version;
  target:string * version;
  meta:meta option;
  units: u array;
  linkmap:(string,string) Hashtbl.t
}
and meta = {
  copyright:year * href;
  author:href list;
  license:href option;
  library:href option;
  version:version option;
  build:string option;
}
and u = {
  pdir: string list;
  edir: (string * string) list;
  vdir: int option;
  insym:string list;
  outsym:string list;
  inmac:string list;
  opmac:string list;
  outmac:string list;
  source:string;
}
and url = string
and href = string * url
and year = int
and version = int * int * int
and glom = (string * glo) list
with json

let glo_version = (0,1,0)

let set_field fld value = function
  | Object f_alist ->
      Object (List.map
		(fun (k,v) -> if k=fld then (k,value) else (k,v))
		f_alist)
  | x -> x

(* additive identity maps *)
let strip_zero_fields = function
  | Object f_alist -> Object
      (List.filter
	 (function
	    | (k, Object []) | (k, Array []) | (k, Null) -> false
	    | _ -> true
	 ) f_alist)
  | x -> x
let add_zero_fields fl = function
  | Object f_alist -> Object
      (List.fold_left
	 (fun fal (k,z,m) ->
	    if List.mem_assoc k fal
	    then List.map
	      (fun (fn,v) ->
		 if fn=k then (fn,m v) else (fn,v)
	      ) fal
	    else (k,z)::fal
	 ) f_alist fl)
  | x -> x

let json_of_meta meta = strip_zero_fields (json_of_meta meta)
let json_of_u u = strip_zero_fields (json_of_u u)

let json_of_glo glo =
  let json = strip_zero_fields (json_of_glo glo) in
  let json = match glo.meta with
    | Some meta -> set_field "meta" (json_of_meta meta) json
    | None -> json
  in set_field "units"
       (Array (Array.to_list (Array.map json_of_u glo.units)))
       json

let json_of_glom glom =
  let rec group prefix prev = function
    | ((_::[],_)::_) as glom -> (glom, List.rev prev)
    | (x::xs,glo)::r when x=prefix -> group prefix ((xs,glo)::prev) r
    | glom -> (glom,List.rev prev)
  in
  let rec nest prev = function
    | [] -> Array (List.rev prev)
    | (([],glo)::r) -> nest ((Array [String ""; json_of_glo glo])::prev) r
    | ((x::[],glo)::r) -> nest ((Array [String x; json_of_glo glo])::prev) r
    | ((x::_,_)::_) as r -> let (rest,g) = group x [] r in
	nest ((Array [String x; nest [] g])::prev) rest
  in let split s = Str.split (Str.regexp_string "/") s in
    nest [] (List.map (fun (n,glo) -> (split n, glo)) glom)

let string_of_glo ?(compact=true) glo =
  Json_io.string_of_json ~compact (json_of_glo glo)
let string_of_glom ?(compact=true) glom =
  Json_io.string_of_json ~compact (json_of_glom glom)

let glo_of_json json = let id x = x in
let json = add_zero_fields [
  "meta",Null,
  (add_zero_fields [
     "author",Array [],id;
     "license",Null,id;
     "library",Null,id;
     "version",Null,id;
     "build",Null,id;
   ]);
  "units",Array [],
  (function
     | Array l ->
	 Array (List.map
		  (add_zero_fields [
		     "pdir",Array [],id;
		     "edir",Object [],id;
		     "vdir",Null,id;
		     "insym",Array [],id;
		     "outsym",Array [],id;
		     "inmac",Array [],id;
		     "opmac",Array [],id;
		     "outmac",Array [],id;
		   ]) l)
     | x -> x
  );
  "linkmap",Object [],id;
] json in (*try*) glo_of_json json
(*  with e -> (print_endline (Json_io.string_of_json json);
	     print_endline (Printexc.to_string e);
	     Printexc.print_backtrace stdout;
	     exit 127)
*)
let glom_of_json json =
  let rec flat prefix = function
    | Array jl -> List.fold_left
	(fun p -> function
	   | Array [String n; Object ol] ->
	       (prefix^n, glo_of_json (Object ol))::p
	   | Array [String n; Array al] ->
	       (flat (prefix^n^"/") (Array al))@p
	   | _ ->
	       raise (Json_error "glom array elements must be string * (glo|glom)")
	) [] jl
    | _ -> raise (Json_error "glom must be array")
  in List.rev (flat "" json)
