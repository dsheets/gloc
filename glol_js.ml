open Js
open Json
open Dom_html
open Glo_lib

let reg = Unsafe.variable "register_ocaml_fn" in

let addr_of_js j = match (Optdef.to_option (array_get j 0),
			  Optdef.to_option (array_get j 1)) with
  | (Some s, Some i) -> (to_string s,
			 int_of_float (float_of_number (Unsafe.coerce i)))
  | _ -> raise (Failure "not enough fields in addr")
in

let string_of_js_exc e = string (Glol.string_of_error begin
  match to_string (e##name) with
    | "MissingMacro" ->
      Glol.MissingMacro (addr_of_js e##addr, to_string e##macro)
    | "MissingSymbol" ->
      Glol.MissingSymbol (addr_of_js e##addr, to_string e##symbol)
    | "NotFound" ->
      Failure ("NotFound: "^(to_string e##key))
    | "CircularDependency" ->
      let addrs = Array.to_list (to_array e##addrs) in
      Glol.CircularDependency (List.map addr_of_js addrs)
    | "SymbolConflict" ->
      Glol.SymbolConflict (to_string (e##sym_a_),
			   to_string (e##sym_b_),
			   addr_of_js (e##addr_a_),
			   addr_of_js (e##addr_b_))
    | "UnknownBehavior" ->
      Glol.UnknownBehavior (addr_of_js e##addr, to_string e##behavior)
    | "UnknownGloVersion" ->
      let v = to_array e##version in
      Glol.UnknownGloVersion (to_string e##path, (v.(0),v.(1),v.(2)))
    | n -> Failure ("unknown error "^n)
end)
in

let link prologue syms glom_s =
  let gloms = to_string glom_s in
  let glom = glom_of_string gloms in
  string (Glol.link (to_string prologue)
	    (List.map to_string (Array.to_list (to_array syms)))
	    (Glol.flatten "" glom))
in
reg "link" (wrap_callback link);
reg "string_of_error" (wrap_callback (fun e -> string (Glol.string_of_error e)));
reg "string_of_js_exc" (wrap_callback string_of_js_exc)
