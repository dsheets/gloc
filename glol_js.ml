open Glo_lib
open Js

let link glo_list = string "" (* TODO: do *)
let merge (glo_a,glo_b) =
  string
    (Json_io.string_of_json
       (json_of_glo
	  (glo_of_json
	     (Json_io.json_of_string
		(to_string glo_a))))) (* TODO: do *)

class type glol = object
  method merge : (unit, js_string t * js_string t -> js_string t) meth_callback prop
  method link : (unit, js_string js_array t -> js_string t) meth_callback prop
end

let m : glol t = Unsafe.variable "glol" in
  m##merge <- wrap_callback merge;
  m##link <- wrap_callback link
