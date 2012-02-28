open Js
open Dom_html

external reg : string -> ('a -> 'b) -> unit = "register_ocaml_fn"
external stdout : js_string t -> unit = "gloc_stdout"
external stderr : js_string t -> unit = "gloc_stderr"
external stdin  : unit -> js_string t = "gloc_stdin"
external fs_read : js_string t -> js_string t = "gloc_fs_read"
external fs_write : js_string t -> js_string t -> unit = "gloc_fs_write"

module Platform_js = struct
  let id = `JS
  let get_year () = (jsnew date_now ())##getFullYear ()
  let eprint s = stderr (string s)
  let out_of_filename fn fdf =
    let b = Buffer.create 1024 in
    let r = fdf b in
    (if fn="-" then stdout else fs_write (string fn))
      (string (Buffer.contents b));
    r
  let in_of_filename fn fdf = fdf (to_string (fs_read (string fn)))
end

module Gloc_js = Gloc.Make(Platform_js)
open Gloc_js

let gloc args =
  let args = Array.map to_string (to_array args) in
  let args = Array.of_list ("gloc"::(Array.to_list args)) in
  let exec_state = Gloc_lib.new_exec_state (Gloc_js.default_meta) in
  let (specs, anon) = arg_of_cli exec_state Gloc.cli in
  let () = Arg.parse_argv ~current:(ref 0) args specs anon Gloc.usage_msg in
  begin try gloc exec_state (fun () -> to_string (stdin ())) with
    | Gloc.Exit c -> ()
    | Gloc_lib.CompilerError(_,el) -> (* TODO: FIXME *)
      Platform_js.eprint "CompilerError:\n";
      List.iter (fun exn -> Platform_js.eprint ((Printexc.to_string exn)^"\n")) el
    | e -> Platform_js.eprint (Printexc.to_string e)
  end (* FIXME *)
;;
reg "gloc" gloc
