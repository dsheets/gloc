open Js

external reg : string -> ('a -> 'b) -> unit = "register_ocaml_fn"

external stdout : js_string t -> (unit -> unit) -> unit = "gloc_stdout"
external stderr : js_string t -> (unit -> unit) -> unit = "gloc_stderr"
external fs_write : js_string t -> js_string t -> (unit -> unit) -> unit = "gloc_fs_write"

external stdin  : (js_string t -> unit) -> unit = "gloc_stdin"
external fs_read : js_string t -> (js_string t -> unit) -> unit = "gloc_fs_read"


module Platform_js = struct
  let id = `JS
  let get_year () = (jsnew date_now ())##getFullYear ()
  let eprint s = Lwt.return (stderr (string s) (fun () -> ()))
  let out_of_filename path s =
    let res, w = Lwt.wait () in
    (if path="-" then stdout else fs_write (string path))
      (string s)
      (Lwt.wakeup w);
    res
  let in_of_filename path =
    let res, w = Lwt.wait () in
    (if path="-" then stdin else fs_read (string path))
      (fun s -> Lwt.wakeup w (to_string s));
    res
end

module Gloc_js = Gloc.Make(Platform_js)
open Gloc_js

let gloc args callback errback =
  let args = Array.map to_string (to_array args) in
  let args = Array.of_list ("gloc"::(Array.to_list args)) in
  let exec_state = Gloc_lib.new_exec_state None in
  let (specs, anon) = arg_of_cli exec_state Gloc.cli in
  let () = Arg.parse_argv ~current:(ref 0) args specs anon Gloc.usage_msg in
  try_lwt
    lwt () = gloc exec_state in
    Lwt.return (Js.Unsafe.fun_call callback [||])
  with
    | Gloc.Exit c ->
      lwt () = Platform_js.eprint ("Exit "^(string_of_int c)^"\n") in
      Lwt.return (Js.Unsafe.fun_call errback [||])
    | Gloc_lib.CompilerError(_,el) -> (* TODO: FIXME *)
      lwt () = Platform_js.eprint "CompilerError:\n" in
      lwt () = Lwt_list.iter_s
          (fun exn -> Platform_js.eprint ((Printexc.to_string exn)^"\n"))
          el in
      Lwt.return (Js.Unsafe.fun_call errback [||])
    | e -> lwt () = Platform_js.eprint ((Printexc.to_string e)^"\n") in
           Lwt.return (Js.Unsafe.fun_call errback [||])
;;
reg "gloc" gloc
