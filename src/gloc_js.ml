open Js
open Dom_html

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
    let c = Lwt_condition.create () in
    let wc = Lwt_condition.wait c in
    (if path="-" then stdout else fs_write (string path))
      (string s)
      (fun () -> Lwt_condition.signal c ());
    wc
  let in_of_filename path =
    let c = Lwt_condition.create () in
    let wc = Lwt_condition.wait c in
    (if path="-" then stdin else fs_read (string path))
      (fun s -> Lwt_condition.broadcast c (to_string s));
    wc
end

module Gloc_js = Gloc.Make(Platform_js)
open Gloc_js

let gloc args callback errback =
  let args = Array.map to_string (to_array args) in
  let args = Array.of_list ("gloc"::(Array.to_list args)) in
  let exec_state = Gloc_lib.new_exec_state None in
  let (specs, anon) = arg_of_cli exec_state Gloc.cli in
  let () = Arg.parse_argv ~current:(ref 0) args specs anon Gloc.usage_msg in
  Lwt.catch
    (fun () -> Lwt.bind (gloc exec_state)
      (fun () -> Lwt.return (Js.Unsafe.fun_call callback [||])))
    (function
      | Gloc.Exit c ->
        Lwt.bind (Platform_js.eprint ("Exit "^(string_of_int c)))
          (fun () -> Lwt.return (Js.Unsafe.fun_call errback [||]))
      | Gloc_lib.CompilerError(_,el) -> (* TODO: FIXME *)
        Lwt.bind (Platform_js.eprint "CompilerError:\n")
          (fun () ->
            Lwt.bind
              (Lwt_list.iter_s
                 (fun exn -> Platform_js.eprint ((Printexc.to_string exn)^"\n"))
                 el)
              (fun () -> Lwt.return (Js.Unsafe.fun_call errback [||])))
      | e -> Lwt.bind
        (Platform_js.eprint (Printexc.to_string e))
        (fun () -> Lwt.return (Js.Unsafe.fun_call errback [||]))
    )
;;
reg "gloc" gloc
