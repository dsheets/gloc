module Gloc_posix = Gloc.Make(Platform_posix)
open Gloc_posix
;;

let meta = Gloc_posix.default_meta in
let exec_state = Gloc_lib.new_exec_state meta in
let (args, anon) = arg_of_cli exec_state Gloc.cli in
let () = Arg.parse args anon Gloc.usage_msg in
begin try gloc exec_state (fun () -> Platform_posix.string_of_inchan stdin)
  with Gloc.Exit code -> exit code
end
