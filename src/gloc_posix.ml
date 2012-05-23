module Gloc_posix = Gloc.Make(Platform_posix)
open Gloc_posix
;;

let exec_state = Gloc_lib.new_exec_state None in
let (args, anon) = arg_of_cli exec_state Gloc.cli in
let () = Arg.parse args anon Gloc.usage_msg in
begin try Lwt_main.run (gloc exec_state)
  with Gloc.Exit code -> exit code
end
