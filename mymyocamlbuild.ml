open Ocamlbuild_plugin
open Command
;;

let atdgen t env builder =
  Cmd(S[A"atdgen";A("-j-std");A("-"^t);A(env "%.atd")])
;;
rule "atd: atd -> _t.ml & _t.mli"
  ~prods:["%_t.mli"; "%_t.ml"]
  ~dep:"%.atd"
  (atdgen "t")
;;
rule "atd: atd -> _v.ml & _v.mli"
  ~prods:["%_v.mli"; "%_v.ml"]
  ~dep:"%.atd"
  (atdgen "v")
;;
rule "atd: atd -> _j.ml & _j.mli"
  ~prods:["%_j.mli"; "%_j.ml"]
  ~dep:"%.atd"
  (atdgen "j")
;;
let _ = dispatch begin function
  | Before_rules ->
    rule "c stubs"
      ~prod:"%.o"
      ~dep:"%.c"
      begin fun env build ->
        let o = env "%.o" and c = env "%.c" in
        Cmd(S[A"gcc";A"-c";A"-o";P o;P c])
      end
  | After_rules ->
    rule "js_of_ocaml"
      ~prod:"%.js"
      ~dep:"%.byte"
      begin fun env build ->
        let byte = env "%.byte" in
        Cmd(S[A"js_of_ocaml";A"-pretty";A"-noinline";P byte])
      end;
    flag ["ocaml"; "byte"; "link"] (S[A"-custom"]);
    dep ["ocaml"; "link"] ["libgloc.o"]
  | _ -> ()
end
