open Ocamlbuild_plugin;;

let atdgen t env builder =
  Cmd(S[A"atdgen";A("-"^t);A(env "%.atd")])
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
