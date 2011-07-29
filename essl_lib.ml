open Essl

let parse_essl lex =
  let parse = MenhirLib.Convert.traditional2revised
    (fun t -> t)
    (fun _ -> Lexing.dummy_pos) (* TODO: fixme? *)
    (fun _ -> Lexing.dummy_pos)
    translation_unit in parse lex
