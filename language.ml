type dialect = WebGL
type version = int * int * int
type accuracy = Best | Preprocess (*| Decomment | Minify | Pretty | Obfuscate *)
type bond = Error | Warn | Ignore
type language = {dialect:dialect;
		 version:version;
		 bond:bond;
		}

let accuracy_of_string = function
  | "best" -> Best
  | "preprocess" -> Preprocess
  | _ -> Best
let string_of_dialect = function
  | WebGL -> "webgl"

let with_bond bond = fun lang -> { lang with bond }
let with_dialect = function
  | "webgl" -> (fun lang -> { lang with dialect=WebGL })
  | _ -> (fun lang -> lang)

let target_of_language ({dialect; version}) =
  (string_of_dialect dialect, version)
