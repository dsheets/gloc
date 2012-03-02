open Easy_format
open Glo_lib

let block = { list with
  space_after_opening = false;
  space_after_separator = false;
  space_before_closing = false }
let attrs = { list with
  space_after_opening = false;
  space_after_separator = false;
  space_before_closing = false;
  align_closing = false }
let naked = { list with
  space_after_opening = false;
  space_after_separator = false;
  space_before_closing = false;
  indent_body = 0 }
let tag = { label with space_after_label = false; indent_after_label = 0 }

let format_naked fl = List (("","\n","",naked),fl)

let format_tag name attributes b = Label
  ((List (("<"^name,"",">",attrs),
          List.map (fun (a,v) -> Atom (" "^a^"=\""^v^"\"",atom)) attributes),
    tag),
   List (("","","</"^name^">",block),b))

let format_link name attrs (a,href) =
  format_tag name (("href",href)::attrs) [Atom (a,atom)]

let format_meta meta =
  format_tag "meta" []
    ([format_link "copyright" ["year",string_of_int (fst meta.copyright)]
         (snd meta.copyright)]
     @(List.map
         (fun lnk -> format_link "author" [] lnk)
         meta.author)
     @(match meta.license with None -> []
       | Some lnk -> [format_link "license" [] lnk])
     @(match meta.library with None -> []
       | Some lnk -> [format_link "library" [] lnk])
     @(match meta.version with None -> []
       | Some (v,url) -> [format_tag "version"
                             ["href",url] [Atom (string_of_version v,atom)]])
     @(match meta.build with None -> []
       | Some lnk -> [format_link "build" [] lnk]))

let format_unit u =
  format_tag "unit" []
    ((List.map (fun p -> format_tag "pragma" [] [Atom (p,atom)]) u.pdir)
     @(List.map (fun (e,b) ->
       format_tag "extension" ["behavior",b] [Atom (e,atom)]) u.edir)
     @(match u.vdir with None -> []
       | Some v -> [format_tag "version" [] [Atom (string_of_int v,atom)]])
     @(List.map (fun s ->
       format_tag "symbol" ["sort","in"] [Atom (s,atom)]) u.insym)
     @(List.map (fun s ->
       format_tag "symbol" ["sort","out"] [Atom (s,atom)]) u.outsym)
     @(List.map (fun m ->
       format_tag "macro" ["sort","in"] [Atom (m,atom)]) u.inmac)
     @(List.map (fun m ->
       format_tag "macro" ["sort","open"] [Atom (m,atom)]) u.opmac)
     @(List.map (fun m ->
       format_tag "macro" ["sort","out"] [Atom (m,atom)]) u.outmac)
     @[format_tag "source" [] [Atom (u.source,atom)]])

let attr_of_nameopt = function
  | None -> []
  | Some n -> ["name",n]

let format_glo attrs glo =
  format_tag "glo" (("version",string_of_version glo.glo)::attrs)
    ([format_tag "target" ["version",string_of_version (snd glo.target)]
         [Atom (fst glo.target,atom)]]
     @(match glo.meta with None -> []
       | Some meta -> [format_meta meta])
     @(List.map format_unit (Array.to_list glo.units))
     @(List.map (format_link "link" []) glo.linkmap)
     @[format_tag "json" []
          [Atom (string_of_glo glo,atom)]])

let rec format_glom nameopt = function
  | Glo glo -> format_glo (attr_of_nameopt nameopt) glo
  | Glom [] -> Atom ("",atom)
  | Glom gl -> format_tag "glom" (attr_of_nameopt nameopt)
    (List.map (fun (n,glom) -> format_glom (Some n) glom) gl)
  | Source s -> format_tag "source" (attr_of_nameopt nameopt) [Atom (s,atom)]
  | Other o -> format_tag "extra" (attr_of_nameopt nameopt)
    [Atom (Yojson.Safe.to_string o,atom)]

let header xslopt =
  "<?xml version=\"1.0\"?>\n"
  ^(match xslopt with None -> ""
    | Some href -> "<?xml-stylesheet type=\"text/xsl\" href=\""^href^"\"?>\n")

let xml_of_glom ?xsl ?(pretty=false) glom =
  let ez = format_glom None glom in
  (header xsl)^"\n"^(if pretty
    then (Pretty.to_string ez)^"\n"
    else Compact.to_string ez)
