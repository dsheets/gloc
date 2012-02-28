open Easy_format
open Glo_xml

type flag = string
type groups = { stage : (flag * string) list }

let format_iface flag descr = function
  | Gloc.Set _ -> raise (Failure "cannot format Set iface")
  | Gloc.Group _ -> raise (Failure "cannot format Group iface, extract groups first")
  | Gloc.List (Gloc.Set _) -> format_tag "setlist" ["flag",flag;"descr",descr] []
  | Gloc.List (Gloc.Filename _) ->
    format_tag "filelist" ["flag",flag;"descr",descr] []
  | Gloc.Filename _ -> format_tag "filename" ["flag",flag;"descr",descr] []
  | Gloc.Option _ -> format_tag "option" ["flag",flag;"descr",descr] []
  | Gloc.Choice (cs,_) -> format_tag "choice" ["flag",flag;"descr",descr]
    (List.map (fun s -> format_tag "alt" ["name",s] []) cs)
  | Gloc.List _ -> raise (Failure "cannot format general lists")

let format_groups ({ stage }) =
  [format_tag "group" ["name","stage"]
      (List.map (fun (flag,descr) ->
        format_tag "alt" ["flag",flag;"descr",descr] [])
         stage)]

let extract_groups cli = List.fold_right
  (fun (flag,iface,descr) (grps,cli) -> match iface with
    | Gloc.Group (Gloc.Stage _) -> ({stage=(flag,descr)::grps.stage},cli)
    | _ -> (grps,(flag,iface,descr)::cli)
  ) cli ({stage=[]},[])

let format_cli cli =
  let grps, cli = extract_groups cli in
  format_tag "cli" ["version",Glo_lib.string_of_version Gloc.gloc_version;
                    "distributor",Gloc.gloc_distributor]
    ((format_groups grps)
     @(List.map (fun (flag,iface,descr) -> format_iface flag descr iface) cli))

let xml_of_cli ?xsl ?(pretty=false) cli =
  let ez = format_cli cli in
  (header xsl)^"\n"^(if pretty
    then (Pretty.to_string ez)^"\n"
    else Compact.to_string ez)
;;

print_endline (xml_of_cli ~xsl:"glocode.xsl" ~pretty:true Gloc.cli)
