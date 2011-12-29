open Printf

let rec prod ins prev = function
  | l,[] -> (l@[ins])::prev
  | l,h::r -> prod ins ((List.rev_append l (ins::h::r))::prev) (h::l,r)

let rec permute = function
  | [] -> [[]]
  | e::r -> List.fold_left
    (fun prev s -> prod e prev ([],s))
    [] (permute r)

let set = ref []
let set_append e = set := e::!set
;;
Arg.parse ["-", Arg.String set_append,
	   "escape strings beginning with '-'"] set_append
  "prints all permutations of argument symbols"
;;
let rec read_lines ls =
  try read_lines ((read_line ())::ls)
  with End_of_file -> ls
in
let rec print_list sep = function
  | [] -> () | e::[] -> print_endline e
  | e::r -> (printf "%s%s" e sep; print_list sep r)
in List.iter (print_list " ")
(permute begin if (List.length !set)=0
  then read_lines []
  else !set
end)
