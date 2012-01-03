(* of the form [a_1,b_1,...] * [a_2,b_2,...] * ... *)

open Printf

type expr = string (*tok*) list (*sym*) list (*prod*) ref

let rec pprod ins prev = function
  | l,[] -> (List.rev_append l [ins])::prev
  | l,h::r -> pprod ins ((List.rev_append l (ins::h::r))::prev) (h::l,r)

let rec permute = function
  | [] -> [[]]
  | e::r -> List.rev
    (List.fold_left
       (fun prev s -> pprod e prev ([],s))
       [] (permute r))

let join sep a b = a^sep^b
let rec join_list sep = function
  | [] -> "" | e::[] -> e
  | a::b::r -> join_list sep ((join sep a b)::r)

(* non-commutative *)
let rec cprod p = function
  | [] -> List.rev p
  | a::[] -> cprod (a@p) []
  | a::b::r ->
    let cp = List.fold_left
      (fun prods x -> (List.rev_map (fun y -> join " " x y) b)@prods) [] a
    in cprod p ((List.rev cp)::r)

let rec compute c = function
  | [] -> cprod [] (List.rev c)
  | symgrp::factors ->
    compute ((List.rev_map (join_list " ") (permute symgrp))::c) factors

let factors = ref []
let push_factor e = match !factors with
  | [] -> failwith "Empty factor expression"
  | h::t -> factors := (e::h)::t
let push_product () = match !factors with
  | [] -> factors := [[]]
  | pl -> factors := []::pl
;;
push_product ();
Arg.parse ["-", Arg.String push_factor,
	   "escape strings beginning with '-'";
           "--prod", Arg.Unit push_product,
           "cartesian product"
          ] push_factor
  "prints all permutations of argument symbols"
;;
let rec read_lines () =
  try let line = read_line () in
      if line=""
      then begin
        push_product ();
        read_lines ()
      end
      else begin
        push_factor line;
        read_lines ()
      end
  with End_of_file -> List.rev_map List.rev !factors
in List.iter print_endline
(compute [] begin if (List.length (List.hd !factors))=0
  then read_lines ()
  else List.rev_map List.rev !factors
end)
