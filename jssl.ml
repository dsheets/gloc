open Printf
open Quot
type ('c,'sv,'dv) repr = ('c,'sv,'dv) code

let bool b = literal (string_of_bool b)
let float f = literal (string_of_float f)

let lam fn = func fn
let app = function
  | `Fn f ->
    if f#bound then (fun a -> literal (sprintf "%s(%s)" f#name (v a)))
    else f#fn
  | `Lit _ -> raise (Type_error "Cannot apply literal")

let (&&) a b = literal (sprintf "(%s && %s)" (v a) (v b))
let (||) a b = literal (sprintf "(%s || %s)" (v a) (v b))

let (+:) x y = literal (sprintf "(%s + %s)" (v x) (v y))
let (-:) x y = literal (sprintf "(%s - %s)" (v x) (v y))
let ( *:)x y = literal (sprintf "(%s * %s)" (v x) (v y))
let (/:) x y = literal (sprintf "(%s / %s)" (v x) (v y))

let sqrt f = literal (sprintf "Math.sqrt(%s)" (v f))

let leqf x y = literal (sprintf "(%s <= %s)" (v x) (v y))

let bind name v = v#bind name
let run r = r#string
