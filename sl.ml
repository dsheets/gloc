class type ga = object
    method 
end

module type SLSymantics = sig
    type 'a repr

    val int : int -> int repr
    val bool : bool -> bool repr
    val ga : ga -> ga repr

    val lam : ('a repr -> 'b repr) -> repr ('a -> 'b)
    val app : ('a -> 'b) repr -> 'a repr -> 'b repr

    val bind : string -> 'a repr
    val call : string -> 'a repr -> 'b repr

    val (&&) : bool repr -> bool repr -> bool repr
    val (||) : bool repr -> bool repr -> bool repr

    val (+:) : int repr -> int repr -> int repr
    val (-:) : int repr -> int repr -> int repr
    val ( *:) : int repr -> int repr -> int repr
    val (/:) : int repr -> int repr -> int repr

    val (mod) : int repr -> int repr -> int repr

    val leq : int repr -> int repr -> bool repr

    val run : 'a repr -> string
end

module JSSL : SLSymantics = struct
    type t = Float of float | Addf of t * t
    type 'a repr = t
    let float f = Float f
    let (+.) x y = Addf (x,y)

    let rec run = function
      | Float f -> string_of_float f
      | Addf (x,y) -> (run x)^"+"^(run y)
end

module Eval : SLSymantics = struct
    type 'a repr = float

    let float f = f
    let (+.) = (+.)

    let run = string_of_float
end

let eval m =
  let module M = (val m : SLSymantics) in
  let open M in
      let csqrt a = (float 3.) +. a in
      run (csqrt (float 4.))

;;
print_endline (eval (module JSSL : SLSymantics))
;;
print_endline (eval (module GLSL : SLSymantics))
;;
print_endline (eval (module Eval : SLSymantics))
