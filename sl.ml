(*class type ga = object
    method grade : int
end
*)

module JS = Jssl

module type SLSymantics = sig
  type ('c,'sv,'dv) repr
    
  val bool : bool -> ('c,bool,bool) repr
  val float : float -> ('c,float,float) repr
  (*val ga : ga -> ga repr*)
    
  val lam : (('c,'sa,'da) repr -> ('c,'sb,'db) repr as 'x)
    -> ('c,'x,'da -> 'db) repr
  val app : ('c,'x,'da -> 'db) repr
    -> (('c,'sa,'da) repr -> ('c,'sb,'db) repr as 'x)

  val (&&) : ('c,bool,bool) repr -> ('c,bool,bool) repr -> ('c,bool,bool) repr
  val (||) : ('c,bool,bool) repr -> ('c,bool,bool) repr -> ('c,bool,bool) repr

  val (+:) : ('c,float,float) repr -> ('c,float,float) repr
    -> ('c,float,float) repr
  val (-:) : ('c,float,float) repr -> ('c,float,float) repr
    -> ('c,float,float) repr
  val ( *:) : ('c,float,float) repr -> ('c,float,float) repr
    -> ('c,float,float) repr
  val (/:) : ('c,float,float) repr -> ('c,float,float) repr
    -> ('c,float,float) repr

  val sqrt : ('c,float,float) repr -> ('c,float,float) repr

  val leqf : ('c,float,float) repr -> ('c,float,float) repr
    -> ('c,bool,bool) repr

  val bind : string -> ('c,'s,'d) repr -> ('c,'s,'d) repr
  val run : ('c,'s,'d) repr -> string
end

let eval m =
  let module M = (val m : SLSymantics) in
  let open M in
      let csqrt a = (float 3.) +: a in
      run (csqrt (float 4.))

;;
print_endline (eval (module JS : SLSymantics))
;;
(*print_endline (eval (module GLSL : SLSymantics))
;;
print_endline (eval (module Eval : SLSymantics))*)
