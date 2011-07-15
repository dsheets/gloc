exception Unbound_call

class ['sv,'dv] func f = object
    val name : string option = None

    method bound = match name with Some _ -> true | None -> false
    method bind name = {< name = Some name >}
    method name = match name with
      | Some n -> n
      | None -> raise Unbound_call 
    method fn : 'dv = f
end

class ['t] literal (s : string) = object
    method v = s
end

exception Type_error of string
type ('c,'sv,'dv) code = [ `Fn of ('sv,'dv) func | `Lit of ('dv) literal ]

let func (f : ('c,'sa,'da) code -> ('c,'sb,'db) code as 'a)
    : ('c,'a,'da -> 'db) code
    = `Fn (new func f)
let literal s = `Lit (new literal s)

let v = function
  | `Lit l -> l#v
  | `Fn _ -> raise (Type_error "Cannot produce value of lambda")
