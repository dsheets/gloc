module Eval : SLSymantics = struct
    type 'a repr = float

    let float f = f
    let (+.) = (+.)

    let run = string_of_float
end
