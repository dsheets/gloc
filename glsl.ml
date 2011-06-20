
type vec2 = float * float
type vec3 = float * float * float
type vec4 = float * float * float * float
type ivec2 = int * int
type ivec3 = int * int * int
type ivec4 = int * int * int * int
type bvec2 = bool * bool
type bvec3 = bool * bool * bool
type bvec4 = bool * bool * bool * bool

type mat2 = vec2 * vec2
type mat3 = vec3 * vec3 * vec3
type mat4 = vec4 * vec4 * vec4 * vec4

let dot2 (a,b) (c,d) = (a*.c, b*.d)
let dot3 (a,b,c) (d,e,f) = (a*.d,b*.e,c*.f)
let dot4 (a,b,c,d) (e,f,g,h) = (a*.e,b*.f,c*.g,d*.h)
let cross2 (a,b) (c,d) = ()
let cross3 (a,b,c) (d,e,f) = ()
let cross4 (a,b,c,d) (e,f,g,h) = ()

let matrixCompMult2 ((a00,a10),(a01,a11)) ((b00,b10),(b01,b11)) = ()
let matrixCompMult3 = ()
let matrixCompMult4 = ()

let lessThan2 = ()
let lessThan3 = ()
let lessThan4 = ()

let lessThanEqual2 = ()
let lessThanEqual3 = ()
let lessThanEqual4 = ()

let greaterThan2 = ()
let greaterThan3 = ()
let greaterThan4 = ()

let greaterThanEqual2 = ()
let greaterThanEqual3 = ()
let greaterThanEqual4 = ()

let equal2 = ()
let equal3 = ()
let equal4 = ()

let notEqual2 = ()
let notEqual3 = ()
let notEqual4 = ()

let not2 = ()
let not3 = ()
let not4 = ()
