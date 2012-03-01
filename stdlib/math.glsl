// math functions defined in later GLSL specs

#define SINH(T) \
  T sinh(T x) { \
    T e = exp(x); \
    return 0.5 * (e - 1/e); \
  }

SINH(float)
SINH(vec2)
SINH(vec3)
SINH(vec4)

#define COSH(T) \
  T cosh(T x) { \
    T e = exp(x); \
    return 0.5 * (e + 1.0/e); \
    }

COSH(float)
COSH(vec2)
COSH(vec3)
COSH(vec4)

#define TANH(T) \
  T tanh(T x) { \
    T e = exp(2.0*x) ;\
    return ( e-1.0 )/ (e+1.0) ; \
    }

TANH(float)
TANH(vec2)
TANH(vec3)
TANH(vec4)

#define ASINH(T) \
  T asinh(T x) { \
    return log(x + sqrt(x*x+1.0)); \
    }
 
ASINH(float)
ASINH(vec2)
ASINH(vec3)
ASINH(vec4)

#define ACOSH(T) \
  T acosh(T x) { \
    return log(x + sqrt(x*x-1.0)); \
    }
 
ACOSH(float)
ACOSH(vec2)
ACOSH(vec3)
ACOSH(vec4)

#define ATANH(T) \
  T atanh(T x) { \
    return 0.5 * log( (x+1) / (x-1)); \
    }
 
ATANH(float)
ATANH(vec2)
ATANH(vec3)
ATANH(vec4)


