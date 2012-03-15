// Stubs for browser-provided functions to generate linkable bytecode
#include <stdlib.h>
#define D(f) void f () { exit(1); }

D(register_ocaml_fn)

D(gloc_stdout)
D(gloc_stderr)
D(gloc_stdin)
D(gloc_fs_read)
D(gloc_fs_write)
