*gloc*: (WebGL|ESSL) gl(sl) o(bject) c(ompiler)
=============================================

*gloc* provides modularity and dynamic linking to WebGL and OpenGL ES 2.0 GLSL
shader systems. *gloc 0.1* features:
 - an ESSL preprocessor to expand SL source for analysis
 - preprocessor conditional expression partial evaluation
 - a semantic JSON shader object format *glo* with well-ordered symbolic dependencies
 - a client-side linker to assemble *glo* for rendering
 - compile-time macro definition
 - link-time macro definition
 - source-to-source translation with accurate source locations and
 comment annotations
 - a JavaScript implementation of the linking algorithm *glol*
 - runs as a native binary and in-the-browser via js_of_ocaml (soon)

*gloc* is licensed under a BSD-3-Clause license and distributed by Ashima
Arts as a member of the greater WebGL community.

Interfaces
----------

# [gloc](https://github.com/ashima/gloc/blob/master/gloc.ml#L55)

gloc version 0.1.0 (Ashima Arts)
  -c produce glo and halt; do not link
  -E preprocess and halt; do not parse SL
  -e parse preprocessor and halt; do not preprocess
  -u required symbol (default ['main'])
  -D define a macro
  -o output file
  --accuracy {best|preprocess} output accuracy
  -L disregard incoming line control for errors
  -x {webgl} input language
  -t {webgl} target language
  -v verbose compilation or version information
  --meta the prototypical glo file to use for metadata
  -help  Display this list of options
  --help  Display this list of options

# [glo](https://github.com/ashima/gloc/blob/master/glo.atd)

type glo = {
  glo:version;
  target:(string * version);
  ?meta:meta option;
  ~units: u list <ocaml repr="array">;
  ~linkmap: (string * string) list <json repr="object">
}

type meta = {
  copyright:(year * href);
  ~author:href list;
  ?license:href option;
  ?library:href option;
  ?version:version option;
  ?build:string option;
}

type u = {
  ~pdir: string list;
  ~edir: (string * string) list;
  ?vdir: int option;
  ~insym:string list;
  ~outsym:string list;
  ~inmac:string list;
  ~opmac:string list;
  ~outmac:string list;
  source:string;
}

type url = string
type href = (string * url)
type year = int
type version = (int * int * int)
type glom <ocaml_json module="Yojson.Safe" t="json"> = abstract

# [glol](https://github.com/ashima/gloc/blob/master/glol.ml)

The source code is conversationally-commented. If you have questions or
concerns, just ask! :-)

Commercial users of *gloc* include:
 - Ashima Arts' Ooman (http://ashimagames.com/)

*glo* shader libraries include:
 - Ashima Arts' webgl-noise

Special thanks to the WebGL working group for making this possible by
allowing UTF8 in shader comments.

Suggestions? Bugs? Feature wishes? Comments? Language translations?
Please use the GitHub project management features in the top bar to help
out the project!

Do you use gloc in production? Have you published open source shader
libraries that use gloc? Let us know and we'll add you to this file.

Gloc Links Our Creativity! Carpe Ignem!
