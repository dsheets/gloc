# *gloc*: (WebGL|ESSL) gl(sl) o(bject) c(ompiler)

*gloc* provides modularity and dynamic linking to WebGL and OpenGL ES 2.0 GLSL
shader systems. *gloc 1.0* features:

 - an ESSL preprocessor to expand SL source for analysis
 - preprocessor conditional expression partial evaluation
 - a semantic JSON shader object format *glo* with well-ordered symbolic dependencies
 - a client-side linker to assemble *glo* for rendering
 - compile-time macro definition
 - link-time macro definition
 - source-to-source translation with accurate source locations and comment annotations
 - a JavaScript implementation of the linking algorithm *glol*
 - runs as a native binary and in-the-browser via js_of_ocaml

*gloc* is licensed under a
 [BSD-3-Clause](https://github.com/dsheets/gloc/blob/master/LICENSE)
 license and distributed by Ashima Arts as a member of the greater WebGL community.

## Interfaces

### [gloc](https://github.com/ashima/gloc/blob/master/gloc.ml#L44)

```
gloc version 1.0.0 (Ashima Arts)
  -c produce glo and halt; do not link
  --xml produce glo XML documents
  -E preprocess and halt; do not parse SL
  -e parse preprocessor and halt; do not preprocess
  --source strip the glo format and return the contained source
  -u required symbol (default ['main'])
  --define define a macro unit
  -D define a global macro
  -o output file
  --accuracy {best|preprocess}output accuracy
  -L disregard incoming line directives
  -x {webgl}source language
  -t {webgl}target language
  -v verbose
  --meta prototypical glo file to use for metadata
  -help  Display this list of options
  --help  Display this list of options
```

### [glo](https://github.com/ashima/gloc/blob/master/glo.atd)

```ocaml
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
  ?version:(version * url) option;
  ?build:href option;
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
```

### [glol](https://github.com/ashima/gloc/blob/master/glol.ml)

The source code is conversationally-commented. If you have questions or
concerns, just ask! :-)

## Requirements

### WebGL Application Developers

WebGL application developers need only to use the JavaScript *glol* link
algorithm at *TBA* and the gloc_js compiler available at *TBA*. No local
software installation is required.

### Shader Developers

Shader library developers and those developers with native build systems
will need the following local software packages to build *gloc*:

 - [GNU Make](http://www.gnu.org/software/make/)
 - [OCaml 3.12.0+](http://caml.inria.fr/)
 - [ulex](http://cduce.org/download.html)
 - [menhir](http://gallium.inria.fr/~fpottier/menhir/)
 - [ocaml-re](https://github.com/avsm/ocaml-re)
 - [atdgen](http://oss.wink.com/atdgen/)

These software packages may be available in your operating system's
package repository.

### gloc Developers

Developers of the *gloc* tool itself will need all of the packages
required by shader developers for a local *gloc* build as well as the
latest development version of
[js_of_ocaml](http://ocsigen.org/js_of_ocaml/install) to build the
JavaScript port of the compiler and linker.

## Users

Commercial users of *gloc* include:

 - Ashima Arts' [Ooman](http://ashimagames.com/)

*glo* shader libraries include:

 - Ashima Arts' [webgl-noise](https://github.com/ashima/webgl-noise)

## Contributors
Special thanks to the WebGL working group for making this possible by
allowing UTF8 in shader comments.

## Fine Print
Suggestions? Bugs? Feature wishes? Comments? Language translations?
Please use the GitHub project management features in the top bar to help
out the project!

Do you use gloc in production? Have you published open source shader
libraries that use gloc? Let us know and we'll add you to this file.

Gloc Links Our Creativity! Carpe Ignem!
