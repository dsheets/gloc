var fs = require('fs');

var ocaml = {};

global.caml_sys_getenv = function (name) {
  return process.env[name];
};

global.register_ocaml_fn = (function (ocaml) {
  return function (name, fn) {
    ocaml[name] = fn;
  };
})(ocaml);

global.gloc_stdout = function (msg) {
  process.stdout.write(msg);
};

global.gloc_stderr = function (msg) {
  process.stderr.write(msg);
};

// Node does not seem to support reading from STDIN synchronously
global.gloc_stdin = function () {  
  throw "Input from STDIN is currently not supported under Node JS.\n";
};

// I'm assuming all files are in utf8
global.gloc_fs_read = function (path) {
  return fs.readFileSync(path, "utf8");
};

global.gloc_fs_write = function (path, content) {
  fs.writeFileSync(path, content);
};

require('./_build/gloc_js.d.js');

// The first two arguments are the path to node and your script respectively:
ocaml.gloc(process.argv.slice(2)); 