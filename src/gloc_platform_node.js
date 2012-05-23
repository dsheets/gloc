var fs = require('fs');

var ocaml = {};

function caml_sys_getenv(name) {
  return process.env[name];
}

var register_ocaml_fn = (function (ocaml) {
  return function (name, fn) {
    ocaml[name] = fn;
  };
})(ocaml);

function gloc_stdout(msg) {
  process.stdout.write(msg);
}

function gloc_stderr(msg) {
  process.stderr.write(msg);
}

// Node does not seem to support reading from STDIN synchronously
function gloc_stdin() {  
  throw "Input from STDIN is not supported under Node JS.";
}

// I'm assuming all files are in utf8
function gloc_fs_read(path) {
  return fs.readFileSync(path, "utf8");
}

function gloc_fs_write(path, content) {
  fs.writeFileSync(path, content);
}

