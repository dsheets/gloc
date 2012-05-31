#!/usr/bin/env node

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

global.gloc_stdout = function (msg, cb) {
  process.stdout.write(msg);
  cb();
};

global.gloc_stderr = function (msg, cb) {
  process.stderr.write(msg);
  cb();
};

// Node does not seem to support reading from STDIN synchronously
global.gloc_stdin = function (cb) {
  var s = "";
  process.stdin.resume();
  process.stdin.setEncoding('utf8');
  
  process.stdin.on('data', function (chunk) {
    s += chunk;
  });

  process.stdin.on('end', function () {
    cb(s);
  });
};

// I'm assuming all files are in utf8
global.gloc_fs_read = function (path, cb) {
  fs.readFile(path, "utf8",function(err,data) {
    if (err) throw err;
    cb(data);
  });
};

global.gloc_fs_write = function (path, content, cb) {
  fs.writeFile(path, content, "utf8", function(err) {
    if (err) throw err;
    cb();
  });
};

require('../lib/gloc_js.js');

// The first two arguments are the path to node and your script respectively:
ocaml.gloc(process.argv.slice(2));
