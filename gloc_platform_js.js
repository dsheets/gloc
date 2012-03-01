var MissingEnvVar = function(name) {
  this.message = "Missing environment variable '"+name+"'";
  this.name = name;
};

function caml_sys_getenv(env) {
  if (env.fullBytes=="TMPDIR") return new MlString("/tmp");
  if (env.fullBytes=="TEMP") return new MlString("/tmp");
  console.dir(env);
  throw (new MissingEnvVar(env.string));
}

var ocaml = {};
var register_ocaml_fn = (function(m) {
  return function(name, fn) { m[name] = fn; };
})(ocaml);

var fs = {};

function gloc_stdout(s) {
    var f = fs["[stdout]"];
    f.editor.setValue(s);
    f.show();
}

function gloc_stderr(s) {
    var f = fs["[stderr]"];
    f.editor.setValue(f.editor.getValue()+s);
    f.show();
}

function gloc_stdin() {
    return fs["[stdin]"].editor.getValue();
}

function gloc_fs_read(fn) {
    return fs[fn].editor.getValue();
}

function gloc_fs_write(fn, s) {
    fs[fn].editor.setValue(s);
    fs[fn].show();
}

var std = {"[stdout]": false, "[stdin]": false, "[stderr]": false};
function File(name) {
    var fdiv = document.createElement("div");
    var fhead = document.createElement("div");
    var fdel = document.createElement("button");
    var ftitle = document.createElement("h2");
    var fhide = document.createElement("button");

    fdiv.id = "fs"+name;
    fdiv.className = "fs";
    ftitle.innerText = name;

    fdel.innerText = "X";
    fdel.className = "closefile";
    fdel.onclick = function () {
        fdiv.parentNode.removeChild(fdiv);
        delete fs[name];
    };

    var hidden = false;
    fhide.innerText = "-";
    fhide.className = "hidesource";
    var file = this;
    fhide.onclick = function() {
        if (hidden) file.show();
        else file.hide();
    };

    fhead.appendChild(fdel);
    fhead.appendChild(ftitle);
    fhead.appendChild(fhide);
    fdiv.appendChild(fhead);

    document.getElementById("fs").appendChild(fdiv);

    this.connect = function () { fdel.style.display="none"; return this; };
    this.disconnect = function() { fdel.style.display="inline"; return this; };

    var readonly = false;
    if (name=="[stdout]" || name=="[stderr]") {
        readonly=true;
        fdiv.className += " readonly";
    }

    var feditor = CodeMirror(fdiv, {
        lineNumbers: true,
        mode: "text/x-csrc",
        keyMap: "emacs",
        readOnly: readonly,
        lineWrapping: true
    });

    this.show = function() {
        fhide.innerText = "-";
        fhide.className = "hidesource";
        feditor.getWrapperElement().style.display="block";
        hidden = false;
    };
    this.hide = function() {
        fhide.innerText = "+";
        fhide.className = "showsource";
        feditor.getWrapperElement().style.display="none";
        hidden = true;
    };

    this.name=name;
    this.dom=fdiv;
    this.editor=feditor;
    fs[name] = this;
}

function init_fs() {
    (new File("[stderr]")).connect();
    (new File("[stdout]")).connect();
    (new File("[stdin]")).connect();
}

var cmd = {};
function init_gloc() {
    var form = document.getElementById("gloc-cli");
    form.onsubmit = function(e) {
        e.preventDefault();
        var args = [];
        for (var c in cmd) {
            args[args.length] = cmd[c].trim();
        }
        args = args.join(" ").split(" ").filter(function (s) { return s!=""; });
        fs["[stderr]"].editor.setValue("");
        fs["[stdout]"].editor.setValue("");
        ocaml.gloc(args);
        return false;
    };
}

function init() {
    init_fs();
    init_gloc();
}

function update_cmd(n,flag) {
    var btn = document.getElementById("gloc-cmd");
    var copy = document.getElementById("gloc-cmd-copy");
    var s = "gloc ";
    if (flag=="") delete cmd[n];
    else cmd[n]=flag;
    for (var c in cmd) {
        s += cmd[c].trim() + " ";
    }
    btn.value = copy.innerText = s.trim();
}

function update_fs(e) {
    if (e.target.value in fs) {
        fs[e.target.value].connect();
    } else if (e.target.value != "" && e.target.value != "-") {
        (new File(e.target.value)).connect();
    }
    var filelists = document.getElementsByClassName("filelist");
    var filenames = document.getElementsByClassName("filename");
    var active = {};
    for (var i = 0; i < filelists.length; i++) {
        active[filelists[i].value]=true;
    }
    for (var i = 0; i < filenames.length; i++) {
        active[filenames[i].value]=true;
    }
    for (var fn in fs) {
        if (!(fn in active) && !(fn in std)) {
            fs[fn].disconnect();
        }
    }
}

function expandable_list(row,blur) {
    var els = document.getElementsByClassName(row);
    for (var i = 0; i < els.length; i++) {
        els[i].getElementsByTagName("input")[0].onblur = blur;
        els[i].oninput = function (e) {
            var rows = document.getElementsByClassName(row);
            if (e.target.value=="" && rows.length > 1) {
                if (this.previousSibling.className
                    && this.previousSibling.className.indexOf(row) >= 0) {
                    this.previousSibling.getElementsByTagName("input")[0].focus();
                } else {
                    this.nextSibling.getElementsByTagName("input")[0].focus();
                }
                this.parentNode.removeChild(this);
            }
            var last = rows[rows.length-1];
            var input = last.getElementsByTagName("input")[0];
            var v = input.value;
            if (v!="") {
                input.value = "";
                last.parentNode.insertBefore(last.cloneNode(true),last.nextSibling);
                input.value = v;
                expandable_list(row,blur);
            }
        };
    }
}