var tests = {};

function run_tests() {
    function diff(lbl,c) {
	c.mode = 'diff';
	c.sourcelabel = 'Ideal';
	
	var contain = document.createElement("div");
	var button = document.createElement("button");
	var diff = document.createElement("div");
	diff.style.display = "none";
	diff.innerHTML = prettydiff(c);
	button.innerText = lbl;
	button.onclick = function() {
	    if (diff.style.display=="none") {
		diff.style.display="block";
	    } else {
		diff.style.display="none";
	    }
	};
	contain.appendChild(button);
	contain.appendChild(diff);
	return contain;
    }

    function message(s) {
	var message = document.createElement("div");
	message.innerText = s;
	return message;
    }

    var envs = ["posix","js_of_ocaml","javascript"];
    for (var test in tests) {
	var row = document.getElementById("test-"+test);
	var posix = row.getElementsByClassName("posix")[0];
	if (tests[test].tout==tests[test].posix.out
	    && tests[test].terr==tests[test].posix.err) {
	    posix.className += " pass";
	} else {
	    posix.className += " fail";
	    if (tests[test].tout!=tests[test].posix.out) {
		posix.appendChild(diff("stdout",
				       {'source':tests[test].tout,
					'difflabel':"POSIX",
					'diff':tests[test].posix.out}));
	    }
	    if (tests[test].terr!=tests[test].posix.err) {
		posix.appendChild(diff("stderr",
				       {'source':tests[test].terr,
					'difflabel':"POSIX",
					'diff':tests[test].posix.err}));
	    }
	}

	var js_of_ocaml = row.getElementsByClassName("js_of_ocaml")[0];
	var args = tests[test].args.split(' ');
	var syms = [];
	var defs = [];
	var paths = [];
	for (var i = 0; i < args.length; i++) {
	    if (args[i]=="-u") {
		syms[syms.length]=args[i+1];
		i++;
	    } else if (args[i]=="-D") {
		defs[defs.length]=args[i+1];
		i++;
	    } else {
		paths[paths.length]=args[i];
	    }
	}
	var err = "";
	var out = "";
	try {
	    out = ocaml.link("",syms,JSON.stringify(tests[test].glom));
	} catch (e) {
	    err = ocaml.string_of_error(e)+"\nFatal: unrecoverable link error (9)\n";
	};
	if (tests[test].tout==out && tests[test].terr==err) {
	    js_of_ocaml.className += " pass";
	} else {
	    js_of_ocaml.className += " fail";
	    if (tests[test].tout!=out) {
		if (out=="") js_of_ocaml.appendChild(message(err));
		else js_of_ocaml.appendChild(diff("stdout",
						  {'source':tests[test].tout,
						   'difflabel':"js_of_ocaml",
						   'diff':out}));
	    }
	    if (tests[test].terr!=err) {
		if (err=="") js_of_ocaml.appendChild(message(out));
		else js_of_ocaml.appendChild(diff("stderr",
						  {'source':tests[test].terr,
						   'difflabel':"js_of_ocaml",
						   'diff':err}));
	    }
	}

	var javascript = row.getElementsByClassName("javascript")[0];
	err = "";
	out = "";
	var glol = new GLOL();
	try {
	    out = glol.link("",syms,tests[test].glom);
	} catch (e) {
	    if (e.hasOwnProperty("name")) {
		err = ocaml.string_of_js_exc(e);
		err +="\nFatal: unrecoverable link error (9)\n";
	    } else {
		err = e.type;
	    }
	}
	if (tests[test].tout==out && tests[test].terr==err) {
	    javascript.className += " pass";
	} else {
	    javascript.className += " fail";
	    if (tests[test].tout!=out) {
		if (out=="") javascript.appendChild(message(err));
		else javascript.appendChild(diff("stdout",
						 {'source':tests[test].tout,
						  'difflabel':"javascript",
						  'diff':out}));
	    }
	    if (tests[test].terr!=err) {
		if (err=="") javascript.appendChild(message(out));
		else javascript.appendChild(diff("stderr",
						 {'source':tests[test].terr,
						  'difflabel':"javascript",
						  'diff':err}));
	    }
	}
    }

    var testgroups = document.getElementsByClassName("testgroup");
    for (var testgroup=0; testgroup < testgroups.length; testgroup++) {
	for (var e=0; e < envs.length; e++) {
	    var env = testgroups[testgroup].getElementsByClassName(envs[e])[0];
	    var tests_ = document.getElementsByClassName(testgroups[testgroup].id);
	    var total = tests_.length;
	    var passed = 0;
	    for (var i=0; i < tests_.length; i++) {
		var test = tests_[i].getElementsByClassName(envs[e])[0];
		if (test.className.indexOf("pass") >= 0) passed++;
	    }
	    if (total == passed) {
		env.className += " pass";
	    } else if (passed > 0) {
		env.className += " partial";
	    } else {
		env.className += " fail";
	    }
	}
    }
}
