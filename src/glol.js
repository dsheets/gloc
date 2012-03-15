/**
 * @preserve Copyright 2012 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Licensed under BSD-3-Clause found at http://www.spdx.org/licenses/BSD-3-Clause
 *
 * @constructor
 */
window['GLOL'] = function() {};

(function() {
  // Let's link some glo!

  // There are a number of exceptions that the algorithm can emit
  /** @constructor */
  var MissingMacro = function(addr,macro) {
    this.name = "MissingMacro"; this.addr = addr; this.macro = macro;
  };
  /** @constructor */
  var MissingSymbol = function(addr,sym) {
    this.name = "MissingSymbol"; this.addr = addr; this.symbol = sym;
  };
  /** @constructor */
  var NotFound = function(key) {
    this.name = "NotFound"; this.key = key;
  };
  /** @constructor */
  var CircularDependency = function(addrl) {
    this.name = "CircularDependency"; this.addrs = addrl;
  };
  /** @constructor */
  var SymbolConflict = function(n,sym,addr,caddr) {
    this.name = "SymbolConflict"; this.sym_a = n; this.sym_b = sym;
    this.addr_a = addr; this.addr_b = caddr;
  };
  /** @constructor */
  var UnknownBehavior = function(addr, b) {
    this.name = "UnknownBehavior"; this.addr = addr; this.behavior = b;
  };
  /** @constructor */
  var UnknownGloVersion = function(path,version) {
    this.name = "UnknownGloVersion"; this.path = path; this.version = version;
  };

  // Prepare the packaged source for concatenation.
  /**
   * @type {function({copyright: Array.<number|Array.<string>>},*,*,*,*) : string }
   */
  function armor(meta,name,linkmap,fs_ct,opmac,s) {
    // Replace special symbols in line directives to satisfy linkmap
    // required regexp (greetz jwz) instead of macros due to ANGLE bug 183
    var intpatt = /GLOC_([0-9]+)/g;
    function f(s,n) {
      var anno = "";
      var fn = parseInt(n,10);
      if (linkmap[n]) {
	anno = "/* "+name+linkmap[n]+" */";
      }
      return (""+(fn+fs_ct)+anno);
    }
    s = s.replace(intpatt,f);

    var head = "";
    if (meta) {
      var authors="",license="",library="",version="",build="";
      function field(name,link) {
	return "// "+name+": "+link[0]+" <"+link[1]+">\n";
      }
      if (meta.author) {
	for (var i=0; i < meta.author.length; i++) {
	  authors += field("Author",meta.author[i]);
	}
      }
      if (meta.license) license = field("License",meta.license);
      if (meta.library) library = field("Library",meta.library);
      if (meta.version) {
	var v = meta.version;
	version = "// Version: "+v[0][0]+"."+v[0][1]+"."+v[0][2];
        version+= " <"+v[1]+">\n";
      }
      if (meta.build) build = field("Build",meta.build);
      head = ("// Copyright "+meta.copyright[0]+" "
	      +meta.copyright[1][0]+" <"+meta.copyright[1][1]+"> "
	      +"All rights reserved.\n"+license+authors+library+version+build);
    }

    // Undefine local open macros so they do not leak
    var undefs = "";
    for (var i = 0; i < opmac.length; i++) {
      undefs += "\n#undef "+opmac[i];
    }
    return head+s+undefs;
  }

  // Search a glo for a unit satisfying the supplied predicate p.
  function search(p, glo) {
    for (var u = 0; u < glo.units.length; u++) {
      var unit = glo.units[u];
      if (p(unit)) return u;
    }
    return null;
  }

  // Linearly search for an exported macro.
  function satisfy_mac(addr, macro, glo_alist) {
    for (var i = 0; i < glo_alist.length; i++) {
      var name = glo_alist[i][0], glo = glo_alist[i][1];
      var u = search(function (unit) { return unit.outmac.indexOf(macro) != -1; },
		     glo);
      if (u != null) return [name, u];
    }
    throw (new MissingMacro(addr,macro));
  }

  // Linearly search for an export symbol.
  function satisfy_sym(addr, sym, glo_alist) {
    for (var i = 0; i < glo_alist.length; i++) {
      var name = glo_alist[i][0], glo = glo_alist[i][1];
      var u = search (function(unit) { return (unit.outmac.indexOf(sym) != -1
					       || unit.outsym.indexOf(sym) != -1); },
		      glo);
      if (u != null) return [name, u];
    }
    throw (new MissingSymbol(addr, sym));
  }

  function map_of_list(v,l) {
    var m = {};
    for (var i = 0; i < l.length; i ++) m[l[i]]=v;
    return m;
  }

  // Construct the constraint data structure from a glo unit.
  /** @type {function(Array.<string|number>,
   *                  {insym: Array.<string>, outsym: Array.<string>,
   *                   inmac: Array.<string>, outmac: Array.<string>,
   *                   vdir: number, pdir: string, edir: Array.<Array.<string>>
   *                   })
   *         : {rsym: Array.<string>, rmac: Array.<string>,
   *            tsym: Object.<string,Array.<string|number>>,
   *            tmac: Object.<string,Array.<string|number>>,
   *            bsym: Object.<string,Array.<string|number>>,
   *            bmac: Object.<string,Array.<string|number>>,
   *            addr: Array.<string|number> }}
   */
  function tooth(addr, u) {
    return {
      "rsym":u.insym, "rmac":u.inmac, // Required: to be satisfied
      "tsym":{}, "tmac":{}, // Top: locally satisfied
      // Bottom: cumulatively satisfies
      "bsym":map_of_list(addr,u.outsym), "bmac":map_of_list(addr,u.outmac),
      "addr":addr
    };
  }

  function assoc(key,alist) {
    for (var i = 0; i < alist.length; i++)
      if (alist[i][0] == key) return alist[i][1];
    throw (new NotFound(key));
  }

  function lookup(glo_alist,addr) {
    return assoc(addr[0],glo_alist).units[addr[1]];
  }

  function tooth_of_addr(glo_alist,addr) {
    return tooth(addr,lookup(glo_alist,addr));
  }

  function has_addr(addr,tooth) {
    return tooth.addr[0] == addr[0] && tooth.addr[1] == addr[1];
  }

  // Advertize prior units to later units.
  function mergeb(b,top) {
    if (top.length == 0) return b;
    else {
      var zb = top[0];
      for (var k in zb.bsym) b.bsym[k] = zb.bsym[k];
      for (var k in zb.bmac) b.bmac[k] = zb.bmac[k];
      return b;
    }
  }

  // Satisfy a macro dependency with an already included dependency.
  function connect_mac(b,n,addr) {
    b.rmac=b.rmac.filter(function(m){ return m!=n; });
    b.tmac[n] = addr;
    return b;
  }

  // Satisfy a symbol dependency with an already included dependency.
  function connect_sym(b,n,addr) {
    b.rsym=b.rsym.filter(function(m){ return m!=n; });
    b.tsym[n] = addr;
    return b;
  }

  // Search for an already included macro.
  function provided_mac(n,top) {
    if (top.length == 0) return null;
    else if (n in top[0].bmac) return top[0].bmac[n];
    else return null;
  }

  // Search for an already included symbol.
  function provided_sym(n,top) {
    if (top.length == 0) return null;
    else if (n in top[0].bmac) return top[0].bmac[n];
    else if (n in top[0].bsym) return top[0].bsym[n];
    else return null;
  }

  // Given a tooth and a prefix tooth list, find conflicts.
  function conflicted(tooth,top) {
    if (top.length == 0) return null;
    else {
      for (var key in tooth.bsym) {
	if (key in top[0].bsym) return [key, top[0].bsym[key]];
	if (key in top[0].bmac) return [key, top[0].bmac[key]];
      }
      for (var key in tooth.bmac) {
	if (key in top[0].bsym) return [key, top[0].bsym[key]];
	if (key in top[0].bmac) return [key, top[0].bmac[key]];
      }
    }
    return null;
  }

  // Check for symbol conflicts
  function check_conflicts(n,tooth,zipper) {
    var conflict = conflicted(tooth,zipper[1]);
    if (conflict != null) {
      throw (new SymbolConflict(n,conflict[0],tooth.addr,conflict[1]));
    }
  }

  // Check for circular dependency
  function check_circdep(addr,zipper) {
    if (zipper[0].some(function (tooth){ return has_addr(addr,tooth); })) {
      throw (new CircularDependency(
	       zipper[0].map(function (t) { return t.addr; })));
    }
  }

  // Build a list of units with internal requirements satisfied.
  // TODO: labelled loop + continue to avoid tail calls in TCO-less JS
  function satisfy_zipper(glo_alist,zipper) {
    // At the bottom of the zipper, we must be done.
    if (zipper[0].length == 0) return zipper;
    // Subsequent units require a macro.
    else if (zipper[0][0].rmac.length != 0) {
      var b = zipper[0][0], n = b.rmac[0];
      var addr = provided_mac(n,zipper[1]);
      if (addr != null) {
	connect_mac(b,n,addr);
	return satisfy_zipper(glo_alist,zipper);
      }
      addr = satisfy_mac(b.addr,n,glo_alist);
      check_circdep(addr,zipper);
      var tooth = tooth_of_addr(glo_alist,addr);
      check_conflicts(n,tooth,zipper);
      connect_mac(b,n,addr);
      zipper[0].unshift(tooth);
      return satisfy_zipper(glo_alist,zipper);
    } else // Subsequent units require a symbol.
    if (zipper[0][0].rsym.length != 0) {
      var b = zipper[0][0], n = b.rsym[0];
      var addr = provided_sym(n,zipper[1]);
      if (addr != null) {
	connect_sym(b,n,addr);
	return satisfy_zipper(glo_alist,zipper);
      }
      addr = satisfy_sym(b.addr,n,glo_alist);
      check_circdep(addr,zipper);
      var tooth = tooth_of_addr(glo_alist,addr);
      check_conflicts(n,tooth,zipper);
      connect_sym(b,n,addr);
      zipper[0].unshift(tooth);
      return satisfy_zipper(glo_alist,zipper);
    } else { // Without further needs from below, we are ready to descend.
      var b = zipper[0].shift();
      zipper[1].unshift(mergeb(b,zipper[1]));
      return satisfy_zipper(glo_alist,zipper);
    }
  }

  // Generate a list of unit addresses from a list of required symbols and
  // a search list
  function sort(reqsym,glo_alist) {
    var addrs = reqsym.reduce(function (al,sym) {
      var addr = satisfy_sym(["[-u "+sym+"]",0],sym,glo_alist);
      if (al.every(function (a) { return a[0]!=addr[0] || a[1]!=addr[1]; })) {
	al.push(addr);
      }
      return al;
    },[]);
    var zipper = [addrs.map(function (addr) {
			      return tooth_of_addr(glo_alist,addr);
			    }),[]];
    zipper = satisfy_zipper(glo_alist,zipper);
    return zipper[1].reduce(function (al,tooth) {
			      al.unshift(tooth.addr);
			      return al;
			    },[]);
  }

  // Generate the shader preamble.
  function preamble(glol) {
    // Precedence for conflicting behaviors
    var b_order = ["require","warn","enable","disable"];
    function b_max(x,y) {
      return b_order[Math.min(b_order.indexOf(x),b_order.indexOf(y))];
    }
    function ext_merge(addr,m,ext) {
      if (ext[0] in m) {
	m[ext[0]] = b_max(ext[1],m[ext[0]]); return m;
      } else if (b_order.indexOf(ext[1]) >= 0) {
	m[ext[0]] = ext[1]; return m;
      } else throw (new UnknownBehavior(addr,ext[1]));
    }
    function ext_decl(ext,b) { return "#extension "+ext+" : "+b+"\n"; }
    function ext_segment(m) {
      var exts = "";
      for (var e in m) if (e!="all") exts += ext_decl(e,m[e]);
      return ((("all" in m)?ext_decl("all",m.all):"")+exts);
    }
    var preamble = glol.reduce(function(pre,gap) {
      var u = gap[1].units[gap[0][1]];
      return [u.vdir==null?pre[0]
	      :(pre[0]==null?u.vdir
		:Math.max(u.vdir,pre[0])),
	      u.pdir.reduce(function(p,s) { return p+s+"\n"; },pre[1]),
	      u.edir.reduce(function(m,ext) {
			      return ext_merge(gap[0],m,ext);
			    },pre[2])
	     ];
      },[null,"",{}]);
    return (((preamble[0]!=null)?("#version "+preamble[0]+"\n"):"")
	    +preamble[1]+(ext_segment(preamble[2])));
  }

  function flatten(prefix,glom) {
    return glom.reduce(function(glo_alist,ngp) {
      if (Array.isArray(ngp[1]))
	return glo_alist.concat(flatten(prefix+ngp[0]+"/",ngp[1]));
      else return glo_alist.concat([[prefix+ngp[0],ngp[1]]]);
    },[]);
  }

  function filter(alist) {
    var l = [];
    for (var i = 0; i < alist.length; i++) {
      var glo = alist[i][1];
      if (glo.glo) {
	if (glo.glo[0] != 1 || glo.glo[1] != 0) {
	  throw (new UnknownGloVersion(alist[i][0],glo.glo));
	}
	l.push(alist[i]);
      }
    }
    return l;
  }

  // Zeros may be omitted for transport; add them back.
  function add_zero(o,f,z) { if (!(f in o)) { o[f] = z; } return o; }

  function add_zeros(alist) {
    return alist.map(function (ngp) {
      var name = ngp[0], glo = ngp[1];
      add_zero(glo,"units",[]);
      add_zero(glo,"linkmap",{});
      glo.units = glo.units.map(function (u) {
	add_zero(u,"pdir",[]); add_zero(u,"edir",[]);
	add_zero(u,"insym",[]);	add_zero(u,"outsym",[]);
	add_zero(u,"inmac",[]);	add_zero(u,"opmac",[]);	add_zero(u,"outmac",[]);
	return u;
      });
      return [name,glo];
    });
  }

  // Produce a string representing a valid SL program given a list of required
  // symbols and a search list.
  /** @type {function(string,Array.<string>,*) : string } */
  window['GLOL'].prototype.link = function(prologue,reqsym,glom) {
    var reqsym_ = (reqsym.length==0) ? ["main"] : reqsym.reverse();
    var glo_alist = add_zeros(filter(flatten("",glom)));
    var glol = (sort(reqsym_,glo_alist)).map(function (addr) {
      return [addr,assoc(addr[0],glo_alist)];
    });
    return glol.reduce(function (acc,agp) {
      var sup = 0, glo = agp[1], name=agp[0][0], src=acc[0],
	  pname=acc[1][0], o=acc[1][1];
      for (var k in glo.linkmap) { sup = Math.max(sup,parseInt(k,10)); }
      var u = glo.units[agp[0][1]];
      var unit_begin = (name==pname||pname=="")?"":"// End: Copyright\n";
      return [(src+unit_begin+armor(name==pname?null:glo.meta,name,
				    glo.linkmap,o,u.opmac,u.source)+"\n"),
	      [name,o+sup+1]];
    }, [preamble(glol)+prologue,["",0]])[0];
  };
})();
