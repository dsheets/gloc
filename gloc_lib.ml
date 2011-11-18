type dialect = WebGL
type version = int * int * int
type accuracy = Best (*| Decomment | Minify | Pretty *)
type bondage = Error | Warn | Ignore
type language = {dialect:dialect;
		 version:version;
		 accuracy:accuracy;
		 bond:bondage;
		}

type state = {preprocess:bool ref;
	      compile:bool ref;
	      verbose:bool ref;
	      linectrl:bool ref;
	      output:string option ref;
	      inputs:string list ref;
	      inlang:language ref;
	      outlang:language ref;
	     }
