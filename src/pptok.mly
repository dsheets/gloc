%%

%public fuse_sep_list(LEFT,X,sep,RIGHT)
  : l=LEFT; xs = option(fuse_sep_nonempty_list(X,sep)); r=RIGHT
    { match xs with
        | None -> {(fuse_pptok [proj l; proj r]) with v=[]}
	| Some xs -> {(fuse_pptok [proj l; proj xs; proj r])
	              with v=xs.v}
    }

%public fuse_sep_nonempty_list(X,sep)
    : x=X { { x with v=[x] } }
    | x=X; s=sep; xs=fuse_sep_nonempty_list(X,sep) {
        {(fuse_pptok [proj x; proj s; proj xs]) with v=(x::xs.v)}
    }

%public fuse_sep_slexpr_list(LEFT,X,sep,RIGHT)
  : l=LEFT; xs = option(fuse_sep_slexpr_nonempty_list(X,sep)); r=RIGHT
    { match xs with
        | None -> {(fuse_pptok [proj l; proj r]) with v=[]}
	| Some xs -> {(fuse_pptok [proj l; proj xs; proj r])
	              with v=xs.v}
    }

%public fuse_sep_slexpr_nonempty_list(X,sep)
    : x=X { { (proj_slexpr x) with v=[x] } }
    | x=X; s=sep; xs=fuse_sep_slexpr_nonempty_list(X,sep) {
        {(fuse_pptok [proj_slexpr x; proj s; proj xs]) with v=(x::xs.v)}
    }
