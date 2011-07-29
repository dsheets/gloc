%public %inline fuse_sep_list(LEFT,X,sep,RIGHT)
  : l=LEFT; xs = loption(fuse_sep_nonempty_list(X,sep)); r=RIGHT
    { {(fuse_pptok ((proj l)::xs@[proj r]))
       with v=(List.map (fun x -> x.v) xs)} }
    
%public %inline fuse_sep_nonempty_list(X,sep)
    : x=X { { x with v=[x] } }
    | x=X; s=sep; xs=fuse_sep_nonempty_list(X,sep) {
	{(fuse_pptok (x::s::xs)) with v=x::xs.v}
      }
