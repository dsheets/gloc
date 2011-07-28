%public %inline fuse_sep_list(LEFT,X,sep,RIGHT):
  l=LEFT; xs = loption(fuse_sep_nonempty_list(X,sep)); r=RIGHT
    { {(fuse_pptok ((proj l)::()@[proj r])) with }
