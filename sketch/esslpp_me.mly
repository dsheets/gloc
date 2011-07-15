source_pbal
: v=source_np+ {
  {(fuse_pptok (List.map proj_pptok_type v)) with v}
}
| l=LEFT_PAREN; s=source_pbal*; r=RIGHT_PAREN {
    let ch = (Punc l)::(List.rev (Punc r)::(List.rev s)) in
      {(fuse_pptok (List.map proj_pptok_type ch))
       with v=ch}
  }

fmacro_args
: v=source_ncnp+ {
  {(fuse_pptok (List.map proj_pptok_type v)) with v}
}
| l=LEFT_PAREN; s=source_pbal*; r=RIGHT_PAREN {
    let ch = (Punc l)::(List.rev (Punc r)::(List.rev s)) in
      {(fuse_pptok (List.map proj_pptok_type ch))
       with v=ch}
  }
