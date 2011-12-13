rm -f circ.glsl
$GLOC -o circ.glsl circ_a.glsl circ_b.glsl 2>circ.err
ERR_CODE=$?
diff circ.err circ.true
exit $(($ERR_CODE==7 && $?==0 && [ -e circ.glsl ]==1))
