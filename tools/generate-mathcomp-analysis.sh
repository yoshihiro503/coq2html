#!/bin/bash
set -eux
MATHCOMP_ANALYSIS=${MATHCOMP_ANALYSIS:-"./analysis"}
REVISION=${REVISION:-"no"}
DIR=$(pwd `dirname .`)
OUTDIR=${OUTDIR:-$DIR/html/analysis-$REVISION}
CoqProject=${CoqProject:-_CoqProject}
INDEX_BLACKLIST_FILE=$DIR/tools/index-blacklist

rm -rf $OUTDIR
mkdir -p $OUTDIR

cd $MATHCOMP_ANALYSIS

ls -l

FILES=$(find analysis classical reals -name "*.v" -or -name "*.glob")

coqdep -f $CoqProject > depend.d
cat -n depend.d >&2
$DIR/ocamldot/ocamldot --style "bgcolor=white; splines=true; nodesep=1; node [fontsize=18, shape=rect, color=\"#dbc3b6\", style=filled];" depend.d > depend.dot
cat -n depend.dot >&2

sed -i 's/Classical/mathcomp\.classical/' depend.dot
sed -i 's/Theories/mathcomp\.analysis/' depend.dot
sed -i 's/Reals_stdlib/mathcomp\.reals_stdlib/' depend.dot
sed -i 's/Experimental_reals/mathcomp\.experimental_reals/' depend.dot
sed -i 's/Reals/mathcomp\.reals/' depend.dot
sed -i 's/Analysis_stdlib/mathcomp\.analysis_stdlib/' depend.dot
sed -i 's/\//\./g' depend.dot

cat -n depend.dot >&2
$DIR/tools/generate-hierarchy-graph.sh

$DIR/rocqnavi -title "MathComp-Analysis-$REVISION" -d $OUTDIR \
  -coqlib https://coq.inria.fr/doc/V8.20.1/stdlib/ \
  -Q analysis mathcomp.analysis \
  -Q classical mathcomp.classical \
  -Q reals mathcomp.reals \
  -Q reals_stdlib mathcomp.reals_stdlib \
  -Q experimental_reals mathcomp.experimental_reals \
  -Q theories mathcomp.analysis \
  -Q analysis_stdlib mathcomp.analysis_stdlib \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.ssreflect \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.algebra \
  -hierarchy-graph "hierarchy-graph.dot" \
  -dependency-graph "depend.dot" \
  -index-blacklist $INDEX_BLACKLIST_FILE \
  -show-type-infomation-using-coqtop-process \
  $FILES

cp hierarchy-graph.dot depend.dot $OUTDIR/
