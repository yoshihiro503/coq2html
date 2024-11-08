#!/bin/sh
set -eux
MATHCOMP_ANALYSIS=./analysis
DIR=$(pwd `dirname .`)
OUTDIR=$DIR/html

rm -rf $OUTDIR
mkdir $OUTDIR

cd $MATHCOMP_ANALYSIS

ls -l

FILES=$(find classical/ theories/ -name "*.v" -or -name "*.glob")

coqdep -f _CoqProject > depend.d
cat -n depend.d >&2
$DIR/ocamldot/ocamldot depend.d > depend.dot
sed -i 's/Classical\//mathcomp\.classical\./' depend.dot
sed -i 's/Theories\//mathcomp\.analysis\./' depend.dot

$DIR/tools/generate-hierarchy-graph.sh

$DIR/coq2html -title "MathComp-Analysis" -d $OUTDIR -base mathcomp \
  -Q theories analysis -coqlib https://coq.inria.fr/doc/V8.18.0/stdlib/ \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.ssreflect \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.algebra \
  -hierarchy-graph "hierarchy-graph.dot" \
  -dependency-graph "depend.dot" \
  $FILES

cp hierarchy-graph.dot depend.dot $OUTDIR/
