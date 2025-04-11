#!/bin/sh
set -eux
PROJECT=./infotheo
COMMIT_HASH=$1
DIR=$(pwd `dirname .`)
OUTDIR=$DIR/html/infotheo

rm -rf $OUTDIR
mkdir -p $OUTDIR

cd $PROJECT

ls -l

FILES=$(find . -name "*.v" -or -name "*.glob")

coqdep -f _CoqProject > depend.d
cat -n depend.d >&2
$DIR/ocamldot/ocamldot --style "bgcolor=white; splines=true; nodesep=1; node [fontsize=18, shape=rect, color=\"#dbc3b6\", style=filled];" depend.d > depend.dot

sed -i 's/\//\./g' depend.dot

$DIR/coq2html -title "Infotheo ($COMMIT_HASH)" -d $OUTDIR -base infotheo \
  -coqlib https://coq.inria.fr/doc/V8.20.1/stdlib/ \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.ssreflect \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.algebra \
  -external https://math-comp.github.io/analysis/htmldoc_1_9_0/ mathcomp.analysis \
  -dependency-graph "depend.dot" \
  $FILES

