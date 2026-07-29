#!/bin/sh
set -eux
PROJECT=${SOURCE:-"./infotheo"}
REVISION=${REVISION:-"no"}
DIR=$(pwd `dirname .`)
OUTDIR=${OUTDIR:-$DIR/html/infotheo-$REVISION}
CoqProject=${CoqProject:-_CoqProject}

rm -rf $OUTDIR
mkdir -p $OUTDIR

cd $PROJECT

ls -l

FILES=$(find . -name "*.v" -or -name "*.glob")

coqdep -f $CoqProject > depend.d
cat -n depend.d >&2

$DIR/rocqnavi -title "Infotheo-$REVISION" -d $OUTDIR -base infotheo \
  -coqlib https://coq.inria.fr/doc/V8.20.1/stdlib/ \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.ssreflect \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.algebra \
  -external https://math-comp.github.io/analysis/htmldoc_1_9_0/ mathcomp.analysis \
  -file-graph-from-depend "depend.d" \
  -show-type-information-using-coqtop-process \
  $FILES

