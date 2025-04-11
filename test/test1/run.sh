#!/bin/sh
set -eux

DIR=$(cd $(dirname $0) && pwd)

RocqNavi=$DIR/../../coq2html

rm -rf $DIR/html
mkdir $DIR/html

VFiles="$DIR/Main.v"
coqc $VFiles

GlobFiles="$DIR/Main.glob"
$RocqNavi -title "test1" -d $DIR/html $VFiles $GlobFiles

diff -r $DIR/expected_html $DIR/html
