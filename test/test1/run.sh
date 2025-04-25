#!/bin/sh
set -eux

DIR=$(cd $(dirname $0) && pwd)

RocqNavi=$DIR/../../rocqnavi

rm -rf $DIR/html
mkdir $DIR/html

cd $DIR
VFiles="Main.v"
coqc $VFiles

GlobFiles="Main.glob"
$RocqNavi -title "test1" -d ./html $VFiles $GlobFiles

diff -r --exclude=rocqnavi.css --exclude=rocqnavig.js \
  $DIR/expected_html $DIR/html
