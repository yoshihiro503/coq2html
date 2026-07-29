#!/bin/sh

set -e
test/test1/run.bash
test/test_type_tooltip/run.bash
test/hb/run.bash
test/deflist/run.bash

echo 'All test Success!'
