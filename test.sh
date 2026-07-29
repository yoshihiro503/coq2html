#!/bin/sh

set -e
examples/test1/run.bash
examples/test_type_tooltip/run.bash
examples/hb/run.bash

echo 'All test Success!'
