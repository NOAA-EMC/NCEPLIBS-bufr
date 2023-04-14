#!/bin/bash
# This is a test script for NCEPLIBS-bufr, to runs some tests of the ufbrw subrouine
#
# Jack Woollen 2022-02-18

set -eu

# Get the base directory for the tests, then cd to that directory.
basedir="`dirname $0`/../test"
cd ${basedir}

# Test #1, reading input prepbufr2.ref file with a number of user strings to test ufbrw and its callee subrouines
args_1="-n testfiles/data/prepbufr2.ref"
../utils/ufbrw_test ${args_1} 
[[ ${?} -ne 0 ]] && exit 1
exit 0


