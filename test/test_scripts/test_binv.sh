#!/bin/bash
# This is a test script for NCEPLIBS-bufr, to runs some tests of the binv utility.
#
# Jack Woollen 2022-02-18

set -eu

# Get the base directory for the tests, then cd to that directory.
basedir="`dirname $0`/../test"
cd ${basedir}

# Test #1, reading input satwndbufr file.
outfile_1=testrun/binv_1.out
../utils/binv testfiles/data/satwndbufr > ${outfile_1} && diff -w ${outfile_1} testfiles/testoutput/binv.out
[[ ${?} -ne 0 ]] && exit 1

# We expect the following tests to return a non-zero exit code, but we don't want
# to immediately exit the script when that happens.
set +e

# Test #2, for wrong number of arguments.
outfile_2=testrun/binv_2.out
../utils/binv > ${outfile_2}
[[ ${?} -ne 2 || `grep -c "Usage: binv <bufrfile>" ${outfile_2}` -ne 1 ]] && exit 2

# Success!
exit 0
