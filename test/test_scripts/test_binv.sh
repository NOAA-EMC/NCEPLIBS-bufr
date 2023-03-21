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

# We expect some of the following tests may return a non-zero exit code, but we don't want
# to immediately exit the script when that happens.
set +e

# Test #2, for wrong number of arguments.
outfile_2=testrun/binv_2.out
../utils/binv > ${outfile_2}
[[ ${?} -ne 2 || `grep -c "Usage: binv <bufrfile>" ${outfile_2}` -ne 1 ]] && exit 2

# Test #3, for non-existent input file.
outfile_3=testrun/binv_3.out
../utils/binv BUFRLIB_DUMMY > ${outfile_3}
[[ ${?} -ne 3 || `grep -c "does not exist" ${outfile_3}` -ne 1 ]] && exit 3

# Success!
exit 0
