#!/bin/bash
# This is a test script for NCEPLIBS-bufr, to runs some tests of the readmp utility.
#
# Jack Woollen 2022-02-18

set -eu

# Get the base directory for the tests, then cd to that directory.
basedir="`dirname $0`/../test"
cd ${basedir}

# Test #1, reading input prepbufr2.ref file.
args_1="testfiles/data/prepbufr2.ref q"
outfile_1=testrun/readmp_1.out
../utils/readmp ${args_1} > ${outfile_1} && diff -w ${outfile_1} testfiles/testoutput/readmp.out
[[ ${?} -ne 0 ]] && exit 1

# We expect some of the following tests may return a non-zero exit code, but we don't want
# to immediately exit the script when that happens.
set +e

# Test #2, for wrong number of arguments.
outfile_2=testrun/readmp_2.out
../utils/readmp > ${outfile_2}
[[ ${?} -ne 2 || `grep -c "Usage: readmp <bufrfile>" ${outfile_2}` -ne 1 ]] && exit 2

# Test #3, for non-existent input file.
outfile_3=testrun/readmp_3.out
../utils/readmp BUFRLIB_DUMMY > ${outfile_3}
[[ ${?} -ne 3 || `grep -c "does not exist" ${outfile_3}` -ne 1 ]] && exit 3

# Success!
exit 0
