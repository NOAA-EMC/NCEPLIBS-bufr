#!/bin/bash
# This is a test script for NCEPLIBS-bufr, to runs some tests of the gettab utility.
#
# Jack Woollen 2022-02-18

set -eu

# Get the base directory for the tests, then cd to that directory.
basedir="`dirname $0`/../test"
cd ${basedir}

# Test #1, reading input 1bamua file.
outfile_1=testrun/1bamua.table.run
../utils/gettab testfiles/data/1bamua > ${outfile_1} && diff -w ${outfile_1} testfiles/testoutput/1bamua.table.out
[[ ${?} -ne 0 ]] && exit 1

# We expect some of the following tests may return a non-zero exit code, but we don't want
# to immediately exit the script when that happens.
set +e

# Test #2, for wrong number of arguments.
outfile_2=testrun/gettab_2.out
../utils/gettab > ${outfile_2}
[[ ${?} -ne 2 || `grep -c "Usage: gettab <bufrfile>" ${outfile_2}` -ne 1 ]] && exit 2

# Test #3, for non-existent input file.
outfile_3=testrun/gettab_3.out
../utils/gettab BUFRLIB_DUMMY > ${outfile_3}
[[ ${?} -ne 3 || `grep -c "does not exist" ${outfile_3}` -ne 1 ]] && exit 3

# Success!
exit 0
