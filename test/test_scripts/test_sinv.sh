#!/bin/bash
# This is a test script for NCEPLIBS-bufr, to runs some tests of the sinv utility.
#
# Jack Woollen 2022-02-18

set -eu

# Get the base directory for the tests, then cd to that directory.
basedir="`dirname $0`/../test"
cd ${basedir}

# Test #1, reading satwndbufr file.
outfile_1=testrun/sinv_1.out
args_1="testfiles/data/satwndbufr ../tables"
../utils/sinv ${args_1} > ${outfile_1} && diff -w ${outfile_1} testfiles/testoutput/sinv_1.out
[[ ${?} -ne 0 ]] && exit 1

# We can't run the following 2 tests without passing in the 2nd argument, because "ctest" runs
# before "make install", so the default location of the master tables doesn't exist yet.

# Test #2, reading sinv_2 file.
outfile_2=testrun/sinv_2.out
args_2="testfiles/data/sinv_2 ../tables"
../utils/sinv ${args_2} > ${outfile_2} && diff -w ${outfile_2} testfiles/testoutput/sinv_2.out
[[ ${?} -ne 0 ]] && exit 2

# Test #3, reading gpsro file.
outfile_3=testrun/sinv_3.out
args_3="testfiles/data/gpsro ../tables"
../utils/sinv ${args_3} > ${outfile_3} && diff -w ${outfile_3} testfiles/testoutput/sinv_3.out
[[ ${?} -ne 0 ]] && exit 3

# We expect some of the following tests may return a non-zero exit code, but we don't want
# to immediately exit the script when that happens.
set +e

# Test #4, for wrong number of arguments.
outfile_4=testrun/sinv_4.out
../utils/sinv > ${outfile_4}
[[ ${?} -ne 2 || `grep -c "Usage: sinv satbufrfile" ${outfile_4}` -ne 1 ]] && exit 4

# Success!
exit 0
