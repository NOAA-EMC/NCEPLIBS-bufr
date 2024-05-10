#!/bin/bash
# This is a test script for NCEPLIBS-bufr, to runs some tests of the apxdx utility.
#
# J. Ator 2024-05-01

set -eu

# Get the base directory for the tests, then cd to that directory.
basedir="`dirname $0`/../test"
cd ${basedir}

# Test #1, appending apxdx_1 file.
outfile_1=testrun/apxdx_1.out
# Make a copy of the input BUFR file to use in this test, since the apxdx utility appends directly to its input.
cp testfiles/data/apxdx_1 ${outfile_1}
../utils/apxdx ${outfile_1} testfiles/IN_11_bufrtab && diff -w ${outfile_1} testfiles/testoutput/apxdx_1.out
[[ ${?} -ne 0 ]] && exit 1

# We expect some of the following tests may return a non-zero exit code, but we don't want
# to immediately exit the script when that happens.
set +e

# Test #2, for wrong number of arguments.
outfile_2=testrun/apxdx_2.out
../utils/apxdx > ${outfile_2}
[[ ${?} -ne 1 || `grep -c "Usage: apxdx " ${outfile_2}` -ne 1 ]] && exit 2

# Test #3, for non-existent BUFR file.
outfile_3=testrun/apxdx_3.out
../utils/apxdx BUFRLIB_DUMMY testfiles/IN_11_bufrtab > ${outfile_3}
[[ ${?} -ne 2 || `grep -c "Specified BUFR file.*does not exist" ${outfile_3}` -ne 1 ]] && exit 3

# Test #4, for non-existent DX table.
outfile_4=testrun/apxdx_4.out
../utils/apxdx ${outfile_1} BUFRLIB_DUMMY > ${outfile_4}
[[ ${?} -ne 3 || `grep -c "Specified DX table.*does not exist" ${outfile_4}` -ne 1 ]] && exit 4

# Success!
exit 0
