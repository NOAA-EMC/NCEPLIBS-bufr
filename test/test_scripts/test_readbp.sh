#!/bin/bash
# This is a test script for NCEPLIBS-bufr, to runs some tests of the readbp utility.
#
# Jack Woollen 2022-02-18

set -eu

# Get the base directory for the tests, then cd to that directory.
basedir="`dirname $0`/../test"
cd ${basedir}

# Test #1, reading input prepbufr2.ref file with -n option.
args_1="-n testfiles/data/prepbufr2.ref"
outfile_1=testrun/readbp_1.out
../utils/readbp ${args_1} > ${outfile_1} && diff -w ${outfile_1} testfiles/testoutput/readbp.out
[[ ${?} -ne 0 ]] && exit 1

# Test #2, reading input prepbufr2.ref file with -n and -s options.
args_2="-n -s KBOU testfiles/data/prepbufr2.ref"
outfile_2=testrun/readbp_2.out
../utils/readbp ${args_2} > ${outfile_2}
[[ ${?} -ne 0 || `egrep -c '(72293|KTKI)' ${outfile_2}` -ne 0 || `egrep -c KBOU ${outfile_2}` -ne 1 ]] && exit 2

# Test #3, reading input prepbufr2.ref file with -n, -h, and -w options.
args_3="-n -h -w 240.0 245.0 30.0 35.0 testfiles/data/prepbufr2.ref"
outfile_3=testrun/readbp_3.out
../utils/readbp ${args_3} > ${outfile_3}
[[ ${?} -ne 0 || `egrep -c '(KBOU|KTKI)' ${outfile_3}` -ne 0 || `egrep -c 72293 ${outfile_3}` -ne 2 \
  || `cat ${outfile_3} | wc -l` -ne 2 ]] && exit 3

# We expect some of the following tests may return a non-zero exit code, but we don't want
# to immediately exit the script when that happens.
set +e

# Test #4, for wrong number of arguments.
outfile_4=testrun/readbp_4.out
../utils/readbp > ${outfile_4}
[[ ${?} -ne 2 || `grep -c "Usage: readbp" ${outfile_4}` -ne 1 ]] && exit 4

# Test #5, for non-existent input file.
outfile_5=testrun/readbp_5.out
../utils/readbp BUFRLIB_DUMMY > ${outfile_5}
[[ ${?} -ne 3 || `grep -c "does not exist" ${outfile_5}` -ne 1 ]] && exit 5

# Success!
exit 0
