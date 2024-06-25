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

# Test #4, reading input prepbufr2.ref file with -n, -m, and undefined -j options.  The requested -m message type
# isn't present in the input file, so the resulting output file should be empty.
args_4="-n -j -m SFCSHP testfiles/data/prepbufr2.ref"
outfile_4=testrun/readbp_4.out
../utils/readbp ${args_4} > ${outfile_4}
[[ ${?} -ne 0 || -s ${outfile_4} ]] && exit 4

# Test #5, reading input prepbufr file with -n and -r options.
args_5="-n -r 22 testfiles/data/prepbufr"
outfile_5=testrun/readbp_5.out
../utils/readbp ${args_5} > ${outfile_5}
[[ ${?} -ne 0 || `egrep -c '(ASDK01|ASDE04|ASFR4)' ${outfile_5}` -ne 6 ]] && exit 5

# We expect some of the following tests may return a non-zero exit code, but we don't want
# to immediately exit the script when that happens.
set +e

# Test #6, for wrong number of arguments.
outfile_6=testrun/readbp_6.out
../utils/readbp > ${outfile_6}
[[ ${?} -ne 2 || `grep -c "Usage: readbp" ${outfile_6}` -ne 1 ]] && exit 6

# Test #7, for non-existent input file.
outfile_7=testrun/readbp_7.out
../utils/readbp BUFRLIB_DUMMY > ${outfile_7}
[[ ${?} -ne 3 || `grep -c "does not exist" ${outfile_7}` -ne 1 ]] && exit 7

# Success!
exit 0
