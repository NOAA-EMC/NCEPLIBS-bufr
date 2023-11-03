#!/bin/bash
# This is a test script for NCEPLIBS-bufr, to runs some tests of the debufr utility.
#
# Jeff Ator 2023-03-15

set -eu

# Get the base directory for the tests, then cd to that directory.
basedir="`dirname $0`/../test"
cd ${basedir}

# Test #1, reading debufr_1 file using master tables.
outfile_1=testrun/debufr_1.out
args_1="-t ../tables -o ${outfile_1}"
../utils/debufr ${args_1} testfiles/data/debufr_1 && cmp -s ${outfile_1} testfiles/testoutput/debufr_1.out
[[ ${?} -ne 0 ]] && exit 1

# Test #2, reading debufr_2 file using a separate DX table file.
outfile_2=testrun/debufr_2.out
args_2="-t testfiles/data -f bufrtab.031 -c -o ${outfile_2}"
../utils/debufr ${args_2} testfiles/data/debufr_2 && cmp -s ${outfile_2} testfiles/testoutput/debufr_2.out
[[ ${?} -ne 0 ]] && exit 2

# Test #3, reading debufr_3 file using DX table information embedded in the file.
outfile_3=debufr_3.debufr.out
args_3="-t ../tables -p NFILES=2,MXMSGL=100000"
../utils/debufr ${args_3} testfiles/data/debufr_3 && cmp -s ${outfile_3} testfiles/testoutput/debufr_3.out
[[ ${?} -ne 0 ]] && exit 3

# Test #4, reading debufr_4 file with -m option.
outfile_4=debufr_4.debufr.out
args_4="-t ../tables -m"
../utils/debufr ${args_4} testfiles/data/debufr_4 && cmp -s ${outfile_4} testfiles/testoutput/debufr_4.out
[[ ${?} -ne 0 ]] && exit 4

# Test #5, reading debufr_4 file with -b option.
outfile_5=testrun/debufr_5.out
args_5="-b -o ${outfile_5}"
../utils/debufr ${args_5} testfiles/data/debufr_4 && cmp -s ${outfile_5} testfiles/testoutput/debufr_5.out
[[ ${?} -ne 0 ]] && exit 5

# We expect some of the following tests may return a non-zero exit code, but we don't want
# to immediately exit the script when that happens.
set +e

# Test #6, for wrong number of arguments.
outfile_6=testrun/debufr_6.out
../utils/debufr > ${outfile_6}
[[ ${?} -eq 0 || `grep -c "ERROR: You must specify an input BUFR file to be decoded" ${outfile_6}` -ne 1 ]] && exit 6

# Test #7, for -v option.
outfile_7=testrun/debufr_7.out
../utils/debufr -v > ${outfile_7}
[[ ${?} -ne 0 || `grep -c "This is debufr v.* built with NCEPLIBS-bufr" ${outfile_7}` -ne 1 ]] && exit 7

# Test #8, for -h option.
outfile_8=testrun/debufr_8.out
../utils/debufr -h > ${outfile_8}
[[ ${?} -ne 0 || `egrep -c "(ABSTRACT|USAGE|WHERE):" ${outfile_8}` -ne 3 ]] && exit 8

# Test #9, for non-existent DX tables file.
outfile_9=testrun/debufr_9.out
../utils/debufr -t. -f BUFRLIB_DUMMY testfiles/data/debufr_1 > ${outfile_9}
[[ ${?} -ne 0 || `egrep -c "Error: Could not find file" ${outfile_9}` -ne 1 ]] && exit 9

# Test #10, which should call NCEPLIBS-bufr subroutine bort from within subroutine readerme.
outfile_10=debufr_10.debufr.out
args_10="-t ../tables -p MXMSGL=40000"
../utils/debufr ${args_10} testfiles/data/debufr_3 > ${outfile_10}
[[ ${?} -eq 0 || `grep -c "READERME - INPUT BUFR MESSAGE LENGTH.*LARGER THAN LIMIT" ${outfile_10}` -ne 1 ]] && exit 10

# Test #11, for unwriteable output directory.
outfile_11=testrun/debufr_11.out
../utils/debufr -o /BUFRLIB_DUMMY_DIRECTORY/BUFRLIB_DUMMY testfiles/data/debufr_1 > ${outfile_11}
[[ ${?} -eq 0 || `egrep -c "Error: Cannot write output file" ${outfile_11}` -ne 1 ]] && exit 11

# Success!
exit 0
