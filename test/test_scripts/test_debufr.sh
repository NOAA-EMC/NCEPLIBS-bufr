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
echo "starting test #1 in test_debufr.sh script"
../utils/debufr ${args_1} testfiles/data/debufr_1 && cmp -s ${outfile_1} testfiles/testoutput/debufr_1.out
[[ ${?} -ne 0 ]] && exit 1

# Test #2, reading debufr_2 file using a separate DX table file and a superficially-long tables directory string.
outfile_2=testrun/debufr_2.out
ltdstr=testfiles/data
for ii in {1..8}; do
  ltdstr+=/../../testfiles/data
done
args_2="-t ${ltdstr} -f bufrtab.031 -c -o ${outfile_2}"
echo "starting test #2 in test_debufr.sh script"
../utils/debufr ${args_2} testfiles/data/debufr_2 && cmp -s ${outfile_2} testfiles/testoutput/debufr_2.out
[[ ${?} -ne 0 ]] && exit 2

# Test #3, reading debufr_3 file using DX table information embedded in the file.
outfile_3=debufr_3.debufr.out
args_3="-t ../tables -p NFILES=2,MXMSGL=100000"
echo "starting test #3 in test_debufr.sh script"
../utils/debufr ${args_3} testfiles/data/debufr_3 && cmp -s ${outfile_3} testfiles/testoutput/debufr_3.out
[[ ${?} -ne 0 ]] && exit 3

# Test #4, reading debufr_4 file with -m option.
outfile_4=debufr_4.debufr.out
args_4="-t ../tables -m"
echo "starting test #4 in test_debufr.sh script"
../utils/debufr ${args_4} testfiles/data/debufr_4 && cmp -s ${outfile_4} testfiles/testoutput/debufr_4.out
[[ ${?} -ne 0 ]] && exit 4

# Test #5, reading debufr_4 file with -b option.
outfile_5=testrun/debufr_5.out
args_5="-b -o ${outfile_5}"
echo "starting test #5 in test_debufr.sh script"
../utils/debufr ${args_5} testfiles/data/debufr_4 && cmp -s ${outfile_5} testfiles/testoutput/debufr_5.out
[[ ${?} -ne 0 ]] && exit 5

# Test #6, reading debufr_6 file using master tables.
outfile_6=testrun/debufr_6.out
args_6="-t ../tables -o ${outfile_6}"
echo "starting test #6 in test_debufr.sh script"
../utils/debufr ${args_6} testfiles/data/debufr_6 && cmp -s ${outfile_6} testfiles/testoutput/debufr_6.out
[[ ${?} -ne 0 ]] && exit 6

# Test #7, reading debufr_7 file using master tables.
outfile_7=testrun/debufr_7.out
args_7="-t ../tables -o ${outfile_7}"
echo "starting test #7 in test_debufr.sh script"
../utils/debufr ${args_7} testfiles/data/debufr_7 && cmp -s ${outfile_7} testfiles/testoutput/debufr_7.out
[[ ${?} -ne 0 ]] && exit 7

# We expect some of the following tests may return a non-zero exit code, but we don't want
# to immediately exit the script when that happens.
set +e

# Test #8, which should call NCEPLIBS-bufr subroutine bort from within subroutine nummtb.
outfile_8=testrun/debufr_8.out
args_8="-t ../tables"
echo "starting test #8 in test_debufr.sh script"
../utils/debufr ${args_8} testfiles/data/debufr_8 > ${outfile_8}
[[ ${?} -eq 0 || `grep -c "NUMMTB - COULD NOT FIND DESCRIPTOR" ${outfile_8}` -ne 1 ]] && exit 8

# Test #9, for wrong number of arguments.
outfile_9=testrun/debufr_9.out
echo "starting test #9 in test_debufr.sh script"
../utils/debufr > ${outfile_9}
[[ ${?} -eq 0 || `grep -c "ERROR: You must specify an input BUFR file to be decoded" ${outfile_9}` -ne 1 ]] && exit 9

# Test #10, for -v option.
outfile_10=testrun/debufr_10.out
echo "starting test #10 in test_debufr.sh script"
../utils/debufr -v > ${outfile_10}
[[ ${?} -ne 0 || `grep -c "This is the debufr utility, built with NCEPLIBS-bufr" ${outfile_10}` -ne 1 ]] && exit 10

# Test #11, for -h option.
outfile_11=testrun/debufr_11.out
echo "starting test #11 in test_debufr.sh script"
../utils/debufr -h > ${outfile_11}
[[ ${?} -ne 0 || `egrep -c "(ABSTRACT|USAGE|WHERE):" ${outfile_11}` -ne 3 ]] && exit 11

# Test #12, for non-existent DX tables file.
outfile_12=testrun/debufr_12.out
echo "starting test #12 in test_debufr.sh script"
../utils/debufr -t. -f BUFRLIB_DUMMY testfiles/data/debufr_1 > ${outfile_12}
[[ ${?} -ne 0 || `grep -c "Error: Could not find file" ${outfile_12}` -ne 1 ]] && exit 12

# Test #13, which should call NCEPLIBS-bufr subroutine bort from within subroutine readerme.
outfile_13=testrun/debufr_13.out
args_13="-t ../tables -p MXMSGL=40000"
echo "starting test #13 in test_debufr.sh script"
../utils/debufr ${args_13} testfiles/data/debufr_3 > ${outfile_13}
[[ ${?} -eq 0 || `grep -c "READERME - INPUT BUFR MESSAGE LENGTH.*LARGER THAN LIMIT" ${outfile_13}` -ne 1 ]] && exit 13

# Test #14, for unwriteable output directory.
outfile_14=testrun/debufr_14.out
echo "starting test #14 in test_debufr.sh script"
../utils/debufr -o /BUFRLIB_DUMMY_DIRECTORY/BUFRLIB_DUMMY testfiles/data/debufr_1 > ${outfile_14}
[[ ${?} -eq 0 || `grep -c "ERROR: Cannot write output file" ${outfile_14}` -ne 1 ]] && exit 14

# Success!
exit 0
