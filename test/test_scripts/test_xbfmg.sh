#!/bin/bash
# This is a test script for NCEPLIBS-bufr, to runs some tests of the xbfmg utility.
#
# Jeff Ator 2023-03-15

set -eu

# Get the base directory for the tests, then cd to that directory.
basedir="`dirname $0`/../test"
cd ${basedir}

# Tests #1 and #2, each generating various output files.
for test in {1..2}; do
  if [ ${test} -eq 1 ]; then
    args="-g"
  else
    args=""
  fi
  infile=xbfmg_${test}
  ../utils/xbfmg ${args} testfiles/data/${infile}  # generate the output files for this test
  [[ ${?} -ne 0 ]] && exit ${test}
  reffiles=`ls -1 testfiles/testoutput/xbfmg/${infile}.xbfmg.out.*`  # get complete list of reference files for this test
  for file in ${reffiles}; do  # compare each reference file to the corresponding output file
    outfile=${infile}.xbfmg.out.`echo ${file} | cut -f4 -d.`
    [[ -s ${outfile} ]] || exit `expr ${test} + 10`   # make sure this output file was generated
    cmp -s ${outfile} ${file}  # make sure this output file is identical to the corresponding reference file
    [[ ${?} -ne 0 ]] && exit `expr ${test} + 20`
  done
done

# We expect some of the following tests may return a non-zero exit code, but we don't want
# to immediately exit the script when that happens.
set +e

# Test #3, for wrong number of arguments.
outfile_3=testrun/xbfmg_3.out
../utils/xbfmg > ${outfile_3}
[[ ${?} -eq 0 || `grep -c "ERROR: You must specify an input BUFR file of BUFR messages" ${outfile_3}` -ne 1 ]] && exit 3

# Test #4, for -v option.
outfile_4=testrun/xbfmg_4.out
../utils/xbfmg -v > ${outfile_4}
[[ ${?} -ne 0 || `grep -c "This is xbfmg v.* built with NCEPLIBS-bufr" ${outfile_4}` -ne 1 ]] && exit 4

# Test #5, for -h option.
outfile_5=testrun/xbfmg_5.out
../utils/xbfmg -h > ${outfile_5}
[[ ${?} -ne 0 || `egrep -c "(ABSTRACT|USAGE|WHERE):" ${outfile_5}` -ne 3 ]] && exit 5

# Test #6, for non-existent input file.
outfile_6=testrun/xbfmg_6.out
../utils/xbfmg BUFRLIB_DUMMY > ${outfile_6}
[[ ${?} -eq 0 || `grep -c "ERROR: Could not stat the file" ${outfile_6}` -ne 1 ]] && exit 6

# Success!
exit 0
