#!/bin/bash
# This is a test script for NCEPLIBS-bufr, to runs some tests of the split_by_subset utility.
#
# Jack Woollen 2022-02-18

set -eu

# Get the base directory for the tests, then cd to that directory.
basedir="`dirname $0`/../test"
cd ${basedir}

# Test #1, reading input satwndbufr file.
../utils/split_by_subset testfiles/data/satwndbufr  # generate the output files for this test
[[ ${?} -ne 0 ]] && exit 1
outdir=testfiles/testoutput/satwndbufr_split
files=$(ls -1 ${outdir}/*)  # get complete list of reference files for this test
for file in ${files}; do  # compare each reference file to the corresponding output file to make sure they're identical
  file=$(basename ${file})
  cmp -s ${file} ${outdir}/${file}
  [[ ${?} -ne 0 ]] && exit 2
done

# We expect some of the following tests may return a non-zero exit code, but we don't want
# to immediately exit the script when that happens.
set +e

# Test #2, for wrong number of arguments.
outfile_2=testrun/split_by_subset_2.out
../utils/split_by_subset > ${outfile_2}
[[ ${?} -ne 2 || `grep -c "Usage: split_by_subset <bufrfile>" ${outfile_2}` -ne 1 ]] && exit 3

# Test #3, for non-existent input file.
outfile_3=testrun/split_by_subset_3.out
../utils/split_by_subset BUFRLIB_DUMMY > ${outfile_3}
[[ ${?} -ne 1 || `grep -c "does not exist" ${outfile_3}` -ne 1 ]] && exit 4

# Success!
exit 0
