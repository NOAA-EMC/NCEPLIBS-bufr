#!/bin/bash
# This is a test for NCEPLIBS-bufr.
# Test script to test split_by_subset.x utility.
# jack-woollen 2022-02-18

set -eu

cmd=$1     #> command to execute
outdir=$2  #> location of output files to check against

rc="-1"
$cmd
rc=$?
[[ $rc -ne 0 ]] && exit $rc

files=$(ls -1 $outdir/*)
for file in $files; do
  file=$(basename $file)
  echo "comparing ... $file"
  cmp $file $outdir/$file
  rc=$?
  [[ $rc -ne 0 ]] && exit $rc
done

exit $rc

