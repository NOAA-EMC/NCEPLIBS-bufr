#!/bin/bash
#> @file
#> @brief Test script to test split_by_subset.x utility
#> @author aerorahul @date 2020-12-22

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

