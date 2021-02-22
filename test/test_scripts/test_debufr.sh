#!/bin/bash

# test the debufr.x utility

set -eu

cmd=$1
infile=$2
outfile=$3
reffile=$4

rc="-1"
$cmd -o $outfile $infile && cmp -s $outfile $reffile
rc=${?}

exit $rc
