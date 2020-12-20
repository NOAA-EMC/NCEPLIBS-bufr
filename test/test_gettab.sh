#!/bin/bash

set -eu

cmd=$1
outfile=$2
reffile=$3

rc="-1"
$cmd > $outfile && cmp $outfile $reffile
rc=${?}

exit $rc
