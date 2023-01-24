#!/bin/bash
# This is a test for NCEPLIBS-bufr.
# Test script to test gettab utility.
# jack-woollen 2022-02-18

set -eu

cmd=$1
outfile=$2
reffile=$3

rc="-1"
$cmd > $outfile && diff -w $outfile $reffile
rc=${?}

exit $rc

