#!/bin/bash
# This is a test for NCEPLIBS-bufr.
# Test script to test debufr utility
# Jeff Ator 2022-02-18

set -eu

cmd=$1
infile=$2
outfile=$3
reffile=$4

rc="-1"
$cmd -o $outfile $infile && cmp -s $outfile $reffile
rc=${?}

exit $rc
