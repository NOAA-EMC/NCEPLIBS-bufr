#!/bin/bash
#> @file
#> @brief Test script to test readmp utility
#> @author jack-woollen @date 2022-02-18

set -eu

cmd=$1
outfile=$2
reffile=$3

rc="-1"
$cmd > $outfile && diff -w $outfile $reffile
rc=${?}

exit $rc

