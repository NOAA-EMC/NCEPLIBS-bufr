#!/bin/bash

# This is a test script for NCEPLIBS-bufr, to run each outtest executable and verify its output.
#
# Jeff Ator 2023-02-23

testID=$1
num=$2

# run the executable

cmd="./${testID}"
echo ${cmd}
eval ${cmd}
exit_code=$?
if test "${exit_code}" != "0"; then
    exit ${exit_code}
fi

# verify the output

cmd="cmp -s out${num}.bufr testfiles/OUT_${num}"
echo ${cmd}
eval ${cmd}
exit_code=$?
if test "${exit_code}" != "0"; then
    exit ${exit_code}
fi

echo 'SUCCESS!'
