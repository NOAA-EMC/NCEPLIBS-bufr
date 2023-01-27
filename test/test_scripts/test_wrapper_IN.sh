#!/bin/bash
# This is a test for NCEPLIBS-bufr.
# Test script to run each test_IN code and verify its output.
# Jeff Ator 2022-02-18

set -eu

exename=${1}

outfile=$(echo ${exename} | cut -d. -f1).out

cmd="./${exename} > ${outfile}"
eval ${cmd}
exit_code=$?
if test "${exit_code}" == "0"; then   # cmd ran successfully
    exit `grep -ic FAILED ${outfile}`  # confirm all tests completed successfully
else
    exit ${exit_code}   # cmd did not run successfully
fi
