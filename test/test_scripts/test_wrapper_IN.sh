#!/bin/bash

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
