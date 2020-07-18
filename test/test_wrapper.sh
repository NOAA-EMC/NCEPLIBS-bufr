#!/bin/bash

exename=$1

# run the executable
echo ""
echo "==============================================================================="
echo "Running test executable"
echo "==============================================================================="

cmd="./${exename}"
echo ${cmd}
eval ${cmd}
exit_code=$?
if test "${exit_code}" == "0"; then
    echo -e "Test run succeed"
else
    echo -e "Test run failed with error code: ${exit_code} \n"
    exit ${exit_code}
fi

# run compare
echo ""
echo "==============================================================================="
echo "Running comparison"
echo "==============================================================================="

testname=$(echo $exename | cut -d. -f1)
refname=$(echo $testname | cut -c6-10)
outname=$(echo ${refname//_} | tr '[:upper:]' '[:lower:]')

cmd="cmp -s ${outname}.bufr testfiles/${refname}"
echo ${cmd}
eval ${cmd}
exit_code=$?
if test "${exit_code}" == "0"; then
   echo -e "Test compare succeed"
else
    echo -e "Test compare failed with error code: ${exit_code}"
    exit ${exit_code}
fi

echo -e "PASSED"
