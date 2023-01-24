#!/bin/bash
# This is a test for NCEPLIBS-bufr.
# Test script to ???.
# jack-woollen 2022-02-18

exename=$1
preAPX=${2:-"NO"}

testname=$(echo $exename | cut -d. -f1)
refname=$(echo $testname | cut -c6-10)
outname=$(echo ${refname//_} | tr '[:upper:]' '[:lower:]')

# preAPX
if [[ ${preAPX} =~ [YyTt] ]]; then
  echo ""
  echo "==============================================================================="
  echo "Copying test data"
  echo "==============================================================================="

  cmd="cp testfiles/${refname}_preAPX ./${outname}.bufr"
  echo ${cmd}
  eval ${cmd}
fi

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
