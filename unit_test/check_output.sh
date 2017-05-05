#/bin/sh -vx

# check the BUFR output from a test program

program_output=${1}
expected_output=${2}

cmp -s ${program_output} ${expected_output}

if [ $? -eq 0 ]
then
    echo "      All tested OK"
#   clean up the test output
    /bin/rm -rf ${program_output}
else
    echo "    One or more tests FAILED!!"
    echo "    Check contents of ${program_output} and compare to ${expected_output}.debufr.out for more details."
fi
