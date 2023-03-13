# This is a test script for NCEPLIBS-bufr.
#
# This script tests aborts.
#
# Ed Hartnett 3/12/23

./test_bort_4 bort 1
if [[ $? != 1 ]]
then
    exit 1
fi

./test_bort_4 bort 2
if [[ $? != 1 ]]
then
    exit 1
fi

# If we made it here, all error codes were correctly returned, and the
# test passed!
return 0
