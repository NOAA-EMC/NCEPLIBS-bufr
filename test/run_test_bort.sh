#!/bin/sh
# This is a test script for NCEPLIBS-bufr.
#
# This script tests aborts.
#
# Ed Hartnett 3/12/23

# Check bort().
./test_bort_4 bort 1
[ $? != 1 ] &&  exit 1

# Check bort2().
./test_bort_4 bort 1
[ $? != 1 ] && exit 1

# Check bvers().
./test_bort_4 bvers 1
[ $? != 1 ] &&  exit 1

# If we made it here, all error codes were correctly returned, and the
# test passed!
exit 0
