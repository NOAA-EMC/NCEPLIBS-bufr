#!/bin/sh
# This is a test script for NCEPLIBS-bufr.
#
# This script tests aborts.
#
# Ed Hartnett 3/12/23

./test_bort_4 bort 1
[ $? != 1 ] &&  return 1

./test_bort_4 bort 2
[ $? != 1 ] && return 1

# If we made it here, all error codes were correctly returned, and the
# test passed!
return 0
