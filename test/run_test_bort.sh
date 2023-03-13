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

# Check wtstat().
./test_bort_4 wtstat 1
[ $? != 1 ] &&  exit 1
./test_bort_4 wtstat 2
[ $? != 1 ] &&  exit 1
./test_bort_4 wtstat 3
[ $? != 1 ] &&  exit 1
./test_bort_4 wtstat 4
[ $? != 1 ] &&  exit 1

# Check writdx().
./test_bort_4 writdx 1
[ $? != 1 ] &&  exit 1

# Check writlc().
./test_bort_4 writlc 1
[ $? != 1 ] &&  exit 1

# Check writsa().
./test_bort_4 writsa 1
[ $? != 1 ] &&  exit 1

# If we made it here, all error codes were correctly returned, and the
# test passed!
exit 0
