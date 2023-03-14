#!/bin/sh
# This is a test script for NCEPLIBS-bufr.
#
# This script tests aborts.
#
# Ed Hartnett 3/12/23

# Check adn30().
./test_bort_4 adn30 1
[ $? != 1 ] &&  exit 1
./test_bort_4 adn30 2
[ $? != 1 ] &&  exit 1

# Check bort().
./test_bort_4 bort 1
[ $? != 1 ] &&  exit 1

# Check bort2().
./test_bort_4 bort2 1
[ $? != 1 ] && exit 1

# Check bvers().
./test_bort_4 bvers 1
[ $? != 1 ] &&  exit 1

# Check cmpmsg().
./test_bort_4 cmpmsg 1
[ $? != 1 ] &&  exit 1

# Check ufbcpy().
./test_bort_4 ufbcpy 1
[ $? != 1 ] &&  exit 1

# Check ufbcup().
./test_bort_4 ufbcup 1
[ $? != 1 ] &&  exit 1

# Check ufbdmp().
./test_bort_4 ufbdmp 1
[ $? != 1 ] &&  exit 1

# Check ufbevn().
./test_bort_4 ufbevn 1
[ $? != 1 ] &&  exit 1

# Check ufbget().
./test_bort_4 ufbget 1
[ $? != 1 ] &&  exit 1

# Check ufbint().
./test_bort_4 ufbint 1
[ $? != 1 ] &&  exit 1

# Check ufbqcp().
./test_bort_4 ufbqcp 1
[ $? != 1 ] &&  exit 1

# Check ufbrep().
./test_bort_4 ufbrep 1
[ $? != 1 ] &&  exit 1

# Check ufbrms().
./test_bort_4 ufbrms 1
[ $? != 1 ] &&  exit 1

# Check ufbseq().
./test_bort_4 ufbseq 1
[ $? != 1 ] &&  exit 1

# Check ufdump().
./test_bort_4 ufdump 1
[ $? != 1 ] &&  exit 1

# Check upftbv().
./test_bort_4 upftbv 1
[ $? != 1 ] &&  exit 1

# Check valx().
./test_bort_4 valx 1
[ $? != 1 ] &&  exit 1

# Check wrdxtb().
./test_bort_4 wrdxtb 1
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

# Check writsb().
./test_bort_4 writsb 1
[ $? != 1 ] &&  exit 1

# If we made it here, all error codes were correctly returned, and the
# test passed!
exit 0
