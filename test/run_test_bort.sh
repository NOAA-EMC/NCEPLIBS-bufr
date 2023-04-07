#!/bin/sh
# This is a test script for NCEPLIBS-bufr.
#
# This script tests aborts. It does this by calling the program
# test_bort.F90 with two arguments, the subroutine name and the test
# case (a number). The program test_bort.F90 has code for each
# subroutine name and test case, which causes an abort. This script
# then checks that test_bort.F90 aborted as expected.
#
# Ed Hartnett 3/12/23

# For now, don't run on _8 version of the library, because not all
# functions tested here handle _8 calls well, since they are not
# intended to be called directly by the user.
for kind in "4" "d"; do
    # Check adn30().
    (./test_bort_$kind adn30 1) && exit 1
    (./test_bort_$kind adn30 2) && exit 1
    (./test_bort_$kind adn30 3) && exit 1
    (./test_bort_$kind adn30 4) && exit 1
    (./test_bort_$kind adn30 5) && exit 1

    # Check bort().
    (./test_bort_$kind bort 1) && exit 1

    # Check bort2().
    (./test_bort_$kind bort2 1) && exit 1

    # Check bvers().
    (./test_bort_$kind bvers 1) && exit 1

    # Check cmpmsg().
    (./test_bort_$kind cmpmsg 1) && exit 1

    # Check codflg().
    (./test_bort_$kind codflg 1) && exit 1

    # Check copybf().
    (./test_bort_$kind copybf 1) && exit 1
    (./test_bort_$kind copybf 2) && exit 1

    # Check copymg().
    (./test_bort_$kind copymg 1) && exit 1
    (./test_bort_$kind copymg 2) && exit 1
    (./test_bort_$kind copymg 3) && exit 1
    (./test_bort_$kind copymg 4) && exit 1
    (./test_bort_$kind copymg 5) && exit 1

    # Check copysb().
    (./test_bort_$kind copysb 1) && exit 1
    (./test_bort_$kind copysb 2) && exit 1
    (./test_bort_$kind copysb 3) && exit 1
    (./test_bort_$kind copysb 4) && exit 1
    (./test_bort_$kind copysb 5) && exit 1
    (./test_bort_$kind copysb 6) && exit 1
    
    # Check datebf().
    (./test_bort_$kind datebf 1) && exit 1
    
    # Check dumpbf().
    (./test_bort_$kind dumpbf 1) && exit 1
    
    # Check idn30().
    (./test_bort_$kind idn30 1) && exit 1
    (./test_bort_$kind idn30 2) && exit 1
    (./test_bort_$kind idn30 3) && exit 1
    (./test_bort_$kind idn30 4) && exit 1

    # Check ifbget().
    (./test_bort_$kind ifbget 1) && exit 1
    (./test_bort_$kind ifbget 2) && exit 1
    (./test_bort_$kind ifbget 3) && exit 1

    # Check nemtba().
    (./test_bort_$kind nemtba 1) && exit 1

    # Commented out. See https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/384.
    # Check nemtbb().
    #(./test_bort_$kind nemtbb 1) && exit 1

    # Commented out. See https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/384.
    # Check nemtbd().
    #(./test_bort_$kind nemtbd 1) && exit 1

    # Check nmsub().
    (./test_bort_$kind nmsub 1) && exit 1
    (./test_bort_$kind nmsub 2) && exit 1

    # Check openbf().
    (./test_bort_$kind openbf 1) && exit 1
    (./test_bort_$kind openbf 2) && exit 1
    (./test_bort_$kind openbf 3) && exit 1

    # Check openmg().
    (./test_bort_$kind openmg 1) && exit 1
    # Commented out. See https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/395.    
    # (./test_bort_$kind openmg 2) && exit 1

    # Check pkb().
    (./test_bort_$kind pkb 1) && exit 1

    # Check pkb8().
    (./test_bort_$kind pkb8 1) && exit 1
    (./test_bort_$kind pkb8 2) && exit 1

    # Check posapx().
    (./test_bort_$kind posapx 1) && exit 1
    (./test_bort_$kind posapx 2) && exit 1

    # Check readerme().
    (./test_bort_$kind readerme 1) && exit 1
    (./test_bort_$kind readerme 2) && exit 1

    # Check readlc().
    (./test_bort_$kind readlc 1) && exit 1
    (./test_bort_$kind readlc 2) && exit 1

    # Check readmg().
    (./test_bort_$kind readmg 1) && exit 1
    (./test_bort_$kind readmg 2) && exit 1

    # Check rdmemm().
    (./test_bort_$kind rdmemm 1) && exit 1

    # Check readns().
    (./test_bort_$kind readns 1) && exit 1
    (./test_bort_$kind readns 2) && exit 1

    # Check readsb().
    (./test_bort_$kind readsb 1) && exit 1
    (./test_bort_$kind readsb 2) && exit 1

    # Check rtrcpt().
    (./test_bort_$kind rtrcpt 1) && exit 1
    (./test_bort_$kind rtrcpt 2) && exit 1
    (./test_bort_$kind rtrcpt 3) && exit 1

    # Check status().
    (./test_bort_$kind status 1) && exit 1
    (./test_bort_$kind status 2) && exit 1

    # Check sntbde().
    (./test_bort_$kind sntbde 1) && exit 1

    # Check stdmsg().
    (./test_bort_$kind stdmsg 1) && exit 1

    # Check stndrd().
    (./test_bort_$kind stndrd 1) && exit 1

    # Check strcpt().
    (./test_bort_$kind strcpt 1) && exit 1

    # Commented out until https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/384
    # is resolved.
    # Check string().
    #./test_bort_$kind string 1
    #[ $? != 1 ] &&  exit 1

    # Check ufbcnt().
    (./test_bort_$kind ufbcnt 1) && exit 1

    # Check ufbcpy().
    (./test_bort_$kind ufbcpy 1) && exit 1
    (./test_bort_$kind ufbcpy 2) && exit 1
    (./test_bort_$kind ufbcpy 3) && exit 1

    # Check ufbcup().
    (./test_bort_$kind ufbcup 1) && exit 1
    (./test_bort_$kind ufbcup 2) && exit 1
    (./test_bort_$kind ufbcup 3) && exit 1

    # Check ufbdmp().
    (./test_bort_$kind ufbdmp 1) && exit 1
    (./test_bort_$kind ufbdmp 2) && exit 1
    (./test_bort_$kind ufbdmp 3) && exit 1

    # Check ufbevn().
    (./test_bort_$kind ufbevn 1) && exit 1
    (./test_bort_$kind ufbevn 2) && exit 1
    (./test_bort_$kind ufbevn 3) && exit 1

    # Check ufbget().
    (./test_bort_$kind ufbget 1) && exit 1
    (./test_bort_$kind ufbget 2) && exit 1
    (./test_bort_$kind ufbget 3) && exit 1

    # Check ufbin3().
    (./test_bort_$kind ufbin3 1) && exit 1
    (./test_bort_$kind ufbin3 2) && exit 1
    (./test_bort_$kind ufbin3 3) && exit 1

    # Check ufbint().
    (./test_bort_$kind ufbint 1) && exit 1
    (./test_bort_$kind ufbint 2) && exit 1

    # Check ufbqcp().
    (./test_bort_$kind ufbqcp 1) && exit 1

    # Check ufbrep().
    (./test_bort_$kind ufbrep 1) && exit 1
    (./test_bort_$kind ufbrep 2) && exit 1

    # Check ufbrms().
    (./test_bort_$kind ufbrms 1) && exit 1

    # Check ufbseq().
    (./test_bort_$kind ufbseq 1) && exit 1
    (./test_bort_$kind ufbseq 2) && exit 1

    # Check ufbstp().
    (./test_bort_$kind ufbstp 1) && exit 1
    (./test_bort_$kind ufbstp 2) && exit 1
    (./test_bort_$kind ufbstp 3) && exit 1

    # Check ufdump().
    (./test_bort_$kind ufdump 1) && exit 1
    (./test_bort_$kind ufdump 2) && exit 1
    (./test_bort_$kind ufdump 3) && exit 1

    # Check upftbv().
    (./test_bort_$kind upftbv 1) && exit 1
    (./test_bort_$kind upftbv 2) && exit 1

    # Check wrdxtb().
    (./test_bort_$kind wrdxtb 1) && exit 1
    (./test_bort_$kind wrdxtb 2) && exit 1
    (./test_bort_$kind wrdxtb 3) && exit 1

    # Check wtstat().
    (./test_bort_$kind wtstat 1) && exit 1
    (./test_bort_$kind wtstat 2) && exit 1
    (./test_bort_$kind wtstat 3) && exit 1
    (./test_bort_$kind wtstat 4) && exit 1

    # Check writdx().
    (./test_bort_$kind writdx 1) && exit 1

    # Check writlc().
    (./test_bort_$kind writlc 1) && exit 1
    (./test_bort_$kind writlc 2) && exit 1
    (./test_bort_$kind writlc 3) && exit 1

    # Check writsa().
    (./test_bort_$kind writsa 1) && exit 1
    (./test_bort_$kind writsa 2) && exit 1
    (./test_bort_$kind writsa 3) && exit 1

    # Check writsb().
    (./test_bort_$kind writsb 1) && exit 1
    (./test_bort_$kind writsb 2) && exit 1
    (./test_bort_$kind writsb 3) && exit 1

done

# If we made it here, all error codes were correctly returned, and the
# test passed!
exit 0
