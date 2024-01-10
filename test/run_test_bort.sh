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

    # Check chekstab().
    (./test_bort_$kind chekstab 1) && exit 1

    # Check closmg().
    (./test_bort_$kind closmg 1) && exit 1
    (./test_bort_$kind closmg 2) && exit 1

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
    
    # Check elemdx().
    (./test_bort_$kind elemdx 1) && exit 1
    (./test_bort_$kind elemdx 2) && exit 1
    (./test_bort_$kind elemdx 3) && exit 1
    (./test_bort_$kind elemdx 4) && exit 1
    (./test_bort_$kind elemdx 5) && exit 1

    # Check idn30().
    (./test_bort_$kind idn30 1) && exit 1
    (./test_bort_$kind idn30 2) && exit 1
    (./test_bort_$kind idn30 3) && exit 1
    (./test_bort_$kind idn30 4) && exit 1

    # Check ifbget().
    (./test_bort_$kind ifbget 1) && exit 1
    (./test_bort_$kind ifbget 2) && exit 1
    (./test_bort_$kind ifbget 3) && exit 1

    # Check isize().
    (./test_bort_$kind isize 1) && exit 1
    
    # Check iupm().
    (./test_bort_$kind iupm 1) && exit 1
    
    # Check iupvs01().
    (./test_bort_$kind iupvs01 1) && exit 1
    (./test_bort_$kind iupvs01 2) && exit 1
    (./test_bort_$kind iupvs01 3) && exit 1
    
    # Check jstnum().
    (./test_bort_$kind jstnum 1) && exit 1

    # Check lstjpb().
    (./test_bort_$kind lstjpb 1) && exit 1
    (./test_bort_$kind lstjpb 2) && exit 1

    # Check mtfnam().
    (./test_bort_$kind mtfnam 1) && exit 1
    # For the next test, we need to temporarily create a dummy placeholder file
    # in the current working directory.
    touch bufrtab.TableB_STD_999_15
    (./test_bort_$kind mtfnam 2) && exit 1

    # Check nemtba().
    (./test_bort_$kind nemtba 1) && exit 1

    # Check nemtbb().
    (./test_bort_$kind nemtbb 1) && exit 1
    (./test_bort_$kind nemtbb 2) && exit 1
    (./test_bort_$kind nemtbb 3) && exit 1

    # Check nemtbd().
    (./test_bort_$kind nemtbd 1) && exit 1

    # Commented out. See https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/384.
    # Check nemtbb().
    #(./test_bort_$kind nemtbb 1) && exit 1

    # Commented out. See https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/384.
    # Check nemtbd().
    #(./test_bort_$kind nemtbd 1) && exit 1

    # Check nenubd().
    (./test_bort_$kind nenubd 1) && exit 1
    (./test_bort_$kind nenubd 2) && exit 1
    (./test_bort_$kind nenubd 3) && exit 1
    (./test_bort_$kind nenubd 4) && exit 1

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

    # Check parstr().
    (./test_bort_$kind parstr 1) && exit 1
    (./test_bort_$kind parstr 2) && exit 1
    (./test_bort_$kind parstr 3) && exit 1

    # Check parusr().
    (./test_bort_$kind parusr 1) && exit 1
    (./test_bort_$kind parusr 2) && exit 1
    (./test_bort_$kind parusr 3) && exit 1
    (./test_bort_$kind parusr 4) && exit 1
    (./test_bort_$kind parusr 5) && exit 1

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
    (./test_bort_$kind readerme 3) && exit 1

    # Check readlc().
    (./test_bort_$kind readlc 1) && exit 1
    (./test_bort_$kind readlc 2) && exit 1

    # Check readmg().
    (./test_bort_$kind readmg 1) && exit 1
    (./test_bort_$kind readmg 2) && exit 1

    # Check rdmemm().
    (./test_bort_$kind rdmemm 1) && exit 1

    # Check rdusdx().
    (./test_bort_$kind rdusdx 1) && exit 1
    (./test_bort_$kind rdusdx 2) && exit 1
    (./test_bort_$kind rdusdx 3) && exit 1
    (./test_bort_$kind rdusdx 4) && exit 1
    (./test_bort_$kind rdusdx 5) && exit 1
    (./test_bort_$kind rdusdx 6) && exit 1
    (./test_bort_$kind rdusdx 7) && exit 1

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

    # Check seqsdx().
    (./test_bort_$kind seqsdx 1) && exit 1
    (./test_bort_$kind seqsdx 2) && exit 1
    (./test_bort_$kind seqsdx 3) && exit 1
    (./test_bort_$kind seqsdx 4) && exit 1
    (./test_bort_$kind seqsdx 5) && exit 1
    (./test_bort_$kind seqsdx 6) && exit 1
    (./test_bort_$kind seqsdx 7) && exit 1
    (./test_bort_$kind seqsdx 8) && exit 1
    (./test_bort_$kind seqsdx 9) && exit 1
    (./test_bort_$kind seqsdx 10) && exit 1
    (./test_bort_$kind seqsdx 11) && exit 1
    (./test_bort_$kind seqsdx 12) && exit 1
    (./test_bort_$kind seqsdx 13) && exit 1
    (./test_bort_$kind seqsdx 14) && exit 1
    (./test_bort_$kind seqsdx 15) && exit 1

    # Check status().
    (./test_bort_$kind status 1) && exit 1
    (./test_bort_$kind status 2) && exit 1

    # Check sntbbe().
    (./test_bort_$kind sntbbe 1) && exit 1
    (./test_bort_$kind sntbbe 2) && exit 1
    (./test_bort_$kind sntbbe 3) && exit 1
    (./test_bort_$kind sntbbe 4) && exit 1
    (./test_bort_$kind sntbbe 5) && exit 1
    (./test_bort_$kind sntbbe 6) && exit 1

    # Check sntbde().
    (./test_bort_$kind sntbde 1) && exit 1
    (./test_bort_$kind sntbde 2) && exit 1
    (./test_bort_$kind sntbde 3) && exit 1
    (./test_bort_$kind sntbde 4) && exit 1
    (./test_bort_$kind sntbde 5) && exit 1

    # Check sntbfe().
    (./test_bort_$kind sntbfe 1) && exit 1
    (./test_bort_$kind sntbfe 2) && exit 1
    (./test_bort_$kind sntbfe 3) && exit 1
    (./test_bort_$kind sntbfe 4) && exit 1
    (./test_bort_$kind sntbfe 5) && exit 1
    (./test_bort_$kind sntbfe 6) && exit 1
    (./test_bort_$kind sntbfe 7) && exit 1

    # Check stdmsg().
    (./test_bort_$kind stdmsg 1) && exit 1

    # Check stndrd().
    (./test_bort_$kind stndrd 1) && exit 1
    (./test_bort_$kind stndrd 2) && exit 1
    (./test_bort_$kind stndrd 3) && exit 1
    (./test_bort_$kind stndrd 4) && exit 1
    (./test_bort_$kind stndrd 5) && exit 1
    (./test_bort_$kind stndrd 6) && exit 1

    # Check strcpt().
    (./test_bort_$kind strcpt 1) && exit 1

    # Check string().
    (./test_bort_$kind string 1) && exit 1
    (./test_bort_$kind string 2) && exit 1

    # Check tabsub().
    (./test_bort_$kind tabsub 1) && exit 1
    (./test_bort_$kind tabsub 2) && exit 1
    (./test_bort_$kind tabsub 3) && exit 1
    (./test_bort_$kind tabsub 4) && exit 1
    (./test_bort_$kind tabsub 5) && exit 1
    (./test_bort_$kind tabsub 6) && exit 1
    (./test_bort_$kind tabsub 7) && exit 1
    (./test_bort_$kind tabsub 8) && exit 1
    (./test_bort_$kind tabsub 9) && exit 1
    (./test_bort_$kind tabsub 10) && exit 1

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
    (./test_bort_$kind ufbcup 4) && exit 1
    (./test_bort_$kind ufbcup 5) && exit 1
    (./test_bort_$kind ufbcup 6) && exit 1
    (./test_bort_$kind ufbcup 7) && exit 1

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

    # Check ufbint3().
    (./test_bort_$kind ufbin3 1) && exit 1
    (./test_bort_$kind ufbin3 2) && exit 1
    (./test_bort_$kind ufbin3 3) && exit 1

    # Check ufbint().
    (./test_bort_$kind ufbint 1) && exit 1
    (./test_bort_$kind ufbint 2) && exit 1

    # Check ufbinx().
    (./test_bort_$kind ufbinx 1) && exit 1
    (./test_bort_$kind ufbinx 2) && exit 1

    # Check ufbmms().
    (./test_bort_$kind ufbmms 1) && exit 1
    (./test_bort_$kind ufbmms 2) && exit 1
    (./test_bort_$kind ufbmms 3) && exit 1

    # Check ufbmns().
    (./test_bort_$kind ufbmns 1) && exit 1
    
    # Check ufbovr().
    (./test_bort_$kind ufbovr 1) && exit 1
    (./test_bort_$kind ufbovr 2) && exit 1
    (./test_bort_$kind ufbovr 3) && exit 1
    
    # Check ufbpos().
    (./test_bort_$kind ufbpos 1) && exit 1
    (./test_bort_$kind ufbpos 2) && exit 1
    (./test_bort_$kind ufbpos 3) && exit 1
    (./test_bort_$kind ufbpos 4) && exit 1
    (./test_bort_$kind ufbpos 5) && exit 1

    # Check ufbqcd().
    (./test_bort_$kind ufbqcd 1) && exit 1
    (./test_bort_$kind ufbqcd 2) && exit 1

    # Check ufbqcp().
    (./test_bort_$kind ufbqcp 1) && exit 1

    # Check ufbrep().
    (./test_bort_$kind ufbrep 1) && exit 1
    (./test_bort_$kind ufbrep 2) && exit 1

    # Check ufbrms().
    (./test_bort_$kind ufbrms 1) && exit 1
    (./test_bort_$kind ufbrms 2) && exit 1
    (./test_bort_$kind ufbrms 3) && exit 1

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

    # Check upb8().
    (./test_bort_$kind upb8 1) && exit 1

    # Check upftbv().
    (./test_bort_$kind upftbv 1) && exit 1
    (./test_bort_$kind upftbv 2) && exit 1

    # Check usrtpl().
    # Oddly this does not cause a bort() in intel. Why?
    #(./test_bort_$kind usrtpl 1) && exit 1

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

# Now test the C borts().
(./test_c_bort cobfl 1) && exit 1
(./test_c_bort cobfl 2) && exit 1
(./test_c_bort cobfl 3) && exit 1

(./test_c_bort crbmg 1) && exit 1

(./test_c_bort cwbmg 1) && exit 1

(./test_c_bort wrdesc 1) && exit 1

# If we made it here, all error codes were correctly returned, and the
# test passed!
exit 0
