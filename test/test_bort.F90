! This is a test for NCEPLIBS-bufr library.
!
! This tests the bort() and bort2() subroutines. It will also test the
! bort() calls of other subroutines.
!
! This program is called (repeatedly) by run_test_bort.sh, which
! passes in a series of subroutine names and test case numbers, and
! expects each case to cause an abort.
!
! Ed Hartnett 3/12/23
program test_bort
  use bufr_interface
  implicit none
  integer iret, jret, iunit, iqcd
  ! integer i1
  integer int_1d(1), int_1d_2(1), int_2d(1,5)
  character char_1
  character*2 char_short
  character*30 char_30
  character*8 tags(5)
  character*4 char_4(1)
  character*8 char_8(1), char_val_8, nems(20)
  character*12 char_12(1)
  character*24 char_24(1)
  character*85 char_85
  character*120 char_120(1), char_120_2(1,5)
  character*5 adn30_val_5
  real*8 real_1d(1)
  real*8 real_2d(1,1)
  real*8 real_2d_3x1(3,1)
  integer idn30, idn30_val
  integer :: num_args, len, stat, ios, u
  character(len=32) :: sub_name, test_case
  character*5 adn30
  character*80 card
  integer ibay(1), ibit, jdate
  integer mtyp, msbt, inod
  character*28 unit
  integer iscl, iref
  integer ierr
  integer mear, mmon, mday, mour, idate
  integer iyr, imo, idy, ihr, imi
  integer jdate1(5), jdump1(5)
  integer lmsgt, msgt(100), msgl
  integer nseq, irps(20), knts(20)
  integer imt, imtv, iogce, iltv
  integer*8 nval

  integer*4 isize, iupm, iupvs01, isetprm, nmsub

  character*25 filnam
  character bfmg(200000)
  integer ibfmg(50000), ibfmg2(50000)
  equivalence (bfmg(1),ibfmg(1))

#ifdef KIND_8
  call setim8b(.true.)
#endif

  num_args = command_argument_count()
  if (num_args /= 2) then
     print *, "Two command line arguments expected: subroutine name and test case"
     ! Return with 0 to fail the test.
     stop 0 
  end if

  ! Read the command line arguments, a name of subroutine, and a test
  ! case number.
  call get_command_argument(1, sub_name, len, stat)
  if (stat .ne. 0) stop 3
  call get_command_argument(2, test_case, len, stat)
  if (stat .ne. 0) stop 4
  print *, 'Testing ', sub_name, ' case ', test_case

  ! Run the test for the subroutine and test case.
  if (sub_name .eq. 'adn30') then
     if (test_case .eq. '1') then
        char_short = adn30(0, 6)
     elseif (test_case .eq. '2') then
        char_30 = adn30(0, 9)
     elseif (test_case .eq. '3') then
        char_30 = adn30(-1, 5)
     elseif (test_case .eq. '4') then
        char_30 = adn30(65536, 5)
     elseif (test_case .eq. '5') then
        char_30 = adn30(0, 3)
     endif
  elseif (sub_name .eq. 'bort') then
     if (test_case .eq. '1') then
        call bort('goodbye!')
     endif
  elseif (sub_name .eq. 'bort2') then
     if (test_case .eq. '1') then
        call bort2('goodbye!', 'goodbye again!')
     endif
  elseif (sub_name .eq. 'bvers') then
     if (test_case .eq. '1') then
        call bvers(char_short)
     endif
  elseif (sub_name .eq. 'chekstab') then
     open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     if (ios .ne. 0) stop 3
     if (test_case .eq. '1') then
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       if (ios .ne. 0) stop 3
       card = '| YEAR     | 004001 | YEAR                                                     |'
       write (12,'(A)') card
       card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
       write (12,'(A)') card
       close (12)
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       call openbf(11, 'OUT', 12)
       call chekstab(1)
     endif
  elseif (sub_name .eq. 'closmg') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call closmg(11)
     elseif (test_case .eq. '2') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call closmg(11)
     endif
  elseif (sub_name .eq. 'cmpmsg') then
     if (test_case .eq. '1') then
        call cmpmsg('W')
     endif
  elseif (sub_name .eq. 'codflg') then
     if (test_case .eq. '1') then
        call codflg('W')
     endif
  elseif (sub_name .eq. 'copybf') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call copybf(11, 0)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 12)
        call copybf(11, 12)
     endif
  elseif (sub_name .eq. 'copymg') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call copymg(11, 0)     
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call copymg(12, 0)     
     elseif (test_case .eq. '3') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call copymg(11, 0)     
     elseif (test_case .eq. '4') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call readmg(11, char_val_8, jdate, iret)
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 12)
        call copymg(11, 12)     
     elseif (test_case .eq. '5') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call readmg(11, char_val_8, jdate, iret)
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call copymg(11, 12)     
     endif
  elseif (sub_name .eq. 'copysb') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call copysb(11, 0, ierr)     
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call copysb(12, 0, ierr)     
     elseif (test_case .eq. '3') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 10)
        call copysb(11, 0, ierr)     
     elseif (test_case .eq. '4') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 10)
        call readmg(11, char_val_8, jdate, iret)
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call copysb(11, 12, ierr)     
     elseif (test_case .eq. '5') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 10)
        call readmg(11, char_val_8, jdate, iret)
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 10)
        call copysb(11, 12, ierr)     
     elseif (test_case .eq. '6') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 10)
        call readmg(11, char_val_8, jdate, iret)
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'OUT', 10)
        call copysb(11, 12, ierr)     
     endif
  elseif (sub_name .eq. 'cpdxmm') then
     if (test_case .eq. '1') then
       open(unit = 11, file = 'testfiles/IN_6_infile1', form = 'UNFORMATTED', iostat = ios)
       if (ios .ne. 0) stop 3
       open(unit = 12, file = 'testfiles/IN_6_infile2', form = 'UNFORMATTED', iostat = ios)
       if (ios .ne. 0) stop 3
       if (isetprm('MXDXTS',1) .ne. 0) stop 3
       call ufbmem(11, 0, iret, iunit)
       call ufbmem(12, 1, iret, iunit)
       call ufbmns(18364, char_val_8, jdate)
     endif
  elseif (sub_name .eq. 'cpymem') then
     open(unit = 11, file = 'testfiles/IN_6_infile1', form = 'UNFORMATTED', iostat = ios)
     if (ios .ne. 0) stop 3
     open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     if (ios .ne. 0) stop 3
     call ufbmem(11, 0, iret, iunit)
     if (test_case .eq. '1') then
        call cpymem(12)
     elseif (test_case .eq. '2') then
        call rdmemm(167, char_val_8, jdate, ierr)
        call cpymem(12)
     elseif (test_case .eq. '3') then
        call rdmemm(167, char_val_8, jdate, ierr)
        call openbf(12, 'IN', 11)
        call cpymem(12)
     elseif (test_case .eq. '4') then
        call rdmemm(167, char_val_8, jdate, ierr)
        call openbf(12, 'OUT', 11)
        call openmg(12, 'NC004001', 2024020112)
        call cpymem(12)
     endif
  elseif (sub_name .eq. 'datebf') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call datebf(11, mear, mmon, mday, mour, idate) 
     endif
  elseif (sub_name .eq. 'datelen') then
     if (test_case .eq. '1') then
        call datelen(11) 
     endif
  elseif (sub_name .eq. 'dumpbf') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call dumpbf(11, jdate1, jdump1) 
     endif
  elseif (sub_name .eq. 'dxdump') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        call dxdump(11, 6)
     endif
  elseif (sub_name .eq. 'elemdx') then
     open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
     if (ios .ne. 0) stop 3
     call openbf(11, 'IN', 11)
     if (test_case .eq. '1') then
        card = '| RCPTIM   |    2 |           0 |  16 | DEGREES KELVIN           |-------------|'
        call elemdx(card,1)
     elseif (test_case .eq. '2') then
        card = '| MXTM     |    2 |           0 |  16 |                          |-------------|'
        call elemdx(card,1)
     elseif (test_case .eq. '3') then
        card = '| MXTM     |   2A |           0 |  16 | DEGREES KELVIN           |-------------|'
        call elemdx(card,1)
     elseif (test_case .eq. '4') then
        card = '| MXTM     |    2 |       -15@0 |  16 | DEGREES KELVIN           |-------------|'
        call elemdx(card,1)
     elseif (test_case .eq. '5') then
        card = '| MXTM     |    2 |           0 |  1x | DEGREES KELVIN           |-------------|'
        call elemdx(card,1)
     endif
  elseif (sub_name .eq. 'getntbe') then
     open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
     if (ios .ne. 0) stop 3
     if (test_case .eq. '1') then
        card = ' DUMMY |                                                                        '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios .ne. 0) stop 3
        call getntbe(11, iret, card, jret)
     endif
  elseif (sub_name .eq. 'gettbh') then
     open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
     if (ios .ne. 0) stop 3
     open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
     if (ios .ne. 0) stop 3
     if (test_case .eq. '1') then
        card = 'Table B STD |  0                                                                '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios .ne. 0) stop 3
        call gettbh(11, 12, 'B', imt, imtv, iogce, iltv)
     elseif (test_case .eq. '2') then
        card = 'Table B STX |  0 | 38                                                           '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios .ne. 0) stop 3
        call gettbh(11, 12, 'B', imt, imtv, iogce, iltv)
     elseif (test_case .eq. '3') then
        card = 'Table B STD |  0 | 38                                                           '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios .ne. 0) stop 3
        card = 'Table B LOC |  0 | 7                                                            '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
        if (ios .ne. 0) stop 3
        call gettbh(11, 12, 'B', imt, imtv, iogce, iltv)
     elseif (test_case .eq. '4') then
        card = 'Table B STD |  0 | 38                                                           '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios .ne. 0) stop 3
        card = 'Table B LOX |  0 | 7 |  1                                                       '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
        if (ios .ne. 0) stop 3
        call gettbh(11, 12, 'B', imt, imtv, iogce, iltv)
     elseif (test_case .eq. '5') then
        card = 'Table B STD |  0 | 38                                                           '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios .ne. 0) stop 3
        card = 'Table B LOC |  1 | 7 |  1                                                       '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
        if (ios .ne. 0) stop 3
        call gettbh(11, 12, 'B', imt, imtv, iogce, iltv)
     endif
  elseif (sub_name .eq. 'idn30') then
     if (test_case .eq. '1') then
        idn30_val = idn30(adn30_val_5, 6)
     elseif (test_case .eq. '2') then
        idn30_val = idn30(adn30_val_5, 2)
     elseif (test_case .eq. '3') then
        idn30_val = idn30('-0042', 5)
     elseif (test_case .eq. '4') then
        idn30_val = idn30('65536', 5)
     endif
  elseif (sub_name .eq. 'ifbget') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call ifbget(11)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call ifbget(11)
     elseif (test_case .eq. '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'OUT', 10)
        call ifbget(11)
     endif
  elseif (sub_name .eq. 'isize') then
     if (test_case .eq. '1') then
        iret = isize(1000000)
     elseif (test_case .eq. '2') then
        iret = isize(-10)
     endif
  elseif (sub_name .eq. 'iupm') then
     if (test_case .eq. '1') then
        iret = iupm(char_8, 100)
     endif
  elseif (sub_name .eq. 'iupvs01') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        iret = iupvs01(11, 'LENM')
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 10)
        iret = iupvs01(11, 'LENM')
     elseif (test_case .eq. '3') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 10)
        iret = iupvs01(11, 'LENM')
     endif
  elseif (sub_name .eq. 'jstnum') then
     if (test_case .eq. '1') then
        char_val_8 = '        '
        call jstnum(char_val_8,char_1,iret)
     endif
  elseif (sub_name .eq. 'lstjpb') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call readmg(11, char_val_8, jdate, iret)
        call lstjpb(-1, 1, 'DRP')
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call readmg(11, char_val_8, jdate, iret)
        call lstjpb(10000, 1, 'DRP')
     endif
  elseif (sub_name .eq. 'msgwrt') then
     filnam = 'testfiles/IN_2'
     call cobfl_c( filnam, 'r' )
     open(unit = 31, file = '/dev/null')
     call openbf(31, 'INUL', 31)
     call crbmg_c(bfmg, 200000, msgl, iret)
     if (test_case .eq. '1') then
        ibit = 64
        call pkb(25, 24, ibfmg, ibit)
     elseif (test_case .eq. '2') then
        ibit = 256
        call pkb(25, 24, ibfmg, ibit)
     elseif (test_case .eq. '3') then
        ! Make it look like there's a Section 2 in the message.
        ibit = 120
        call pkb(1, 1, ibfmg, ibit)
        ibit = 256
        call pkb(3, 24, ibfmg, ibit)
     endif
     call msgwrt(31, ibfmg, 19926)
  elseif (sub_name .eq. 'mtfnam') then
     if (test_case .eq. '1') then
        call mtinfo('../tables', 80, 81)
        call mtfnam(999, 15, 7, 1, 'TableB', char_85, char_120)
     elseif (test_case .eq. '2') then
        call mtinfo('.', 80, 81)
        call mtfnam(999, 15, 7, 1, 'TableB', char_85, char_120)
     endif
  elseif (sub_name .eq. 'nemtba') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call nemtba(11, 'SPOCK', mtyp, msbt, inod)
     endif
  elseif (sub_name .eq. 'nemtbb') then
     open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     if (ios .ne. 0) stop 3
     if (test_case .eq. '1') then
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       if (ios .ne. 0) stop 3
       card = '| NC007200 | A54124 | MTYPE 007-200                                            |'
       write (12,'(A)') card
       card = '| YEAR     | 004001 | YEAR                                                     |'
       write (12,'(A)') card
       card = '| NC007200 | YEAR                                                              |'
       write (12,'(A)') card
       card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
       write (12,'(A)') card
       close (12)
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       call openbf(11, 'OUT', 12)
       call nemtbb(1,-1,unit,iscl,iref,ibit)
     elseif (test_case .eq. '2') then
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       if (ios .ne. 0) stop 3
       card = '| NC007200 | A54124 | MTYPE 007-200                                            |'
       write (12,'(A)') card
       card = '| STMID    | 001025 | STORM IDENTIFIER                                         |'
       write (12,'(A)') card
       card = '| NC007200 | STMID                                                             |'
       write (12,'(A)') card
       card = '| STMID    |    0 |           0 |  26 | CCITT IA5                |-------------|'
       write (12,'(A)') card
       close (12)
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       call openbf(11, 'OUT', 12)
     elseif (test_case .eq. '3') then
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       if (ios .ne. 0) stop 3
       card = '| NC007200 | A54124 | MTYPE 007-200                                            |'
       write (12,'(A)') card
       card = '| YEAR     | 004001 | YEAR                                                     |'
       write (12,'(A)') card
       card = '| NC007200 | YEAR                                                              |'
       write (12,'(A)') card
       card = '| YEAR     |    0 |           0 |  33 | YEAR                     |-------------|'
       write (12,'(A)') card
       close (12)
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       call openbf(11, 'OUT', 12)
     endif
  elseif (sub_name .eq. 'nemtbd') then
     open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     if (ios .ne. 0) stop 3
     if (test_case .eq. '1') then
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       if (ios .ne. 0) stop 3
       card = '| NC007200 | A54124 | MTYPE 007-200                                            |'
       write (12,'(A)') card
       card = '| YEAR     | 004001 | YEAR                                                     |'
       write (12,'(A)') card
       card = '| NC007200 | YEAR                                                              |'
       write (12,'(A)') card
       card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
       write (12,'(A)') card
       close (12)
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       call openbf(11, 'OUT', 12)
       call nemtbd(1,-1,nseq,nems,irps,knts)
     endif
  elseif (sub_name .eq. 'nenubd') then
     open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
     if (ios .ne. 0) stop 3
     call openbf(11, 'IN', 11)
     if (test_case .eq. '1') then
        call nenubd('BPID    ','001008',1)
     elseif (test_case .eq. '2') then
        call nenubd('BPID2   ','001005',1)
     elseif (test_case .eq. '3') then
        call nenubd('LALOLV  ','301025',1)
     elseif (test_case .eq. '4') then
        call nenubd('LALOLV2 ','301024',1)
     endif
  elseif (sub_name .eq. 'nmsub') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        iret = nmsub(11)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_7_bufrtab', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        iret = nmsub(11)
     elseif (test_case .eq. '3') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        iret = nmsub(11)
     endif
  elseif (sub_name .eq. 'nvnwin') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call readns(11, char_val_8, jdate, iret)
        call nvnwin(1717, 1, 25, 175, jdate1, 5)
     endif
  elseif (sub_name .eq. 'openbf') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'BBB', 11)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call openbf(11, 'IN', 11)
     elseif (test_case .eq. '3') then
        do u = 1, 33
           open(unit = u+10, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
           if (ios .ne. 0) stop 3
           call openbf(u+10, 'IN', 11)
        end do
     endif
  elseif (sub_name .eq. 'openmg') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call openmg(11, 'F5FCMESG', 2021022312)
        ! Commented out. See https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/395.
        !     elseif (test_case .eq. '2') then
        !        call openmg(11, 'F5FCMESG', 2021022312)        
     endif
  elseif (sub_name .eq. 'parstr') then
     if (test_case .eq. '1') then
        call parstr(char_85, tags, 5, iret, ' ', .true.)
     elseif (test_case .eq. '2') then
        card = 'MNEM1 MNEM2 MNEM3 MNEM4 MNEM5 MNEM6                                             '
        call parstr(card, tags, 5, iret, ' ', .true.)
     elseif (test_case .eq. '3') then
        card = 'MNEM1MNEM2 MNEM3 MNEM4 MNEM5 MNEM6                                              '
        call parstr(card, tags, 5, iret, ' ', .true.)
     endif
  elseif (sub_name .eq. 'parusr') then
     if (test_case .eq. '6') then
        open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
     else
        open(unit = 11, file = 'testfiles/data/prepbufr', form = 'UNFORMATTED', iostat = ios)
     endif
     if (ios .ne. 0) stop 3
     call openbf(11, 'IN', 11)
     call readns(11, char_val_8, jdate, iret)
     if (iret .ne. 0) stop 3
     if (test_case .eq. '1') then
        call parusr(char_85, 1, 1, 1)
     elseif (test_case .eq. '2') then
        card = 'POB>0 QOB>0 TOB>0 VOB>0 UOB>0 XOB>0 YOB>0 ELV>0 TYP>0 T29>0 ITP>0               '
        call parusr(card, 1, 11, 0)
     elseif (test_case .eq. '3') then
        card = 'POB QOB TOB VOB UOB XOB YOB ELV TYP T29 ITP A1 A2 A3 B1 B2 B3 S1 S2 S3 E1       '
        call parusr(card, 1, 21, 0)
     elseif (test_case .eq. '4') then
        card = 'PRSLEVEL^0                                                                      '
        call parusr(card, 1, 11, 0)
     elseif (test_case .eq. '5') then
        card = 'POB QOB TOB VOB UOB XOB YOB ELV TYP T29 ITP                                     '
        call parusr(card, 1, 10, 0)
     elseif (test_case .eq. '6') then
        card = 'HGTSIG DCHSIG                                                                   '
        call parusr(card, 1, 2, 0)
     endif
  elseif (sub_name .eq. 'pkb') then
     if (test_case .eq. '1') then
        call pkb(1, 65, ibay, ibit)        
     endif
  elseif (sub_name .eq. 'pkb8') then
     if (test_case .eq. '1') then
        call pkb(1, 1, ibay, ibit)        
     elseif (test_case .eq. '2') then
        call pkb(1, 65, ibay, ibit)        
     endif
  elseif (sub_name .eq. 'pkbs1') then
     filnam = 'testfiles/IN_2'
     call cobfl_c( filnam, 'r' )
     call crbmg_c(bfmg, 200000, msgl, iret)
     if (test_case .eq. '1') then
        call pkbs1(88, ibfmg, 'DUMMY')
     endif
  elseif (sub_name .eq. 'pkvs01') then
     if (test_case .eq. '1') then
        if (isetprm('MXS01V',1) .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call pkvs01('OGCE', 88)
        call pkvs01('OGCE', 84) ! test the overwrite logic too
        call pkvs01('USN', 2)
     endif
  elseif (sub_name .eq. 'posapx') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call posapx(11)        
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call posapx(12)        
     endif
  elseif (sub_name .eq. 'rdmgsb') then
     open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
     if (ios .ne. 0) stop 3
     if (test_case .eq. '1') then
        call rdmgsb(11, 3, 1)
     elseif (test_case .eq. '2') then
        call rdmgsb(11, 1, 3)
     endif
  elseif (sub_name .eq. 'rdmtbb') then
     open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
     if (ios .ne. 0) stop 3
     open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
     if (ios .ne. 0) stop 3
     if (test_case .eq. '1') then
        card = 'Table B STD |  0 | 38                                                           '
        write (11,'(A)') card
        card = ' 0-01-001 |  0 |     0 |   7 | Numeric   | WMOB   ; ; WMO block number          '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios .ne. 0) stop 3
        card = 'Table B LOC |  0 | 7 |  1                                                       '
        write (12,'(A)') card
        card = ' 001001 |  0 |     2 |  12 | Code table   | QCWS  ; ; Wind speed quality mark   '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
        if (ios .ne. 0) stop 3
        call rdmtbb(11, 12, 1, imt, imtv, iogce, iltv, iret, &
                    int_1d, char_4, char_12, char_4, char_24, char_8, char_4, char_120)
     endif
  elseif (sub_name .eq. 'rdmtbd') then
     open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
     if (ios .ne. 0) stop 3
     open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
     if (ios .ne. 0) stop 3
     if (test_case .eq. '1') then
        card = 'Table D STD |  0 | 38                                                           '
        write (11,'(A)') card
        card = '   3-01-058 | UNTFROLD   ;     ; Universal lightning event                      '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios .ne. 0) stop 3
        card = 'Table D LOC |  0 | 7 |  1                                                       '
        write (12,'(A)') card
        card = '   3-01-058 | LOWRESSEQ   ;     ; Low-resolution data sequence                  '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
        if (ios .ne. 0) stop 3
        call rdmtbd(11, 12, 1, 5, imt, imtv, iogce, iltv, iret, &
                    int_1d, char_8, char_4, char_120, int_1d_2, int_2d, char_120_2)
     endif
  elseif (sub_name .eq. 'rdmtbf') then
     open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
     if (ios .ne. 0) stop 3
     open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
     if (ios .ne. 0) stop 3
     if (test_case .eq. '1') then
        card = 'Table F STD |  0 | 35                                                           '
        write (11,'(A)') card
        card = '   0-02-002 | TIWM ; FLAG                                                       '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios .ne. 0) stop 3
        card = 'Table F LOC |  0 | 7 |  1                                                       '
        write (12,'(A)') card
        card = '   002002 | NCDY3 ; CODE                                                        '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
        if (ios .ne. 0) stop 3
        call rdmtbf(11, 12)
     endif
  elseif (sub_name .eq. 'rdusdx') then
     open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
     if (ios .ne. 0) stop 3
     if (test_case .eq. '1') then
        card = '| MY-MNEM  |        |                                                          |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     elseif (test_case .eq. '2') then
        card = '| MYMNEM   | 405001 |                                                          |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     elseif (test_case .eq. '3') then
        card = '| MYMNEM   | 0H5001 |                                                          |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     elseif (test_case .eq. '4') then
        card = '| MYMNEM   | 065001 |                                                          |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     elseif (test_case .eq. '5') then
        card = '| MYMNEM   | 005256 |                                                          |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     elseif (test_case .eq. '6') then
        card = '| NC011004 | A63255 |                                                          |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     elseif (test_case .eq. '7') then
        card = '| MYMNEM                                                                       |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     endif
  elseif (sub_name .eq. 'readerme') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 12)
        call readerme(int_1d, 12, char_val_8, jdate, iret)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call readerme(int_1d, 11, char_val_8, jdate, iret)
     elseif (test_case .eq. '3') then
        filnam = 'testfiles/data/debufr_3'
        call cobfl_c( filnam, 'r' )
        open(unit = 31, file = '/dev/null')
        call openbf(31, 'INUL', 31)
        call crbmg_c(bfmg, 200000, msgl, iret)
        bfmg(1) = 'C'
        call readerme(ibfmg, 31, char_val_8, jdate, iret)
     endif
  elseif (sub_name .eq. 'readlc') then
     open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
     if (ios .ne. 0) stop 3
     open(unit = 12, file = 'testfiles/IN_2_bufrtab', iostat = ios)
     if (ios .ne. 0) stop 3
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call readlc(12, char_val_8, char_val_8)
     elseif (test_case .eq. '2') then
        open(unit = 13, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(13, 'OUT', 12)
        call readlc(13, char_val_8, char_val_8)
     elseif (test_case .eq. '3') then
        call openbf(11, 'IN', 11)
        call readlc(11, char_val_8, char_val_8)
     elseif (test_case .eq. '4') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call readlc(11, char_val_8, 'YEAR MNTH')
     elseif (test_case .eq. '5') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call readlc(11, char_val_8, 'YEAR')
     elseif (test_case .eq. '6') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call readlc(11, char_1, 'BULTIM')
     endif
  elseif (sub_name .eq. 'readmg') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 12)
        call readmg(12, char_val_8, jdate, iret)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call readmg(11, char_val_8, jdate, iret)
     endif
  elseif (sub_name .eq. 'rdmems') then
     open(unit = 11, file = 'testfiles/IN_6_infile1', form = 'UNFORMATTED', iostat = ios)
     if (ios .ne. 0) stop 3
     call ufbmem(11, 0, iret, iunit)
     if (test_case .eq. '1') then
        call rdmems(11, jret)
     endif
  elseif (sub_name .eq. 'readns') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 12)
        call readns(12, char_val_8, jdate, iret)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call readns(11, char_val_8, jdate, iret)
     endif
  elseif (sub_name .eq. 'readsb') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call readsb(11, iret)
     elseif (test_case .eq. '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'OUT', 10)
        call readsb(11, iret)
     endif
  elseif (sub_name .eq. 'rewnbf') then
     open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
     if (ios .ne. 0) stop 3
     open(unit = 12, file = 'testfiles/IN_2_bufrtab', iostat = ios)
     if (ios .ne. 0) stop 3
     if (test_case .eq. '1') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call rewnbf(11, 4)
     elseif (test_case .eq. '2') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call rewnbf(11, 0)
        call rewnbf(11, 0)
     elseif (test_case .eq. '3') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call rewnbf(11, 0)
        call rewnbf(11, 1)
        call rewnbf(11, 1)
     elseif (test_case .eq. '4') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call rewnbf(11, 1)
     elseif (test_case .eq. '5') then
        call openbf(11, 'FIRST', 12)
        call rewnbf(11, 0)
     endif
  elseif (sub_name .eq. 'rtrcpt') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call rtrcpt(11, iyr, imo, idy, ihr, imi, iret)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call rtrcpt(11, iyr, imo, idy, ihr, imi, iret)
     elseif (test_case .eq. '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'OUT', 10)
        call rtrcpt(11, iyr, imo, idy, ihr, imi, iret)
     endif
  elseif (sub_name .eq. 'seqsdx') then
     open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     if (ios .ne. 0) stop 3
     if ((test_case .eq. '14') .or. (test_case .eq. '15')) then
        if (isetprm('MAXCD',22) .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/OUT_6_bufrtab', iostat = ios)
     else
        open(unit = 12, file = 'testfiles/OUT_2_bufrtab', iostat = ios)
     endif
     if (ios .ne. 0) stop 3
     call openbf(11, 'OUT', 12)
     if (test_case .eq. '1') then
        card = '| DUMMYD   |                                                                   |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '2') then
        card = '| DRPSTAK  |                                                                   |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '3') then
        card = '| DRPSTAK  | <YYMMDD                                                           |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '4') then
        card = '| DRPSTAK  | "YYMMDD"0                                                         |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '5') then
        card = '| DRPSTAK  | "YYMMDD"256                                                       |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '6') then
        card = '| DRPSTAK  | {YYMMDD}10                                                        |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '7') then
        card = '| DRPSTAK  | DUMMYBMNEM                                                        |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '8') then
        card = '| DRPSTAK  | WNDSQ-1                                                           |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '9') then
        card = '| DRPSTAK  | {FOST}                                                            |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '10') then
        card = '| DRPSTAK  | .DTMMXGS TMDB                                                     |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '11') then
        card = '| DRPSTAK  | .DTMMXGS                                                          |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '12') then
        card = '| DRPSTAK  | .DTMMXGG MXGG                                                     |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '13') then
        card = '| DRPSTAK  | DUMMYB                                                            |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '14') then
        card = '| F5FCMESG | "F5FCRSEQ"3                                                       |'
        call seqsdx(card, 1)
     elseif (test_case .eq. '15') then
        card = '| F5FCMESG | DBSS                                                              |'
        call seqsdx(card, 1)
     endif
  elseif (sub_name .eq. 'status') then
     if (test_case .eq. '1') then
        call status(0, 0, 0, 0)        
     elseif (test_case .eq. '2') then
        call status(100, 0, 0, 0)        
     endif
  elseif (sub_name .eq. 'sntbbe') then
     if (test_case .eq. '1') then
        call sntbbe(0, 'c', 1, 2, int_1d, char_4, char_12, char_4, char_24, char_8, char_4, char_120)        
     elseif (test_case .eq. '2') then
        card = '  0-00-007 |   0 |                                                              '
     elseif (test_case .eq. '3') then
        card = '  0-00-007 |     |           0 |  16 | CCITT IA5         | CMTVN    ;     ;     '
     elseif (test_case .eq. '4') then
        card = '  0-00-007 |   0 |             |  16 | CCITT IA5         | CMTVN    ;     ;     '
     elseif (test_case .eq. '5') then
        card = '  0-00-007 |   0 |           0 |     | CCITT IA5         | CMTVN    ;     ;     '
     elseif (test_case .eq. '6') then
        card = '  0-00-007 |   0 |           0 |  16 |                   | CMTV$    ;     ;     '
     endif
     jret = 0
     call sntbbe(0, card, 1, jret, int_1d, char_4, char_12, char_4, char_24, char_8, char_4, char_120)
  elseif (sub_name .eq. 'sntbde') then
     card = '  3-01-022 | LTLONHHT   ;     ;                                                 '
     if (test_case .eq. '1') then
        call sntbde(0, 0, 'c', 1, 1, 2, int_1d, char_8, char_4, char_120, int_1d, int_1d, char_120)
     elseif (test_case .eq. '2') then
        card(21:21) = '$'
     elseif (test_case .eq. '3') then
        open(unit = 12, file = '/dev/null')
     elseif (test_case .eq. '4') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
        char_85 = '             0-05-001                                                                '
        write (12,'(A)') char_85
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
     elseif (test_case .eq. '5') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
        char_85 = '           | 0-05-001 >                                                              '
        write (12,'(A)') char_85
        char_85 = '           | 0-06-300                                                                '
        write (12,'(A)') char_85
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
     endif
     jret = 0
     call wrdlen
     call sntbde(12, 49430, card, 1, 1, jret, int_1d, char_8, char_4, char_120, int_1d, int_1d, char_120)
  elseif (sub_name .eq. 'sntbfe') then
     if (test_case .eq. '1') then
        open(unit = 12, file = '/dev/null')
     elseif (test_case .eq. '2') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
        card = '            0-01-031,0-01-033,0-01-035=176                                      '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
     elseif (test_case .eq. '3') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
        card = '          | 0-01-031,0-01-033,0-01-035 176                                      '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
     elseif (test_case .eq. '4') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
        card = '          |                           =176                                      '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
     elseif (test_case .eq. '5') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
        card = '          | 0-01-331,0-01-033,0-01-035=176                                      '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
     elseif (test_case .eq. '6') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
        card = '          | 0-01-031,0-01-033,0-01-035=                                         '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
     elseif (test_case .eq. '7') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
        card = '          | 0-01-031,0-01-033,0-01-035=17T                                      '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios .ne. 0) stop 3
     endif
     call wrdlen
     call sntbfe(12, 288)
  elseif (sub_name .eq. 'stdmsg') then
     if (test_case .eq. '1') then
        call stdmsg('W')
     endif
  elseif (sub_name .eq. 'stndrd') then
     filnam = 'testfiles/IN_11'
     call cobfl_c ( filnam, 'r' )
     call crbmg_c ( bfmg, 200000, msgl, iret )
     if ( iret .ne. 0 ) stop 3
     call ccbfl_c ()
     open ( unit = 21, file = filnam, form = 'unformatted', iostat = ios )
     if (ios .ne. 0) stop 3
     open ( unit = 22, file = 'testfiles/IN_11_bufrtab', iostat = ios )
     if (ios .ne. 0) stop 3
     call openbf ( 21, 'IN', 22 )
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call stndrd(12, int_1d, 1, int_1d_2)
     elseif (test_case .eq. '2') then
        bfmg(7) = '3'
        call stndrd ( 21, ibfmg, 50000, ibfmg2 )
     elseif (test_case .eq. '3') then
        bfmg(188210) = '8'
        call stndrd ( 21, ibfmg, 50000, ibfmg2 )
     elseif (test_case .eq. '4') then
        bfmg(46) = '8'
        call stndrd ( 21, ibfmg, 50000, ibfmg2 )
     elseif (test_case .eq. '5') then
        bfmg(17468) = 'z'
        bfmg(17469) = 'z'
        bfmg(17470) = 'z'
        call stndrd ( 21, ibfmg, 50000, ibfmg2 )
     elseif (test_case .eq. '6') then
        call stndrd ( 21, ibfmg, 5000, ibfmg2 )
     endif
  elseif (sub_name .eq. 'strtbfe') then
     if (test_case .eq. '1') then
       if (isetprm('MXMTBF',100) .ne. 0) stop 3
       open(unit = 11, file = 'testfiles/IN_4', iostat = ios)
       call openbf(11, 'SEC3', 11)
       call codflg('Y')
       call readns(11, char_val_8, jdate, iret)
     endif
  elseif (sub_name .eq. 'strbtm') then
     if (test_case .eq. '1') then
       if (isetprm('MXBTMSE',8) .ne. 0) stop 3
       open(unit = 11, file = 'testfiles/IN_4', iostat = ios)
       call openbf(11, 'SEC3', 11)
       call readns(11, char_val_8, jdate, iret)
     endif
  elseif (sub_name .eq. 'strcpt') then
     if (test_case .eq. '1') then
        call strcpt('W', 1960, 12, 15, 12, 0)
     endif
  elseif (sub_name .eq. 'string') then
     open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
     if (ios .ne. 0) stop 3
     call openbf(11, 'IN', 11)
     if (test_case .eq. '1') then
       call string('012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789', 1, 1, 0)
     elseif (test_case .eq. '2') then
       call readns(11, char_val_8, jdate, iret)
       call ufbint(11, real_2d_3x1, 3, 1, iret, 'YEAR MNTH DAYS')
       call string('YEAR MNTH DAYS', 1, 2, 0)
     endif
  elseif (sub_name .eq. 'tabent') then
     if (test_case .eq. '1') then
       if (isetprm('MXNRV',1) .ne. 0) stop 3
       open(unit = 11, file = 'testfiles/IN_7', form = 'UNFORMATTED', iostat = ios)
       if (ios .ne. 0) stop 3
       open(unit = 12, file = 'testfiles/IN_7_bufrtab', iostat = ios)
       if (ios .ne. 0) stop 3
       call openbf(11, 'IN', 12)
       open(unit = 13, file = 'testfiles/OUT_1', form = 'UNFORMATTED', iostat = ios)
       if (ios .ne. 0) stop 3
       call openbf(13, 'IN', 13)
     endif
  elseif (sub_name .eq. 'tabsub') then
     if (test_case .eq. '11') then
       if (isetprm('MXTCO',3) .ne. 0) stop 3
       open(unit = 11, file = 'testfiles/IN_4', iostat = ios)
       call openbf(11, 'SEC3', 11)
       call readns(11, char_val_8, jdate, iret)
     elseif (test_case .eq. '12') then
       if (isetprm('MXTAMC',1) .ne. 0) stop 3
       open(unit = 11, file = 'testfiles/IN_4', iostat = ios)
       call openbf(11, 'SEC3', 11)
       call readns(11, char_val_8, jdate, iret)
       open(unit = 13, file = 'testfiles/OUT_3', iostat = ios)
       call openbf(13, 'IN', 13)
     else
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       if (ios .ne. 0) stop 3
       card = '| NC007200 | A54124 | MTYPE 007-200                                            |'
       write (12,'(A)') card
       if (test_case .eq. '1') then
       elseif (test_case .eq. '2') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 201129 YEAR                                                       |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case .eq. '3') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 202129 YEAR                                                       |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case .eq. '4') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 207002 YEAR                                                       |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case .eq. '5') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 208024 YEAR                                                       |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case .eq. '6') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 201129 201133 YEAR                                                |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case .eq. '7') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 202129 202133 YEAR                                                |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case .eq. '8') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 203012 YEAR 203006                                                |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case .eq. '9') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 203012 YEAR                                                       |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case .eq. '10') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 203000 YEAR                                                       |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       endif
       close (12)
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       call openbf(11, 'OUT', 12)
     endif
  elseif (sub_name .eq. 'ufbcnt') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbcnt(11, 1, 1)
     endif
  elseif (sub_name .eq. 'ufbcpy') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbcpy(11, 0)
     elseif (test_case .eq. '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'OUT', 10)
        call ufbcpy(12, 0)
     elseif (test_case .eq. '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 10)
        call ufbcpy(12, 0)
     elseif (test_case .eq. '4') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 12)
        call readmg(12, char_val_8, jdate, iret)
        if (iret .ne. 0) stop 100
        call readsb(12, iret)
        if (iret .ne. 0) stop 101
        call ufbcpy(12, 12)
     endif
  elseif (sub_name .eq. 'ufbcup') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbcup(11, 0)
     elseif (test_case .eq. '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'OUT', 10)
        call ufbcup(12, 0)
     elseif (test_case .eq. '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 10)
        call ufbcup(12, 0)
     elseif (test_case .eq. '4') then
        open(unit = 11, file = 'testfiles/IN_9', form ='unformatted')
        call openbf(11, 'IN', 11)
        open(unit = 12, file = 'test_ufbcup_out', form ='unformatted')
        call openbf(12, 'IN', 11)
        call readmg(11, char_val_8, jdate, iret)
        call ufbcup(11, 12)
     elseif (test_case .eq. '5') then
        open(unit = 11, file = 'testfiles/IN_9', form ='unformatted')
        call openbf(11, 'IN', 11)
        open(unit = 12, file = 'test_ufbcup_out', form ='unformatted')
        call readmg(11, char_val_8, jdate, iret)
        call readsb(11, iret)        
        call ufbcup(11, 12)
     elseif (test_case .eq. '6') then
        open(unit = 11, file = 'testfiles/IN_9', form ='unformatted')
        call openbf(11, 'IN', 11)
        open(unit = 12, file = 'test_ufbcup_out', form ='unformatted')
        call openbf(12, 'IN', 11)
        call readmg(11, char_val_8, jdate, iret)
        call readsb(11, iret)        
        call ufbcup(11, 12)
     elseif (test_case .eq. '7') then
        open(unit = 11, file = 'testfiles/IN_9', form ='unformatted')
        call openbf(11, 'IN', 11)
        open(unit = 12, file = 'test_ufbcup_out', form ='unformatted')
        call openbf(12, 'OUT', 11)
        call readmg(11, char_val_8, jdate, iret)
        call readsb(11, iret)        
        call ufbcup(11, 12)
     endif
  elseif (sub_name .eq. 'ufbdmp') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbdmp(11, 0)
     elseif (test_case .eq. '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'OUT', 10)
        call ufbdmp(12, 0)
     elseif (test_case .eq. '3') then
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 10)
        call ufbdmp(12, 0)
     endif
  elseif (sub_name .eq. 'ufbevn') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbevn(11, real_2d, 1, 2, 3, iret, 'c')
     elseif (test_case .eq. '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'OUT', 10)
        call ufbevn(12, real_2d, 1, 2, 3, iret, 'c')
     elseif (test_case .eq. '3') then
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 10)
        call ufbevn(12, real_2d, 1, 2, 3, iret, 'c')
     endif
  elseif (sub_name .eq. 'ufbget') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbget(11, real_1d, 1, iret, 's')
     elseif (test_case .eq. '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'OUT', 10)
        call ufbget(12, real_1d, 1, iret, 's')
     elseif (test_case .eq. '3') then
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 10)
        call ufbget(12, real_1d, 1, iret, 's')
     endif
  elseif (sub_name .eq. 'ufbin3') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbin3(11, real_2d, 1, 2, 3, iret, jret, 'c')
     elseif (test_case .eq. '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'OUT', 10)
        call ufbin3(12, real_2d, 1, 2, 3, iret, jret, 'c')
     elseif (test_case .eq. '3') then
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 10)
        call ufbin3(12, real_2d, 1, 2, 3, iret, jret, 'c')
     endif
  elseif (sub_name .eq. 'ufbint') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbint(11, real_2d, 1, 2, iret, 'c')
     elseif (test_case .eq. '2') then
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 10)
        call ufbint(12, real_2d, 1, 2, iret, 'c')
     endif
  elseif (sub_name .eq. 'ufbinx') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call ufbinx(11, 999, 999, 1, 1, iret, 'c')
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call ufbinx(11, 1, 999, 1, 1, iret, 'c')
     endif
  elseif (sub_name .eq. 'ufbmms') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbmem(11, 0, iret, iunit)
        if (iret .ne. 5 .or. iunit .ne. 11) stop 200
        call ufbmms(1, 999, char_val_8, jdate)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbmem(11, 0, iret, iunit)
        if (iret .ne. 5 .or. iunit .ne. 11) stop 200
        call ufbmms(0, 999, char_val_8, jdate)
     elseif (test_case .eq. '3') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbmem(11, 0, iret, iunit)
        if (iret .ne. 5 .or. iunit .ne. 11) stop 200
        call ufbmms(999, 999, char_val_8, jdate)
     endif
  elseif (sub_name .eq. 'ufbmns') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbmem(11, 0, iret, iunit)
        if (iret .ne. 5 .or. iunit .ne. 11) stop 200
        call ufbmns(9999, char_val_8, jdate)
     endif
  elseif (sub_name .eq. 'ufbovr') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)        
        call ufbovr(11, real_2d, 1, 1, iret, 'c')
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 10)        
        call ufbovr(11, real_2d, 1, 1, iret, 'c')
     elseif (test_case .eq. '3') then
        call openbf(12, 'FIRST', 11)        
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbovr(11, real_2d, 1, 1, iret, 'c')
     endif
  elseif (sub_name .eq. 'ufbpos') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbpos(11, 1, 1, char_val_8, jdate)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 10)        
        call ufbpos(11, 1, 1, char_val_8, jdate)
     elseif (test_case .eq. '3') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 10)        
        call ufbpos(11, 0, 1, char_val_8, jdate)
     elseif (test_case .eq. '4') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 10)        
        call ufbpos(11, 999, 1, char_val_8, jdate)
     elseif (test_case .eq. '5') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 10)        
        call ufbpos(11, 1, 9999, char_val_8, jdate)
     endif
  elseif (sub_name .eq. 'ufbqcd') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbqcd(11, 'c', iqcd)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 10)        
        call ufbqcd(11, 'c', iqcd)
     elseif (test_case .eq. '3') then
        open(unit = 11, file = 'testfiles/IN_5', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call readns(11, char_val_8, jdate, iret)
        call ufbqcd(11, 'ADPUPA', iqcd)
     endif
  elseif (sub_name .eq. 'ufbqcp') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbqcp(11, 0, 'c')
     endif
  elseif (sub_name .eq. 'ufbrep') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbrep(11, real_2d, 1, 2, iret, 'c')
     elseif (test_case .eq. '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 10)
        call ufbrep(12, real_2d, 1, 2, iret, 'c')
     endif
  elseif (sub_name .eq. 'ufbrms') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbmem(11, 0, iret, iunit)
        if (iret .ne. 5 .or. iunit .ne. 11) stop 200
        call ufbrms(999, 1, real_2d, 1, 1, iret, 'c')
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbmem(11, 0, iret, iunit)
        if (iret .ne. 5 .or. iunit .ne. 11) stop 200
        call ufbrms(1, 9999, real_2d, 1, 1, iret, 'c')
     elseif (test_case .eq. '3') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbmem(11, 0, iret, iunit)
        if (iret .ne. 5 .or. iunit .ne. 11) stop 200
        call ufbrms(0, 9999, real_2d, 1, 1, iret, 'c')
     endif
  elseif (sub_name .eq. 'ufbstp') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call ufbstp(11, real_2d, 1, 1, iret, 'LALALA')
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call ufbstp(11, real_2d, 1, 1, iret, 'LALALA')
     elseif (test_case .eq. '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'OUT', 10)
        call ufbstp(11, real_2d, 1, 1, iret, 'LALAL1')
     endif
  elseif (sub_name .eq. 'ufbseq') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbseq(11, real_2d, 1, 1, iret, 'c')
     elseif (test_case .eq. '2') then
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 10)
        call ufbseq(12, real_2d, 1, 1, iret, 'c')
     elseif (test_case .eq. '3') then
        open(unit = 12, file = 'testfiles/IN_6_infile2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 12)
        call readns(12, char_val_8, jdate, iret)
        if (iret .ne. 0) stop 3
        call ufbseq(12, real_2d, 1, 1, iret, ' ')
     elseif (test_case .eq. '4') then
        open(unit = 12, file = 'testfiles/IN_6_infile2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 12)
        call readns(12, char_val_8, jdate, iret)
        if (iret .ne. 0) stop 3
        call ufbseq(12, real_2d, 1, 1, iret, 'YEAR MNTH')
     elseif (test_case .eq. '5') then
        open(unit = 12, file = 'testfiles/IN_6_infile2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 12)
        call readns(12, char_val_8, jdate, iret)
        if (iret .ne. 0) stop 3
        call ufbseq(12, real_2d, 1, 1, iret, 'YEAR')
     elseif (test_case .eq. '6') then
        open(unit = 12, file = 'testfiles/IN_6_infile2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 12)
        call readns(12, char_val_8, jdate, iret)
        if (iret .ne. 0) stop 3
        call ufbseq(12, real_2d, 1, 1, iret, 'UARID')
     endif
  elseif (sub_name .eq. 'ufdump') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call ufdump(11, 11)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call ufdump(11, 12)
     elseif (test_case .eq. '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'OUT', 10)
        call ufdump(11, 12)
     endif
  elseif (sub_name .eq. 'upb8') then
     if (test_case .eq. '1') then
        call upb8(nval, -1, ibit, ibay)
     endif
  elseif (sub_name .eq. 'upftbv') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call upftbv(11, 'n', 1.0, 1, 1, 1)
     elseif (test_case .eq. '2') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call upftbv(11, 'n', 1.0, 1, 1, 1)
     endif
  elseif (sub_name .eq. 'usrtpl') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call usrtpl(11, 1, 1)
     endif
  elseif (sub_name .eq. 'wrdxtb') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call wrdxtb(11, 11)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call wrdxtb(11, 12)
     elseif (test_case .eq. '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'OUT', 10)
        call wrdxtb(11, 12)
     endif
  elseif (sub_name .eq. 'writdx') then
     if (test_case .eq. '1') then
        call writdx(0, 0, 0)
     endif
  elseif (sub_name .eq. 'writlc') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call writlc(11, char_val_8, char_val_8)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call writlc(11, char_val_8, char_val_8)
     elseif (test_case .eq. '3') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 12)
        call writlc(11, char_val_8, char_val_8)
     endif
  elseif (sub_name .eq. 'writsa') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call writsa(11, lmsgt, msgt, msgl)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call writsa(11, lmsgt, msgt, msgl)
     elseif (test_case .eq. '3') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 12)
        call writsa(11, lmsgt, msgt, msgl)
     endif
  elseif (sub_name .eq. 'writsb') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call writsb(11)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call writsb(11)
     elseif (test_case .eq. '3') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 12)
        call writsb(11)
     endif
  elseif (sub_name .eq. 'wtstat') then
     if (test_case .eq. '1') then
        call wtstat(0, 0, 0, 0)
     elseif (test_case .eq. '2') then
        call wtstat(1, 0, 0, 0)
     elseif (test_case .eq. '3') then
        call wtstat(1, 1, -2, 0)
     elseif (test_case .eq. '4') then
        call wtstat(1, 1, 0, -1)
     endif
  else
     print *, "Unknown test function"
     ! Return with 0 to fail the test.
     stop 0 
  endif

end program test_bort
