! This is a test for NCEPLIBS-bufr library.
!
! This tests the bort() and bort2() subroutines. It will also test the
! bort() calls of other subroutines.
!
! This program is called (repeatedly) by run_test_bort.sh, which
! passes in a series of subroutine names and test case numbers, and
! expects each case to either cause an abort or catch an abort message.
! In either case, this program returns a value >0 if it was successful
! in causing the expected abort or catching the expected abort message.
! Otherwise, or if some other problem occurred, it returns a value
! of 0 to indicate failure.
!
! Ed Hartnett 3/12/23
program test_bort
  use bufr_interface
  implicit none
  integer iret, jret, iunit, iqcd
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
  character*5 adn30, adn30_val_5
  real*8 real_1d(1)
  real*8 real_2d(1,1)
  real*8 real_2d_3x1(3,1)
  integer idn30, idn30_val
  integer :: num_args, len, stat, ios, u
  character(len=32) :: sub_name, test_case
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
  integer lun, il, im, kmsg, ksub
  integer*8 nval
  character*400 errstr
  integer errstr_len

  integer*4 isize, iupm, iupvs01, isetprm, nmsub, igettdi, igetsc, lcmgdf, catch_borts
  integer*4 msgl4, iret4
  integer*4, parameter :: mxmb = 200000
  integer, parameter :: mxmbd4 = mxmb/4

  character*25 filnam
  character bfmg(mxmb)
  integer ibfmg(mxmbd4), ibfmg2(mxmbd4)
  equivalence (bfmg(1),ibfmg(1))

#ifdef KIND_8
  call setim8b(.true.)
#endif

  num_args = command_argument_count()
  if (num_args /= 2 .and. num_args /= 3) then
     print *, "Either 2 or 3 command line arguments expected:"
     print *, "    1. subroutine or function name"
     print *, "    2. test case number"
     print *, "    3. (optional) switch to catch the abort message rather than aborting"
     ! Return with 0 to fail the test.
     stop 0
  end if

  ! Read the command line arguments, a name of subroutine or function, and a test
  ! case number.
  call get_command_argument(1, sub_name, len, stat)
  if (stat /= 0) stop 0
  call get_command_argument(2, test_case, len, stat)
  if (stat /= 0) stop 0
  if (num_args == 3) then
    print *, 'Testing ', sub_name, ' case ', test_case, ' with bort catching enabled'
    if (catch_borts('Y') /= 0) stop 0
  else
    print *, 'Testing ', sub_name, ' case ', test_case
  endif

  ! Run the test for the routine and test case.  We should only ever reach any of the
  ! checks for errstr_len and errstr if we're catching borts; otherwise, an actual abort
  ! should occur inside of the routine.
  if (sub_name == 'adn30') then
     if (test_case == '1') then
        char_30 = adn30(0, 9)
     elseif (test_case == '2') then
        char_30 = adn30(-1, 5)
     elseif (test_case == '3') then
        char_30 = adn30(65536, 5)
     elseif (test_case == '4') then
        char_30 = adn30(0, 3)
     endif
  elseif (sub_name == 'atrcpt') then
     if (test_case == '1') then
        filnam = 'testfiles/IN_11'
        call cobfl_c ( filnam, 'r' )
        call crbmg_c ( bfmg, mxmb, msgl4, iret4 )
        if ( iret4 /= 0 ) stop 0
        call ccbfl_c ()
        call atrcpt ( ibfmg, 5000, ibfmg2 )
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'ATRCPT - OVERFLOW OF OUTPUT MESSAGE ARRAY' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'bort') then
     if (test_case == '1') then
        call bort('goodbye!')
     endif
  elseif (sub_name == 'bort2') then
     if (test_case == '1') then
        call bort2('goodbye!', 'goodbye again!')
     endif
  elseif (sub_name == 'bvers') then
     if (test_case == '1') then
        call bvers(char_short)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'BVERS - INPUT STRING MUST CONTAIN SPACE FOR AT LEAST' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'chekstab') then
     open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       if (ios /= 0) stop 0
       card = '| YEAR     | 004001 | YEAR                                                     |'
       write (12,'(A)') card
       card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
       write (12,'(A)') card
       close (12)
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       call openbf(11, 'OUT', 12)
       call chekstab(1)
     endif
  elseif (sub_name == 'closmg') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call closmg(11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'CLOSMG - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '2') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call closmg(11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'CLOSMG - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'cmpmsg') then
     if (test_case == '1') then
        call cmpmsg('W')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'CMPMSG - INPUT ARGUMENT IS W, IT MUST BE EITHER Y, y, N OR n' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'cnved4') then
     open(unit = 31, file = '/dev/null')
     if (test_case == '1') then
        call openbf(31, 'SEC3', 31)
        filnam = 'testfiles/IN_1'
     elseif (test_case == '2') then
        open(unit = 12, file = 'testfiles/OUT_3_bufrtab', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(31, 'IN', 12)
        filnam = 'testfiles/OUT_3'
     endif
     call cobfl_c( filnam, 'r' )
     call crbmg_c(bfmg, mxmb, msgl4, iret4)
     call readerme(ibfmg, 31, char_val_8, jdate, iret)
     call cnved4(ibfmg, 1, ibay)
     call check_for_bort( errstr, errstr_len )
     if (test_case == '2' .and. errstr_len > 0 .and. &
       index( errstr(1:errstr_len), 'CNVED4 - OVERFLOW OF OUTPUT (EDITION 4) MESSAGE ARRAY' ) /= 0 ) stop 88
     stop 0
  elseif (sub_name == 'codflg') then
     if (test_case == '1') then
        call codflg('W')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'CODFLG - INPUT ARGUMENT IS W, IT MUST BE EITHER Y, y, N OR n' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'copybf') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call copybf(11, 0)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYBF - INPUT BUFR FILE IS OPEN, IT MUST BE CLOSED' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 12)
        call copybf(11, 12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYBF - OUTPUT BUFR FILE IS OPEN, IT MUST BE CLOSED' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'copymg') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call copymg(11, 0)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYMG - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call copymg(12, 0)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYMG - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call copymg(11, 0)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYMG - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '4') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call readmg(11, char_val_8, jdate, iret)
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 12)
        call copymg(11, 12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYMG - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '5') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call readmg(11, char_val_8, jdate, iret)
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call copymg(11, 12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYMG - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'copysb') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call copysb(11, 0, ierr)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYSB - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call copysb(12, 0, ierr)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYSB - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 10)
        call copysb(11, 0, ierr)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYSB - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '4') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 10)
        call readmg(11, char_val_8, jdate, iret)
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call copysb(11, 12, ierr)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYSB - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '5') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 10)
        call readmg(11, char_val_8, jdate, iret)
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 10)
        call copysb(11, 12, ierr)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYSB - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '6') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 10)
        call readmg(11, char_val_8, jdate, iret)
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 10)
        call copysb(11, 12, ierr)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYSB - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '7') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2_bufrtab', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 12)
        call readmg(11, char_val_8, jdate, iret)
        open(unit = 13, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 14, file = 'testfiles/IN_7_bufrtab', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(13, 'OUT', 14)
        call openmb(13, 'NC008023', 2021022312)
        call copysb(11, 13, ierr)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'COPYSB - INPUT AND OUTPUT BUFR FILES MUST HAVE THE SAME INTERNAL TABLES' ) /= 0 ) &
          stop 88
        stop 0
     endif
  elseif (sub_name == 'cpdxmm') then
     if (test_case == '1') then
       open(unit = 11, file = 'testfiles/IN_6_infile1', form = 'UNFORMATTED', iostat = ios)
       if (ios /= 0) stop 0
       open(unit = 12, file = 'testfiles/IN_6_infile2', form = 'UNFORMATTED', iostat = ios)
       if (ios /= 0) stop 0
       if (isetprm('MXDXTS',1) /= 0) stop 0
       call ufbmem(11, 0, iret, iunit)
       call ufbmem(12, 1, iret, iunit)
       call ufbmns(18364, char_val_8, jdate)
     endif
  elseif (sub_name == 'cpymem') then
     open(unit = 11, file = 'testfiles/IN_6_infile1', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     call ufbmem(11, 0, iret, iunit)
     if (test_case == '1') then
        call cpymem(12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), &
          'CPYMEM - A MESSAGE MUST BE OPEN IN INPUT BUFR MESSAGES IN INTERNAL MEMORY, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        call rdmemm(167, char_val_8, jdate, ierr)
        call cpymem(12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'CPYMEM - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        call rdmemm(167, char_val_8, jdate, ierr)
        call openbf(12, 'IN', 11)
        call cpymem(12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'CPYMEM - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '4') then
        call rdmemm(167, char_val_8, jdate, ierr)
        call openbf(12, 'OUT', 11)
        call openmg(12, 'NC004001', 2024020112)
        call cpymem(12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), &
          'CPYMEM - ALL MESSAGES MUST BE CLOSED IN OUTPUT BUFR FILE, A MESSAGE IS OPEN' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'datebf') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call datebf(11, mear, mmon, mday, mour, idate)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'DATEBF - INPUT BUFR FILE IS OPEN, IT MUST BE CLOSED' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'datelen') then
     if (test_case == '1') then
        call datelen(11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'DATELEN - INPUT ARGUMENT IS' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'dumpbf') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call dumpbf(11, jdate1, jdump1)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'DUMPBF - INPUT BUFR FILE IS OPEN, IT MUST BE CLOSED' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'dxdump') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        call dxdump(11, 6)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'DXDUMP - BUFR FILE IS CLOSED, IT MUST BE OPEN' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'elemdx') then
     open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     call openbf(11, 'IN', 11)
     if (test_case == '1') then
        card = '| RCPTIM   |    2 |           0 |  16 | DEGREES KELVIN           |-------------|'
        call elemdx(card,1)
     elseif (test_case == '2') then
        card = '| MXTM     |    2 |           0 |  16 |                          |-------------|'
        call elemdx(card,1)
     elseif (test_case == '3') then
        card = '| MXTM     |   2A |           0 |  16 | DEGREES KELVIN           |-------------|'
        call elemdx(card,1)
     elseif (test_case == '4') then
        card = '| MXTM     |    2 |       -15@0 |  16 | DEGREES KELVIN           |-------------|'
        call elemdx(card,1)
     elseif (test_case == '5') then
        card = '| MXTM     |    2 |           0 |  1x | DEGREES KELVIN           |-------------|'
        call elemdx(card,1)
     endif
  elseif (sub_name == 'getcfmng') then
     open(unit = 11, file = 'testfiles/IN_4', form ='unformatted', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
       call getcfmng(11, 'GCLONG', 254, ' ', -1, char_30, len, iret)
       call check_for_bort( errstr, errstr_len )
       if ( errstr_len > 0 .and. &
         index( errstr(1:errstr_len), 'GETCFMNG - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
       stop 0
     elseif (test_case == '2') then
       open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
       if (ios /= 0) stop 0
       call openbf(12, 'OUT', 11)
       call getcfmng(12, 'GCLONG', 254, ' ', -1, char_30, len, iret)
       call check_for_bort( errstr, errstr_len )
       if ( errstr_len > 0 .and. &
         index( errstr(1:errstr_len), 'GETCFMNG - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
         stop 88
       stop 0
     elseif (test_case == '3') then
       call openbf(11, 'IN', 11)
       call getcfmng(11, 'GCLONG', 254, ' ', -1, char_30, len, iret)
       call check_for_bort( errstr, errstr_len )
       if ( errstr_len > 0 .and. &
         index( errstr(1:errstr_len), 'GETCFMNG - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
       stop 0
     elseif (test_case == '4') then
       call openbf(11, 'SEC3', 11)
       call readns(11, char_val_8, jdate, iret)
       call getcfmng(11, 'GCLONG', 254, ' ', -1, char_30, len, iret)
     elseif (test_case == '5') then
       call openbf(11, 'SEC3', 11)
       call readns(11, char_val_8, jdate, iret)
       call codflg('Y')
       call getcfmng(11, 'GXLONG', 254, ' ', -1, char_30, len, iret)
     elseif (test_case == '6') then
       call openbf(11, 'SEC3', 11)
       call readns(11, char_val_8, jdate, iret)
       call codflg('Y')
       call getcfmng(11, 'SSNY', 254, ' ', -1, char_30, len, iret)
     elseif (test_case == '7') then
       call openbf(11, 'SEC3', 11)
       call readns(11, char_val_8, jdate, iret)
       call codflg('Y')
       call getcfmng(11, 'SAID', 254, 'GXES', 7, char_30, len, iret)
     elseif (test_case == '8') then
       call openbf(11, 'SEC3', 11)
       call readns(11, char_val_8, jdate, iret)
       call codflg('Y')
       call getcfmng(11, 'SAID', 254, 'SSNX', 7, char_30, len, iret)
     endif
  elseif (sub_name == 'getntbe') then
     open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
        card = ' DUMMY |                                                                        '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios /= 0) stop 0
        call getntbe(11, iret, card, jret)
     endif
  elseif (sub_name == 'gettbh') then
     open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
     if (ios /= 0) stop 0
     open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
        card = 'Table B STD |  0                                                                '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios /= 0) stop 0
        call gettbh(11, 12, 'B', imt, imtv, iogce, iltv)
     elseif (test_case == '2') then
        card = 'Table B STX |  0 | 38                                                           '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios /= 0) stop 0
        call gettbh(11, 12, 'B', imt, imtv, iogce, iltv)
     elseif (test_case == '3') then
        card = 'Table B STD |  0 | 38                                                           '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios /= 0) stop 0
        card = 'Table B LOC |  0 | 7                                                            '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
        if (ios /= 0) stop 0
        call gettbh(11, 12, 'B', imt, imtv, iogce, iltv)
     elseif (test_case == '4') then
        card = 'Table B STD |  0 | 38                                                           '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios /= 0) stop 0
        card = 'Table B LOX |  0 | 7 |  1                                                       '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
        if (ios /= 0) stop 0
        call gettbh(11, 12, 'B', imt, imtv, iogce, iltv)
     elseif (test_case == '5') then
        card = 'Table B STD |  0 | 38                                                           '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios /= 0) stop 0
        card = 'Table B LOC |  1 | 7 |  1                                                       '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
        if (ios /= 0) stop 0
        call gettbh(11, 12, 'B', imt, imtv, iogce, iltv)
     endif
  elseif (sub_name == 'idn30') then
     if (test_case == '1') then
        idn30_val = idn30(adn30_val_5, 6)
     elseif (test_case == '2') then
        idn30_val = idn30(adn30_val_5, 2)
     elseif (test_case == '3') then
        idn30_val = idn30('-0042', 5)
     elseif (test_case == '4') then
        idn30_val = idn30('65536', 5)
     endif
  elseif (sub_name == 'ifbget') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call ifbget(11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'IFBGET - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call ifbget(11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'IFBGET - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 10)
        call ifbget(11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'IFBGET - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'igetntbi') then
     if (test_case == '1') then
       if (isetprm('MAXTBB',15) /= 0) stop 0
       open(unit = 11, file = 'testfiles/IN_7', form = 'UNFORMATTED', iostat = ios)
       if (ios /= 0) stop 0
       open(unit = 12, file = 'testfiles/IN_7_bufrtab', iostat = ios)
       if (ios /= 0) stop 0
       call openbf(11, 'IN', 12)
     endif
  elseif (sub_name == 'igetrfel') then
     filnam = 'testfiles/IN_4'
     call cobfl_c( filnam, 'r' )
     open(unit = 31, file = '/dev/null')
     call openbf(31, 'SEC3', 31)
     call crbmg_c(bfmg, mxmb, msgl4, iret4)
     if (test_case == '1') then
        ! Change the last 2-37-000 operator in Section 3 to 2-35-000, so that the bitmap can't be located
        ! for any of the subsequent marker operators.
        ibit = 1016
        call pkb(163, 8, ibfmg, ibit)
     elseif (test_case == '2') then
        ! Change the first 2-24-000 operator in Section 3 to 2-22-000, so that the "follow" operator can't
        ! be located for any of the subsequent marker operators.
        ibit = 888
        call pkb(150, 8, ibfmg, ibit)
     elseif (test_case == '3') then
        ! Change the first 2-22-000 operator in Section 3 to 2-35-000, so that the previous referenced
        ! element can't be located for any of the subsequent marker operators.
        ibit = 312
        call pkb(163, 8, ibfmg, ibit)
     endif
     call mtinfo('../tables', 80, 81)
     call readerme(ibfmg, 31, char_val_8, jdate, iret)
     call readsb(31, iret)
  elseif (sub_name == 'igetsc') then
     if (test_case == '1') then
       iret = igetsc(11)
       call check_for_bort( errstr, errstr_len )
       if ( errstr_len > 0 .and. &
         index( errstr(1:errstr_len), 'IGETSC - BUFR FILE IS CLOSED, IT MUST BE OPEN' ) /= 0 ) stop 88
       stop 0
     endif
  elseif (sub_name == 'igettdi') then
     if (test_case == '1') then
       iret = igettdi(0)
       do u = 1, 257
         iret = igettdi(1)
       enddo
     endif
  elseif (sub_name == 'inctab') then
     if (test_case == '1') then
       open(unit = 11, file = 'testfiles/OUT_1', iostat = ios)
       if (ios /= 0) stop 0
       if (isetprm('MAXJL',10) /= 0) stop 0
       call openbf(11, 'IN', 11)
     endif
  elseif (sub_name == 'ipkm') then
     if (test_case == '1') then
       call ipkm(char_val_8, 6, 29)
       call check_for_bort( errstr, errstr_len )
       if ( errstr_len > 0 .and. &
         index( errstr(1:errstr_len), 'IPKM - NUMBER OF BYTES BEING PACKED , NBYT' ) /= 0 ) stop 88
       stop 0
     endif
  elseif (sub_name == 'isize') then
     if (test_case == '1') then
        iret = isize(1000000)
     elseif (test_case == '2') then
        iret = isize(-10)
     endif
  elseif (sub_name == 'iupm') then
     if (test_case == '1') then
        iret = iupm(char_8, 100)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'IUPM - NUMBER OF BITS BEING UNPACKED, NBITS' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'iupvs01') then
     if (test_case == '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        iret = iupvs01(11, 'LENM')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'IUPVS01 - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 10)
        iret = iupvs01(11, 'LENM')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'IUPVS01 - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 10)
        iret = iupvs01(11, 'LENM')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'IUPVS01 - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'jstnum') then
     if (test_case == '1') then
        char_val_8 = '        '
        call jstnum(char_val_8,char_1,iret)
     endif
  elseif (sub_name == 'lcmgdf') then
     open(unit = 11, file = 'testfiles/IN_4', form ='unformatted', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
       iret = lcmgdf(11, 'NC021206')
       call check_for_bort( errstr, errstr_len )
       if ( errstr_len > 0 .and. &
         index( errstr(1:errstr_len), 'LCMGDF - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN' ) /= 0 ) stop 88
       stop 0
     endif
  elseif (sub_name == 'lstjpb') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call readmg(11, char_val_8, jdate, iret)
        call lstjpb(-1, 1, 'DRP')
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call readmg(11, char_val_8, jdate, iret)
        call lstjpb(10000, 1, 'DRP')
     endif
  elseif (sub_name == 'minimg') then
     if (test_case == '1') then
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
     elseif (test_case == '3') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUX', 11)
     endif
     call minimg(11, 16)
     if (test_case == '1') then
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'MINIMG - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'MINIMG - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'MINIMG - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'msgwrt') then
     filnam = 'testfiles/IN_2'
     call cobfl_c( filnam, 'r' )
     open(unit = 31, file = '/dev/null')
     call openbf(31, 'INUL', 31)
     call crbmg_c(bfmg, mxmb, msgl4, iret4)
     if (test_case == '1') then
        ibit = 64
        call pkb(25, 24, ibfmg, ibit)
     elseif (test_case == '2') then
        ibit = 256
        call pkb(25, 24, ibfmg, ibit)
     elseif (test_case == '3') then
        ! Make it look like there's a Section 2 in the message.
        ibit = 120
        call pkb(1, 1, ibfmg, ibit)
        ibit = 256
        call pkb(3, 24, ibfmg, ibit)
     endif
     call msgwrt(31, ibfmg, 19926)
  elseif (sub_name == 'mtfnam') then
     if (test_case == '1') then
        call mtinfo('../tables', 80, 81)
        call mtfnam(999, 15, 7, 1, 'TableB', char_85, char_120)
     elseif (test_case == '2') then
        call mtinfo('.', 80, 81)
        call mtfnam(999, 15, 7, 1, 'TableB', char_85, char_120)
     endif
  elseif (sub_name == 'nemtba') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call nemtba(11, 'SPOCK', mtyp, msbt, inod)
     endif
  elseif (sub_name == 'nemtbax') then
     open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
       char_val_8 = 'NC337200'
     elseif (test_case == '2') then
       char_val_8 = 'NC007300'
     endif
     card = '| ' // char_val_8 // ' | A54124 | MTYPE TESTING                                            |'
     write (12,'(A)') card
     card = '| YEAR     | 004001 | YEAR                                                     |'
     write (12,'(A)') card
     card = '| NC337200 | YEAR                                                              |'
     card = '| ' // char_val_8 // ' | YEAR                                                              |'
     write (12,'(A)') card
     card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
     write (12,'(A)') card
     close (12)
     open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
     call openbf(11, 'OUT', 12)
     call openmg(11, char_val_8, 2024020112)
  elseif (sub_name == 'nemtbb') then
     open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       if (ios /= 0) stop 0
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
     elseif (test_case == '2') then
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       if (ios /= 0) stop 0
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
     elseif (test_case == '3') then
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       if (ios /= 0) stop 0
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
  elseif (sub_name == 'nemtbd') then
     open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       if (ios /= 0) stop 0
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
  elseif (sub_name == 'nenubd') then
     open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     call openbf(11, 'IN', 11)
     if (test_case == '1') then
        call nenubd('BPID    ','001008',1)
     elseif (test_case == '2') then
        call nenubd('BPID2   ','001005',1)
     elseif (test_case == '3') then
        call nenubd('LALOLV  ','301025',1)
     elseif (test_case == '4') then
        call nenubd('LALOLV2 ','301024',1)
     endif
  elseif (sub_name == 'nmsub') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        iret = nmsub(11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'NMSUB - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_7_bufrtab', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        iret = nmsub(11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'NMSUB - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        iret = nmsub(11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'NMSUB - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'nvnwin') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call readns(11, char_val_8, jdate, iret)
        call nvnwin(1717, 1, 25, 175, jdate1, 5)
     endif
  elseif (sub_name == 'openbf') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'BBB', 11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'OPENBF - ILLEGAL SECOND (INPUT) ARGUMENT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call openbf(11, 'IN', 11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'OPENBF - THE FILE CONNECTED TO UNIT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        do u = 1, 33
           open(unit = u+10, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
           if (ios /= 0) stop 0
           call openbf(u+10, 'IN', 11)
        end do
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'OPENBF - THERE ARE ALREADY 32 BUFR FILES OPENED' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'openmg') then
     open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
        call openbf(11, 'IN', 11)
        call openmg(11, 'F5FCMESG', 2021022312)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'OPENMG - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '2') then
        call openmg(11, 'F5FCMESG', 2021022312)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'OPENMG - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'openmb') then
     open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
        call openbf(11, 'IN', 11)
        call openmb(11, 'F5FCMESG', 2021022312)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'OPENMB - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '2') then
        call openmb(11, 'F5FCMESG', 2021022312)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'OPENMB - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'pad') then
     if (test_case == '1') then
        ibit = 16
        call pad(ibay, ibit, ierr, 27)
     endif
  elseif (sub_name == 'parstr') then
     if (test_case == '1') then
        call parstr(char_85, tags, 5, iret, ' ', .true.)
     elseif (test_case == '2') then
        card = 'MNEM1 MNEM2 MNEM3 MNEM4 MNEM5 MNEM6                                             '
        call parstr(card, tags, 5, iret, ' ', .true.)
     elseif (test_case == '3') then
        card = 'MNEM1MNEM2 MNEM3 MNEM4 MNEM5 MNEM6                                              '
        call parstr(card, tags, 5, iret, ' ', .true.)
     endif
  elseif (sub_name == 'parusr') then
     if (test_case == '6') then
        open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
     else if (test_case == '7') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     else
        open(unit = 11, file = 'testfiles/data/prepbufr', form = 'UNFORMATTED', iostat = ios)
     endif
     if (ios /= 0) stop 0
     if (test_case == '7') then
        open(unit = 12, file = 'testfiles/OUT_7_bufrtab', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call openmg(11, 'NC002104', 2024020112)
        call ufbint(11, real_2d, 1, 2, iret, 'RASCN>20')
     else
        call openbf(11, 'IN', 11)
        call readns(11, char_val_8, jdate, iret)
        if (iret /= 0) stop 0
     endif
     if (test_case == '1') then
        call parusr(char_85, 1, 1, 1)
     elseif (test_case == '2') then
        card = 'POB>0 QOB>0 TOB>0 VOB>0 UOB>0 XOB>0 YOB>0 ELV>0 TYP>0 T29>0 ITP>0               '
        call parusr(card, 1, 11, 0)
     elseif (test_case == '3') then
        card = 'POB QOB TOB VOB UOB XOB YOB ELV TYP T29 ITP A1 A2 A3 B1 B2 B3 S1 S2 S3 E1       '
        call parusr(card, 1, 21, 0)
     elseif (test_case == '4') then
        card = 'PRSLEVEL^0                                                                      '
        call parusr(card, 1, 11, 0)
     elseif (test_case == '5') then
        card = 'POB QOB TOB VOB UOB XOB YOB ELV TYP T29 ITP                                     '
        call parusr(card, 1, 10, 0)
     elseif (test_case == '6') then
        card = 'HGTSIG DCHSIG                                                                   '
        call parusr(card, 1, 2, 0)
     endif
  elseif (sub_name == 'parutg') then
     open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     open(unit = 10, file = 'testfiles/OUT_7_bufrtab', iostat = ios)
     if (ios /= 0) stop 0
     call openbf(12, 'OUT', 10)
     call openmg(12, 'NC002104', 2024020112)
     if (test_case == '1') then
        call ufbint(12, real_2d, 1, 2, iret, 'RATCN>20')
     elseif (test_case == '2') then
        call ufbint(12, real_2d, 1, 2, iret, 'RASCN>2t')
     elseif (test_case == '3') then
        call ufbint(12, real_2d, 1, 2, iret, 'UARLVB')
     endif
  elseif (sub_name == 'pkb') then
     if (test_case == '1') then
        call pkb(1, 65, ibay, ibit)
     endif
  elseif (sub_name == 'pkb8') then
     if (test_case == '1') then
        call pkb8(1, -1, ibay, ibit)
     elseif (test_case == '2') then
        call pkb8(1, 65, ibay, ibit)
     endif
  elseif (sub_name == 'pkbs1') then
     filnam = 'testfiles/IN_2'
     call cobfl_c( filnam, 'r' )
     call crbmg_c(bfmg, mxmb, msgl4, iret4)
     if (test_case == '1') then
        call pkbs1(88, ibfmg, 'DUMMY')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'PKBS1 - CANNOT OVERWRITE LOCATION CORRESPONDING TO MNEMONIC (DUMMY)' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'pkvs01') then
     if (test_case == '1') then
        if (isetprm('MXS01V',1) /= 0) stop 0
        call openbf(11, 'IN', 11)
        call pkvs01('OGCE', 88)
        call pkvs01('OGCE', 84) ! test the overwrite logic too
        call pkvs01('USN', 2)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'PKVS01 - CANNOT OVERWRITE MORE THAN  1 DIFFERENT LOCATION' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'posapx') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call posapx(11)
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call posapx(12)
     endif
  elseif (sub_name == 'rdmgsb') then
     open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
        call rdmgsb(11, 3, 1)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'RDMGSB - HIT END OF FILE BEFORE READING REQUESTED MESSAGE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        call rdmgsb(11, 1, 3)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'RDMGSB - ALL SUBSETS READ BEFORE READING REQ. SUBSET' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'rdmtbb') then
     open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
     if (ios /= 0) stop 0
     open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
        card = 'Table B STD |  0 | 38                                                           '
        write (11,'(A)') card
        card = ' 0-01-001 |  0 |     0 |   7 | Numeric   | WMOB   ; ; WMO block number          '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios /= 0) stop 0
        card = 'Table B LOC |  0 | 7 |  1                                                       '
        write (12,'(A)') card
        card = ' 001001 |  0 |     2 |  12 | Code table   | QCWS  ; ; Wind speed quality mark   '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
        if (ios /= 0) stop 0
        call rdmtbb(11, 12, 1, imt, imtv, iogce, iltv, iret, &
                    int_1d, char_4, char_12, char_4, char_24, char_8, char_4, char_120)
     endif
  elseif (sub_name == 'rdmtbd') then
     open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
     if (ios /= 0) stop 0
     open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
        card = 'Table D STD |  0 | 38                                                           '
        write (11,'(A)') card
        card = '   3-01-058 | UNTFROLD   ;     ; Universal lightning event                      '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios /= 0) stop 0
        card = 'Table D LOC |  0 | 7 |  1                                                       '
        write (12,'(A)') card
        card = '   3-01-058 | LOWRESSEQ   ;     ; Low-resolution data sequence                  '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
        if (ios /= 0) stop 0
        call rdmtbd(11, 12, 1, 5, imt, imtv, iogce, iltv, iret, &
                    int_1d, char_8, char_4, char_120, int_1d_2, int_2d, char_120_2)
     endif
  elseif (sub_name == 'rdmtbf') then
     open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
     if (ios /= 0) stop 0
     open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
        card = 'Table F STD |  0 | 35                                                           '
        write (11,'(A)') card
        card = '   0-02-002 | TIWM ; FLAG                                                       '
        write (11,'(A)') card
        close (11)
        open(unit = 11, file = 'testfiles/test_bort_master_std', iostat = ios)
        if (ios /= 0) stop 0
        card = 'Table F LOC |  0 | 7 |  1                                                       '
        write (12,'(A)') card
        card = '   002002 | NCDY3 ; CODE                                                        '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master_loc', iostat = ios)
        if (ios /= 0) stop 0
        call rdmtbf(11, 12)
     endif
  elseif (sub_name == 'rdusdx') then
     open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
        card = '| MY-MNEM  |        |                                                          |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     elseif (test_case == '2') then
        card = '| MYMNEM   | 405001 |                                                          |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     elseif (test_case == '3') then
        card = '| MYMNEM   | 0H5001 |                                                          |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     elseif (test_case == '4') then
        card = '| MYMNEM   | 065001 |                                                          |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     elseif (test_case == '5') then
        card = '| MYMNEM   | 005256 |                                                          |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     elseif (test_case == '6') then
        card = '| NC011004 | A63255 |                                                          |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     elseif (test_case == '7') then
        card = '| MYMNEM                                                                       |'
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
        call openbf(11, 'OUT', 12)
     endif
  elseif (sub_name == 'readerme') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 12)
        call readerme(int_1d, 12, char_val_8, jdate, iret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READERME - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call readerme(int_1d, 11, char_val_8, jdate, iret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READERME - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        filnam = 'testfiles/data/debufr_3'
        call cobfl_c( filnam, 'r' )
        open(unit = 31, file = '/dev/null')
        call openbf(31, 'INUL', 31)
        call crbmg_c(bfmg, mxmb, msgl4, iret4)
        bfmg(1) = 'C'
        call readerme(ibfmg, 31, char_val_8, jdate, iret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'BUFRLIB: READERME - FIRST 4 BYTES READ FROM RECORD NOT "BUFR"' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'readlc') then
     if (test_case == '7') then
        open(unit = 11, file = 'testfiles/OUT_1', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call readns(11, char_val_8, jdate, iret)
        call readlc(11, char_1, 'ACRN')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READLC - MNEMONIC ACRN           IS A CHARACTER STRING OF LENGTH  10' ) /= 0 ) stop 88
        stop 0
     endif
     open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     open(unit = 12, file = 'testfiles/IN_2_bufrtab', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
        call openbf(11, 'IN', 11)
        call readlc(12, char_val_8, char_val_8)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READLC - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 13, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(13, 'OUT', 12)
        call readlc(13, char_val_8, char_val_8)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READLC - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        call openbf(11, 'IN', 11)
        call readlc(11, char_val_8, char_val_8)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READLC - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '4') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call readlc(11, char_val_8, 'YEAR MNTH')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READLC - THERE CANNOT BE MORE THAN ONE MNEMONIC IN THE INPUT STRING' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '5') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call readlc(11, char_val_8, 'YEAR')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READLC - MNEMONIC YEAR           DOES NOT REPRESENT A CHARACTER ELEMENT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '6') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call readlc(11, char_1, 'BULTIM')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READLC - MNEMONIC BULTIM         IS A CHARACTER STRING OF LENGTH   6' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'readmg') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 12)
        call readmg(12, char_val_8, jdate, iret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READMG - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call readmg(11, char_val_8, jdate, iret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READMG - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     endif
  elseif (sub_name == 'rdmems') then
     open(unit = 11, file = 'testfiles/IN_6_infile1', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     call ufbmem(11, 0, iret, iunit)
     if (test_case == '1') then
        call rdmems(11, jret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'RDMEMS - A MEMORY MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'readns') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 12)
        call readns(12, char_val_8, jdate, iret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READNS - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call readns(11, char_val_8, jdate, iret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READNS - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     endif
  elseif (sub_name == 'readsb') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call readsb(11, iret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READSB - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 10)
        call readsb(11, iret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'READSB - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'reads3') then
     if (test_case == '1') then
        if (isetprm('MXCNEM',1) /= 0) stop 0
        open(unit = 31, file = '/dev/null')
        call openbf(31, 'SEC3', 31)
        call mtinfo('../tables', 80, 81)
        filnam = 'testfiles/IN_1'
        call cobfl_c( filnam, 'r' )
        call crbmg_c(bfmg, mxmb, msgl4, iret4)
        call readerme(ibfmg, 31, char_val_8, jdate, iret)
        filnam = 'testfiles/IN_4'
        call cobfl_c( filnam, 'r' )
        call crbmg_c(bfmg, mxmb, msgl4, iret4)
        ! Make it look like the message uses version 14 of the WMO master tables.
        ibit = 168
        call pkb(14, 8, ibfmg, ibit)
        call readerme(ibfmg, 31, char_val_8, jdate, iret)
     endif
  elseif (sub_name == 'rewnbf') then
     open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     open(unit = 12, file = 'testfiles/IN_2_bufrtab', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call rewnbf(11, 4)
     elseif (test_case == '2') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call rewnbf(11, 0)
        call rewnbf(11, 0)
     elseif (test_case == '3') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call rewnbf(11, 0)
        call rewnbf(11, 1)
        call rewnbf(11, 1)
     elseif (test_case == '4') then
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        call rewnbf(11, 1)
     elseif (test_case == '5') then
        call openbf(11, 'FIRST', 12)
        call rewnbf(11, 0)
     endif
  elseif (sub_name == 'rtrcpt') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call rtrcpt(11, iyr, imo, idy, ihr, imi, iret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'RTRCPT - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE; NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call rtrcpt(11, iyr, imo, idy, ihr, imi, iret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'RTRCPT - INPUT BUFR FILE IS OPEN FOR OUTPUT; IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 10)
        call rtrcpt(11, iyr, imo, idy, ihr, imi, iret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'RTRCPT - INPUT BUFR FILE IS CLOSED; IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'seqsdx') then
     open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     if ((test_case == '14') .or. (test_case == '15')) then
        if (isetprm('MAXCD',22) /= 0) stop 0
        open(unit = 12, file = 'testfiles/OUT_6_bufrtab', iostat = ios)
     else
        open(unit = 12, file = 'testfiles/OUT_2_bufrtab', iostat = ios)
     endif
     if (ios /= 0) stop 0
     call openbf(11, 'OUT', 12)
     if (test_case == '1') then
        card = '| DUMMYD   |                                                                   |'
        call seqsdx(card, 1)
     elseif (test_case == '2') then
        card = '| DRPSTAK  |                                                                   |'
        call seqsdx(card, 1)
     elseif (test_case == '3') then
        card = '| DRPSTAK  | <YYMMDD                                                           |'
        call seqsdx(card, 1)
     elseif (test_case == '4') then
        card = '| DRPSTAK  | "YYMMDD"0                                                         |'
        call seqsdx(card, 1)
     elseif (test_case == '5') then
        card = '| DRPSTAK  | "YYMMDD"256                                                       |'
        call seqsdx(card, 1)
     elseif (test_case == '6') then
        card = '| DRPSTAK  | {YYMMDD}10                                                        |'
        call seqsdx(card, 1)
     elseif (test_case == '7') then
        card = '| DRPSTAK  | DUMMYBMNEM                                                        |'
        call seqsdx(card, 1)
     elseif (test_case == '8') then
        card = '| DRPSTAK  | WNDSQ-1                                                           |'
        call seqsdx(card, 1)
     elseif (test_case == '9') then
        card = '| DRPSTAK  | {FOST}                                                            |'
        call seqsdx(card, 1)
     elseif (test_case == '10') then
        card = '| DRPSTAK  | .DTMMXGS TMDB                                                     |'
        call seqsdx(card, 1)
     elseif (test_case == '11') then
        card = '| DRPSTAK  | .DTMMXGS                                                          |'
        call seqsdx(card, 1)
     elseif (test_case == '12') then
        card = '| DRPSTAK  | .DTMMXGG MXGG                                                     |'
        call seqsdx(card, 1)
     elseif (test_case == '13') then
        card = '| DRPSTAK  | DUMMYB                                                            |'
        call seqsdx(card, 1)
     elseif (test_case == '14') then
        card = '| F5FCMESG | "F5FCRSEQ"3                                                       |'
        call seqsdx(card, 1)
     elseif (test_case == '15') then
        card = '| F5FCMESG | DBSS                                                              |'
        call seqsdx(card, 1)
     endif
  elseif (sub_name == 'status') then
     if (test_case == '1') then
        call status(0, lun, il, im)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'STATUS - INPUT UNIT NUMBER (  0) OUTSIDE LEGAL RANGE OF 1-99' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        call status(100, lun, il, im)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'STATUS - INPUT UNIT NUMBER (100) OUTSIDE LEGAL RANGE OF 1-99' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'stseq') then
     filnam = 'testfiles/IN_1'
     call cobfl_c( filnam, 'r' )
     open(unit = 31, file = '/dev/null')
     if (test_case == '1') then
        if (isetprm('MXNAF',1) /= 0) stop 0
        call openbf(31, 'SEC3', 31)
        call crbmg_c(bfmg, mxmb, msgl4, iret4)
        ! Make Section 3 of the message look like it contains two consecutive occurrences of descriptor 3-03-021.
        ibit = 296
        call pkb(195, 8, ibfmg, ibit)
        call pkb(21, 8, ibfmg, ibit)
        call pkb(195, 8, ibfmg, ibit)
        call pkb(21, 8, ibfmg, ibit)
     elseif (test_case == '2') then
        call openbf(31, 'SEC3', 31)
        call crbmg_c(bfmg, mxmb, msgl4, iret4)
        ! Make Section 3 of the message look like it contains one occurrence of descriptor 3-03-021 followed
        ! by two occurrences of descriptor 2-04-000.
        ibit = 296
        call pkb(195, 8, ibfmg, ibit)
        call pkb(21, 8, ibfmg, ibit)
        ibit = 360
        call pkb(132, 8, ibfmg, ibit)
        call pkb(0, 8, ibfmg, ibit)
        call pkb(132, 8, ibfmg, ibit)
        call pkb(0, 8, ibfmg, ibit)
     elseif (test_case == '3') then
        call openbf(31, 'SEC3', 31)
        call crbmg_c(bfmg, mxmb, msgl4, iret4)
        ! Make Section 3 of the message look like it contains an occurrence of replication descriptor 1-03-000
        ! without a following delayed descriptor replication factor.
        ibit = 296
        call pkb(67, 8, ibfmg, ibit)
        call pkb(0, 8, ibfmg, ibit)
     endif
     call mtinfo('../tables', 80, 81)
     call readerme(ibfmg, 31, char_val_8, jdate, iret)
  elseif (sub_name == 'sntbbe') then
     if (test_case == '1') then
        call sntbbe(0, 'c', 1, 2, int_1d, char_4, char_12, char_4, char_24, char_8, char_4, char_120)
     elseif (test_case == '2') then
        card = '  0-00-007 |   0 |                                                              '
     elseif (test_case == '3') then
        card = '  0-00-007 |     |           0 |  16 | CCITT IA5         | CMTVN    ;     ;     '
     elseif (test_case == '4') then
        card = '  0-00-007 |   0 |             |  16 | CCITT IA5         | CMTVN    ;     ;     '
     elseif (test_case == '5') then
        card = '  0-00-007 |   0 |           0 |     | CCITT IA5         | CMTVN    ;     ;     '
     elseif (test_case == '6') then
        card = '  0-00-007 |   0 |           0 |  16 |                   | CMTV$    ;     ;     '
     endif
     jret = 0
     call sntbbe(7, card, 1, jret, int_1d, char_4, char_12, char_4, char_24, char_8, char_4, char_120)
  elseif (sub_name == 'sntbde') then
     card = '  3-01-022 | LTLONHHT   ;     ;                                                 '
     if (test_case == '1') then
        call sntbde(0, 0, 'c', 1, 1, 2, int_1d, char_8, char_4, char_120, int_1d, int_1d, char_120)
     elseif (test_case == '2') then
        card(21:21) = '$'
     elseif (test_case == '3') then
        open(unit = 12, file = '/dev/null')
     elseif (test_case == '4') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
        char_85 = '             0-05-001                                                                '
        write (12,'(A)') char_85
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
     elseif (test_case == '5') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
        char_85 = '           | 0-05-001 >                                                              '
        write (12,'(A)') char_85
        char_85 = '           | 0-06-300                                                                '
        write (12,'(A)') char_85
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
     endif
     jret = 0
     call openbf(12, 'FIRST', 12)
     call sntbde(12, 49430, card, 1, 1, jret, int_1d, char_8, char_4, char_120, int_1d, int_1d, char_120)
  elseif (sub_name == 'sntbfe') then
     if (test_case == '1') then
        open(unit = 12, file = '/dev/null')
     elseif (test_case == '2') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
        card = '            0-01-031,0-01-033,0-01-035=176                                      '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
        card = '          | 0-01-031,0-01-033,0-01-035 176                                      '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
     elseif (test_case == '4') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
        card = '          |                           =176                                      '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
     elseif (test_case == '5') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
        card = '          | 0-01-331,0-01-033,0-01-035=176                                      '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
     elseif (test_case == '6') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
        card = '          | 0-01-031,0-01-033,0-01-035=                                         '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
     elseif (test_case == '7') then
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
        card = '          | 0-01-031,0-01-033,0-01-035=17T                                      '
        write (12,'(A)') card
        close (12)
        open(unit = 12, file = 'testfiles/test_bort_master', iostat = ios)
        if (ios /= 0) stop 0
     endif
     call openbf(12, 'FIRST', 12)
     call sntbfe(12, 288)
  elseif (sub_name == 'stdmsg') then
     if (test_case == '1') then
        call stdmsg('W')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'STDMSG - INPUT ARGUMENT IS W, IT MUST BE EITHER Y, y, N OR n' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'stndrd') then
     filnam = 'testfiles/IN_11'
     call cobfl_c ( filnam, 'r' )
     call crbmg_c ( bfmg, mxmb, msgl4, iret4 )
     if ( iret4 /= 0 ) stop 0
     call ccbfl_c ()
     open ( unit = 21, file = filnam, form = 'unformatted', iostat = ios )
     if (ios /= 0) stop 0
     open ( unit = 22, file = 'testfiles/IN_11_bufrtab', iostat = ios )
     if (ios /= 0) stop 0
     call openbf ( 21, 'IN', 22 )
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call stndrd(12, int_1d, 1, int_1d_2)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'STNDRD - BUFR FILE IS CLOSED, IT MUST BE OPEN' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        bfmg(7) = '3'
        call stndrd ( 21, ibfmg, mxmbd4, ibfmg2 )
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'STNDRD - INPUT MESSAGE LENGTH FROM SECTION 01' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        bfmg(188210) = '8'
        call stndrd ( 21, ibfmg, mxmbd4, ibfmg2 )
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'STNDRD - INPUT MESSAGE DOES NOT END WITH "7777"' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '4') then
        bfmg(46) = '8'
        call stndrd ( 21, ibfmg, mxmbd4, ibfmg2 )
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'STNDRD - TABLE A SUBSET DESCRIPTOR NOT FOUND' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '5') then
        bfmg(17468) = 'z'
        bfmg(17469) = 'z'
        bfmg(17470) = 'z'
        call stndrd ( 21, ibfmg, mxmbd4, ibfmg2 )
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'STNDRD - BIT MISMATCH COPYING SECTION 4 FROM INPUT TO OUTPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '6') then
        call stndrd ( 21, ibfmg, 5000, ibfmg2 )
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'STNDRD - OVERFLOW OF OUTPUT (STANDARD) MESSAGE ARRAY' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'stntbia') then
     open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '1') then
       card = '| NC007200 | A54124 | MTYPE 007-200                                            |'
       write (12,'(A)') card
       card = '| NC008200 | A54124 | MTYPE 008-200                                            |'
       write (12,'(A)') card
     elseif (test_case == '2') then
       card = '| NC007200 | A54124 | MTYPE 007-200                                            |'
       write (12,'(A)') card
       card = '| NC007200 | A54125 | MTYPE 007-200                                            |'
       write (12,'(A)') card
     endif
     close (12)
     open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
     if (ios /= 0) stop 0
     call openbf(11, 'OUT', 12)
  elseif (sub_name == 'strtbfe') then
     if (test_case == '1') then
       if (isetprm('MXMTBF',100) /= 0) stop 0
       open(unit = 11, file = 'testfiles/IN_4', iostat = ios)
       call openbf(11, 'SEC3', 11)
       call mtinfo('../tables', 80, 81)
       call codflg('Y')
       call readns(11, char_val_8, jdate, iret)
     endif
  elseif (sub_name == 'strbtm') then
     if (test_case == '1') then
       if (isetprm('MXBTMSE',8) /= 0) stop 0
       open(unit = 11, file = 'testfiles/IN_4', iostat = ios)
       call openbf(11, 'SEC3', 11)
       call mtinfo('../tables', 80, 81)
       call readns(11, char_val_8, jdate, iret)
     endif
  elseif (sub_name == 'strcpt') then
     if (test_case == '1') then
        call strcpt('W', 1960, 12, 15, 12, 0)
     endif
  elseif (sub_name == 'string') then
     open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     call openbf(11, 'IN', 11)
     if (test_case == '1') then
       call string('012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789', 1, 1, 0)
     elseif (test_case == '2') then
       call readns(11, char_val_8, jdate, iret)
       call ufbint(11, real_2d_3x1, 3, 1, iret, 'YEAR MNTH DAYS')
       call string('YEAR MNTH DAYS', 1, 2, 0)
     endif
  elseif (sub_name == 'tabent') then
     if (test_case == '1') then
       if (isetprm('MXNRV',1) /= 0) stop 0
       open(unit = 11, file = 'testfiles/IN_7', form = 'UNFORMATTED', iostat = ios)
       if (ios /= 0) stop 0
       open(unit = 12, file = 'testfiles/IN_7_bufrtab', iostat = ios)
       if (ios /= 0) stop 0
       call openbf(11, 'IN', 12)
       open(unit = 13, file = 'testfiles/OUT_1', form = 'UNFORMATTED', iostat = ios)
       if (ios /= 0) stop 0
       call openbf(13, 'IN', 13)
     endif
  elseif (sub_name == 'tabsub') then
     if (test_case == '11') then
       if (isetprm('MXTCO',3) /= 0) stop 0
       open(unit = 11, file = 'testfiles/IN_4', iostat = ios)
       call openbf(11, 'SEC3', 11)
       call mtinfo('../tables', 80, 81)
       call readns(11, char_val_8, jdate, iret)
     elseif (test_case == '12') then
       if (isetprm('MXTAMC',1) /= 0) stop 0
       open(unit = 11, file = 'testfiles/IN_4', iostat = ios)
       call openbf(11, 'SEC3', 11)
       call mtinfo('../tables', 80, 81)
       call readns(11, char_val_8, jdate, iret)
       open(unit = 13, file = 'testfiles/OUT_3', iostat = ios)
       call openbf(13, 'IN', 13)
     else
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       if (ios /= 0) stop 0
       card = '| NC007200 | A54124 | MTYPE 007-200                                            |'
       write (12,'(A)') card
       if (test_case == '1') then
       elseif (test_case == '2') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 201129 YEAR                                                       |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case == '3') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 202129 YEAR                                                       |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case == '4') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 207002 YEAR                                                       |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case == '5') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 208024 YEAR                                                       |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case == '6') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 201129 201133 YEAR                                                |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case == '7') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 202129 202133 YEAR                                                |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case == '8') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 203012 YEAR 203006                                                |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case == '9') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 203012 YEAR                                                       |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case == '10') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 203000 YEAR                                                       |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case == '13') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 201129 207002 YEAR                                                |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       elseif (test_case == '14') then
         card = '| YEAR     | 004001 | YEAR                                                     |'
         write (12,'(A)') card
         card = '| NC007200 | 202129 207001 YEAR                                                |'
         write (12,'(A)') card
         card = '| YEAR     |    0 |           0 |  12 | YEAR                     |-------------|'
         write (12,'(A)') card
       endif
       close (12)
       open(unit = 12, file = 'testfiles/test_bort_DX', iostat = ios)
       call openbf(11, 'OUT', 12)
     endif
  elseif (sub_name == 'ufbcnt') then
     if (test_case == '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbcnt(11, kmsg, ksub)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCNT - BUFR FILE IS CLOSED, IT MUST BE OPEN FOR EITHER INPUT OR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbcpy') then
     if (test_case == '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbcpy(11, 0)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCPY - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 10)
        call ufbcpy(12, 0)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCPY - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 10)
        call ufbcpy(12, 0)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCPY - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '4') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call readmg(11, char_val_8, jdate, iret)
        call ufbcpy(11, 12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCPY - LOCATION OF INTERNAL TABLE FOR INPUT BUFR FILE DOES NOT AGREE' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '5') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2_bufrtab', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        open(unit = 13, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbcpy(11, 13)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCPY - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '6') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2_bufrtab', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        open(unit = 13, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(13, 'IN', 12)
        call ufbcpy(11, 13)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCPY - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '7') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2_bufrtab', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        open(unit = 13, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(13, 'OUT', 12)
        call ufbcpy(11, 13)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCPY - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '8') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2_bufrtab', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 12)
        call readns(11, char_val_8, jdate, iret)
        open(unit = 13, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 14, file = 'testfiles/IN_7_bufrtab', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(13, 'OUT', 14)
        call openmb(13, 'NC008023', 2021022312)
        call ufbcpy(11, 13)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCPY - INPUT AND OUTPUT BUFR FILES MUST HAVE THE SAME INTERNAL TABLES' ) /= 0 ) &
          stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbcup') then
     if (test_case == '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbcup(11, 0)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCUP - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 10)
        call ufbcup(12, 0)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCUP - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 10)
        call ufbcup(12, 0)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCUP - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '4') then
        open(unit = 11, file = 'testfiles/IN_9', form ='unformatted')
        call openbf(11, 'IN', 11)
        open(unit = 12, file = 'test_ufbcup_out', form ='unformatted')
        call openbf(12, 'IN', 11)
        call readmg(11, char_val_8, jdate, iret)
        call ufbcup(11, 12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCUP - LOCATION OF INTERNAL TABLE FOR INPUT BUFR FILE DOES NOT AGREE' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '5') then
        open(unit = 11, file = 'testfiles/IN_9', form ='unformatted')
        call openbf(11, 'IN', 11)
        open(unit = 12, file = 'test_ufbcup_out', form ='unformatted')
        call readmg(11, char_val_8, jdate, iret)
        call readsb(11, iret)
        call ufbcup(11, 12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCUP - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '6') then
        open(unit = 11, file = 'testfiles/IN_9', form ='unformatted')
        call openbf(11, 'IN', 11)
        open(unit = 12, file = 'test_ufbcup_out', form ='unformatted')
        call openbf(12, 'IN', 11)
        call readmg(11, char_val_8, jdate, iret)
        call readsb(11, iret)
        call ufbcup(11, 12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCUP - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '7') then
        open(unit = 11, file = 'testfiles/IN_9', form ='unformatted')
        call openbf(11, 'IN', 11)
        open(unit = 12, file = 'test_ufbcup_out', form ='unformatted')
        call openbf(12, 'OUT', 11)
        call readmg(11, char_val_8, jdate, iret)
        call readsb(11, iret)
        call ufbcup(11, 12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBCUP - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbdmp') then
     if (test_case == '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbdmp(11, 0)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBDMP - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 10)
        call ufbdmp(12, 0)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBDMP - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 10)
        call ufbdmp(12, 0)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBDMP - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbevn') then
     if (test_case == '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbevn(11, real_2d, 1, 2, 3, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBEVN - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 10)
        call ufbevn(12, real_2d, 1, 2, 3, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBEVN - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 10)
        call ufbevn(12, real_2d, 1, 2, 3, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBEVN - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbget') then
     if (test_case == '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbget(11, real_1d, 1, iret, 's')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBGET - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 10)
        call ufbget(12, real_1d, 1, iret, 's')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBGET - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 10)
        call ufbget(12, real_1d, 1, iret, 's')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBGET - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbint') then
     if (test_case == '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbint(11, real_2d, 1, 2, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBINT - BUFR FILE IS CLOSED, IT MUST BE OPEN' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 10)
        call ufbint(12, real_2d, 1, 2, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBINT - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbinx') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call ufbinx(11, 999, 999, real_2d, 1, 2, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBINX - HIT END OF FILE BEFORE READING REQUESTED MESSAGE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call ufbinx(11, 1, 999, real_2d, 1, 2, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBINX - ALL SUBSETS READ BEFORE READING REQ. SUBSET' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbmms') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbmem(11, 0, iret, iunit)
        if (iret /= 5 .or. iunit /= 11) stop 0
        call ufbmms(1, 999, char_val_8, jdate)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBMMS - REQ. SUBSET NUMBER TO READ IN (999) EXCEEDS THE NUMBER OF SUBSETS' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbmem(11, 0, iret, iunit)
        if (iret /= 5 .or. iunit /= 11) stop 0
        call ufbmms(0, 999, char_val_8, jdate)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBMMS - REQUESTED MEMORY MESSAGE NUMBER TO READ IN IS ZERO' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbmem(11, 0, iret, iunit)
        if (iret /= 5 .or. iunit /= 11) stop 0
        call ufbmms(999, 999, char_val_8, jdate)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBMMS - REQUESTED MEMORY MESSAGE NUMBER TO READ IN (  999) EXCEEDS' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbmns') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbmem(11, 0, iret, iunit)
        if (iret /= 5 .or. iunit /= 11) stop 0
        call ufbmns(9999, char_val_8, jdate)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBMNS - REQ. SUBSET NO. TO READ IN ( 9999) EXCEEDS TOTAL NO. OF SUBSETS' ) /= 0 ) &
          stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbovr') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call ufbovr(11, real_2d, 1, 1, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBOVR - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 10)
        call ufbovr(11, real_2d, 1, 1, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBOVR - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbovr(11, real_2d, 1, 1, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBOVR - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbpos') then
     if (test_case == '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbpos(11, 1, 1, char_val_8, jdate)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBPOS - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 10)
        call ufbpos(11, 1, 1, char_val_8, jdate)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBPOS - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 10)
        call ufbpos(11, 0, 1, char_val_8, jdate)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBPOS - REQUESTED MESSAGE NUMBER TO READ IN (    0) IS NOT VALID' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '4') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 10)
        call ufbpos(11, 999, 1, char_val_8, jdate)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBPOS - REQUESTED MESSAGE NUMBER TO READ IN (  999) EXCEEDS' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '5') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 10)
        call ufbpos(11, 1, 9999, char_val_8, jdate)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBPOS - REQ. SUBSET NUMBER TO READ IN ( 9999) EXCEEDS' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '6') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 10)
        call ufbpos(11, 1, 0, char_val_8, jdate)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBPOS - REQUESTED SUBSET NUMBER TO READ IN (    0) IS NOT VALID' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbqcd') then
     if (test_case == '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbqcd(11, 'c', iqcd)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBQCD - BUFR FILE IS CLOSED, IT MUST BE OPEN' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 10)
        call ufbqcd(11, 'c', iqcd)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBQCD - INPUT MNEMONIC c NOT DEFINED AS A SEQUENCE DESCRIPTOR' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 11, file = 'testfiles/IN_5', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call readns(11, char_val_8, jdate, iret)
        call ufbqcd(11, 'ADPUPA', iqcd)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBQCD - BUFR TABLE SEQ. DESCRIPTOR ASSOC. WITH INPUT MNEMONIC' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbqcp') then
     if (test_case == '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbqcp(11, 0, char_val_8)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBQCP - BUFR FILE IS CLOSED, IT MUST BE OPEN' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbrep') then
     if (test_case == '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbrep(11, real_2d, 1, 2, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBREP - BUFR FILE IS CLOSED, IT MUST BE OPEN' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 10)
        call ufbrep(12, real_2d, 1, 2, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBREP - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 11, file = 'testfiles/IN_7_bufrtab', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 11)
        call openmb(12, 'NC008023', 2021022312)
        call ufbrep(12, real_2d, 1, 2, iret, 'TOST')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBREP - MNEMONIC STRING READ IN IS: TOST' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbrms') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbmem(11, 0, iret, iunit)
        if (iret /= 5 .or. iunit /= 11) stop 0
        call ufbrms(999, 1, real_2d, 1, 1, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBRMS - REQUESTED MEMORY MESSAGE NUMBER TO READ IN (  999) EXCEEDS' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbmem(11, 0, iret, iunit)
        if (iret /= 5 .or. iunit /= 11) stop 0
        call ufbrms(1, 9999, real_2d, 1, 1, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBRMS - REQ. SUBSET NUMBER TO READ IN (***) EXCEEDS' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbmem(11, 0, iret, iunit)
        if (iret /= 5 .or. iunit /= 11) stop 0
        call ufbrms(0, 9999, real_2d, 1, 1, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBRMS - REQUESTED MEMORY MESSAGE NUMBER TO READ IN IS ZERO' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbstp') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call ufbstp(11, real_2d, 1, 1, iret, 'LALALA')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBSTP - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call ufbstp(11, real_2d, 1, 1, iret, 'LALALA')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBSTP - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 10)
        call ufbstp(11, real_2d, 1, 1, iret, 'LALAL1')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBSTP - BUFR FILE IS CLOSED, IT MUST BE OPEN' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '4') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 11, file = 'testfiles/IN_7_bufrtab', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 11)
        call openmb(12, 'NC008023', 2021022312)
        call ufbstp(12, real_2d, 1, 2, iret, 'TOST')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBSTP - MNEMONIC STRING READ IN IS: TOST' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufbseq') then
     if (test_case == '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call ufbseq(11, real_2d, 1, 1, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBSEQ - BUFR FILE IS CLOSED, IT MUST BE OPEN' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 10)
        call ufbseq(12, real_2d, 1, 1, iret, 'c')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBSEQ - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/IN_6_infile2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 12)
        call readns(12, char_val_8, jdate, iret)
        if (iret /= 0) stop 0
        call ufbseq(12, real_2d, 1, 1, iret, ' ')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBSEQ - THE INPUT STRING ( ) DOES NOT CONTAIN ANY MNEMONICS' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '4') then
        open(unit = 12, file = 'testfiles/IN_6_infile2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 12)
        call readns(12, char_val_8, jdate, iret)
        if (iret /= 0) stop 0
        call ufbseq(12, real_2d, 1, 1, iret, 'YEAR MNTH')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBSEQ - THERE CANNOT BE MORE THAN ONE MNEMONIC IN THE INPUT STRING' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '5') then
        open(unit = 12, file = 'testfiles/IN_6_infile2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 12)
        call readns(12, char_val_8, jdate, iret)
        if (iret /= 0) stop 0
        call ufbseq(12, real_2d, 1, 1, iret, 'YEAR')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBSEQ - INPUT MNEMONIC YEAR       MUST BE A SEQUENCE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '6') then
        open(unit = 12, file = 'testfiles/IN_6_infile2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 12)
        call readns(12, char_val_8, jdate, iret)
        if (iret /= 0) stop 0
        call ufbseq(12, real_2d, 1, 1, iret, 'UARID')
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFBSEQ - INPUT SEQ. MNEM. UARID      CONSISTS OF   5 TABLE B MNEM' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'ufdump') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call ufdump(11, 11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFDUMP - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call ufdump(11, 12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFDUMP - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 10)
        call ufdump(11, 12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UFDUMP - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'upb8') then
     if (test_case == '1') then
        call upb8(nval, -1, ibit, ibay)
     endif
  elseif (sub_name == 'upds3') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_1', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        if (isetprm('MAXNC',6) /= 0) stop 0
        call openbf(11, 'SEC3', 11)
        call mtinfo('../tables', 80, 81)
        call readmg(11, char_val_8, jdate, iret)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UPDS3 - OVERFLOW OF OUTPUT DESCRIPTOR ARRAY' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'upftbv') then
     open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     real_1d(1) = 1.0
     if (test_case == '1') then
        call openbf(11, 'IN', 11)
        call upftbv(11, 'n', real_1d(1), 20, irps, ierr)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UPFTBV - MNEMONIC n NOT FOUND IN TABLE B' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '2') then
        call openbf(12, 'FIRST', 11)
        call upftbv(11, 'n', real_1d(1), 20, irps, ierr)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UPFTBV - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/IN_2_bufrtab')
        call openbf(11, 'IN', 12)
        call upftbv(11, 'SSNX', real_1d(1), 20, irps, ierr)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UPFTBV - MNEMONIC SSNX IS NOT A FLAG TABLE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '4') then
        open(unit = 12, file = 'testfiles/IN_2_bufrtab')
        call openbf(11, 'IN', 12)
        real_1d(1) = 4194314.0
        call upftbv(11, 'SIDP', real_1d(1), 1, int_1d, ierr)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'UPFTBV - IBIT ARRAY OVERFLOW' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'uptdd') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/OUT_5_infile', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call uptdd(65, 1, 20, iret)
     endif
  elseif (sub_name == 'usrtpl') then
     open(unit = 11, file = 'testfiles/OUT_5_infile', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     call openbf(11, 'IN', 11)
     call readns(11, char_val_8, jdate, iret)
     if (test_case == '1') then
        call usrtpl(1, 1, 2)
     elseif (test_case == '2') then
        call usrtpl(1, 53, 2)
     elseif (test_case == '3') then
        call usrtpl(1, 51, 1)
     elseif ((test_case == '4') .or. (test_case == '5')) then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        call openbf(12, 'OUT', 11)
        if (test_case == '4') then
          char_val_8 = 'MAXJL   '
        else
          char_val_8 = 'MAXSS   '
        endif
        if (isetprm(char_val_8,10) /= 0) stop 0
        call openmg(12, 'NC001103', 2021022312)
     endif
  elseif (sub_name == 'wrcmps') then
     open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
     if (ios /= 0) stop 0
     open(unit = 12, file = 'testfiles/IN_2_bufrtab', iostat = ios)
     if (ios /= 0) stop 0
     if (test_case == '2') then
       if (isetprm('MXCDV',20) /= 0) stop 0
     endif
     call openbf(11, 'IN', 12)
     open(unit = 13, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
     call openbf(13, 'OUT', 12)
     if (ios /= 0) stop 0
     call readns(11, char_val_8, jdate, iret)
     call openmb(13, char_val_8, jdate)
     call ufbcpy(11, 13)
     if (test_case == '1') then
        call wrcmps(13)
        open(unit = 14, file = 'testfiles/test_bort_OUT_2', form = 'UNFORMATTED', iostat = ios)
        call openbf(14, 'OUT', 12)
        call openmb(14, char_val_8, jdate)
        call ufbcpy(11, 14)
        call wrcmps(14)
     elseif (test_case == '2') then
        call wrcmps(13)
     elseif (test_case == '3') then
        call maxout(200)
        call wrcmps(13)
     endif
  elseif (sub_name == 'wrdesc') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        if (isetprm('MAXNC',20) /= 0) stop 0
        call openbf(11, 'IN', 11)
        call openbf(12, 'OUT', 11)
        call readmg(11, char_val_8, jdate, iret)
        call stdmsg('Y')
        call copymg(11, 12)
     endif
  elseif (sub_name == 'wrdxtb') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call wrdxtb(11, 11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'WRDXTB - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call wrdxtb(11, 12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'WRDXTB - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 12, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'OUT', 10)
        call wrdxtb(11, 12)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'WRDXTB - DX TABLE FILE IS CLOSED, IT MUST BE OPEN' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'writdx') then
     if (test_case == '1') then
        call writdx(0, 0, 0)
     endif
  elseif (sub_name == 'writlc') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call writlc(11, char_val_8, char_val_8)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'WRITLC - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 12)
        call writlc(11, char_val_8, char_val_8)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'WRITLC - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     else
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2_bufrtab', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        if (test_case == '2') then
          call writlc(11, char_val_8, char_val_8)
          call check_for_bort( errstr, errstr_len )
          if ( errstr_len > 0 .and. &
            index( errstr(1:errstr_len), 'WRITLC - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
          stop 0
        elseif (test_case == '4') then
          call openmb(11, 'NC005065', 20240512)
          call writlc(11, char_val_8, 'BORG BBB')
          call check_for_bort( errstr, errstr_len )
          if ( errstr_len > 0 .and. &
            index( errstr(1:errstr_len), 'WRITLC - THERE CANNOT BE MORE THAN  ONE MNEMONIC IN THE INPUT STRING' ) /= 0 ) &
            stop 88
          stop 0
        elseif (test_case == '5') then
          call openmb(11, 'NC005065', 20240512)
          call writsb(11)
          call writlc(11, char_val_8, 'SAID')
          call check_for_bort( errstr, errstr_len )
          if ( errstr_len > 0 .and. &
            index( errstr(1:errstr_len), 'WRITLC - MNEMONIC SAID       DOES NOT REPRESENT A CHARACTER ELEMENT' ) /= 0 ) &
            stop 88
          stop 0
        elseif (test_case == '6') then
          call openmb(11, 'NC005065', 20240512)
          call writcp(11)
          call writlc(11, char_val_8, 'SAID')
          call check_for_bort( errstr, errstr_len )
          if ( errstr_len > 0 .and. &
            index( errstr(1:errstr_len), 'WRITLC - MNEMONIC SAID       DOES NOT REPRESENT A CHARACTER ELEMENT' ) /= 0 ) &
            stop 88
          stop 0
        endif
     endif
  elseif (sub_name == 'writsa') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call writsa(11, lmsgt, msgt, msgl)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'WRITSA - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call writsa(11, lmsgt, msgt, msgl)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'WRITSA - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 12)
        call writsa(11, lmsgt, msgt, msgl)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'WRITSA - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'writsb') then
     if (test_case == '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'IN', 11)
        call writsb(11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'WRITSB - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) &
          stop 88
        stop 0
     elseif (test_case == '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(11, 'OUT', 12)
        call writsb(11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'WRITSB - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE' ) /= 0 ) stop 88
        stop 0
     elseif (test_case == '3') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 12)
        call writsb(11)
        call check_for_bort( errstr, errstr_len )
        if ( errstr_len > 0 .and. &
          index( errstr(1:errstr_len), 'WRITSB - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT' ) /= 0 ) stop 88
        stop 0
     endif
  elseif (sub_name == 'wtstat') then
     if (test_case == '1') then
        call wtstat(0, 0, 0, 0)
     elseif (test_case == '2') then
        call wtstat(1, 0, 0, 0)
     elseif (test_case == '3') then
        call wtstat(1, 1, -2, 0)
     elseif (test_case == '4') then
        call wtstat(1, 1, 0, -1)
     elseif (test_case == '5') then
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios /= 0) stop 0
        call openbf(12, 'IN', 12)
        call wtstat(2, 1, 0, 1)
     endif
  else
     print *, "Unknown test function"
     ! Return with 0 to fail the test.
     stop 0
  endif

end program test_bort
