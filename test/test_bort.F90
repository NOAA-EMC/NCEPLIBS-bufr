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
  implicit none
  integer iret, jret, iunit, iqcd
  ! integer i1
  integer int_1d(1), int_1d_2(1)
  character*2 char_short
  character*30 char_30
  character*4 char_4(1)
  character*8 char_8(1), char_val_8
  character*12 char_12(1)
  character*24 char_24(1)
  character*120 char_120(1)
  character*5 adn30_val_5
  real*8 real_1d(1)
  real*8 real_2d(1,1)
  integer idn30, idn30_val
  integer :: num_args, len, stat, ios, u
  character(len=32) :: sub_name, test_case
  character*5 adn30
  integer ibay(1), ibit, subset, jdate
  integer mtyp, msbt, inod
!  character*28 unit
!  integer iscl, iref, nseq
  integer nmsub, ierr
  integer mear, mmon, mday, mour, idate
  integer iyr, imo, idy, ihr, imi
  integer jdate1(5), jdump1(5)
  integer lmsgt, msgt(100), msgl
  
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
  elseif (sub_name .eq. 'nemtba') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call nemtba(11, 'SPOCK', mtyp, msbt, inod)
     endif
     ! Commented out. See https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/384.
     ! elseif (sub_name .eq. 'nemtbb') then
     !    if (test_case .eq. '1') then
     !       call nemtbb(0, -1, unit, iscl, iref, ibit) 
     !    endif
     ! Commented out. See https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/384.     
     ! elseif (sub_name .eq. 'nemtbd') then
     !    if (test_case .eq. '1') then
     !       call nemtbd(0, -1, nseq, char_8, int_1d, int_1d)
     !    endif
  elseif (sub_name .eq. 'nmsub') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'BBB', 11)
        iret = nmsub(12)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'tmp', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 11)
        iret = nmsub(11)
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
     endif
  elseif (sub_name .eq. 'readlc') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 12)
        call readlc(12, char_val_8, char_val_8)
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call readlc(11, char_val_8, char_val_8)
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
  elseif (sub_name .eq. 'rdmemm') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call rdmemm(1, subset, jdate, iret)        
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
  elseif (sub_name .eq. 'status') then
     if (test_case .eq. '1') then
        call status(0, 0, 0, 0)        
     elseif (test_case .eq. '2') then
        call status(100, 0, 0, 0)        
     endif
  elseif (sub_name .eq. 'sntbbe') then
     if (test_case .eq. '1') then
        call sntbbe(0, 'c', 1, 2, int_1d, char_4, char_12, char_4, char_24, char_8, char_4, char_120)        
     endif
  elseif (sub_name .eq. 'sntbde') then
     if (test_case .eq. '1') then
        call sntbde(0, 0, 'c', 1, 1, 2, int_1d, char_8, char_4, char_120, int_1d_2, int_1d, char_120)        
     endif
  elseif (sub_name .eq. 'stdmsg') then
     if (test_case .eq. '1') then
        call stdmsg('W')
     endif
  elseif (sub_name .eq. 'stndrd') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'OUT', 12)
        call stndrd(12, int_1d, 1, int_1d_2)
     endif
  elseif (sub_name .eq. 'strcpt') then
     if (test_case .eq. '1') then
        call strcpt('W', 1960, 12, 15, 12, 0)
     endif
  ! Next test commented out until
  ! https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/384 is resolved.
  ! elseif (sub_name .eq. 'string') then if (test_case .eq. '1') then
  ! call
  ! STRING('0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789',
  ! & 1, i1, 0) endif
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
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 10)
        call readns(12, char_val_8, jdate, iret)
        call ufbcup(12, 0)
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
     endif
  elseif (sub_name .eq. 'ufbqcp') then
     if (test_case .eq. '1') then
        call openbf(12, 'FIRST', 11)
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call ufbqcp(11, 0, 'c')
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/test_bort_OUT', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 10)        
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
        call ufbseq(11, real_2d, 1, 2, iret, 'c')
     elseif (test_case .eq. '2') then
        open(unit = 12, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(12, 'IN', 10)
        call ufbseq(12, real_2d, 1, 2, iret, 'c')
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
