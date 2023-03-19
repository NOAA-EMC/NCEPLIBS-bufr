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
  integer iret
  ! integer i1
  integer int_1d(1), int_1d_2(1)
  character*2 char_short
  character*30 char_30
  character*4 char_4(1)
  character*8 char_8(1)
  character*12 char_12(1)
  character*24 char_24(1)
  character*120 char_120(1)
  character*5 adn30_val_5
  real r, valx
  real*8 real_1d(1)
  real*8 real_2d(1,1)
  integer idn30, idn30_val
  integer :: num_args, len, stat, ios, u
  character(len=32) :: sub_name, test_case
  character*5 adn30
  integer ibay(1), ibit, subset, jdate
  
  num_args = command_argument_count()
  if (num_args /= 2) then
     print *, "Two command line arguments expected: subroutine name and test case"
     stop 2
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
        call copybf(0, 0)
     endif
  elseif (sub_name .eq. 'copymg') then
     if (test_case .eq. '1') then
        call copymg(0, 0)     
     endif
  ! This is commented out until
  ! https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/395 is
  ! resolved.
  ! elseif (sub_name .eq. 'copysb') then
  !    if (test_case .eq. '1') then
  !       call copysb(1, 1, iret)     
  !    endif
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
        do u = 1, 101
           open(unit = u, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
           if (ios .ne. 0) stop 3
           call openbf(u, 'IN', 11)
        end do
     endif
  elseif (sub_name .eq. 'openmg') then
     if (test_case .eq. '1') then
        call openmg(11, 'F5FCMESG', 2021022312)        
     elseif (test_case .eq. '2') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call openmg(11, 'F5FCMESG', 2021022312)        
     endif
  elseif (sub_name .eq. 'pkb') then
     if (test_case .eq. '1') then
        call pkb(1, 65, ibay, ibit)        
     endif
  elseif (sub_name .eq. 'pkb8') then
     if (test_case .eq. '1') then
        call pkb(1, -1, ibay, ibit)        
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
  elseif (sub_name .eq. 'rdmemm') then
     if (test_case .eq. '1') then
        open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
        if (ios .ne. 0) stop 3
        call openbf(11, 'IN', 11)
        call rdmemm(1, subset, jdate, iret)        
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
        call stndrd(0, int_1d, 1, int_1d_2)
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
        call ufbcnt(0, 1, 1)
     endif
  elseif (sub_name .eq. 'ufbcpy') then
     if (test_case .eq. '1') then
        call ufbcpy(0, 0)
     endif
  elseif (sub_name .eq. 'ufbcup') then
     if (test_case .eq. '1') then
        call ufbcup(0, 0)
     endif
  elseif (sub_name .eq. 'ufbdmp') then
     if (test_case .eq. '1') then
        call ufbdmp(0, 0)
     endif
  elseif (sub_name .eq. 'ufbevn') then
     if (test_case .eq. '1') then
        call ufbevn(0, real_2d, 1, 2, 3, iret, 'c')
     endif
  elseif (sub_name .eq. 'ufbget') then
     if (test_case .eq. '1') then
        call ufbget(0, real_1d, 1, iret, 's')
     endif
  elseif (sub_name .eq. 'ufbint') then
     if (test_case .eq. '1') then
        call ufbint(0, real_2d, 1, 2, iret, 'c')
     endif
  elseif (sub_name .eq. 'ufbqcp') then
     if (test_case .eq. '1') then
        call ufbqcp(0, 0, 'c')
     endif
  elseif (sub_name .eq. 'ufbrep') then
     if (test_case .eq. '1') then
        call ufbrep(0, real_2d, 1, 2, iret, 'c')
     endif
  elseif (sub_name .eq. 'ufbrms') then
     if (test_case .eq. '1') then
        call ufbrms(1, 1, real_2d, 1, 2, iret, 'c')
     endif
  elseif (sub_name .eq. 'ufbseq') then
     if (test_case .eq. '1') then
        call ufbseq(0, real_2d, 1, 2, iret, 'c')
     endif
  elseif (sub_name .eq. 'ufdump') then
     if (test_case .eq. '1') then
        call ufdump(0, 0)
     endif
  elseif (sub_name .eq. 'upftbv') then
     if (test_case .eq. '1') then
        call upftbv(0, 'n', 1.0, 1, 1, 1)
     endif
  elseif (sub_name .eq. 'valx') then
     if (test_case .eq. '1') then
        r = valx('0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789')
     endif
  elseif (sub_name .eq. 'wrdxtb') then
     if (test_case .eq. '1') then
        call wrdxtb(0, 0)        
     endif
  elseif (sub_name .eq. 'writdx') then
     if (test_case .eq. '1') then
        call writdx(0, 0, 0)
     endif
  elseif (sub_name .eq. 'writlc') then
     if (test_case .eq. '1') then
        call writlc(0, char_30, char_30)
     endif
  elseif (sub_name .eq. 'writsa') then
     if (test_case .eq. '1') then
        call writsa(0, 0, 0, 0)
     endif
  elseif (sub_name .eq. 'writsb') then
     if (test_case .eq. '1') then
        call writsb(0)
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
     stop 2
  endif

end program test_bort
