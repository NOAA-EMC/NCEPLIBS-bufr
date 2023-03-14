! This is a test for NCEPLIBS-bufr library.
!
! This tests the bort() and bort2() subroutines. It will also test the
! bort() calls of other subroutines.
!
! Ed Hartnett 3/12/23
program test_bort
  implicit none
  character*2 char_short
  character*30 char_30

  integer :: num_args, len, status
  character(len=32) :: sub_name, test_case
  character adn30
  
  num_args = command_argument_count()
  if (num_args /= 2) then
     print *, "Two command line arguments expected: subroutine name and test case"
     stop 2
  end if

  ! Read the command line arguments, a name of subroutine, and a test
  ! case number.
  call get_command_argument(1, sub_name, len, status)
  if (status .ne. 0) stop 3
  call get_command_argument(2, test_case, len, status)
  if (status .ne. 0) stop 4
  print *, 'Testing ', sub_name, ' case ', test_case

  ! Run the test for the subroutine and test case.
  if (sub_name .eq. 'adn30') then
     if (test_case .eq. '1') then
        char_short = adn30(0, 5)
     elseif (test_case .eq. '2') then
        char_30 = adn30(0, 9)
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
