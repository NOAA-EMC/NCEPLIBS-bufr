! This is a test for NCEPLIBS-bufr library.
!
! This tests random subroutines that needed some tests to complete
! test code coverage.
!
! Ed Hartnett 3/17/23
program test_bort
  implicit none
  character*5 char5
  character*5 adn30
  integer a, idn30
  integer lun, il, im
  integer ios

  print *, 'Testing misc subroutines.'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  ! adn30/idn30.
  char5 = adn30(42, 5)
  if (char5 .ne. '00042') stop 1
  a = idn30(char5, 5)
  if (a .ne. 42) stop 2

  ! testiing status()
  open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
  if (ios .ne. 0) stop 3
  call openbf(11, 'IN', 11)
  call status(11, lun, il, im)
  if (lun .ne. 1 .or. il .ne. -1 .or. im .ne. 0) stop 4
  call closbf(11)

  ! Try again.
  open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
  if (ios .ne. 0) stop 3
  call openbf(11, 'IN', 11)
  call status(11, lun, il, im)
  if (lun .ne. 1 .or. il .ne. -1 .or. im .ne. 0) stop 4
  call closbf(11)

  ! ! Open for OUT.
  ! open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
  ! if (ios .ne. 0) stop 3
  ! call openbf(11, 'OUT', 11)
  ! call status(11, lun, il, im)
  ! print *, lun, il, im
  ! if (lun .ne. 1 .or. il .ne. -1 .or. im .ne. 0) stop 4
  ! call closbf(11)
  
  print *, 'SUCCESS'
end program test_bort
