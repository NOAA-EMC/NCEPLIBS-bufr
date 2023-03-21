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

  print *, 'Testing misc subroutines.'
  char5 = adn30(42, 5)
  if (char5 .ne. '00042') stop 1
  a = idn30(char5, 5)
  if (a .ne. 42) stop 2
  print *, 'SUCCESS'

  ! Demonstrate how internal read can be used to replace strnum().
  st1 = '12'
  st2 = '10001'
  read(st1, '(I6)', iostat = ios) i1
  if (ios .ne. 0) stop 500
  if (i1 .ne. 12) stop 500
  read(st2, '(I6)', iostat = ios) i2
  if (ios .ne. 0) stop 500
  if (i2 .ne. 10001) stop 501

  st1 = 'slsls'
  read(st1, '(I6)', iostat = ios) i1
  if (ios .eq. 0) stop 500

  st1 = '+12'
  read(st1, '(I6)', iostat = ios) i1
  if (ios .ne. 0) stop 500
  if (i1 .ne. 12) stop 500

  st1 = '-12'
  read(st1, '(I6)', iostat = ios) i1
  if (ios .ne. 0) stop 500
  if (i1 .ne. -12) stop 500

  st1 = '-12-'
  read(st1, '(I6)', iostat = ios) i1
  if (ios .eq. 0) stop 500
  
  
end program test_bort
