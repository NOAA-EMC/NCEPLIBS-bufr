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
  integer i1, i2, ios
  character*8 st1, st2

  print *, 'Testing misc subroutines.'
  char5 = adn30(42, 5)
  if (char5 .ne. '00042') stop 1
  a = idn30(char5, 5)
  if (a .ne. 42) stop 2

  ! Demonstrate how internal read can be used to replace strnum().
  ! These work.
  st1 = '12'
  st2 = '10001'
  read(st1, '(I6)', iostat = ios) i1
  if (ios .ne. 0) stop 500
  if (i1 .ne. 12) stop 500
  read(st2, '(I6)', iostat = ios) i2
  if (ios .ne. 0) stop 500
  if (i2 .ne. 10001) stop 501

  ! THis fails because  the string is not numeric.
  st1 = 'slsls'
  read(st1, '(I6)', iostat = ios) i1
  if (ios .eq. 0) stop 500

  ! This works.
  st1 = '+12'
  read(st1, '(I6)', iostat = ios) i1
  if (ios .ne. 0) stop 500
  if (i1 .ne. 12) stop 500

  ! This works.
  st1 = '-12'
  read(st1, '(I6)', iostat = ios) i1
  if (ios .ne. 0) stop 500
  if (i1 .ne. -12) stop 500

  ! THis fails because  the string is not numeric.
  st1 = '-12-'
  read(st1, '(I6)', iostat = ios) i1
  if (ios .eq. 0) stop 500

  ! Blank input returns 0 just as we desire.
  st1 = ' '
  read(st1, '(I6)', iostat = ios) i1
  if (ios .ne. 0) stop 500
  if (i1 .ne. 0) stop 500
  
  ! Blank input returns 0 just as we desire.
  st1 = '   '
  read(st1, '(I6)', iostat = ios) i1
  if (ios .ne. 0) stop 500
  if (i1 .ne. 0) stop 500
  
  print *, 'SUCCESS'
end program test_bort
