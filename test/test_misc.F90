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
  
end program test_bort
