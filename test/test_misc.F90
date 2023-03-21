! This is a test for NCEPLIBS-bufr library.
!
! This tests random subroutines that needed some tests to complete
! test code coverage.
!
! Ed Hartnett 3/17/23
program test_misc
  implicit none
  character*5 char5
  character*5 adn30
  integer a, idn30
  integer lun, il, im
  integer ios
  integer ierr, nemock
  integer numbck, iret

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

  ! Open for OUT.
  ! This fails but I don't know why yet. I get a bort() message that contains:
  ! BUFRLIB: RDUSDX
  ! THIS CARD HAS A BAD FORMAT - IT IS NOT RECOGNIZED BY THIS SUBROUTINE
  ! open(unit = 11, file = 'test_misc.bufr', form = 'UNFORMATTED', iostat = ios)
  ! if (ios .ne. 0) stop 3
  ! open(unit = 12, file = 'testfiles/IN_1', iostat = ios)
  ! if (ios .ne. 0) stop 4
  ! call openbf(11, 'OUT', 12)
  ! call status(11, lun, il, im)
  ! print *, lun, il, im
  ! if (lun .ne. 1 .or. il .ne. -1 .or. im .ne. 0) stop 4
  ! call closbf(11)

  ! Testing nemock(). Commented out until this issue is resolved:
  ! https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/400
  ! ierr = nemock('')
  ! if (ierr .ne. -1) stop 100
  ! ierr = nemock('012345678')
  ! if (ierr .ne. -1) stop 100
  ! ierr = nemock('???')
  ! if (ierr .ne. -2) stop 100

  ! Testing numbck(numb)...
  ! Commented out until this issue is resolved:
  ! https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/400
  ! iret = numbck('ABCDEF')
  ! print *,iret
  ! if (iret .ne. -1) stop 200
  ! iret = numbck('01CDEF')
  ! if (iret .ne. -2) stop 201
    
  print *, 'SUCCESS'
end program test_misc
