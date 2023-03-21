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
  character*8 st1, st2
  integer i1, i2

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

  print *, 'SUCCESS'
end program test_misc

