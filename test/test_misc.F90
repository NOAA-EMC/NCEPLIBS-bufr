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
  integer numbck, num, iret
  integer mtyp, msbt, inod
  integer igetprm

  print *, 'Testing misc subroutines.'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  ! This prints a warning because no file is open, but otherwise has
  ! no effect.
  call closbf(11)  

  ! testing status()
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

  ! Testing strnum
  call strnum('8DUMMY8',num,iret)
  if (iret .ne. -1) stop 400
  call strnum('',num,iret)
  if ((iret .ne. 0) .or. (num .ne. 0)) stop 400
  call strnum(' ',num,iret)
  if ((iret .ne. 0) .or. (num .ne. 0)) stop 400
  call strnum('    ',num,iret)
  if ((iret .ne. 0) .or. (num .ne. 0)) stop 400

  ! These tests only for the _4 run of test_misc.
#ifdef KIND_4

  ! adn30/idn30.
  char5 = adn30(42, 5)
  if (char5 .ne. '00042') stop 1
  a = idn30(char5, 5)
  if (a .ne. 42) stop 2

  ! Testing nemock()
  ierr = nemock('')
  if (ierr .ne. -1) stop 100
  ierr = nemock('012345678')
  if (ierr .ne. -1) stop 100
  ierr = nemock('???')
  if (ierr .ne. -2) stop 100

  ! Testing numbck()
  iret = numbck('ABCDEF')
  if (iret .ne. -1) stop 200
  iret = numbck('01CDEF')
  if (iret .ne. -2) stop 201

  ! Testing nemtbax()
  open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
  if (ios .ne. 0) stop 3
  call openbf(11, 'IN', 11)
  call nemtbax(11, 'DUMB', mtyp, msbt, inod)
  if (inod .ne. 0) stop 300
  call closbf(11)

  ! Test igetprm().
  open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED', iostat = ios)
  if (ios .ne. 0) stop 3
  call openbf(11, 'IN', 11)
  if (igetprm('MAXSS') .ne. 120000) stop 600
  if (igetprm('MAXTBA') .ne. 150) stop 600
  if (igetprm('MAXTBB') .ne. 500) stop 600
  if (igetprm('MAXTBD') .ne. 500) stop 600
  if (igetprm('MXBTM') .ne. 5) stop 600
  if (igetprm('MXBTMSE') .ne. 500) stop 600
  if (igetprm('MXCDV') .ne. 3000) stop 600
  if (igetprm('MXCSB') .ne. 4000) stop 600
  if (igetprm('MXDXTS') .ne. 200) stop 600
  if (igetprm('MXLCC') .ne. 32) stop 600
  if (igetprm('MXMSGL') .ne. 600000) stop 600
  if (igetprm('MXMTBB') .ne. 4000) stop 600
  if (igetprm('MXMTBD') .ne. 1000) stop 600
  if (igetprm('MXMTBF') .ne. 25000) stop 600
  if (igetprm('MXNRV') .ne. 15) stop 600
  if (igetprm('MXRST') .ne. 50) stop 600
  if (igetprm('MXS01V') .ne. 10) stop 600
  if (igetprm('MXTAMC') .ne. 15) stop 600
  if (igetprm('MXTCO') .ne. 30) stop 600
  if (igetprm('NFILES') .ne. 32) stop 600
  if (igetprm('MAXSS') .ne. 120000) stop 600
  if (igetprm('MXDXTS') .ne. 200) stop 600
  if (igetprm('MAXMSG') .ne. 200000) stop 600
  if (igetprm('MAXMEM') .ne. 50000000) stop 600
  if (igetprm('MAXTBA') .ne. 150) stop 600
  if (igetprm('MAXTBB') .ne. 500) stop 600
  if (igetprm('MAXTBD') .ne. 500) stop 600
  if (igetprm('MXBTM') .ne. 5) stop 600
  if (igetprm('MXBTMSE') .ne. 500) stop 600
  if (igetprm('MXCDV') .ne. 3000) stop 600
  if (igetprm('MXCSB') .ne. 4000) stop 600
  if (igetprm('MXDXTS') .ne. 200) stop 600
  if (igetprm('MXLCC') .ne. 32) stop 600
  if (igetprm('MXMSGL') .ne. 600000) stop 600
  
#endif
  
  print *, 'SUCCESS'
end program test_misc
