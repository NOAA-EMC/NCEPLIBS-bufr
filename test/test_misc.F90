! This is a test for NCEPLIBS-bufr library.
!
! This tests random subroutines that needed some tests to complete
! test code coverage.
!
! Ed Hartnett 3/17/23
program test_misc
  implicit none
  integer lun, il, im
  integer ios
  integer num, iret
  integer imsg, idate, iunit
  integer*4 ireadmm
  integer mbay(2)
  character*8 subset
  integer iupb
  integer isbyt, iwid
  
#ifdef KIND_4
  character*5 char5
  character*5 adn30
  character*6 adn_char
  integer a, idn30, idn
  integer ierr, nemock
  integer numbck
  integer mtyp, msbt, inod
  integer igetprm
  integer imrkopr
#endif

  print *, 'Testing misc subroutines, ignore warnings.'

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
  if (ios .ne. 0) stop 5
  call openbf(11, 'IN', 11)
  call status(11, lun, il, im)
  if (lun .ne. 1 .or. il .ne. -1 .or. im .ne. 0) stop 6
  call closbf(11)

  ! Test ireadmm()
  open(unit = 11, file = 'testfiles/IN_9', form = 'UNFORMATTED', iostat = ios)
  if (ios .ne. 0) stop 7
  call ufbmem(11, 0, iret, iunit)
  if (iret .ne. 5 .or. iunit .ne. 11) stop 8
  imsg = 1
  if (ireadmm(imsg, subset, idate) .ne. 0) stop 9
  if (imsg .ne. 2 .or. subset .ne. 'ADPSFC' .or. idate .ne. 23022519) stop 10
  call closbf(11)

  ! Test iupb().
  mbay(1) = 1
  mbay(2) = 2
  if (iupb(mbay, 1, 1) .ne. 0) stop 11

  ! Test gets1loc for YCEN and CENT positions
  call gets1loc('YCEN', 4, isbyt, iwid, iret)
  if ( iret .ne. -1 ) stop 12
  call gets1loc('CENT', 4, isbyt, iwid, iret)
  if ( iret .ne. -1 ) stop 13
  call gets1loc('YCEN', 3, isbyt, iwid, iret)
  if ( any((/isbyt,iwid,iret/).ne.(/13,8,0/)) ) stop 14
  call gets1loc('CENT', 3, isbyt, iwid, iret)
  if ( any((/isbyt,iwid,iret/).ne.(/18,8,0/)) ) stop 15
  ! Nonexistent Section 1 value deisgnation should return -1
  call gets1loc('NONEXISTENT', 3, isbyt, iwid, iret)
  if ( iret .ne. -1 ) stop 16
  
  ! Open for OUT.
  ! This fails but I don't know why yet. I get a bort() message that contains:
  ! BUFRLIB: RDUSDX
  ! THIS CARD HAS A BAD FORMAT - IT IS NOT RECOGNIZED BY THIS SUBROUTINE
  ! open(unit = 11, file = 'test_misc.bufr', form = 'UNFORMATTED', iostat = ios)
  ! if (ios .ne. 0) stop 11
  ! open(unit = 12, file = 'testfiles/IN_1', iostat = ios)
  ! if (ios .ne. 0) stop 12
  ! call openbf(11, 'OUT', 12)
  ! call status(11, lun, il, im)
  ! print *, lun, il, im
  ! if (lun .ne. 1 .or. il .ne. -1 .or. im .ne. 0) stop 13
  ! call closbf(11)

  ! Testing strnum
  call strnum('8DUMMY8',num,iret)
  if (iret .ne. -1) stop 400
  call strnum('',num,iret)
  if ((iret .ne. 0) .or. (num .ne. 0)) stop 401
  call strnum(' ',num,iret)
  if ((iret .ne. 0) .or. (num .ne. 0)) stop 402
  call strnum('    ',num,iret)
  if ((iret .ne. 0) .or. (num .ne. 0)) stop 403

  ! These tests only for the _4 run of test_misc.
#ifdef KIND_4

  ! adn30/idn30.
  char5 = adn30(42, 5)
  if (char5 .ne. '00042') stop 100
  a = idn30(char5, 5)
  if (a .ne. 42) stop 101
  idn = 42
  call cadn30(idn, adn_char)
  if (adn_char .ne. '000042') stop 103

  ! Testing nemock()
  ierr = nemock('')
  if (ierr .ne. -1) stop 202
  ierr = nemock('012345678')
  if (ierr .ne. -1) stop 203
  ierr = nemock('???')
  if (ierr .ne. -2) stop 204

  ! Testing numbck()
  iret = numbck('ABCDEF')
  if (iret .ne. -1) stop 250
  iret = numbck('01CDEF')
  if (iret .ne. -2) stop 251

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
  if (igetprm('MAXTBA') .ne. 150) stop 601
  if (igetprm('MAXTBB') .ne. 500) stop 602
  if (igetprm('MAXTBD') .ne. 500) stop 603
  if (igetprm('MXBTM') .ne. 5) stop 604
  if (igetprm('MXBTMSE') .ne. 500) stop 605
  if (igetprm('MXCDV') .ne. 3000) stop 606
  if (igetprm('MXCSB') .ne. 4000) stop 607
  if (igetprm('MXDXTS') .ne. 200) stop 608
  if (igetprm('MXLCC') .ne. 32) stop 609
  if (igetprm('MXMSGL') .ne. 600000) stop 610
  if (igetprm('MXMTBB') .ne. 4000) stop 611
  if (igetprm('MXMTBD') .ne. 1000) stop 612
  if (igetprm('MXMTBF') .ne. 25000) stop 613
  if (igetprm('MXNRV') .ne. 15) stop 614
  if (igetprm('MXRST') .ne. 50) stop 615
  if (igetprm('MXS01V') .ne. 10) stop 616
  if (igetprm('MXTAMC') .ne. 15) stop 617
  if (igetprm('MXTCO') .ne. 30) stop 618
  if (igetprm('NFILES') .ne. 32) stop 619
  if (igetprm('MAXSS') .ne. 120000) stop 620
  if (igetprm('MXDXTS') .ne. 200) stop 621
  if (igetprm('MAXMSG') .ne. 200000) stop 622
  if (igetprm('MAXMEM') .ne. 50000000) stop 623
  if (igetprm('MAXTBA') .ne. 150) stop 624
  if (igetprm('MAXTBB') .ne. 500) stop 625
  if (igetprm('MAXTBD') .ne. 500) stop 626
  if (igetprm('MXBTM') .ne. 5) stop 627
  if (igetprm('MXBTMSE') .ne. 500) stop 628
  if (igetprm('MXCDV') .ne. 3000) stop 629
  if (igetprm('MXCSB') .ne. 4000) stop 630
  if (igetprm('MXDXTS') .ne. 200) stop 631
  if (igetprm('MXLCC') .ne. 32) stop 632
  if (igetprm('MXMSGL') .ne. 600000) stop 633
  if (igetprm('MAXJL') .ne. 96000) stop 634
  if (igetprm('MXH4WLC') .ne. 10) stop 635

  ! Test imrkopr().
  if (imrkopr('nn') .ne. 0) stop 700
  if (imrkopr('223255') .ne. 1) stop 701
  if (imrkopr('224255') .ne. 1) stop 702
  if (imrkopr('225255') .ne. 1) stop 703
  if (imrkopr('232255') .ne. 1) stop 704
  if (imrkopr('123456') .ne. 0) stop 705
  
#endif
  
  print *, 'SUCCESS'
end program test_misc
