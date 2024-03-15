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
  integer*4 ireadmm, igetfxy, isetprm
  integer mbay(2)
  character*8 subset
  integer iupb
  integer isbyt, iwid
  character*6 cfxy
  
#ifndef KIND_8
  character*5 char5
  character sign
  character*5 adn30
  character*6 adn_char
  integer a, idn30, idn, i
  integer ierr, nemock
  integer numbck
  integer mtyp, msbt, inod
  integer*4 igetprm, invcon, invtag
  integer*4 imrkopr
  character*7 prms(15)
  character*4 char_4(1), char_4_2(2)
  character*8 char_8(1), char_8_2(2)
  character*12 char_12(1)
  character*24 char_24(1)
  character*120 char_120(1), char_120_2(2), char_120_2d(2,5)
  character*80 card
  integer int_1d(1), int_1d_2(2), int_1d_3(2), int_2d(2,5)
  integer imt, imtv, iogce, iltv
#endif

  print *, 'Testing misc subroutines, ignore warnings.'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  ! This prints a warning because no file is open, but otherwise has
  ! no effect.
  call closbf(11)  

  ! Test a special case in arallocf when mod(MXMSGL,4) .ne. 0
  iret = isetprm('MXMSGL', 600006)
  if (iret .ne. 0) stop 2
  call openbf(15, 'FIRST', 15)

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
  
  ! Testing copysb for "no more subsets" condition
  open(unit = 11, file = 'testfiles/OUT_4_infile1', form = 'UNFORMATTED', iostat = ios)
  if (ios .ne. 0) stop 17
  call openbf(11, 'IN', 11)
  call readns(11, subset, idate, iret)
  open(unit = 12, file = 'testfiles/test_misc_OUT', form = 'UNFORMATTED', iostat = ios)
  if (ios .ne. 0) stop 18
  call openbf(12, 'OUT', 11)
  call openmg(12, 'NC007000', 2021022312)
  call copysb(11, 12, iret)
  if ( iret .ne. -1 ) stop 19
  call closbf(11)
  call closbf(12)

  ! Testing strnum
  call strnum('8DUMMY8',num,iret)
  if (iret .ne. -1) stop 400
  call strnum('',num,iret)
  if ((iret .ne. 0) .or. (num .ne. 0)) stop 401
  call strnum(' ',num,iret)
  if ((iret .ne. 0) .or. (num .ne. 0)) stop 402
  call strnum('    ',num,iret)
  if ((iret .ne. 0) .or. (num .ne. 0)) stop 403

  ! Test various igetfxy() cases.
  iret = igetfxy("SHORT", cfxy)
  if (iret .ne. -1) stop 900
  iret = igetfxy("352003", cfxy)
  if (iret .ne. 0) stop 901

  ! The following tests are only for the _4 and _d runs of test_misc, because many
  ! of the routines below aren't intended to ever be called directly by users, and
  ! therefore those routines aren't configured to handle the passing of 8-byte
  ! integer arguments.

#ifndef KIND_8

  ! adn30/idn30.
  char5 = adn30(42, 5)
  if (char5 .ne. '00042') stop 100
  a = idn30(char5, 5)
  if (a .ne. 42) stop 101
  idn = 42
  call cadn30(idn, adn_char)
  if (adn_char .ne. '000042') stop 103

  ! Testing jstnum().
  char5 = '  +42'
  call jstnum(char5, sign, ierr)
  if ( ierr .ne. 0 .or. char5 .ne. '42   ' .or. sign .ne. '+' ) stop 104
  char5 = 'DUMMY'
  call jstnum(char5, sign, ierr)
  if ( ierr .ne. -1 ) stop 105

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
  if (igetprm('MXMSGL') .ne. 600006) stop 610
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
  if (igetprm('MAXJL') .ne. 96000) stop 634
  if (igetprm('MXH4WLC') .ne. 10) stop 635
  if (igetprm('MXCNEM') .ne. 450) stop 636
  if (igetprm('MAXNC') .ne. 600) stop 637
  if (igetprm('MXNAF') .ne. 4) stop 638
  call closbf(11)

  ! Test imrkopr().
  if (imrkopr('nn') .ne. 0) stop 700
  if (imrkopr('223255') .ne. 1) stop 701
  if (imrkopr('224255') .ne. 1) stop 702
  if (imrkopr('225255') .ne. 1) stop 703
  if (imrkopr('232255') .ne. 1) stop 704
  if (imrkopr('123456') .ne. 0) stop 705

  ! Test cpdxmm() on a file which contains DX table messages at the end of the file
  open(unit = 11, file = 'testfiles/OUT_2_preAPX', iostat = ios)
  call openbf(11, 'IN', 11)
  ! skip ahead to the start of the DX table messages at the end of the file
  do i = 1, 213
    call readmg(11, subset, idate, iret)
  enddo
  call openbf(11, 'QUIET', 2)
  ! read all of the DX messages up to the end of the file and print the count
  call cpdxmm(11)
  ! we should now be at EOF, so confirm that another call to cpdxmm properly traps this EOF condition
  call cpdxmm(11)
  call openbf(11, 'QUIET', 0)
  call closbf(11)

  ! Test readmg() on a file which contains DX table messages at the end of the file
  open(unit = 11, file = 'testfiles/OUT_2_preAPX', iostat = ios)
  call openbf(11, 'IN', 11)
  ! skip ahead to the start of the DX table messages at the end of the file
  do i = 1, 213
    call readmg(11, subset, idate, iret)
  enddo
  call openbf(11, 'QUIET', 1)
  ! read the next message which should be a DX message
  call readmg(11, subset, idate, iret)
  call openbf(11, 'QUIET', 0)
  call closbf(11)

  ! Test print of warning statement in nvnwin()
  call openbf(11, 'QUIET', 1)
  call nvnwin(0, 1, 1, 1, mbay, 2)
  call openbf(11, 'QUIET', 0)

  ! Test invcon() and invtag()
  open(unit = 11, file = 'testfiles/OUT_5_infile', iostat = ios)
  call openbf(11, 'IN', 11)
  call readns(11, subset, idate, iret)
  call openbf(11, 'QUIET', 2)
  if (invcon(1, 1, 0, 3) .ne. 0) stop 706
  if (invcon(1, 1, 3, 0) .ne. 0) stop 707
  if (invtag(0, 1, 3, 0) .ne. 0) stop 708
  call openbf(11, 'QUIET', 0)
  call closbf(11)

  ! Test rcstpl() passing non-zero iret values back through rdtree() and readsb()
  open(unit = 11, file = 'testfiles/IN_3', form = 'UNFORMATTED', iostat = ios)
  if (ios .ne. 0) stop 3
  call openbf(11, 'IN', 11)
  call readmg(11, subset, idate, iret)
  iret = isetprm('MAXJL', 20)
  call openbf(11, 'QUIET', 1)
  call readsb(11, iret)
  if ( iret .ne. -1 ) stop 709
  iret = isetprm('MAXJL', 96000)
  iret = isetprm('MAXSS', 20)
  call readmg(11, subset, idate, iret)
  call readsb(11, iret)
  if ( iret .ne. -1 ) stop 710
  call closbf(11)
  open(unit = 11, file = 'testfiles/IN_1', form = 'UNFORMATTED', iostat = ios)
  if (ios .ne. 0) stop 3
  call openbf(11, 'SEC3', 11)
  call mtinfo('../tables', 80, 81)
  call readns(11, subset, idate, iret)
  if ( iret .ne. -1 ) stop 711
  iret = isetprm('MAXSS', 120000)
  call openbf(11, 'QUIET', 0)
  call closbf(11)

  ! Test various parameters for isetprm().
  prms = (/ 'MAXTBA ', 'MAXTBB ', 'MAXTBD ', 'MXMTBB ', 'MXMTBD ', 'MAXJL  ', 'MXNAF  ', &
    'MXMTBF ', 'MXS01V ', 'MXBTM  ', 'MXBTMSE', 'MXTAMC ', 'MXTCO  ', 'MXRST  ', 'MAXNC  ' /)
  do i = 1, size(prms, 1)
    iret = isetprm(trim(prms(i)), 42+i)
    if ( (iret .ne. 0) .or. (igetprm(trim(prms(i))) .ne. 42+i) ) then
      print*, prms(i)
      stop 800
    endif
  enddo

  ! Test rdmtbb()
  open(unit = 11, file = 'testfiles/test_misc_rdmtb_std', iostat = ios)
  if (ios .ne. 0) stop 3
  open(unit = 12, file = 'testfiles/test_misc_rdmtb_loc', iostat = ios)
  if (ios .ne. 0) stop 3
  card = 'Table B STD |  0 | 38                                                           '
  write (11,'(A)') card
  card = ' 0-01-001 |  0 |     0 |   7 | Numeric   | WMOB   ; ; WMO block number          '
  write (11,'(A)') card
  close (11)
  open(unit = 11, file = 'testfiles/test_misc_rdmtb_std', iostat = ios)
  if (ios .ne. 0) stop 3
  card = 'Table B LOC |  0 | 7 |  1                                                       '
  write (12,'(A)') card
  close (12)
  open(unit = 12, file = 'testfiles/test_misc_rdmtb_loc', iostat = ios)
  if (ios .ne. 0) stop 3
  call rdmtbb(11, 12, 1, imt, imtv, iogce, iltv, iret, &
              int_1d, char_4, char_12, char_4, char_24, char_8, char_4, char_120)
  if ( iret .ne. 1 ) stop 801
  close (11)
  close (12)

  ! Test rdmtbd()
  open(unit = 11, file = 'testfiles/test_misc_rdmtb_std', iostat = ios)
  if (ios .ne. 0) stop 3
  open(unit = 12, file = 'testfiles/test_misc_rdmtb_loc', iostat = ios)
  if (ios .ne. 0) stop 3
  card = 'Table D STD |  0 | 38                                                           '
  write (11,'(A)') card
  card = '   3-01-058 | UNTFROLD   ;     ; Universal lightning event                      '
  write (11,'(A)') card
  card = '         | 3-01-011 > | Year, month, day                                        '
  write (11,'(A)') card
  card = '         | 3-01-012 > | Hour, minute                                            '
  write (11,'(A)') card
  card = '         | 0-20-118 > | Lightning detection error                               '
  write (11,'(A)') card
  card = '         | 0-20-119 > | Lightning discharge polarity                            '
  write (11,'(A)') card
  card = '         | 0-25-035   | Decision method for polarity                            '
  write (11,'(A)') card
  close (11)
  open(unit = 11, file = 'testfiles/test_misc_rdmtb_std', iostat = ios)
  if (ios .ne. 0) stop 3
  card = 'Table D LOC |  0 | 7 |  1                                                       '
  write (12,'(A)') card
  card = '   3-01-055 | LOWRESSQ   ;     ; Low-resolution data sequence                   '
  write (12,'(A)') card
  card = '         | 3-01-025 > | Latitude/longitude (coarse accuracy), day/time          '
  write (12,'(A)') card
  card = '         | 0-02-121 > | Mean frequency                                          '
  write (12,'(A)') card
  card = '         | 0-20-023   | Other weather phenomena                                 '
  write (12,'(A)') card
  close (12)
  open(unit = 12, file = 'testfiles/test_misc_rdmtb_loc', iostat = ios)
  if (ios .ne. 0) stop 3
  call rdmtbd(11, 12, 2, 5, imt, imtv, iogce, iltv, iret, &
              int_1d_2, char_8_2, char_4_2, char_120_2, int_1d_3, int_2d, char_120_2d)
  if ( iret .ne. 2 ) stop 802
  close (11)
  close (12)

  ! Test rdmtbf()
  open(unit = 11, file = 'testfiles/test_misc_rdmtb_std', iostat = ios)
  if (ios .ne. 0) stop 3
  open(unit = 12, file = 'testfiles/test_misc_rdmtb_loc', iostat = ios)
  if (ios .ne. 0) stop 3
  card = 'Table F STD |  0 | 38                                                           '
  write (11,'(A)') card
  card = '   0-02-002 | TIWM ; FLAG                                                       '
  write (11,'(A)') card
  card = '              | 1 > | Certified Instruments                                     '
  write (11,'(A)') card
  card = '              | 2 > | Originally measured in knots                              '
  write (11,'(A)') card
  card = '              | 3   | Originally measured in km h**-1                           '
  write (11,'(A)') card
  close (11)
  open(unit = 11, file = 'testfiles/test_misc_rdmtb_std', iostat = ios)
  if (ios .ne. 0) stop 3
  card = 'Table F LOC |  0 | 7 |  1                                                       '
  write (12,'(A)') card
  close (12)
  open(unit = 12, file = 'testfiles/test_misc_rdmtb_loc', iostat = ios)
  if (ios .ne. 0) stop 3
  call rdmtbf(11, 12)
  close (11)
  close (12)

  ! Test sntbbe()
  card = '  0-00-007 |   0 |           0 |  16                                            '
  iret = 0
  call sntbbe(0, card, 1, iret, int_1d, char_4, char_12, char_4, char_24, char_8, char_4, char_120)
  if ( char_24(1) .ne. ' ' ) stop 803

#endif

  print *, 'SUCCESS'
end program test_misc
