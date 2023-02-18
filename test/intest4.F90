! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_4' using CRBMG with OPENBF IO = 'SEC3'
! using bitmap and marker operators.
!
! Ed Hartnett, J. Ator, 2/16/23
program intest4
  implicit none
  integer*4 ireadsb, iupbs01, iupbs3, ibfms
  integer*4 mxbf, nbyt, ierr
  integer mxbfd4, mxds3, mxr8lv, mxr8pm
  integer ier1,ier2, ier3, ierme, imgdt, nds3
  integer nr8lv, nr8lv2, ntag1, ntag2, ntag3
  parameter (mxbf = 20000)
  parameter (mxbfd4 = mxbf/4)
  parameter (mxds3 = 60)
  parameter (mxr8pm = 10)
  parameter (mxr8lv = 255)
  real*8 r8arr (mxr8pm, mxr8lv), r8arr2 (mxr8pm, mxr8lv)
  integer ibfmg (mxbfd4)
  character cmgtag*8, bfmg(mxbf), cds3(mxds3)*6, tag1*8, tag2*8, tag3*8
  character*20 filnam / 'testfiles/IN_4' /
  character filost / 'r' /
  equivalence (bfmg (1), ibfmg (1))

  print *, 'Testing reading IN_1, CRBMG with OPENBF IO = SEC3 ', &
       'using bitmap and marker operators.'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  ! Open the test file.
  call cobfl(filnam, filost)

  ! Set date format.
  call datelen(10)

  ! Open /dev/null for output. ???
  open(unit = 11, file = '/dev/null')
  call openbf(11, 'SEC3', 11)

  ! Specify location of master BUFR tables on local file system.  
  call mtinfo('../tables', 90, 91)

  ! Read the BUFR message from the BUFR file.
  call crbmg(bfmg, mxbf, nbyt, ierr)
  if (ierr .ne. 0) stop 1

  ! Check some values from the message.
  if (iupbs01(ibfmg, 'MTYP') .ne. 5 .or. iupbs01(ibfmg, 'MTV' ) .ne. 12 &
       .or. iupbs01(ibfmg, 'LENM') .ne. 3588) stop 2

  if (iupbs3(ibfmg, 'NSUB') .ne. 31 .or. iupbs3(ibfmg, 'ICMP') .ne. 1) stop 3

  call upds3(ibfmg, mxds3, cds3, nds3)
  if (nds3 .ne. 51 .or. cds3(1) .ne. '310023' .or. cds3(5) .ne. '031031' .or. &
       cds3(32) .ne. '237000' .or. cds3(44) .ne. '224255') stop 4

  call readerme(ibfmg, 11, cmgtag, imgdt, ierme)
  IF (ierme .ne. 0 .or. cmgtag .ne. 'MSTTB001') stop 5

  if (imgdt .ne. 2016041815) stop 6

  ! Read the data subset from the BUFR message.
  if (ireadsb(11) .ne. 0) stop 7
  
  call ufbint(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'CLONH SAID SAZA HITE')
  if (nr8lv .ne. 1 .or. nint(r8arr(1,1)*100000) .ne. -4246453 .or. &
       nint(r8arr(2,1)) .ne. 57 .or. nint(r8arr(3,1)*100) .ne. 5407 .or. &
       ibfms(r8arr(4,1)) .ne. 1) stop 8

  call ufbrep(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'PCCF')
  call ufbrep(11, r8arr2, mxr8pm, mxr8lv, nr8lv2, '224255')
  if (nr8lv .ne. 180 .or. nint(r8arr(1,12)) .ne. 86 .or. nint(r8arr(1,15)) .ne. 38 .or. &
       nint(r8arr(1,102)) .ne. 88 .or. nint(r8arr(1,141)) .ne. 10 .or. nr8lv2 .ne. 72 .or. &
       nint(r8arr2(1,12)*10) .ne. 6 .or. nint(r8arr2(1,33)*10) .ne. 4) stop 9

  call gettagre(11, 'PCCF', 57, tag1, ntag1, ier1)
  call gettagre(11, 'PCCF', 154, tag2, ntag2, ier2)
  call gettagre(11, '224255', 65, tag3, ntag3, ier3)
  if (ier1 .ne. 0 .or. ier2 .ne. 0 .or. ier3 .ne. 0 .or. tag1 .ne. 'TMBRST  ' .or. &
       ntag1 .ne. 7 .or. tag2 .ne. 'SPRD    ' .or. ntag2 .ne. 4 .or. &
       tag3 .ne. 'RDNE    ' .or. ntag3 .ne. 10) stop 10
  call ccbfl()
  print *, 'SUCCESS!'
end program intest4
