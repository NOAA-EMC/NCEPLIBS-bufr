! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_4' using CRBMG_C with OPENBF IO = 'SEC3'
! using bitmap and marker operators.
!
! Ed Hartnett, J. Ator, 2/22/2023
program intest4
  use bufr_interface

  implicit none

  integer*4 ireadsb, iupbs01, iupbs3, ibfms, lmsg, nmwrd, iupm
  integer*4 mxbf, nbyt, ierr

  integer ier, imgdt, nds3, ii
  integer nr8lv, ntag

  integer mxbfd4, mxds3, mxr8lv, mxr8pm
  parameter (mxbf = 20000)
  parameter (mxbfd4 = mxbf/4)
  parameter (mxds3 = 60)
  parameter (mxr8pm = 10)
  parameter (mxr8lv = 255)

  real*8 r8arr (mxr8pm, mxr8lv)

  integer ibfmg (mxbfd4)

  character cmgtag*8, bfmg(mxbf), cds3(mxds3)*6, tag*8, sec0*8, cbay*8
  character*20 filnam / 'testfiles/IN_4' /
  character filost / 'r' /

  equivalence (bfmg (1), ibfmg (1))

  print *, 'Testing reading IN_4, using CRBMG_C with OPENBF IO = SEC3, and using bitmap and marker operators.'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  ! Open the test file.
  call cobfl_c(filnam, filost)

  ! Set date format.
  call datelen(10)

  ! For this test we're going to use cobfl_c and crbmg_c to read a BUFR message from filnam, and then we're going to
  ! call readerme to pass that message as input to the library, rather than having the library read directly from
  ! a logical unit number via openbf.  However, we do still need to call openbf to specify that messages will be
  ! decoded according to Section 3, and since we still need to call openbf, then we also need to pass in a logical
  ! unit number associated with an actual file on the system, even though openbf will never actually try to read
  ! anything from that file.  So /dev/null is a good choice here.
  open(unit = 11, file = '/dev/null')
  call openbf(11, 'SEC3', 11)

  ! Specify location of master BUFR tables on local file system.
  call mtinfo('../tables', 90, 91)

  ! Read the BUFR message from the BUFR file.
  call crbmg_c(bfmg, mxbf, nbyt, ierr)
  if (ierr /= 0) stop 1

  ! Check some values in Section 1 of the message.
  if (iupbs01(ibfmg, 'MTYP') /= 5 .or. iupbs01(ibfmg, 'MTV' ) /= 12 &
       .or. iupbs01(ibfmg, 'LENM') /= 3588) stop 2

  ! Check some values in Section 3 of the message.
  if (iupbs3(ibfmg, 'NSUB') /= 31 .or. iupbs3(ibfmg, 'ICMP') /= 1) stop 3

  call upds3(ibfmg, mxds3, cds3, nds3)
  if (nds3 /= 51 .or. cds3(1) /= '310023' .or. cds3(5) /= '031031' .or. &
       cds3(32) /= '237000' .or. cds3(44) /= '224255') stop 4

  ! Pass the message into the library so that Section 4 data can be read.
  call readerme(ibfmg, 11, cmgtag, imgdt, ier)
  if (ier /= 0 .or. cmgtag /= 'MSTTB001' .or. imgdt /= 2016041815 ) stop 5

  ! Read a data subset from the BUFR message.
  if (ireadsb(11) /= 0) stop 6

  ! Check some data values in the data subset.
  call ufbint(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'CLONH SAID SAZA HITE')
  if (nr8lv /= 1 .or. nint(r8arr(1,1)*100000) /= -4246453 .or. &
       nint(r8arr(2,1)) /= 57 .or. nint(r8arr(3,1)*100) /= 5407 .or. &
       ibfms(r8arr(4,1)) /= 1) stop 7

  call ufbrep(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'PCCF')
  if ( nr8lv /= 180 .or. nint(r8arr(1,12)) /= 86 .or. nint(r8arr(1,15)) /= 38 .or. &
       nint(r8arr(1,102)) /= 88 .or. nint(r8arr(1,141)) /= 10 ) stop 8
  call ufbrep(11, r8arr, mxr8pm, mxr8lv, nr8lv, '224255')
  if ( nr8lv /= 72 .or. nint(r8arr(1,12)*10) /= 6 .or. nint(r8arr(1,33)*10) /= 4) stop 9

  ! Check some bitmap and marker operator references in the data subset.
  call gettagre(11, 'PCCF', 57, tag, ntag, ier)
  if ( ier /= 0 .or. ntag /= 7 .or. tag /= 'TMBRST  ' ) stop 10
  call gettagre(11, 'PCCF', 154, tag, ntag, ier)
  if ( ier /= 0 .or. ntag /= 4 .or. tag /= 'SPRD    ' ) stop 11
  call gettagre(11, '224255', 65, tag, ntag, ier)
  if ( ier /= 0 .or. ntag /= 10 .or. tag /= 'RDNE    ' ) stop 12

  ! Check the output from lmsg, nmwrd, ipkm, and iupm.
  do ii = 1, 8
    sec0(ii:ii) = bfmg(ii)
  end do
  if ( lmsg(sec0) /= 898 ) stop 13
  if ( nmwrd(ibfmg) /= 898 ) stop 14
  call ipkm(cbay,3,3588)
  do ii = 1, 3
    if ( cbay(ii:ii) /= sec0(ii+4:ii+4) ) stop 15
  end do
  if ( iupm(cbay(1:3),24) /= 3588 ) stop 16

  ! Close the test file.
  call ccbfl_c()

  print *, 'SUCCESS!'
end program intest4
