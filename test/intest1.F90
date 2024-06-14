! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_1' using CRBMG_C with
! OPENBF IO = 'SEC3'.
!
! Ed Hartnett, J. Ator, 2/3/2023
program intest1
  use bufr_interface

  implicit none

  integer mxbfd4, mxds3, nds3, ierme, imgdt
  integer ierndv, iernds, mxr8pm, mxr8lv, iertgp, nr8lv
  integer len0, len1, len2, len3, len4, len5
  integer*4 mxbf, nbyt, ierr
  integer*4 iupbs01, iupbs3, ireadsb, ibfms
  parameter (mxbf = 20000)
  parameter (mxbfd4 = mxbf/4)
  parameter (mxds3 = 20)
  parameter (mxr8pm = 10)
  parameter (mxr8lv = 255)
  real*8 r8arr(mxr8pm, mxr8lv)
  integer ibfmg(mxbfd4)
  character smidstg*9, softvstg*12, cmgtag*8, &
       bfmg(mxbf), cds3(mxds3)*6, tagpr*8, celem*60, cunit*22
  character*20 filnam / 'testfiles/IN_1' /
  character filost / 'r' /
  integer*4 i4dy, idxmsg

  equivalence (bfmg(1), ibfmg(1))

  print *, 'Testing reading IN_1, CRBMG_C with OPENBF IO = SEC3'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  ! Open the test file.
  call cobfl_c(filnam, filost)

  ! Specify format of Section 1 date/time when reading.
  call datelen(10)

  ! Specify the use of Section 3 decoding.
  ! For this test we're going to use cobfl_c and crbmg_c to read a BUFR message from filnam, and then we're going to
  ! call readerme to pass that message as input to the library, rather than having the library read directly from
  ! a logical unit number via openbf.  However, we do still need to call openbf to specify that messages will be
  ! decoded according to Section 3, and since we still need to call openbf, then we also need to pass in a logical
  ! unit number associated with an actual file on the system, even though openbf will never actually try to read
  ! anything from that file.  So /dev/null is a good choice here.
  open (unit = 11, file = '/dev/null')
  call openbf(11, 'SEC3', 11)

  ! Specify location of master BUFR tables.
  call mtinfo('../tables', 90, 91)

  ! Read a BUFR message from the test file into a memory array.
  call crbmg_c(bfmg, mxbf, nbyt, ierr)
  if (ierr /= 0) stop 1

  ! Read and check some values from Section 1.
  if (iupbs01(ibfmg, 'MTYP') /= 2) stop 2
  if (iupbs01(ibfmg, 'MTV') /= 14) stop 3
  if (iupbs01(ibfmg, 'LENM') /= 4169) stop 4

  ! Read and check some values from Section 3.
  if (iupbs3(ibfmg, 'NSUB') /= 1) stop 5
  if (iupbs3(ibfmg, 'ICMP') /= 0) stop 6

  ! Read and check some data descriptors from Section 3.
  call upds3(ibfmg, mxds3, cds3, nds3)
  IF (nds3 /= 8 .or. cds3(1) /= '309052' .or. cds3(5) /= '002095') stop 7

  ! Pass the BUFR message from the memory array into the library.
  call openbf(11, 'QUIET', 2)  ! Turn on some extra print statements for testing
  call readerme(ibfmg, 11, cmgtag, imgdt, ierme)
  ! Call readerme a second time with the same BUFR message, to test reusing the Section 3 cache from the first call.
  call readerme(ibfmg, 11, cmgtag, imgdt, ierme)
  if (ierme /= 0 .or. cmgtag /= 'MSTTB001') stop 8
  call openbf(11, 'QUIET', 0)  ! Turn off extra print statements

  ! Get and check the element names and units associated with some
  ! Table B mnemonics.
  call nemdefs(11, 'VSIGX', celem, cunit, ierndv)
  if (ierndv /= 0 .or. celem(1:40) /= 'Extended vertical sounding significance ' .or. &
       cunit(1:12) /= 'FLAG TABLE  ') stop 9
  call nemdefs(11, 'SMID', celem, cunit, iernds)
  if (iernds /= 0 .or. celem(1:39) /= 'Ship or mobile land station identifier ' .or. &
       cunit(1:10) /= 'CCITT IA5 ') stop 10

  ! Read and check the Section 1 date-time.
  if (imgdt /= 2012093012) stop 11

  ! Read a data subset from the BUFR message.
  if (ireadsb(11) /= 0 ) stop 12

  ! Get and check the parent of a Table B mnemonic.
  call gettagpr(11, 'PRLC', 192, tagpr, iertgp)
  if (iertgp /= 0 .or. tagpr /= 'WSPLRAOB') stop 13

  ! Read and check some data values.
  call ufbint(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'CLONH A4ME HSMSL QCEVR')
  IF (nr8lv /= 1 .or. nint(r8arr(1,1)*100000) /= 10388797 .or. &
       nint(r8arr(2,1)) /= 7 .or. nint(r8arr(3,1)) /= 14 .or. &
       ibfms(r8arr(4,1)) /= 1) stop 14

  ! Read and check a sequence of data values.
  call ufbseq(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'TDWPRAOB')
  IF (nr8lv /= 191 .or. nint(r8arr(8,3)*100) /= 29416 .or. &
       nint(r8arr(10,11)*10) /= 55 .or. nint(r8arr(2,12)) /= 2048 .or. &
       nint(r8arr(5,67)*100000) /= -1167 .or. nint(r8arr(1,186)) /= 2523) stop 15

  ! Read and check some long character strings.
  call readlc(11, smidstg, 'SMID')
  IF (smidstg(7:9) /= 'UAO') stop 16
  call readlc(11, softvstg, 'SOFTV')
  IF (softvstg(5:12) /= '5.8.5.10') stop 17

  ! Read and check section lengths
  call getlens(ibfmg,5,len0,len1,len2,len3,len4,len5)
  if (.not. all((/len0,len1,len2,len3,len4,len5/) == (/8,22,0,24,4111,4/))) stop 18

  ! Close the test file.
  call ccbfl_c()

  ! Test the i4dy() function.
  if (i4dy(80123023) /= 1980123023) stop 19

  ! Test idxmsg().
  if (idxmsg(1) /= 0) stop 20

  print *, 'SUCCESS!'
end program intest1
