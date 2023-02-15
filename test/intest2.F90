! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_2' with OPENBF IO = IN and LUNIN !=
! LUNDX.
!
! Ed Hartnett, J. Ator, 2/15/23
program intest2
  implicit none
  integer*4 ireadmg, iupvs01, nmsub, ibfms
  integer mxr8pm, mxr8lv
  parameter (mxr8pm = 10)
  parameter (mxr8lv = 255)
  real*8 r8arr(mxr8pm, mxr8lv), getvalnb
  integer ibit(32)
  character cmgtag*8
  integer ierrsb, ii, imgdt, nib, nr8lv

  print *, 'Testing reading IN_2, OPENBF IO = IN and LUNIN != LUNDX'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  ! Open the test files.
  open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED')
  open(unit = 12, file = 'testfiles/IN_2_bufrtab')

  call openbf(11, 'IN', 12)

  ! Read the BUFR message from the BUFR file.
  if (ireadmg(11, cmgtag, imgdt) .ne. 0 ) stop 1

  ! Check some values from the message.
  if (cmgtag .ne. 'NC005064' .or. imgdt .ne. 12101013) stop 2
  if (iupvs01(11,'MSBT') .ne. 64 .or. iupvs01(11,'OGCE') .ne. 7 .or. &
       iupvs01(11,'LENM') .ne. 19926) stop 3
  if (nmsub(11) .ne. 154) stop 4

  ! Read the 5th data subset from the BUFR message.
  do ii = 1, 5
     call readsb(11, ierrsb)
  end do
  if (ierrsb .ne. 0) stop 5

  ! Read some data values from the file and check them.
  call ufbint(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'CLAT SAZA PRLC WDIR RPID SIDP')
  if (nr8lv .ne. 1 .or. nint(r8arr(1,1)*100) .ne. 1260 .or. &
       nint(r8arr(2,1)*100) .ne. 2765 .or. nint(r8arr(3,1)) .ne. 25540 .or. &
       nint(r8arr(4,1)) .ne. 218 .or. ibfms(r8arr(5,1)) .ne. 1) stop 6

  ! Find and check the bit settings for SIDP.
  call upftbv(11, 'SIDP', r8arr(6,1), 32, ibit, nib)
  if (nib .ne. 1 .or. ibit(1) .ne. 9) stop 7

  ! Read and check some values from the data subset currently open.
  call ufbrep(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'GNAP PCCF MAQC NCTH')
  if (nr8lv .ne. 12 .or. nint(r8arr(1,2)) .ne. 2 .or. nint(r8arr(2,4)) .ne. 86 .or. &
       nint(r8arr(2,6)) .ne. 0 .or. ibfms(r8arr(3,8)) .ne. 1 .or. &
       ibfms(r8arr(4,9)) .ne. 1 .or. nint(r8arr(2,11)) .ne. 97 .or. &
       nint(r8arr(1,12)) .ne. 3) stop 8
  if (nint(getvalnb(11,'NCTH',3,'PCCF',-1)) .ne. 0 .or. &
       nint(getvalnb(11,'SSNX',1,'SWCM',1)) .ne. 1) stop 9

  print *, 'SUCCESS!'
end program

