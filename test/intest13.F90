! This is a test for NCEPLIBS-bufr.
!
! Reads compressed and uncompressed test files using UFBTAB with SETPART
!
! J. Ator, 4/4/2025
program intest13
  implicit none

  integer mxr8pm, mxr8lv, mxr8pm_uc, mxr8lv_uc
  parameter (mxr8pm = 3, mxr8pm_uc = 4)
  parameter (mxr8lv = 120000, mxr8lv_uc = 300)

  integer nr8lv, nr8lv_uc

  real*8 r8arr(mxr8pm, mxr8lv), r8arr_uc(mxr8pm_uc, mxr8lv_uc)

  print *, 'Testing compressed and uncompressed files using UFBTAB with SETPART'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  ! Call setpart to indicate that we want to make repeated calls to ufbtab to return
  ! the contents of the input file in successive chunks.
  call setpart(.true.)

  ! Open the compressed file.
  open(unit = 11, file = 'testfiles/data/satwndbufr', form ='unformatted')

  ! Make the necessary repeated calls to ufbtab and check some output values from each
  ! successive chunk.
  nr8lv = 0  ! When using ufbtab in this way, we need to explicitly initialize this value to 0 before the first call.
  call ufbtab(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'SAID SSNX SCCF')
  if (nr8lv /= -120000 .or. nint(r8arr(1,50000)) /= 259 .or. &
        nint(r8arr(2,50000)) /= 30000 .or. nint(r8arr(3,50000)/1000000) /= 461538400) stop 1

  call ufbtab(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'SAID SSNX SCCF')
  if (nr8lv /= -120000 .or. nint(r8arr(1,50000)) /= 54 .or. &
        nint(r8arr(2,50000)) /= 80000 .or. nint(r8arr(3,50000)/1000000) /= 428300000) stop 2

  call ufbtab(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'SAID SSNX SCCF')
  if (nr8lv /= 67711  .or. nint(r8arr(1,50000)) /= 57 .or. &
        nint(r8arr(2,50000)) /= 72000 .or. nint(r8arr(3,50000)/1000000) /= 40788100) stop 3

  call closbf(11)

  ! Open the uncompressed file.
  open(unit = 11, file = 'testfiles/IN_6_infile2', form ='unformatted')

  ! Make the necessary repeated calls to ufbtab and check some output values from each
  ! successive chunk.
  nr8lv_uc = 0  ! When using ufbtab in this way, we need to explicitly initialize this value to 0 before the first call.
  call ufbtab(11, r8arr_uc, mxr8pm_uc, mxr8lv_uc, nr8lv_uc, 'IREC ISUB CLAT {UARID}')
  if (nr8lv_uc /= -300 .or. nint(r8arr_uc(1,95)) /= 62 .or. nint(r8arr_uc(2,95)) /= 2 .or. &
        nint(r8arr_uc(3,95)*100) /= 720 .or.  nint(r8arr_uc(4,95)) /= 8) stop 4

  call ufbtab(11, r8arr_uc, mxr8pm_uc, mxr8lv_uc, nr8lv_uc, 'IREC ISUB CLAT {UARID}')
  if (nr8lv_uc /= -300 .or. nint(r8arr_uc(1,95)) /= 221 .or. nint(r8arr_uc(2,95)) /= 4 .or. &
        nint(r8arr_uc(3,95)*100) /= 4360 .or.  nint(r8arr_uc(4,95)) /= 3) stop 5

  call ufbtab(11, r8arr_uc, mxr8pm_uc, mxr8lv_uc, nr8lv_uc, 'IREC ISUB CLAT {UARID}')
  if (nr8lv_uc /= 109 .or. nint(r8arr_uc(1,95)) /= 340 .or. nint(r8arr_uc(2,95)) /= 1 .or. &
        nint(r8arr_uc(3,95)*100) /= 1620 .or.  nint(r8arr_uc(4,95)) /= 2) stop 6

  call closbf(11)

  print *, 'SUCCESS!'
end program intest13
