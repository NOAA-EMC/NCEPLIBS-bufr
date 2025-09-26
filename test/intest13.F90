! This is a test for NCEPLIBS-bufr.
!
! Reads compressed and uncompressed test files using UFBTAB with SETPART
!
! J. Ator, 4/4/2025
program intest13
  implicit none

  integer mxr8pm, mxr8lv, mxr8pm_uc, mxr8lv_uc
  parameter (mxr8pm = 3, mxr8pm_uc = 4)
  parameter (mxr8lv = 240, mxr8lv_uc = 300)

  integer nr8lv, nr8lv_uc
  integer*4 catch_borts

  real*8 r8arr(mxr8pm, mxr8lv), r8arr_uc(mxr8pm_uc, mxr8lv_uc)

  print *, 'Testing compressed and uncompressed files using UFBTAB with SETPART'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  ! Activate bort catching. No bort errors should occur, but this way we can fully exercise all of
  ! the lines of code in any routines where bort catching is enabled.
  if (catch_borts('Y') /= 0) stop 99

  ! Call setpart to indicate that we want to make repeated calls to ufbtab to return
  ! the contents of the input file in successive chunks.
  call setpart(.true.)

  ! Open the compressed file.
  open(unit = 11, file = 'testfiles/IN_12', form ='unformatted')

  ! Make the necessary repeated calls to ufbtab and check some output values from each
  ! successive chunk.
  nr8lv = 0  ! When using ufbtab in this way, we need to explicitly initialize this value to 0 before the first call.
  call ufbtab(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'SAZA SOZA HMSL')
  if (nr8lv /= -235 .or. nint(r8arr(1,120)*100) /= 966 .or. &
        nint(r8arr(2,120)*100) /= 15729 .or. nint(r8arr(3,120)) /= 829360) stop 1

  call ufbtab(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'SAZA SOZA HMSL')
  if (nr8lv /= -212 .or. nint(r8arr(1,120)*100) /= 481 .or. &
        nint(r8arr(2,120)*100) /= 15619 .or. nint(r8arr(3,120)) /= 829400) stop 2

  call ufbtab(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'SAZA SOZA HMSL')
  if (nr8lv /= 215 .or. nint(r8arr(1,120)*100) /= 3189 .or. &
        nint(r8arr(2,120)*100) /= 15357 .or. nint(r8arr(3,120)) /= 829450) stop 3

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
