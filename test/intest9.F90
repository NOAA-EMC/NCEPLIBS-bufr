! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_9' using UFBIN3 to read prepfits file.
!
! J. Ator, 3/1/2023
program intest9
  implicit none

  integer*4 ireadmg, ireadsb

  integer, parameter :: mxr8pm = 15
  integer, parameter :: mxr8lv = 500
  integer, parameter :: mxr8en = 10

  real*8 hdr(5,1), r8vals( mxr8pm, mxr8lv, mxr8en )

  integer ier, iret, jret, ii, imgdt

  character*8 cmgtag

  print *, 'Testing reading IN_9 using UFBIN3 to read prepfits file.'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  open ( unit = 11, file = 'testfiles/IN_9', form ='unformatted' )

  ! Read the 5th subset from the 1st message of the prepfits file and check some values.
  call rdmgsb ( 11, 1, 5 )
  call ufbint ( 11, hdr, 5, 1, ier, 'XOB YOB ELV TYP T29' )
  if ( ( nint(hdr(1,1)*100) /= 25242 ) .or. ( nint(hdr(2,1)*100) /= 4907 ) .or. &
       ( nint(hdr(3,1)) /= 798 ) .or. ( nint(hdr(4,1)) /= 284 ) .or. &
       ( nint(hdr(5,1)) /= 512 ) ) stop 1
  call ufbin3 ( 11, r8vals, mxr8pm, mxr8lv, mxr8en, iret, jret, 'POB QOB TOB TDO' )
  if ( ( iret /= 2 ) .or. ( jret /= 1 ) .or. &
       ( nint(r8vals(1,2,1)*10) /= 9137 ) .or. ( nint(r8vals(2,1,1)) /= 785 ) .or. &
       ( nint(r8vals(3,1,1)*10) /= -159 ) .or. ( nint(r8vals(4,2,1)*10) /= -209 ) ) stop 2

  ! Now, read the 1st subset from the 3rd message of the prepfits file and check some values.
  do ii = 1, 2
    if ( ireadmg ( 11, cmgtag, imgdt ) /= 0 ) stop 3
  end do
  if ( cmgtag .ne. 'ADPUPA  ' ) stop 4
  if ( ireadsb (11) /= 0 ) stop 5
  call ufbin3 ( 11, r8vals, mxr8pm, mxr8lv, mxr8en, iret, jret, 'POB QOB UOB CAPE VENT' )
  if ( ( iret /= 49 ) .or. ( jret /= 1 ) .or. &
       ( nint(r8vals(4,1,1)) /= 0 ) .or. ( nint(r8vals(5,1,1)) /= 12174 ) .or. &
       ( nint(r8vals(1,9,1)*10) /= 2760 ) .or. ( nint(r8vals(2,9,1)) /= 47 ) .or. &
       ( nint(r8vals(1,26,1)*10) /= 6748 ) .or. ( nint(r8vals(3,26,1)*10) /= 88 ) ) stop 6

  ! Now, read the 7th subset from the 4th message of the prepfits file, and check some wind
  ! values for levels where the pressure is between 800mb and 400mb.
  if ( ireadmg ( 11, cmgtag, imgdt ) /= 0 ) stop 7
  if ( cmgtag .ne. 'VADWND  ' ) stop 8
  do ii = 1, 7
    if ( ireadsb (11) /= 0 ) stop 9
  end do
  call ufbint ( 11, hdr, 5, 1, ier, 'XOB YOB ELV TYP T29' )
  if ( ( nint(hdr(1,1)*100) /= 28299 ) .or. ( nint(hdr(2,1)*100) /= 3698 ) .or. &
       ( nint(hdr(3,1)) /= 77 ) .or. ( nint(hdr(4,1)) /= 224 ) .or. &
       ( nint(hdr(5,1)) /= 72 ) ) stop 10
  call ufbin3 ( 11, r8vals, mxr8pm, mxr8lv, mxr8en, iret, jret, 'POB<800 POB>400 POB UOB VOB' )
  if ( ( iret /= 9 ) .or. ( jret /= 1 ) .or. &
       ( nint(r8vals(1,1,1)*10) /= 7818 ) .or. ( nint(r8vals(2,1,1)*10) /= 180 ) .or. &
       ( nint(r8vals(1,7,1)*10) /= 5491 ) .or. ( nint(r8vals(3,7,1)*10) /= -67 ) .or. &
       ( nint(r8vals(2,8,1)*10) /= 353 ) .or. ( nint(r8vals(3,8,1)*10) /= -69 ) ) stop 11

  print *, 'SUCCESS!'
end program intest9
