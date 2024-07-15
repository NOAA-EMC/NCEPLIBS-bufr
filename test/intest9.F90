! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_9' using UFBINT to read prepfits file.
!
! J. Ator, 3/1/2023

program intest9

  implicit none

  integer*4 ireadmg, ireadsb

  real*8 hdr(5,1)

  integer ier, ii, imgdt

  character*8 cmgtag

  print *, 'Testing reading IN_9 using UFBINT to read prepfits file.'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  open ( unit = 11, file = 'testfiles/IN_9', form ='unformatted' )

  ! Read the 5th subset from the 1st message of the file and check some values.
  call rdmgsb ( 11, 1, 5 )
  call ufbint ( 11, hdr, 5, 1, ier, 'XOB YOB ELV TYP T29' )
  if ( ( nint(hdr(1,1)*100) /= 25242 ) .or. ( nint(hdr(2,1)*100) /= 4907 ) .or. &
       ( nint(hdr(3,1)) /= 798 ) .or. ( nint(hdr(4,1)) /= 284 ) .or. &
       ( nint(hdr(5,1)) /= 512 ) ) stop 1

  ! Now, read the 1st subset from the 3rd message of the file.
  do ii = 1, 2
    if ( ireadmg ( 11, cmgtag, imgdt ) /= 0 ) stop 2
  end do
  if ( cmgtag /= 'ADPUPA  ' ) stop 3
  if ( ireadsb (11) /= 0 ) stop 4

  ! Now, read the 7th subset from the 4th message of the file and check some values.
  if ( ireadmg ( 11, cmgtag, imgdt ) /= 0 ) stop 5
  if ( cmgtag /= 'VADWND  ' ) stop 6
  do ii = 1, 7
    if ( ireadsb (11) /= 0 ) stop 7
  end do
  call ufbint ( 11, hdr, 5, 1, ier, 'XOB YOB ELV TYP T29' )
  if ( ( nint(hdr(1,1)*100) /= 28299 ) .or. ( nint(hdr(2,1)*100) /= 3698 ) .or. &
       ( nint(hdr(3,1)) /= 77 ) .or. ( nint(hdr(4,1)) /= 224 ) .or. &
       ( nint(hdr(5,1)) /= 72 ) ) stop 8

  print *, 'SUCCESS!'
end program intest9
