! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_11 using various branches within WRCMPS.
!
! J. Ator, 4/18/2024
program outtest11
  implicit none

  integer imgdt, ios1, ios2, ios3
  integer*4 ireadns, isetprm

  character*8 cmgtag

  print *, 'Testing writing OUT_11 using various branches within WRCMPS'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  open ( unit = 11, file = 'testfiles/OUT_11_infile1', form = 'unformatted', iostat = ios1 )
  open ( unit = 12, file = 'testfiles/OUT_11_infile2', form = 'unformatted', iostat = ios2 )
  open ( unit = 50, file = 'out11.bufr', form = 'unformatted', iostat = ios3  )
  if ( any( (/ios1,ios2,ios3/) /= (/0,0,0/) ) ) stop 1
  if ( isetprm ( 'MXCSB', 50 ) /= 0 ) stop 2

  call pkvs01 ( 'MTV', 40 )
  ! Specify that output messages should be BUFR edition 4.
  call pkvs01 ( 'BEN', 4 )

  ! Open the first input file for reading.
  call openbf ( 11, 'IN', 11 )

  ! Open the output file.
  call openbf ( 50, 'OUT', 11 )

  ! Specify compression for output messages.
  call cmpmsg ( 'Y' )

  ! Copy all of the data subsets from the first input file to the output file.
  do while ( ireadns ( 11, cmgtag, imgdt ) == 0 )
    call openmb ( 50, cmgtag, imgdt )
    call ufbcpy ( 11, 50 )
    call writsb ( 50 )
  end do
  call closmg ( 50 )
  close ( 11 )

  ! Open the second input file for reading.
  call openbf ( 12, 'IN', 12 )

  ! Copy the DX table information from the second input file to the output file.
  call wrdxtb ( 12, 50 )

  ! Change the maximum output message size.
  call maxout ( 5000 )

  ! Copy all of the data subsets from the second input file and append them to the output file.
  do while ( ireadns ( 12, cmgtag, imgdt ) == 0 )
    call openmb ( 50, cmgtag, imgdt )
    call ufbcpy ( 12, 50 )
    call writsb ( 50 )
  end do
  call closmg ( 50 )
  close ( 12 )

  ! Reset the library so we can use isetprm to resize a global library parameter.
  call exitbufr

  open ( unit = 13, file = 'testfiles/OUT_11_infile3', form = 'unformatted', iostat = ios1 )
  open ( unit = 50, file = 'out11.bufr', form = 'unformatted', iostat = ios2  )
  if ( any( (/ios1,ios2/) /= (/0,0/) ) ) stop 3
  if ( isetprm ( 'MXCSB', 30 ) /= 0 ) stop 4

  ! Specify that output messages should be BUFR edition 4.
  call pkvs01 ( 'BEN', 4 )

  ! Open the third input file for reading.
  call openbf ( 13, 'IN', 13 )

  ! Open the output file for appending.
  call openbf ( 50, 'APN', 13 )
  call wrdxtb ( 13, 50 )

  ! Specify compression for output messages.
  call cmpmsg ( 'Y' )

  ! Change the maximum output message size.
  call maxout ( 11500 )

  ! Copy all of the data subsets from the third input file to the output file.
  do while ( ireadns ( 13, cmgtag, imgdt ) == 0 )
    call openmb ( 50, cmgtag, imgdt )
    call ufbcpy ( 13, 50 )
    call writsb ( 50 )
  end do
  call closmg ( 50 )
  close ( 13 )

  ! Close the output file.
  call closbf ( 50 )

end program outtest11
