! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_9 using INVMRG, MRGINV, and UFBOVR.
!
! J. Ator, 3/1/2023

module Share_errstr
  ! This module is needed in order to share information between the test program and subroutine errwrt, because
  ! the latter is not called by the former but rather is called directly from within the BUFRLIB software.

  character*1500 errstr

  integer errstr_len
end module Share_errstr

subroutine errwrt(str)
  ! This subroutine supersedes the subroutine of the same name within the BUFRLIB software, so that we can
  ! easily test the generation of error messages from within the library.

  use Share_errstr

  character*(*) str

  integer str_len

  str_len = len(str)
  errstr ( errstr_len + 1 : errstr_len + str_len + 1 ) = str
  errstr_len = errstr_len + str_len

  return
end subroutine errwrt

program outtest9

  use Share_errstr
 
  implicit none

  integer ii, jj, imgdt, ier

  integer*4 nmsub, icopysb

  real*8 r8arr1(4,1), r8arr2(4,2)

  character*8 cmgtag

  print *, 'Testing writing OUT_9 using INVMRG, MRGINV, and UFBOVR'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  open ( unit = 11, file = 'testfiles/OUT_9_infile1', form = 'unformatted' )
  open ( unit = 12, file = 'testfiles/OUT_9_infile2', form = 'unformatted' )
  open ( unit = 21, file = 'out9.bufr',form = 'unformatted' )

  call openbf ( 11, 'IN', 11 )
  call openbf ( 12, 'IN', 11 )
  call openbf ( 21, 'OUT', 11 )

  ! Use BUFR compression when writing to the output file.
  call cmpmsg ('Y')

  ! Read the first BUFR message from each of the input files.
  call readmg ( 11, cmgtag, imgdt, ier )
  if ( ier .ne. 0 ) stop 1
  call readmg ( 12, cmgtag, imgdt, ier )
  if ( ier .ne. 0 ) stop 2

  ! Open a new BUFR message for output.
  call openmb ( 21, cmgtag, imgdt )

  ! Copy each data subset from the first message of infile1 to the output message, while merging in some additional data values
  ! from each corresponding subset in the first message of infile2.
  do ii = 1, 3

    ! Read in the next corresponding subset from each file.
    call readsb (11, ier)
    if ( ier .ne. 0 ) stop 3
    call readsb (12, ier)
    if ( ier .ne. 0 ) stop 4

    ! Copy the first subset to the output message.
    call invmrg (11, 21)

    ! Merge REHU from the second subset into the output message.
    call ufbint (12, r8arr1, 4, 1, ier, 'REHU' )
    if ( ier .ne. 1 ) stop 5
    call ufbint (21, r8arr1, 4, 1, ier, 'REHU' )
    if ( ier .ne. 1 ) stop 6

    ! Merge the PWEATHER data from the second subset into the output message.
    call ufbint (11, r8arr2, 4, 2, ier, 'PRWE TPHR PSW1 PSW2' )
    if ( ier .ne. 1 ) stop 7
    call ufbint (12, r8arr1, 4, 1, ier, 'PRWE TPHR PSW1 PSW2' )
    if ( ier .ne. 1 ) stop 8
    do jj = 1, 4
      r8arr2(jj,2) = r8arr1(jj,1)
    end do
    call ufbovr (21, r8arr2, 4, 2, ier, 'PRWE TPHR PSW1 PSW2' )
    if ( ier .ne. 2 ) stop 9

    ! Write the subset into the output message.
    call writsb (21)
  end do

  ! Close the first output message and write it to the output file.
  call closmg (21)

  ! Check the invmrg output.
  errstr_len = 0
  call mrginv
  if ( ( index( errstr(1:errstr_len), 'NUMBER OF DRB EXPANSIONS  =        3' ) .eq. 0 ) .or. &
       ( index( errstr(1:errstr_len), 'NUMBER OF MERGES          =       42' ) .eq. 0 ) ) stop 10

  ! Read the second data message from infile1, which includes a new preceding DX BUFR table.
  call readmg ( 11, cmgtag, imgdt, ier )
  if ( ier .ne. 0 ) stop 11

  ! Copy the new DX BUFR table to the output file.
  call wrdxtb (11, 21)

  ! Copy the data message to the output file.
  call copymg (11, 21)

  ! Read the third data message from infile1.
  call readmg ( 11, cmgtag, imgdt, ier )
  if ( ier .ne. 0 ) stop 12

  ! Get a count of the number of data subsets in the message.
  jj = nmsub(11)
  if ( jj .ne. 660 ) stop 13

  ! Open a new BUFR message for output.
  call openmb ( 21, cmgtag, imgdt )

  ! Copy the third data message subset-by-subset into the output file.
  do ii = 1, jj 
   if ( icopysb (11, 21) .ne. 0 ) stop 14
  end do

  ! Close the output file.
  call closbf(21)

end program outtest9
