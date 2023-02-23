! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_7' using UFBMEX, UFBRMS, UFBMMS, CPYMEM, ICOPYSB, FORTRAN_OPEN and FORTRAN_CLOSE,
! and reading integer values larger than 32 bits.
!
! J. Ator, 2/17/23
program outtest7
  implicit none

  integer*4 ireadmg, icopysb

  integer imesg(10), isub(3), iostat1, iostat2, icnt1, icnt2, idate, ier, ii, istart, nlv

  character cmgtag*8

  real*8 r8vals(2,5)

  print *, 'Testing writing OUT_7 using UFBMEX, UFBRMS, UFBMMS, CPYMEM, ICOPYSB, FORTRAN_OPEN and FORTRAN_CLOSE, '
  print *, 'and reading integer values larger than 32 bits'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  ! Open the input files.
  call fortran_open ( 'testfiles/OUT_7_infile1', 21, 'unformatted', 'rewind', iostat1 )
  call fortran_open ( 'testfiles/OUT_7_infile2', 22, 'unformatted', 'rewind', iostat2 )
  if ( ( iostat1 .ne. 0 ) .or. ( iostat2 .ne. 0 ) ) stop 1

  ! Open the output file.
  open ( unit = 23, file = 'testfiles/OUT_7_bufrtab')
  open ( unit = 50, file = 'out7.bufr', form = 'unformatted')
  call openbf ( 50, 'OUT', 23 )

  ! Read the input files into internal memory arrays.
  call ufbmex ( 21, 23, 0, icnt1, imesg )
  call ufbmex ( 22, 23, 1, icnt2, imesg )
  if ( ( icnt1 .ne. 1 ) .or. ( icnt2 .ne. 3 ) .or. &
       ( imesg(1) .ne. 5 ) .or. ( imesg(2) .ne. 8 ) .or. ( imesg(3) .ne. 2 ) .or. ( imesg(4) .ne. 0 ) ) stop 2

  ! Check some specified values within the 1st subset of the 4th message.
  call ufbrms ( 4, 1, r8vals, 2, 5, nlv, 'SWDE A2CFDFS' )
  if ( ( nlv .ne. 5 ) .or. &
      ( nint(r8vals(1,1)*10000000) .ne. 325011000 ) .or. ( nint(r8vals(1,2)*10000000) .ne. 264548000 ) .or. &
      ( nint(r8vals(1,3)*10000000) .ne. 419641000 ) .or. ( nint(r8vals(1,4)*10000000) .ne. 289883000 ) .or. &
      ( nint(r8vals(1,5)*10000000) .ne. 117109000 ) .or. ( nint(r8vals(2,1)*10000) .ne. 6140 ) .or. &
      ( nint(r8vals(2,2)*10000) .ne. 5909 ) .or. ( nint(r8vals(2,3)*10000) .ne. 7302 ) .or. &
      ( nint(r8vals(2,4)*10000) .ne. 7413 ) .or. ( nint(r8vals(2,5)*10000) .ne. 4968 ) ) stop 3

  ! Check some specified values within the 633rd subset of the 2nd message.
  call ufbmms ( 2, 633, cmgtag, idate )
  call ufbint ( 21, r8vals, 2, 5, nlv, 'CLATH CLONH' )
  if ( ( cmgtag .ne. 'NC008032' ) .or. ( idate .ne. 22053116 ) .or. ( nlv .ne. 1 ) .or. &
      ( nint(r8vals(1,1)*100000) .ne. 4081139 ) .or. ( nint(r8vals(2,1)*100000) .ne. -7787666 ) ) stop 4

  ! Copy the 3rd message to the output file.
  call rdmemm ( 3, cmgtag, idate, ier )
  if ( ( ier .ne. 0 ) .or. ( cmgtag .ne. 'NC002104' ) ) stop 5
  call cpymem ( 50 )

  ! Stop using the internal memory arrays, and instead now re-open the 1st input file as a regular file and
  ! read the 1st message.
  call closbf ( 21 )
  call fortran_open ( 'testfiles/OUT_7_infile1', 21, 'unformatted', 'rewind', iostat1 )
  call openbf ( 21, 'IN', 23 )
  if ( ireadmg ( 21, cmgtag, idate ) .ne. 0 ) stop 6

  ! Open a new output message, then copy subsets #115, 288, and 530 from the 1st message to that output message.

  call openmg ( 50, 'NC005067', 22060102 )

  isub(1) = 115
  isub(2) = 288
  isub(3) = 530

  istart = 1
  do ii = 1, 3
    do while ( istart < isub(ii) )
      ! calling copysb (or icopysb) with the 2nd argument negative prevents writing to that output file, but
      ! the read pointer in the input (1st argument) file is still advanced to the next subset
      call copysb ( 21, -50, ier )
      istart = istart + 1
    end do
    if ( ( icopysb ( 21, 50 ) ) .ne. 0 ) stop 7
    istart = istart + 1
  end do

  ! Close the input and output files.
  call fortran_close ( 21, iostat1 )
  call fortran_close ( 22, iostat2 )
  if ( ( iostat1 .ne. 0 ) .or. ( iostat2 .ne. 0 ) ) stop 8

  ! Close the output file.
  call closbf ( 50 )

end program outtest7
