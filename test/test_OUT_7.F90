program test_OUT_7

#ifdef INTSIZE_8
  integer*4	ireadmg, icopysb
#endif

  character cmgtag*8

  integer imesg(10), isub(3)

  real*8 r8vals(2,5)

!-----------------------------------------------------------------------

  print *, '----------------------------------------------------'
  print *, 'testing BUFRLIB: writing OUT_7'
  print *, '  using UFBMEX, UFBRMS, UFBMMS, CPYMEM, and ICOPYSB'
  print *, '  using FORTRAN_OPEN and FORTRAN_CLOSE'
  print *, '  reading integer values larger than 32 bits'
  print *, '----------------------------------------------------'

#ifdef INTSIZE_8
  call setim8b ( .true. )
#endif

! Open the input and output files.

  call fortran_open ( 'testfiles/OUT_7_infile1', 21, 'unformatted', 'rewind', iostat1 )
  call fortran_open ( 'testfiles/OUT_7_infile2', 22, 'unformatted', 'rewind', iostat2 )

  if ( ( iostat1 == 0 ) .and. ( iostat2 == 0 ) ) then
    print *, '        FORTRAN_OPEN'
  else
    call bort ( 'FORTRAN_OPEN FAILURE!!')
  end if

  open ( unit = 23, file = 'testfiles/OUT_7_bufrtab')

  open ( unit = 50, file = 'out7.bufr', form = 'unformatted')
  call openbf ( 50, 'OUT', 23 )
        
! Read the input files into internal memory arrays.

  call ufbmex ( 21, 23, 0, icnt1, imesg )
  call ufbmex ( 22, 23, 1, icnt2, imesg )

  if ( ( icnt1 == 1 ) .and. ( icnt2 == 3 ) .and. &
       ( imesg(1) == 5 ) .and. ( imesg(2) == 8 ) .and. ( imesg(3) == 2 ) .and. ( imesg(4) == 0 ) ) then
    print *, '        UFBMEX'
  else
    call bort ( 'UFBMEX FAILURE!!')
  end if

! Check some specified values within the 1st subset of the 4th message.

  call ufbrms ( 4, 1, r8vals, 2, 5, nlv, 'SWDE A2CFDFS' )

  if ( ( nlv == 5 ) .and. &
      ( nint(r8vals(1,1)*10000000) == 325011000 ) .and. ( nint(r8vals(1,2)*10000000) == 264548000 ) .and. &
      ( nint(r8vals(1,3)*10000000) == 419641000 ) .and. ( nint(r8vals(1,4)*10000000) == 289883000 ) .and. &
      ( nint(r8vals(1,5)*10000000) == 117109000 ) .and. ( nint(r8vals(2,1)*10000) == 6140 ) .and. &
      ( nint(r8vals(2,2)*10000) == 5909 ) .and. ( nint(r8vals(2,3)*10000) == 7302 ) .and. &
      ( nint(r8vals(2,4)*10000) == 7413 ) .and. ( nint(r8vals(2,5)*10000) == 4968 ) ) then
    print *, '        UFBRMS'
  else
    call bort ( 'UFBRMS FAILURE!!')
  end if

! Check some specified values within the 633rd subset of the 2nd message.

  call ufbmms ( 2, 633, cmgtag, idate )

  call ufbint ( 21, r8vals, 2, 5, nlv, 'CLATH CLONH' )

  if ( ( cmgtag == 'NC008032' ) .and. ( idate == 22053116 ) .and. ( nlv == 1 ) .and. &
      ( nint(r8vals(1,1)*100000) == 4081139 ) .and. ( nint(r8vals(2,1)*100000) == -7787666 ) ) then
    print *, '        UFBMMS'
  else
    call bort ( 'UFBMMS FAILURE!!')
  end if

! Copy the 3rd message to the output file.

  call rdmemm ( 3, cmgtag, idate, ier )

  if ( ( ier == 0 ) .and. ( cmgtag == 'NC002104' ) ) then
    call cpymem ( 50 )
    print *, '        CPYMEM'
  end if

! Stop using the internal memory arrays, and instead now re-open the 1st input file as a regular file and
! read the 1st message.

  call closbf ( 21 )
  call fortran_open ( 'testfiles/OUT_7_infile1', 21, 'unformatted', 'rewind', iostat )
  call openbf ( 21, 'IN', 23 )
  if ( ireadmg ( 21, cmgtag, idate ) /= 0 ) call bort ( 'IREADMG FAILURE!!' )

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
    if ( ( icopysb ( 21, 50 ) ) /= 0 ) call bort ( 'ICOPYSB FAILURE!!' )
    istart = istart + 1
  end do

  print *, '        ICOPYSB'

! Close the input and output files.

  call fortran_close ( 21, iostat1 )
  call fortran_close ( 22, iostat2 )

  if ( ( iostat1 == 0 ) .and. ( iostat2 == 0 ) ) then
    print *, '        FORTRAN_CLOSE'
  else
    call bort ( 'FORTRAN_CLOSE FAILURE!!')
  end if

  call closbf ( 50 )

  stop

end program test_OUT_7
