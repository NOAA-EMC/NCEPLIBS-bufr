! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_3' using ISETPRM and IGETPRM, using EXITBUFR with multiple allocations,
! and using 2-22, 2-36 and 2-37 operators.
!
! J. Ator, 2/17/23
program outtest3
  implicit none

  integer*4 isetprm, igetprm

  integer ii, nlv

  real*8 r8vals ( 11, 4 ), r8bitmap ( 26 )

  print *, 'Testing writing OUT_3 using ISETPRM and IGETPRM, using EXITBUFR with multiple allocations,'
  print *, 'and using 2-22, 2-36 and 2-37 operators.'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  ! First message.

  ! Set some custom array sizes.
  if ( ( isetprm ( 'NFILES', 2 ) .ne. 0 ) .or. ( isetprm ( 'MXMSGL', 8000 ) .ne. 0 ) ) stop 1

  ! Set some custom Section 1 values.
  call pkvs01 ( 'MTV', 18 )
  call pkvs01 ( 'USN', 2 )

  ! Open the BUFR table and output file.
  open ( unit = 11, file = 'out3.bufr', form ='unformatted')
  open ( unit = 12, file = 'testfiles/OUT_3_bufrtab' )
  call openbf ( 11, 'OUT', 12 )

  ! Confirm the values from the previous isetprm settings.
  if ( ( igetprm ( 'NFILES' ) .ne. 2 ) .or. ( igetprm ( 'MXMSGL' ) .ne. 8000 ) ) stop 2

  ! Write a standard message.
  call stdmsg ('Y')

  ! Store the data values.

  call openmb ( 11, 'FN005000', 2015030212 )

  r8vals(1,1) = 2015
  r8vals(2,1) = 3
  r8vals(3,1) = 2
  r8vals(4,1) = 12
  r8vals(5,1) = 57
  r8vals(6,1) = -12.538
  r8vals(7,1) = 157.66
  r8vals(8,1) = 20170.
  r8vals(9,1) = 37.
  r8vals(10,1) = 2.1
  r8vals(11,1) = 244.5
  call ufbint ( 11, r8vals, 11, 1, nlv, 'YEAR MNTH DAYS HOUR MINU CLATH CLONH PRLC WDIR WSPD CCST' )

  do ii = 1, 26
    r8bitmap(ii) = 0.
  end do
  r8bitmap(16) = 1.
  r8bitmap(17) = 1.
  r8bitmap(18) = 1.
  r8bitmap(21) = 1.
  call ufbrep ( 11, r8bitmap, 1, 26, nlv, 'DPRI' )

  r8vals(1,1) = 7.
  r8vals(2,1) = 51.
  r8vals(1,2) = 254.
  r8vals(2,2) = 1.
  r8vals(1,3) = 254.
  r8vals(2,3) = 3.
  call ufbrep ( 11, r8vals, 11, 3, nlv, 'GCLONG GNAP' )

  r8vals(1,1) = 97.
  r8vals(1,2) = 96.
  r8vals(1,3) = 93.
  r8vals(1,4) = 93.
  call ufbrep ( 11, r8vals, 11, 4, nlv, 'PCCF' )

  r8vals(1,1) = 77.
  r8vals(1,2) = 84.
  r8vals(1,3) = 83.
  r8vals(1,4) = 61.
  call ufbrep ( 11, r8vals, 11, 4, nlv, 'NCTH' )

  call writsb ( 11 )

  ! Reset the library in order to be able to reallocate arrays.
  call exitbufr

  ! Second message.

  ! Set some new custom array sizes.
  if ( ( isetprm ( 'NFILES', 5 ) .ne. 0 ) .or. ( isetprm ( 'MXMSGL', 12000 ) .ne. 0 ) ) stop 3

  ! Set some new custom Section 1 values.
  call pkvs01 ( 'BEN', 4 )
  call pkvs01 ( 'MSBTI', 40 )
  call pkvs01 ( 'MTV', 17 )

  ! Open the BUFR table, and re-open the output file for append.
  open ( unit = 11, file = 'out3.bufr', form ='unformatted')
  open ( unit = 12, file = 'testfiles/OUT_3_bufrtab' )
  call openbf ( 11, 'APX', 12 )

  ! Confirm the values from the previous isetprm settings.
  if ( ( igetprm ( 'NFILES' ) .ne. 5 ) .or. ( igetprm ( 'MXMSGL' ) .ne. 12000 ) ) stop 4

  ! Write a standard message.
  call stdmsg ('Y')

  ! Store the data values.

  call openmb ( 11, 'FN005010', 2015030215 )

  r8vals(1,1) = 2015
  r8vals(2,1) = 3
  r8vals(3,1) = 2
  r8vals(4,1) = 15
  r8vals(5,1) = 44
  r8vals(6,1) = -12.538
  r8vals(7,1) = 157.66
  r8vals(8,1) = 19930.
  r8vals(9,1) = 305.
  r8vals(10,1) = 12.5
  r8vals(11,1) = 233.0
  call ufbint ( 11, r8vals, 11, 1, nlv, 'YEAR MNTH DAYS HOUR MINU CLATH CLONH PRLC WDIR WSPD CCST' )

  do ii = 1, 26
    r8bitmap(ii) = 0.
  end do
  r8bitmap(16) = 1.
  r8bitmap(17) = 1.
  r8bitmap(18) = 1.
  r8bitmap(26) = 1.
  call ufbrep ( 11, r8bitmap, 1, 26, nlv, 'DPRI' )

  r8vals(1,1) = 7.
  r8vals(2,1) = 51.
  r8vals(1,2) = 254.
  r8vals(2,2) = 1.
  r8vals(1,3) = 254.
  r8vals(2,3) = 3.
  call ufbrep ( 11, r8vals, 11, 3, nlv, 'GCLONG GNAP' )

  r8vals(1,1) = 92.
  r8vals(1,2) = 91.
  r8vals(1,3) = 91.
  r8vals(1,4) = 98.
  call ufbrep ( 11, r8vals, 11, 4, nlv, 'PCCF' )

  r8vals(1,1) = 3.
  r8vals(1,2) = 4.
  r8vals(1,3) = 4.
  r8vals(1,4) = 3.
  call ufbrep ( 11, r8vals, 11, 4, nlv, 'MAQC' )

  call writsb ( 11 )

  ! Close the output file.
  call closbf ( 11 )

end program outtest3
