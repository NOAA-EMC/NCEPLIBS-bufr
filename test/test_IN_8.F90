program test_IN_8

  integer, parameter :: MXR8PM = 15
  integer, parameter :: MXR8LV = 500
  integer, parameter :: MXR8EN = 10

  integer ibit1 ( 6 ), ibit2 ( 6 )

  real*8 hdr ( 6, 1 )
  real*8 r8vals ( MXR8PM, MXR8LV, MXR8EN )
  real*8 r8v1, r8v2

  character*8 mnem1, mnem2

!-----------------------------------------------------------------------

  print *, '----------------------------------------------------'
  print *, 'testing BUFRLIB: reading IN_8'
  print *, '  using RDMGSB, UFBEVN, UFBQCD, and UFBQCP to read prepbufr file'
  print *, '----------------------------------------------------'

#ifdef INTSIZE_8
  call setim8b ( .true. )
#endif

  open ( unit = 11, file = 'testfiles/data/prepbufr', form ='unformatted' )

! read the 3rd subset from the 27th message of the prepbufr file and check some values

  call rdmgsb ( 11, 27, 3 )

  call ufbint ( 11, hdr, 6, 1, ier, 'XOB YOB ELV TYP T29 ITP' )
  if ( ( NINT(hdr(1,1)*100) == 30233 ) .and. ( NINT(hdr(2,1)*100) == -1900 ) .and. &
       ( NINT(hdr(3,1)) == 142 ) .and. ( NINT(hdr(4,1)) == 120 ) .and. &
       ( NINT(hdr(5,1)) == 11 ) .and. ( NINT(hdr(6,1)) == 80 ) ) then
    print *, '        RDMGSB -> OK'
  else
    print *, '        RDMGSB -> FAILED!!'
  endif

! get all of the moisture data from this subset and check some values

  call ufbevn ( 11, r8vals, MXR8PM, MXR8LV, MXR8EN, ilv, 'QOB QQM QPC QRC' )

  if ( ( ilv == 51 ) .and. &
       ( NINT(r8vals(1,2,2)) == 17895 ) .and. ( NINT(r8vals(2,2,2)) == 2 ) .and. &
       ( NINT(r8vals(3,2,2)) == 1 ) .and. ( NINT(r8vals(4,2,2)) == 100 ) .and. &
       ( NINT(r8vals(1,36,1)) == 126 ) .and. ( NINT(r8vals(2,36,1)) == 9 ) .and. &
       ( NINT(r8vals(3,36,1)) == 8 ) .and. ( NINT(r8vals(4,36,1)) == 1 ) .and. &
       ( NINT(r8vals(1,50,3)) == 3 ) .and. ( NINT(r8vals(2,50,3)) == 15 ) .and. &
       ( NINT(r8vals(3,50,3)) == 1 ) .and. ( NINT(r8vals(4,50,3)) == 100 ) ) then
    print *, '        UFBEVN -> OK'
  else
    print *, '        UFBEVN -> FAILED!!'
  endif

! now, get all of the temperature data from this subset which meets the conditions of being on 
! a level where the pressure is between 490mb and 44mb, and check some of those values

  call ufbevn ( 11, r8vals, MXR8PM, MXR8LV, MXR8EN, ilv, 'POB<490 POB>44 POB TOB TQM TPC TRC' )

  if ( ( ilv == 33 ) .and. &
       ( NINT(r8vals(1,5,1)) == 378 ) .and. ( NINT(r8vals(2,5,1)*10) == -149 ) .and. &
       ( NINT(r8vals(4,5,1)) == 8 ) .and. ( NINT(r8vals(2,5,2)*10) == -151 ) .and. &
       ( NINT(r8vals(5,5,2)) == 100 ) .and. ( NINT(r8vals(1,29,1)*10) == 699 ) .and. &
       ( NINT(r8vals(2,29,1)*10) == -809 ) .and. ( NINT(r8vals(3,29,1)) == 2 ) ) then
    print *, '        UFBEVN w/conditions -> OK'
  else
    print *, '        UFBEVN w/conditions -> FAILED!!'
  endif

! other checks

  call ufbqcd ( 11, 'RADCOR', iqcd1 )
  call ufbqcd ( 11, 'ACARSQC', iqcd2 )
  if ( ( iqcd1 == 6 ) .and. ( iqcd2 == 14 ) ) then
    print *, '        UFBQCD -> OK'
  else
    print *, '        UFBQCD -> FAILED!!'
  endif

  call ufbqcp ( 11, 2, mnem1 )
  call ufbqcp ( 11, 8, mnem2 )
  if ( ( mnem1(1:7) .eq. 'SYNDATA' ) .and. ( mnem2(1:6) .eq. 'VIRTMP' ) ) then
    print *, '        UFBQCP -> OK'
  else
    print *, '        UFBQCP -> FAILED!!'
  endif

  r8v1 = 224.
  call upftbv ( 11, 'RSRD', r8v1, 6, ibit1, nib1 )
  r8v2 = 264192.
  call upftbv ( 11, 'WVCQ', r8v2, 6, ibit2, nib2 )
  if ( ( nib1 == 3 ) .and. ( ibit1(1) == 2 ) .and. ( ibit1(2) == 3 ) .and. ( ibit1(3) == 4 ) .and. &
       ( nib2 == 2 ) .and. ( ibit2(1) == 6 ) .and. ( ibit2(2) == 13 ) ) then
    print *, '        UPFTBV -> OK'
  else
    print *, '        UPFTBV -> FAILED!!'
  endif

  stop

end program test_IN_8
