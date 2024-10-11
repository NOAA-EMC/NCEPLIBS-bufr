! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_13 with multiple delayed replication factors'
!
! J. Ator, 10/7/2024

program outtest13

  implicit none

  integer*4 ireadsb

  integer ier, ii, ios1, ios2, imgdt, nrept, nchn(1), ndrpcs(3), nvals

  character cmgtag*8, ctagpr*10

  integer, parameter :: mxvals = 5000

  real*8 r8drp(3), r8vals ( mxvals )

  print *, 'Testing writing OUT_13 with multiple delayed replication factors'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  ! Open the BUFR input and output files.

  open ( unit = 11, file = 'testfiles/OUT_13_infile', iostat = ios1 )
  open ( unit = 12, file = 'testfiles/OUT_13_bufrtab', iostat = ios2 )
  if ( any( (/ios1,ios2/) /= (/0,0/) ) ) stop 1
  open ( unit = 13, file = 'out13.bufr', form ='unformatted' )

  call openbf ( 11, 'SEC3', 11 )
  call openbf ( 13, 'OUT', 12 )

  call cmpmsg ( 'Y' )
  call pkvs01 ( 'BEN', 4 )
  call maxout ( 199900 )

  ! Set the location of the master BUFR tables.
  call mtinfo ( '../tables', 90, 91 )

  call readmg ( 11, cmgtag, imgdt, ier )
  if ( ier /= 0 ) stop 2

  nrept = 0

  do while ( ireadsb ( 11 ) == 0 )
    nrept = nrept + 1

    if ( nrept == 1 ) then
      ! Get the number of delayed replication levels for each subset in the message.  Since the message is compressed,
      ! then these counts are guaranteed to be identical for each subset in the message.
      ! First, get the number of channels.
      call gettagpr ( 11, 'SCRA', 1, ctagpr, ier )
      ctagpr = '(' // trim( ctagpr ) // ')'
      call ufbint ( 11, r8drp, 1, 3, ier, ctagpr )
      nchn(1) = nint ( r8drp(1) )
      if ( nchn(1) /= 500 ) stop 3
      ! Next, get the number of principal component scores within each of the 3 replications.
      call gettagpr ( 11, 'NNPCS', 1, ctagpr, ier )
      ctagpr = '(' // trim( ctagpr ) // ')'
      call ufbrep ( 11, r8drp, 1, 3, ier, ctagpr )
      do ii = 1, 3
        ndrpcs(ii) = nint ( r8drp(ii) )
      enddo
      if ( any( (/ndrpcs(1),ndrpcs(2),ndrpcs(3)/) /= (/90,120,90/) ) ) stop 4
      ! Now, compute the total number of values in each subset.
      nvals = ( nchn(1) * 2 ) + ndrpcs(1) + ndrpcs(2) + ndrpcs(3) + 322
    endif

    ! Read in all of the data values for the subset.
    call ufbseq ( 11, r8vals, mxvals, 1, ier, 'IASIL1CT' )

    ! Write out all of the data values for the subset.
    call openmb ( 13, 'NC021039', imgdt )
    call drfini ( 13, nchn, 1, '(IASICHN)' )
    call drfini ( 13, ndrpcs, 3, '(IASIPCS)' )
    call ufbseq ( 13, r8vals, mxvals, 1, ier, 'NC021039' )
    call writsb ( 13 )
  enddo

  if ( nrept /= 120 ) stop 5

  call closbf ( 13 )

end program outtest13
