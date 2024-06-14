! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_1' using OPENBF IO = 'OUT' and LUNIN != LUNDX,
! and using 2-03-YYY to change reference values.
!
! J. Ator, 2/16/2023
program outtest1
  implicit none

  real*8 r8ymd(3,1), r8ltl(2,1), r8flv(1,5), r8oth(10,1), r8acrn(1,3), r8val

  integer*4 lcmgdf

  integer nsc(5), nrf(5), nbt(5), ierns(5)
  integer nsa, nra, nba, iernsa, nsm, nrm, nbm, iernsm
  integer iertgp, jj, nlv

  character acrn*10, libvrsn*8, tagpr*6, c8val*8

  equivalence (r8val, c8val)

  print *, 'Testing writing OUT_1 using OPENBF IO = OUT and LUNIN != LUNDX,'
  print *, 'and using 2-03-YYY to change reference values'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  ! Get the library version number, and specify the use of big-endian blocking.
  call bvers ( libvrsn )
  if ( lgt( libvrsn, '10.1.1' ) ) then
     call setblock (1)
  endif

  ! Open the BUFR table and output file.

  open ( unit = 11, file = 'out1.bufr', form ='unformatted')
  open ( unit = 12, file = 'testfiles/OUT_1_bufrtab' )

  call openbf ( 11, 'OUT', 12 )

  ! Write a standard, compressed BUFR message with 3 subsets.  Compression will be implemented using WRITCP.

  call stdmsg ('Y')

  ! First subset.

  call openmb ( 11, 'FR004029', 2012031212 )

  ! Confirm there's exactly one long character string in the subset definition.
  if ( lcmgdf ( 11, 'FR004029' ) /= 1 ) stop 1

  ! Get and check the parent of a Table B mnemonic.
  call gettagpr ( 11, 'MNTH', 1, tagpr, iertgp )
  if ( ( iertgp /= 0 ) .or. ( tagpr /= 'YYMMDD' ) ) stop 2

  ! The output of the following calls will be checked below, after making additional calls to this same
  ! subroutine to verify reference values that will be modified with the 2-03 operator.

  call nemspecs ( 11, 'ACRN', 1, nsa, nra, nba, iernsa )
  call nemspecs ( 11, 'MDEVG', 1, nsm, nrm, nbm, iernsm )

  r8ymd(1,1) = 2012
  r8ymd(2,1) = 3
  r8ymd(3,1) = 12
  call ufbseq ( 11, r8ymd, 3, 1, nlv, 'YYMMDD' )
  r8ltl(1,1) = -35.77
  r8ltl(2,1) = 172.38
  call ufbseq ( 11, r8ltl, 2, 1, nlv, 'LTLONH' )

  ! The r8flv array tests the 2-03 operator.  r8flv(1,2) contains the new reference value, which is
  ! applied to the FLVLST values in r8flv(1,3) and r8flv(1,4) when writing the message.

  r8flv(1,1) = 3500
  r8flv(1,2) = -1000
  r8flv(1,3) = 4000
  r8flv(1,4) = 5750
  r8flv(1,5) = 10722
  call ufbrep ( 11, r8flv, 1, 5, nlv, 'FLVLST')

  r8oth(1,1) = 13
  r8oth(2,1) = 45
  r8oth(3,1) = 235.77
  r8oth(4,1) = 1
  r8oth(5,1) = 5.322
  r8oth(6,1) = 1
  r8oth(7,1) = 3
  r8oth(8,1) = 5
  r8oth(9,1) = 35
  r8oth(10,1) = 10.7
  call ufbint ( 11, r8oth, 10, 1, nlv, 'HOUR MINU TMDB DGOT MDEVG ROLQ INTV DPOF WDIR WSPD' )

  call writcp ( 11 )

  ! We need to run the following check after the above call to WRITCP, because new reference values
  ! aren't stored into a message (nor applied when packing any other values within that message) until
  ! WRITCP internally calls WRITSB, which in turn calls WRTREE, which in turn calls IPKS.
  do jj = 1, 5
     call nemspecs ( 11, 'FLVLST', jj, nsc(jj), nrf(jj), nbt(jj), ierns(jj) )
  end do
  if ( ( iernsa /= 0 ) .or. ( iernsm /= 0 ) .or. ( nba /= 80 ) .or. ( nbm /= 17 ) .or. &
       ( nsm /= 3 ) .or. ( ierns(1) /= 0 ) .or. ( nrf(1) /= -1024 ) .or. ( ierns(2) /= 0 ) .or. &
       ( nrf(2) /= -1024 ) .or. ( nbt(2) /= 12 ) .or. ( ierns(3) /= 0 ) .or. ( nrf(3) /= -1000 ) &
       .or. ( ierns(4) /= 0 ) .or. ( nrf(4) /= -1000 ) .or. ( ierns(5) /= 0 ) .or. &
       ( nrf(5) /= -1024 ) .or. ( nbt(3) /= 16 ) .or. ( nbt(5) /= 16 ) ) stop 3

  ! Write a long character string into the output.
  acrn = 'TESTUPS008'
  call writlc ( 11, acrn, 'ACRN' )

  ! Second subset.

  call openmb ( 11, 'FR004029', 2012031212 )

  call ufbseq ( 11, r8ymd, 3, 1, nlv, 'YYMMDD' )

  r8ltl(2,1) = 172.42
  call ufbseq ( 11, r8ltl, 2, 1, nlv, 'LTLONH' )

  r8flv(1,1) = 3600
  r8flv(1,4) = 5760
  r8flv(1,5) = 10730
  call ufbrep ( 11, r8flv, 1, 5, nlv, 'FLVLST')

  r8oth(2,1) = 48
  r8oth(3,1) = 234.69
  r8oth(5,1) = 5.001
  r8oth(8,1) = 3
  r8oth(9,1) = 30
  r8oth(10,1) = 12.2
  call ufbint ( 11, r8oth, 10, 1, nlv, 'HOUR MINU TMDB DGOT MDEVG ROLQ INTV DPOF WDIR WSPD')

  call writcp ( 11 )

  acrn = 'TESTAAL225'
  call writlc ( 11, acrn, 'ACRN' )

  ! Third subset.

  call openmb ( 11, 'FR004029', 2012031212 )

  call ufbseq ( 11, r8ymd, 3, 1, nlv, 'YYMMDD' )

  r8ltl(2,1) = 172.44
  call ufbseq ( 11, r8ltl, 2, 1, nlv, 'LTLONH' )

  r8flv(1,1) = 3610
  r8flv(1,2) = -1200
  r8flv(1,4) = 5775
  r8flv(1,5) = 10730
  call ufbrep ( 11, r8flv, 1, 5, nlv, 'FLVLST')

  r8oth(2,1) = 51
  r8oth(3,1) = 234.11
  r8oth(5,1) = 5.012
  r8oth(8,1) = 6
  r8oth(10,1) = 12.1
  call ufbint ( 11, r8oth, 10, 1, nlv, 'HOUR MINU TMDB DGOT MDEVG ROLQ INTV DPOF WDIR WSPD')

  call writcp ( 11 )

  acrn = 'TESTSWA193'
  ! note that 'ACRN#1' is functionally equivalent to 'ACRN' in the following call
  call writlc ( 11, acrn, 'ACRN#1' )

  ! Close the output file.
  call closbf ( 11 )

  ! Re-open the output file for reading and use ufbtab to check the compressed acrn values.
  ! Note that with ufbtab we can only look at the first 8 characters of each value.
  open ( unit = 11, file = 'out1.bufr', form ='unformatted')
  call ufbtab ( 11, r8acrn, 1, 3, nlv, 'ACRN')
  if (nlv /= 3 ) stop 4
  r8val = r8acrn(1,1)
  if (c8val /= 'TESTUPS0') stop 5
  r8val = r8acrn(1,2)
  if (c8val /= 'TESTAAL2') stop 6
  r8val = r8acrn(1,3)
  if (c8val /= 'TESTSWA1') stop 7

end program outtest1
