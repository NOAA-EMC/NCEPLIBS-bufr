! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_8 using UFBCPY and CWBMG.
!
! J. Ator, 2/24/2023
program outtest8
  implicit none

  integer, parameter :: mxbf = 28000
  integer, parameter :: mxbfd4 = mxbf/4

  integer ibfmg(mxbfd4), imgdt, mtyp, lenbmg

  integer*4 ireadns
  integer*4 nbyt, ierw

  character bfmg(mxbf)

  character*20 filnam / 'out8.bufr' /

  character filost / 'w' /

  equivalence (bfmg(1), ibfmg(1))

  character cmgtag*8

  print *, 'Testing writing OUT_8 using UFBCPY and CWBMG'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  open ( unit = 11, file = 'testfiles/OUT_8_infile', form = 'unformatted')

  ! Verify the type of the first (and only) message in the input file.
  call mesgbf ( 11, mtyp )
  if ( mtyp /= 1 ) stop 1

  ! For this test we need to have an assigned logical unit for use with ufbcpy; however, we're not going to
  ! actually write anything to that file, because instead we'll be using writsa and cwbmg to write to our
  ! output file.  So /dev/null is a good choice for this logical unit.
  open ( unit = 12, file = '/dev/null' )

  call openbf ( 11, 'IN', 11 )
  call openbf ( 12, 'NUL', 11 )

  call maxout ( 25000 )

  ! Copy all of the data subsets into an output message.
  do while ( ireadns ( 11, cmgtag, imgdt ) .eq. 0 )
    call openmb ( 12, cmgtag, imgdt )
    call ufbcpy ( 11, 12 )
    call writsa ( 12, mxbfd4, ibfmg, lenbmg )
  end do

  ! Get the completed output message.
  call writsa ( -12, mxbfd4, ibfmg, lenbmg )
  if ( lenbmg .eq. 0 ) stop 2

  ! Open the output file.
  call cobfl ( filnam, filost )

  ! Write the output message to the output file.
#ifdef KIND_8
  nbyt = lenbmg * 8
#else
  nbyt = lenbmg * 4
#endif
  call cwbmg ( bfmg, nbyt, ierw )

  ! Close the output file.
  call ccbfl()

end program outtest8