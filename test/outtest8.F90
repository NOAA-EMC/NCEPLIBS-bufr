! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_8 using UFBCPY and CWBMG.
!
! J. Ator, 2/24/2023
program outtest8
  use bufr_interface

  implicit none

  integer, parameter :: mxbf = 28000
  integer, parameter :: mxbfd4 = mxbf/4

  integer ibfmg(mxbfd4), imgdt, mtyp, lenbmg, imtvo, imtvn, iusno, iusnn

  integer*4 ireadns, iupbs01
  integer*4 nbyt, ierw

  character bfmg(mxbf)

  character*20 filnam / 'out8.bufr' /

  character filost / 'w' /

  equivalence (bfmg(1), ibfmg(1))

  character cmgtag*8

  print *, 'Testing writing OUT_8 using UFBCPY and CWBMG_C'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  open ( unit = 11, file = 'testfiles/OUT_8_infile', form = 'unformatted')

  ! Verify the type of the first (and only) message in the input file.
  call mesgbf ( 11, mtyp )
  if ( mtyp /= 1 ) stop 1

  ! For this test we need to have an assigned logical unit for use with ufbcpy; however, we're not going to
  ! actually write anything to that file, because instead we'll be using writsa and cwbmg_c to write to our
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
  call cobfl_c ( filnam, filost )

  ! Write the output message to the output file.
#ifdef KIND_8
  ! See issue #300.
  ! For some reason the following code line squawks with a -Wconversion warning, so for now we'll just
  ! hardcode a workaround since lenbmg should have a value of 2237 when using 8-byte integers.
  !   nbyt = int(lenbmg,4) * 8
  if ( lenbmg .ne. 2237 ) stop 3
  nbyt = 2237 * 8
#else
  nbyt = lenbmg * 4
#endif
  call cwbmg_c ( bfmg, nbyt, ierw )

  ! Try overwriting a couple of values in Section 1 of the memory copy of the message.
  imtvo = iupbs01( ibfmg, 'MTV' )
  call pkbs1( 39, ibfmg, 'MTV' )
  imtvn = iupbs01( ibfmg, 'MTV' )
  iusno = iupbs01( ibfmg, 'USN' )
  call pkbs1( 1, ibfmg, 'USN' )
  iusnn = iupbs01( ibfmg, 'USN' )
  if ( any( (/imtvo,imtvn,iusno,iusnn/) .ne. (/36,39,0,1/) ) ) stop 4

  ! Close the output file.
  call ccbfl_c()

end program outtest8
