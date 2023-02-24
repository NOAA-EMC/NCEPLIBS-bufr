! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_6' using UFBMEM, RDMEMM, UFBMNS, and UFBTAM
!
! J. Ator, 2/23/23
program intest6
  implicit none
 
  integer*4 nmsub

  integer iyr, imon, iday, ihour, imgdt, ier, icnt1, icnt2, iunt1, iunt2, nsub

  integer mxr8pm, mxr8lv
  parameter ( mxr8pm = 2 )
  parameter ( mxr8lv = 19000 )

  real*8 r8vals (mxr8pm, mxr8lv)

  character cmgtag*8

  print *, 'Testing reading IN_6 using UFBMEM, RDMEMM, UFBMNS, and UFBTAM'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  open ( unit = 21, file = 'testfiles/IN_6_infile1', form = 'unformatted')
  open ( unit = 22, file = 'testfiles/IN_6_infile2', form = 'unformatted')

  ! Verify the Section 1 date-time in the first data message of one of the input files.
  call datebf ( 22, iyr, imon, iday, ihour, imgdt )
  if ( ( imgdt .ne. 21031900 ) .or. ( iyr .ne. 21 ) .or. ( iday .ne. 19 ) ) stop 1

  ! Rewind that input file.
  rewind ( 22 )

  ! Open both input files and read the contents into internal arrays.
  call ufbmem ( 21, 0, icnt1, iunt1 )
  call ufbmem ( 22, 1, icnt2, iunt2 )
  if ( ( icnt1 .ne. 926 ) .or. ( icnt2 .ne. 344 ) .or. ( iunt1 .ne. 21 ) .or. ( iunt2 .ne. 21 ) ) stop 2

  ! Locate message #167 within the internal arrays and verify some values.
  call rdmemm ( 167, cmgtag, imgdt, ier )
  if ( ( cmgtag .ne. 'NC004002' ) .or. ( imgdt .ne. 21031713 ) .or. ( nmsub(iunt2) .ne. 3 ) ) stop 3

  ! Locate subset #18364 within the internal arrays and verify some values.
  call ufbmns ( 18364, cmgtag, imgdt )
  if ( ( cmgtag .ne. 'NC002003' ) .or. ( imgdt .ne. 21031900 ) .or. ( nmsub(iunt2) .ne. 2 ) ) stop 4

  ! Scan for certain values across all of the data subsets in the internal arrays, and verify some of them.
  call ufbtam ( r8vals, mxr8pm, mxr8lv, nsub, 'CLAT CLON' )
  if ( ( nsub .ne. 18447 ) .or. &
      ( nint(r8vals(1,1285)*100) .ne. 4328 ) .or. ( nint(r8vals(2,1285)*100) .ne. -7910 ) .or. &
      ( nint(r8vals(1,5189)*100) .ne. 3918 ) .or. ( nint(r8vals(2,5189)*100) .ne. 11638 ) .or. &
      ( nint(r8vals(1,17961)*100) .ne. 3070 ) .or. ( nint(r8vals(2,17961)*100) .ne. 10383 ) ) stop 5

  print *, 'SUCCESS!'
end program intest6
