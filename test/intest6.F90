! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_6' using UFBMEM, RDMEMM, UFBMNS, and UFBTAM
!
! J. Ator, 2/23/2023

module Share_errstr_intest6
  ! This module is needed in order to share information between the test program and subroutine errwrt, because
  ! the latter is not called by the former but rather is called directly from within the NCEPLIBS-bufr software.

  character(len=:), allocatable :: errstr

  integer errstr_len
end module Share_errstr_intest6

subroutine errwrt(str)
  ! This subroutine supersedes the subroutine of the same name within the NCEPLIBS-bufr software, so that we can
  ! easily test the generation of error messages from within the library.

  use Share_errstr_intest6

  character*(*) str

  integer str_len

  str_len = len(str)
  errstr ( errstr_len + 1 : errstr_len + str_len + 1 ) = str
  errstr_len = errstr_len + str_len

  return
end subroutine errwrt

program intest6

  use Share_errstr_intest6

  implicit none
 
  integer*4 nmsub

  integer iyr, imon, iday, ihour, imgdt, ier, icnt, iunt, nsub

  integer mxr8pm, mxr8lv
  parameter ( mxr8pm = 2 )
  parameter ( mxr8lv = 19000 )

  real*8 r8vals (mxr8pm, mxr8lv), r8val

  character cmgtag*8, c8val*8

  equivalence (r8val, c8val)

  print *, 'Testing reading IN_6 using UFBMEM, RDMEMM, UFBMNS, and UFBTAM'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  allocate ( character(len=240000) :: errstr )

  open ( unit = 21, file = 'testfiles/IN_6_infile1', form = 'unformatted')
  open ( unit = 22, file = 'testfiles/IN_6_infile2', form = 'unformatted')

  ! Verify the Section 1 date-time in the first data message of one of the input files.
  call datebf ( 22, iyr, imon, iday, ihour, imgdt )
  if ( ( imgdt .ne. 21031900 ) .or. ( iyr .ne. 21 ) .or. ( iday .ne. 19 ) ) stop 1

  ! Rewind that input file.
  rewind ( 22 )

  ! Open both input files and read the contents into internal arrays.
  call ufbmem ( 21, 0, icnt, iunt )
  if ( ( icnt .ne. 926 ) .or. ( iunt .ne. 21 ) ) stop 2
  call ufbmem ( 22, 1, icnt, iunt )
  if ( ( icnt .ne. 344 ) .or. ( iunt .ne. 21 ) ) stop 3

  ! Locate message #167 within the internal arrays and verify some values.
  call rdmemm ( 167, cmgtag, imgdt, ier )
  if ( ( cmgtag .ne. 'NC004002' ) .or. ( imgdt .ne. 21031713 ) .or. ( nmsub(iunt) .ne. 3 ) ) stop 4

  ! Locate subset #18364 within the internal arrays and verify some values.
  ! Also check an errwrt case while doing this.
  call openbf ( 21, 'QUIET', 2 )
  errstr_len = 0
  call ufbmns ( 18364, cmgtag, imgdt )
  if ( ( cmgtag .ne. 'NC002003' ) .or. ( imgdt .ne. 21031900 ) .or. ( nmsub(iunt) .ne. 2 ) ) stop 5
  if ( ( index( errstr(1:errstr_len), 'RDMEMM - RESETTING TO USE DX TABLE #' ) .eq. 0 ) ) stop 6
  call openbf ( 21, 'QUIET', 0 )

  ! Scan for certain values across all of the data subsets in the internal arrays, and verify some of them.
  call ufbtam ( r8vals, mxr8pm, mxr8lv, nsub, 'CLAT CLON' )
  if ( ( nsub .ne. 18447 ) .or. &
      ( nint(r8vals(1,1285)*100) .ne. 4328 ) .or. ( nint(r8vals(2,1285)*100) .ne. -7910 ) .or. &
      ( nint(r8vals(1,5189)*100) .ne. 3918 ) .or. ( nint(r8vals(2,5189)*100) .ne. 11638 ) .or. &
      ( nint(r8vals(1,17961)*100) .ne. 3070 ) .or. ( nint(r8vals(2,17961)*100) .ne. 10383 ) ) stop 7
  call ufbtam ( r8vals, mxr8pm, mxr8lv, nsub, 'BUHD' )
  if ( nsub .ne. 18447 ) stop 8
  r8val = r8vals(1, 6314)
  if (c8val(1:6) .ne. 'IUAD01') stop 9
  r8val = r8vals(1, 17888)
  if (c8val(1:6) .ne. 'IUSN08') stop 10

  ! Test an errwrt case in ufbtam.
  errstr_len = 0
  call ufbtam ( r8vals, mxr8pm, 1000, nsub, 'BUHD' )
  if ( ( index( errstr(1:errstr_len), 'UFBTAM - THE NO. OF DATA SUBSETS IN MEMORY IS .GT. LIMIT' ) .eq. 0 ) ) stop 11

  print *, 'SUCCESS!'
end program intest6
