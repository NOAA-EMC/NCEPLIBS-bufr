! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_10' to test ERRWRT branches in ARALLOCF, UFBMEM, UFBMEX, and OPENBT.
!
! J. Ator, 3/13/2023

module Share_errstr
  ! This module is needed in order to share information between the test program and subroutine errwrt, because
  ! the latter is not called by the former but rather is called directly from within the BUFRLIB software.
  
  character*4000 errstr
  
  integer errstr_len
end module Share_errstr

subroutine errwrt(str)
  ! This subroutine supersedes the subroutine of the same name within the BUFRLIB software, so that we can
  ! easily test the generation of error messages from within the library.
  
  use Share_errstr
  
  character*(*) str
  
  integer str_len
  
  str_len = len(str)
  errstr ( errstr_len + 1 : errstr_len + str_len + 1 ) = str
  errstr_len = errstr_len + str_len
  
  return
end subroutine errwrt

program intest10
  use Share_errstr

  implicit none

  integer*4 isetprm

  integer icnt, iunt, imesg(150), idate, iret

  character cmgtag*8

  print *, 'Testing reading IN_10 to test ERRWRT branches in ARALLOCF, UFBMEM, UFBMEX, and OPENBT'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  if ( ( isetprm ( 'MAXMSG', 125 ) .ne. 0 ) .or. ( isetprm ( 'MAXMEM', 125000 ) .ne. 0 ) ) stop 1

  ! Test some various out-of-bounds verbosity settings, and test the errwrt branch in arallocf.
  errstr_len = 0
  call openbf ( 21, 'QUIET', 3 )
  if ( index( errstr(1:errstr_len), 'ARRAYS WILL BE DYNAMICALLY ALLOCATED USING THE FOLLOWING VALUES' ) .eq. 0 ) stop 2
  call openbf ( 21, 'QUIET', -2 )
  call openbf ( 21, 'QUIET', 1 )

  ! Test the errwrt branches in ufbmem.
  open ( unit = 21, file = 'testfiles/IN_10_infile1', form = 'unformatted')
  open ( unit = 22, file = 'testfiles/IN_10_infile2', form = 'unformatted')
  errstr_len = 0
  call ufbmem ( 21, 0, icnt, iunt )
  if ( ( icnt .ne. 125 ) .or. & 
      ( index( errstr(1:errstr_len), 'UFBMEM - THE NO. OF MESSAGES REQUIRED TO STORE ALL MESSAGES' ) .eq. 0 ) ) stop 3
  call ufbmem ( 22, 0, icnt, iunt )
  if ( ( icnt .ne. 97 ) .or. & 
      ( index( errstr(1:errstr_len), 'UFBMEM - THE NO. OF BYTES REQUIRED TO STORE ALL MESSAGES' ) .eq. 0 ) ) stop 4

  ! Reset the input files.
  call closbf ( 21 )
  call closbf ( 22 )
  open ( unit = 21, file = 'testfiles/IN_10_infile1', form = 'unformatted')
  open ( unit = 22, file = 'testfiles/IN_10_infile2', form = 'unformatted')

  ! Test the errwrt branches in ufbmex.
  errstr_len = 0
  call ufbmex ( 21, 21, 0, icnt, imesg )
  if ( ( icnt .ne. 125 ) .or. & 
      ( index( errstr(1:errstr_len), 'UFBMEX - THE NO. OF MESSAGES REQUIRED TO STORE ALL MESSAGES' ) .eq. 0 ) ) stop 5
  call ufbmex ( 22, 22, 0, icnt, imesg )
  if ( ( icnt .ne. 97 ) .or. & 
      ( index( errstr(1:errstr_len), 'UFBMEX - THE NO. OF BYTES REQUIRED TO STORE ALL MESSAGES' ) .eq. 0 ) ) stop 6

  ! Test the errwrt branch in openbt.
  errstr_len = 0
  call rdmemm ( 50, cmgtag, idate, iret )
  if ( index( errstr(1:errstr_len), 'OPENBT - THIS IS A DUMMY BUFRLIB ROUTINE' ) .eq. 0 ) stop 7

  print *, 'SUCCESS!'
end program intest10
