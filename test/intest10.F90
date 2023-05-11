! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_10' to test ERRWRT branches in ARALLOCF, STATUS, UFBMEM, UFBMEX, and OPENBT.
!
! J. Ator, 3/13/2023

module Share_errstr_intest10
  ! This module is needed in order to share information between the test program and subroutine errwrt, because
  ! the latter is not called by the former but rather is called directly from within the NCEPLIBS-bufr software.
  
  character*4000 errstr
  
  integer errstr_len
end module Share_errstr_intest10

subroutine errwrt(str)
  ! This subroutine supersedes the subroutine of the same name within the NCEPLIBS-bufr software, so that we can
  ! easily test the generation of error messages from within the library.
  
  use Share_errstr_intest10
  
  character*(*) str
  
  integer str_len
  
  str_len = len(str)
  errstr ( errstr_len + 1 : errstr_len + str_len + 1 ) = str
  errstr_len = errstr_len + str_len
  
  return
end subroutine errwrt

program intest10
  use Share_errstr_intest10

  implicit none

  integer*4 isetprm

  integer icnt, iunt, imesg(150), idate, iret, ios1, ios2, lundx, lun, il, im, imsg

  character cmgtag*8

  print *, 'Testing reading IN_10 to test ERRWRT branches in ARALLOCF, STATUS, UFBMEM, UFBMEX, and OPENBT'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  if ( ( isetprm ( 'MAXMSG', 125 ) .ne. 0 ) .or. ( isetprm ( 'MAXMEM', 125000 ) .ne. 0 ) ) stop 1

  ! Test the errwrt branch in status.
  call status ( 21, lun, il, im )
  if ( index( errstr(1:errstr_len), 'STATUS WAS CALLED WITHOUT HAVING PREVIOUSLY CALLED OPENBF' ) .eq. 0 ) stop 2

  ! Test some various out-of-bounds verbosity settings, and test the errwrt branch in arallocf.
  ! The verbosity level is the 3rd argument whenever the 2nd argument to openbf is 'QUIET'.  Any
  ! request greater than 3 should automatically reset internally to the max value of 2, and any
  ! request less than -1 should automatically reset internally to the min value of -1.
  errstr_len = 0
  call openbf ( 21, 'QUIET', 3 )
  if ( index( errstr(1:errstr_len), 'ARRAYS WILL BE DYNAMICALLY ALLOCATED USING THE FOLLOWING VALUES' ) .eq. 0 ) stop 3
  call openbf ( 21, 'QUIET', -2 )
  call openbf ( 21, 'QUIET', 1 )

  ! Test the errwrt branches in ufbmem.
  open ( unit = 21, file = 'testfiles/IN_10_infile1', form = 'unformatted', iostat = ios1 )
  open ( unit = 22, file = 'testfiles/IN_10_infile2', form = 'unformatted', iostat = ios2 )
  if ( ( ios1 .ne. 0 ) .or. ( ios2 .ne. 0 ) ) stop 4
  errstr_len = 0
  call ufbmem ( 21, 0, icnt, iunt )
  if ( ( icnt .ne. 125 ) .or. & 
      ( index( errstr(1:errstr_len), 'UFBMEM - THE NO. OF MESSAGES REQUIRED TO STORE ALL MESSAGES' ) .eq. 0 ) ) stop 5
  call ufbmem ( 22, 0, icnt, iunt )
  if ( ( icnt .ne. 97 ) .or. & 
      ( index( errstr(1:errstr_len), 'UFBMEM - THE NO. OF BYTES REQUIRED TO STORE ALL MESSAGES' ) .eq. 0 ) ) stop 6

  ! Test some errwrt branches in rdmemm (via readmm).
  errstr_len = 0
  imsg = 0
  call readmm ( imsg, cmgtag, idate, iret )
  if ( index( errstr(1:errstr_len), 'REQUESTED MEMORY MESSAGE NUMBER {FIRST (INPUT) ARGUMENT} IS 0' ) .eq. 0 ) stop 7
  imsg = 350
  call readmm ( imsg, cmgtag, idate, iret )
  if ( index( errstr(1:errstr_len), '1ST (INPUT) ARG.} > # OF MESSAGES IN MEMORY' ) .eq. 0 ) stop 8

  ! Reset the input files.
  call closbf ( 21 )
  call closbf ( 22 )
  open ( unit = 21, file = 'testfiles/IN_10_infile1', form = 'unformatted', iostat = ios1 )
  open ( unit = 22, file = 'testfiles/IN_10_infile2', form = 'unformatted', iostat = ios2 )
  if ( ( ios1 .ne. 0 ) .or. ( ios2 .ne. 0 ) ) stop 9

  ! Test the errwrt branches in ufbmex.
  errstr_len = 0
  call ufbmex ( 21, 21, 0, icnt, imesg )
  if ( ( icnt .ne. 125 ) .or. & 
      ( index( errstr(1:errstr_len), 'UFBMEX - THE NO. OF MESSAGES REQUIRED TO STORE ALL MESSAGES' ) .eq. 0 ) ) stop 10
  call ufbmex ( 22, 22, 0, icnt, imesg )
  if ( ( icnt .ne. 97 ) .or. & 
      ( index( errstr(1:errstr_len), 'UFBMEX - THE NO. OF BYTES REQUIRED TO STORE ALL MESSAGES' ) .eq. 0 ) ) stop 11

  ! Test the errwrt branch in openbt, both indirectly and directly.
  errstr_len = 0
  call rdmemm ( 50, cmgtag, idate, iret )
  if ( index( errstr(1:errstr_len), 'OPENBT - THIS IS A DUMMY BUFRLIB ROUTINE' ) .eq. 0 ) stop 12
  errstr_len = 0
  call openbt ( lundx, 255 )
  if ( index( errstr(1:errstr_len), 'OPENBT - THIS IS A DUMMY BUFRLIB ROUTINE' ) .eq. 0 ) stop 13

  print *, 'SUCCESS!'
end program intest10
