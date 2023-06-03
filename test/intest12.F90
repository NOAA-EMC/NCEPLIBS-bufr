! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_12' to test ERRWRT branches in USRTPL.
!
! J. Ator, 6/2/2023

module Share_errstr_intest12
  ! This module is needed in order to share information between the test program and subroutine errwrt, because
  ! the latter is not called by the former but rather is called directly from within the NCEPLIBS-bufr software.
  
  character(len=:), allocatable :: errstr
  
  integer errstr_len
end module Share_errstr_intest12

subroutine errwrt(str)
  ! This subroutine supersedes the subroutine of the same name within the NCEPLIBS-bufr software, so that we can
  ! easily test the generation of error messages from within the library.
  
  use Share_errstr_intest12
  
  character*(*) str
  
  integer str_len
  
  str_len = len(str)
  errstr ( errstr_len + 1 : errstr_len + str_len + 1 ) = str
  errstr_len = errstr_len + str_len
  
  return
end subroutine errwrt

program intest12
  use Share_errstr_intest12

  implicit none

  integer*4 ireadmg, ireadsb, nmsub, igetsc

  integer icnt, idate, ios1, ii, iret

  character cmgtag*8

  print *, 'Testing reading IN_12 to test ERRWRT branches in USRTPL'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  allocate ( character(len=120000) :: errstr )
  errstr_len = 0

  ! Open the input file.
  open ( unit = 21, file = 'testfiles/IN_12', form = 'unformatted', iostat = ios1 )
  if ( ios1 .ne. 0 ) stop 1
  call openbf ( 21, 'IN', 21 )

  ! Subroutine readmg will automatically read past the DX table messages at the front of the file.  However,
  ! we also want to read past the 2 dummy messages at the start of the file which contain the dump center and
  ! initiation times, in order to get to the first message containing actual data subsets.
  do ii = 1, 3
      if ( ireadmg ( 21, cmgtag, idate ) .ne. 0 ) stop 2
  enddo
  icnt = nmsub (21)
  if ( icnt .ne. 215 ) stop 3

  ! Read through all of the subsets in the message.  Increase the print verbosity for the last subset to
  ! check some errwrt branches.
  do ii = 1, icnt
      errstr_len = 0
      if ( ii .eq. icnt ) call openbf ( 21, 'QUIET', 2 )
      if ( ireadsb ( 21 ) .ne. 0 ) stop 4
      if ( ii .eq. icnt ) then
          call openbf ( 21, 'QUIET', 0 )
          if ( ( index( errstr(1:errstr_len), ':INVN:NBMP:TAG(INODE(LUN)) =   1:     66:  431:NC021206' ) .eq. 0 ) .or. &
               ( index( errstr(1:errstr_len), ':NEWN:NBMP:NVAL(LUN) = (CRCHNM)  :    3:  431' ) .eq. 0 ) ) stop 5
      endif
  enddo

  ! Try to read the first subset of the next message, which should trigger a different errwrt branch.
  errstr_len = 0
  if ( ireadmg ( 21, cmgtag, idate ) .ne. 0 ) stop 6
  call readsb ( 21, iret )
  if ( ( iret .ne. -1 ) .or. ( igetsc ( 21 ) .ne. 1 ) .or. &
       ( index( errstr(1:errstr_len), 'USRTPL - REPLICATION FACTOR OVERFLOW' ) .eq. 0 ) ) stop 7

  deallocate ( errstr )

  print *, 'SUCCESS!'
end program intest12
