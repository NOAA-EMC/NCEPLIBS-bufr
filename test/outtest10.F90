! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_10' with subsets larger than 65530 bytes and non-matching BUFR DX tables
!
! J. Ator, 4/20/2023

module Share_errstr
  ! This module is needed in order to share information between the test program and subroutine errwrt, because
  ! the latter is not called by the former but rather is called directly from within the NCEPLIBS-bufr software.

  character*1500 errstr

  integer errstr_len
end module Share_errstr

subroutine errwrt(str)
  ! This subroutine supersedes the subroutine of the same name within the NCEPLIBS-bufr software, so that we can
  ! easily test the generation of error messages from within the library.

  use Share_errstr

  character*(*) str

  integer str_len

  str_len = len(str)
  errstr ( errstr_len + 1 : errstr_len + str_len + 1 ) = str
  errstr_len = errstr_len + str_len

  return
end subroutine errwrt

program outtest10
  use Share_errstr

  implicit none

  integer*4 ireadmg

  integer iostat1, iostat2, iostat3, iostat4, mesgtyp, icomp, jdate, mgct, iret
!  integer iostat1, iostat2, iostat3, iostat4, jdate, mgct, iret

  character subset*8

  print *, 'Testing writing OUT_10 with subsets larger than 65530 bytes and non-matching BUFR DX tables'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  errstr_len = 0

  ! Open the files.
  open ( unit = 21, file = 'testfiles/OUT_10_infile1', iostat = iostat1 )
  open ( unit = 22, file = 'testfiles/OUT_10_infile2', iostat = iostat2 )
  open ( unit = 23, file = 'testfiles/OUT_10_bufrtab', iostat = iostat3 )
  open ( unit = 50, file = 'out10.bufr', form = 'unformatted', iostat = iostat4 )
  if ( ( iostat1 .ne. 0 ) .or. ( iostat2 .ne. 0 ) .or. ( iostat3 .ne. 0 ) .or. ( iostat4 .ne. 0 ) ) stop 1

  ! Get some information from infile1.
  call mesgbc ( 21, mesgtyp, icomp )
  if ( ( mesgtyp .ne. -11 ) .or. ( icomp .ne. -2 ) ) stop 2

  ! (Re)open infile1 since the call to mesgbc will have closed it.
  rewind ( 21 )
  open ( unit = 21, file = 'testfiles/OUT_10_infile1', iostat = iostat1 )
  if ( iostat1 .ne. 0 ) stop 3

  ! Open infile2 and the output file to the library.
  call openbf ( 21, 'IN', 21 )
  call openbf ( 22, 'IN', 23 )
  call maxout ( 150000 )
  call openbf ( 50, 'OUT', 21 )

  ! Turn on verbose output so can check error strings.
  call openbf ( 21, 'QUIET', 1 )

  ! Copy 5 data subsets from infile2 to the output file.  Each data subset is inside of its own message,
  ! even though only subsets 1, 3, and 5 are larger than 65530 bytes.  The first 3 copies will exercise
  ! logic in cpyupd, and the last 2 will exercise logic in msgupd. 
  mgct = 0
  do while ( ireadmg ( 22, subset, jdate ) .eq. 0 )
    mgct = mgct + 1
    errstr_len = 0
    call openmb ( 50, subset, jdate )
    if ( mgct .le. 3 ) then
      call copysb ( 22, 50, iret )
      if ( iret .ne. 0 ) stop 4
      if ( ( mod(mgct,2) .eq. 1 ) .and. &
             index( errstr(1:errstr_len), 'CPYUPD - SUBSET HAS BYTE COUNT =' ) .eq. 0 ) stop 5
    else
      call readsb ( 22, iret )
      if ( iret .ne. 0 ) stop 6
      call ufbcpy ( 22, 50 )
      call writsb ( 50 )
      if ( ( mod(mgct,2) .eq. 1 ) .and. &
             index( errstr(1:errstr_len), 'MSGUPD - SUBSET HAS BYTE COUNT =' ) .eq. 0 ) stop 7
    end if
  end do

  ! Close the output file.
  call closbf ( 50 )

end program outtest10