! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_7' containing 2-03-YYY changed reference values, using inline ERRWRT to
! check error messages, and using UFBPOS and UFBTAB
!
! J. Ator, 2/23/2023

module Share_errstr_intest7
  ! This module is needed in order to share information between the test program and subroutine errwrt, because
  ! the latter is not called by the former but rather is called directly from within the NCEPLIBS-bufr software.

  character*22000 errstr

  integer errstr_len
end module Share_errstr_intest7

subroutine errwrt(str)
  ! This subroutine supersedes the subroutine of the same name within the NCEPLIBS-bufr software, so that we can
  ! easily test the generation of error messages from within the library.

  use Share_errstr_intest7

  character*(*) str

  integer str_len

  str_len = len(str)
  errstr ( errstr_len + 1 : errstr_len + str_len + 1 ) = str
  errstr_len = errstr_len + str_len

  return
end subroutine errwrt

program intest7
  use Share_errstr_intest7

  implicit none

  integer*4 isetprm, igetprm, ireadns, ibfms

  integer imgdt, iret, jdate, nr8v, idx, nsub, kmsg, ksub

  integer mxr8pm, mxr8lv
  parameter ( mxr8pm = 15 )
  parameter ( mxr8lv = 5 )

  real*8 r8arr (mxr8pm, mxr8lv), r8val(1,1)

  character cmgtag*8

  print *, 'Testing reading IN_7 containing 2-03-YYY changed reference values, using inline ERRWRT'
  print *, 'to check error messages, and using UFBPOS and UFBTAB'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  ! Check error messages in ISETPRM.
  iret = isetprm ( 'MXNRV', 5 )
  if ( iret .ne. 0 ) stop 1
  errstr_len = 0
  iret = isetprm ( 'DUMMY', 20 )
  if ( ( iret .ne. -1 ) .or. &
      ( index( errstr(1:errstr_len), 'ISETPRM - UNKNOWN INPUT PARAMETER DUMMY' ) .eq. 0 ) ) stop 2

  ! Open the input file and DX table.
  open ( unit = 11, file = 'testfiles/IN_7', form ='unformatted')
  open ( unit = 12, file = 'testfiles/IN_7_bufrtab' )
  call openbf ( 11, 'FIRST', 11 )
  call openbf ( 11, 'QUIET', 2 )
  errstr_len = 0
  call openbf ( 11, 'IN', 12 )
  if ( index( errstr(1:errstr_len), 'TABLE FROM USER-SUPPLIED TEXT FILE' ) .eq. 0 ) stop 3
  call openbf ( 11, 'QUIET', 1 )

  ! Check error messages in IGETPRM.
  iret = igetprm ( 'MXNRV' )
  if ( iret .ne. 5 ) stop 4
  errstr_len = 0
  iret = igetprm ( 'DUMMY' )
  if ( ( iret .ne. -1 ) .or. &
      ( index( errstr(1:errstr_len), 'IGETPRM - UNKNOWN INPUT PARAMETER DUMMY' ) .eq. 0 ) ) stop 5

  ! Read some data values from the 1st messaage, which uses the 2-03-YYY operator to change one of the
  ! reference values.
  if ( ireadns ( 11, cmgtag, imgdt ) .ne. 0 ) stop 6
  call ufbrep ( 11, r8arr, mxr8pm, mxr8lv, nr8v, 'TIDER' )
  if ( ( nr8v .ne. 2 ) .or. &
      ( nint ( r8arr(1,1) ) .ne. -10000 ) .or. ( nint ( r8arr(1,2) ) .ne. 16 ) ) stop 7
  errstr_len = 0
  call ufbrep ( 11, r8val, 1, 1, nr8v, 'DUMMY' )
  idx = index( errstr(1:errstr_len), 'UFBREP - NO SPECIFIED VALUES READ IN' )
  if ( ( nr8v .ne. 0 ) .or. ( idx .eq. 0 ) ) stop 8
  errstr_len = 0
  call ufbrep ( 11, r8val, 0, 1, nr8v, 'TIDER' )
  idx = index( errstr(1:errstr_len), 'UFBREP - 3rd ARG. (INPUT) IS .LE. 0' )
  if ( ( nr8v .ne. 0 ) .or. ( idx .eq. 0 ) ) stop 9
  errstr_len = 0
  call ufbrep ( 11, r8val, 1, 0, nr8v, 'TIDER' )
  idx = index( errstr(1:errstr_len), 'UFBREP - 4th ARG. (INPUT) IS .LE. 0' )
  if ( ( nr8v .ne. 0 ) .or. ( idx .eq. 0 ) ) stop 10

  ! Jump ahead to the 5th subset of the 23rd message and read some data values.
  call ufbpos ( 11, 23, 5, cmgtag, jdate )
  call ufbint ( 11, r8arr, mxr8pm, mxr8lv, nr8v, 'CLATH CLONH TMDB SWRAD' )
  if ( ( nr8v .ne. 1 ) .or.  &
      ( nint ( r8arr(1,1)*100000 ) .ne. 2001191 ) .or. ( nint ( r8arr(2,1)*100000 ) .ne. -3785017 ) .or. &
      ( nint ( r8arr(3,1)*100 ) .ne. 30035 ) .or. ( nint ( r8arr(4,1) ) .ne. 2187000 ) ) stop 11
  errstr_len = 0
  call ufbint ( 11, r8val, 1, 1, nr8v, 'DUMMY' )
  idx = index( errstr(1:errstr_len), 'UFBINT - NO SPECIFIED VALUES READ IN' )
  if ( ( nr8v .ne. 0 ) .or. ( idx .eq. 0 ) ) stop 12
  errstr_len = 0
  call ufbint ( 11, r8val, 0, 1, nr8v, 'TMDB' )
  idx = index( errstr(1:errstr_len), 'UFBINT - 3rd ARG. (INPUT) IS .LE. 0' )
  if ( ( nr8v .ne. 0 ) .or. ( idx .eq. 0 ) ) stop 13
  errstr_len = 0
  call ufbint ( 11, r8val, 1, 0, nr8v, 'TMDB' )
  idx = index( errstr(1:errstr_len), 'UFBINT - 4th ARG. (INPUT) IS .LE. 0' )
  if ( ( nr8v .ne. 0 ) .or. ( idx .eq. 0 ) ) stop 14

  ! Jump ahead to the 2nd subset of the 30th message and read some data values.
  call ufbpos ( 11, 30, 2, cmgtag, jdate )
  call ufbstp ( 11, r8arr, mxr8pm, mxr8lv, nr8v, 'CLAT CLON HSMSL' )
  if ( ( nr8v .ne. 1 ) .or. &
      ( nint ( r8arr(1,1)*100 ) .ne. 3163 ) .or. ( nint ( r8arr(2,1)*100 ) .ne. -11017 ) .or. &
      ( nint ( r8arr(3,1) ) .ne. 1205 ) ) stop 15
  errstr_len = 0
  call ufbstp ( 11, r8val, 1, 1, nr8v, 'DUMMY' )
  idx = index( errstr(1:errstr_len), 'UFBSTP - NO SPECIFIED VALUES READ IN' )
  if ( ( nr8v .ne. 0 ) .or. ( idx .eq. 0 ) ) stop 16
  errstr_len = 0
  call ufbstp ( 11, r8val, 0, 1, nr8v, 'CLON' )
  idx = index( errstr(1:errstr_len), 'UFBSTP - 3rd ARG. (INPUT) IS .LE. 0' )
  if ( ( nr8v .ne. 0 ) .or. ( idx .eq. 0 ) ) stop 17
  errstr_len = 0
  call ufbstp ( 11, r8val, 1, 0, nr8v, 'CLON' )
  idx = index( errstr(1:errstr_len), 'UFBSTP - 4th ARG. (INPUT) IS .LE. 0' )
  if ( ( nr8v .ne. 0 ) .or. ( idx .eq. 0 ) ) stop 18

  ! Jump backwards to the 88th subset of the 29th message and read some data values.
  call ufbpos ( 11, 29, 88, cmgtag, jdate )
  call ufbseq ( 11, r8arr, mxr8pm, mxr8lv, nr8v, 'NC008023' )
  if ( ( nr8v .ne. 1 ) .or. &
      ( nint ( r8arr(6,1)*100000 ) .ne. 2967000 ) .or. ( nint ( r8arr(7,1)*100000 ) .ne. -9512833 ) .or. &
      ( nint ( r8arr(5,1) ) .ne. 482011039 ) ) stop 19
  errstr_len = 0
  call ufbseq ( 11, r8val, 1, 1, nr8v, 'DUMMY' )
  idx = index( errstr(1:errstr_len), 'UFBSEQ - NO SPECIFIED VALUES READ IN' )
  if ( ( nr8v .ne. 0 ) .or. ( idx .eq. 0 ) ) stop 20
  errstr_len = 0
  call ufbseq ( 11, r8val, 0, 1, nr8v, 'CLON' )
  idx = index( errstr(1:errstr_len), 'UFBSEQ - 3rd ARG. (INPUT) IS .LE. 0' )
  if ( ( nr8v .ne. 0 ) .or. ( idx .eq. 0 ) ) stop 21
  errstr_len = 0
  call ufbseq ( 11, r8val, 1, 0, nr8v, 'CLON' )
  idx = index( errstr(1:errstr_len), 'UFBSEQ - 4th ARG. (INPUT) IS .LE. 0' )
  if ( ( nr8v .ne. 0 ) .or. ( idx .eq. 0 ) ) stop 22

  ! Test ufbcnt.
  call ufbcnt(11, kmsg, ksub)
  if ( kmsg.ne.29 .or. ksub.ne.88) stop 23

  ! Rewind the file and get a total count of the subsets.
  call ufbtab ( -11, r8val, 1, 1, nsub, ' ' )
  if ( ( nsub .ne. 402 ) .or. ( ibfms ( r8val(1,1) ) .ne. 1 ) ) stop 24

  print *, 'SUCCESS!'
end program intest7
