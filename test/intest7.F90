! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_1' using CRBMG with
! OPENBF IO = 'SEC3'.
!
! Ed Hartnett, J. Ator, 2/23/23

! This module is needed in order to share information between the test
! program and subroutine ERRWRT, because the latter is not called by
! the former but rather is called directly from within the BUFRLIB
! software.
MODULE Share_errstr
  CHARACTER*1500       errstr
  INTEGER              errstr_len
END MODULE Share_errstr

! This subroutine supersedes the subroutine of the same name from the
! BUFRLIB software, so that we can easily test the generation of error
! messages from within the library.
SUBROUTINE ERRWRT(str)
  USE Share_errstr

  CHARACTER*(*)   str
  INTEGER         str_len

  str_len = LEN(str)
  errstr (errstr_len + 1 : errstr_len + str_len + 1) = str
  errstr_len = errstr_len + str_len
  RETURN
END SUBROUTINE ERRWRT

! This is the test program.
program intest7
  use share_errstr
  implicit none

  integer*4 isetprm, igetprm, ireadns
  integer mxr8pm, mxr8lv
  parameter (mxr8pm = 15)
  parameter (mxr8lv = 5)
  real*8 r8arr (mxr8pm, mxr8lv), r8val
  character cmgtag*8
  integer idx1, idx2, iret1, iret2, imgdt, jdate, nr8a, nr8v, nr8v2, nsub
  integer ibfms, valx

  print *, 'testing BUFRLIB: reading IN_7 using 2-03-YYY changed reference values,', &
       'inline ERRWRT to check error messages, and UFBPOS, UFBTAB, and VALX.'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  iret1 = isetprm('MXNRV', 5)
  if (iret1 .ne. 0) stop 1
  
  errstr_len = 1
  iret2 = isetprm('DUMMY', 20)
  if (iret2 .ne. -1 .or. index(errstr(1:errstr_len), 'ISETPRM - UNKNOWN INPUT PARAMETER DUMMY') .eq. 0) stop 2

  open(unit = 11, file = 'testfiles/IN_7', form ='UNFORMATTED')
  open(unit = 12, file = 'testfiles/IN_7_bufrtab')

  call openbf(11, 'IN', 12)
  call openbf(11, 'QUIET', 1)

  iret1 = IGETPRM ('MXNRV')
  if (iret1 .ne. 5) stop 3
  errstr_len = 1
  iret2 = IGETPRM ('DUMMY')
  if (iret2 .ne. -1 .or. index(errstr(1:errstr_len), 'IGETPRM - UNKNOWN INPUT PARAMETER DUMMY') .eq. 0) stop 4

  ! Read some data values from the 1st messaage, which uses the
  ! 2-03-YYY operator to change one of the reference values.
  if (ireadns(11, cmgtag, imgdt) .ne. 0) stop 5

  CALL UFBREP(11, r8arr, MXR8PM, MXR8LV, nr8a, 'TIDER')
  errstr_len = 1
  CALL UFBREP(11, r8val, 1, 1, nr8v, 'DUMMY')
  idx1 = INDEX(errstr(1:errstr_len), 'UFBREP - NO SPECIFIED VALUES READ IN')
  errstr_len = 1
  CALL UFBREP(11, r8val, 0, 1, nr8v2, 'TIDER')
  idx2 = INDEX(errstr(1:errstr_len), 'UFBREP - 3rd ARG. (INPUT) IS .LE. 0')
  if (nr8a .ne. 2 .or. nr8v .ne. 0 .or. nr8v2 .ne. 0 .or. idx1 .le. 0) stop 6
  if (idx2 .le. 0 .or. nint(r8arr(1,1)) .ne. -10000 .or. nint(r8arr(1,2)) .ne. 16) stop 7
  
  ! Jump ahead to the 5th subset of the 23rd message and read
  ! some data values.
  CALL UFBPOS(11, 23, 5, cmgtag, jdate)
  CALL UFBINT(11, r8arr, MXR8PM, MXR8LV, nr8a, 'CLATH CLONH TMDB SWRAD')
  errstr_len = 1
  CALL UFBINT(11, r8val, 1, 1, nr8v, 'DUMMY')
  idx1 = INDEX(errstr(1:errstr_len), 'UFBINT - NO SPECIFIED VALUES READ IN')
  errstr_len = 1
  CALL UFBINT(11, r8val, 1, 0, nr8v2, 'TMDB')
  idx2 = INDEX(errstr(1:errstr_len), 'UFBINT - 4th ARG. (INPUT) IS .LE. 0')
  if (nr8a .ne. 1 .or. nr8v .ne. 0 .or. nr8v2 .ne. 0 .or. idx1 .le. 0) stop 8
  if (idx2 .le. 0 .or. nint(r8arr(1,1)*100000) .ne. 2001191) stop 9
  if (nint(r8arr(2,1)*100000) .ne. -3785017) stop 10
  if (nint(r8arr(3,1)*100) .ne. 30035 .or. nint(r8arr(4,1)) .ne. 2187000) stop 11
  
  ! Jump ahead to the 2nd subset of the 30th message and read
  ! some data values.
  call ufbpos(11, 30, 2, cmgtag, jdate)
  call ufbstp(11, r8arr, mxr8pm, mxr8lv, nr8a, 'CLAT CLON HSMSL')
  if (nr8a .ne. 1) stop 12
  if (nint(r8arr(1,1)*100) .ne. 3163 .or. nint(r8arr(2,1)*100) .ne. -11017 &
       .or. nint(r8arr(3,1)) .ne. 1205) stop 13
  
  errstr_len = 1
  call ufbstp(11, r8val, 1, 1, nr8v, 'DUMMY')
  if (nr8v .ne. 0) stop 13
  
  idx1 = index(errstr(1:errstr_len), 'UFBSTP - NO SPECIFIED VALUES READ IN')
  if (idx1 .le. 0) stop 14
  
  errstr_len = 1
  call ufbstp(11, r8val, 1, 0, nr8v2, 'CLON')
  if (nr8v2 .ne. 0) stop 15
  idx2 = index(errstr(1:errstr_len), 'UFBSTP - 4th ARG. (INPUT) IS .LE. 0')
  if (idx2 .le. 0) stop 16
  
  ! Jump backwards to the 88th subset of the 29th message and read
  ! some data values.
  CALL UFBPOS(11, 29, 88, cmgtag, jdate)
  CALL UFBSEQ(11, r8arr, MXR8PM, MXR8LV, nr8a, 'NC008023')
  errstr_len = 1
  CALL UFBSEQ(11, r8val, 1, 1, nr8v, 'DUMMY')
  idx1 = INDEX(errstr(1:errstr_len), 'UFBSEQ - NO SPECIFIED VALUES READ IN')
  errstr_len = 1
  CALL UFBSEQ(11, r8val, 0, 1, nr8v2, 'CLON')
  idx2 = INDEX(errstr(1:errstr_len), 'UFBSEQ - 3rd ARG. (INPUT) IS .LE. 0')
  if (nr8a .ne. 1 .or. nr8v .ne. 0 .or. nr8v2 .ne. 0 .or. idx1 .le. 0) stop 11
  if (idx2 .le. 0 .or. nint(r8arr(6,1)*100000) .ne. 2967000) stop 12
  if (nint(r8arr(7,1)*100000) .ne. -9512833 .or. nint(r8arr(5,1)) .ne. 482011039) stop 14
  
  ! Rewind the file and get a total count of the subsets.
  CALL UFBTAB(-11, r8val, 1, 1, nsub, ' ')
  if (nsub .ne. 402 .or. ibfms(r8val) .ne. 1) stop 22

  ! Test the error handling inside of VALX.
  errstr_len = 1
  r8val = VALX ('75.DUMMY')
  if (index(errstr(1:errstr_len), 'VALX - ERROR READING STRING' ) .eq. 0) stop 24

  print *, 'SUCCESS!'
end program intest7
