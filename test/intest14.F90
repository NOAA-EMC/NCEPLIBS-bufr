! This is a test for NCEPLIBS-bufr.
!
! Test use of CATCH_BORTS for more graceful exits after bort errors
!
! J. Ator, 8/25/2025
program intest14
  use bufr_interface

  implicit none

  integer errstr_len, lunit, idate, iret
  integer*4 isetprm, ireadmg, catch_borts, mxmb, nmb, ierr
  parameter (mxmb = 1000)

  character errstr*400, subset*8
  character bmg*1000
  character*20 filnam / 'testfiles/IN_1' /

  real*8 r8arr(18, 2)

  print *, 'Testing use of CATCH_BORTS for more graceful exits after bort errors'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  if (isetprm('NFILES', 4) /= 0) stop 1

  lunit = 11

  ! The following call should return -1 since we haven't yet activated bort catching.
  call check_for_bort(errstr, errstr_len)
  if (errstr_len /= -1) stop 2

  ! Activate bort catching.
  if (catch_borts('Y') /= 0) stop 99

  ! Test the catching of a bad input argument to subroutine openbf.
  open(unit = lunit, file = 'testfiles/OUT_8_infile', form ='unformatted')
  call openbf(lunit, 'INN', lunit)
  call check_for_bort(errstr, errstr_len)
  if ( errstr_len <= 0 .or. index( errstr(1:errstr_len), 'OPENBF - ILLEGAL SECOND (INPUT) ARGUMENT' ) == 0 ) stop 3
  ! Fix the error and retry so we can continue on.
  call openbf(lunit, 'IN', lunit)
  call check_for_bort(errstr, errstr_len)
  if (errstr_len /= 0) stop 4

  ! Test the catching of a bad input unit number to subroutine readmg.
  iret = ireadmg(111, subset, idate)
  call check_for_bort(errstr, errstr_len)
  if ( errstr_len <= 0 .or. index( errstr(1:errstr_len), 'STATUS - INPUT UNIT NUMBER' ) == 0 ) stop 5
  ! Fix the error and retry so we can continue on.
  iret = ireadmg(lunit, subset, idate)
  call check_for_bort(errstr, errstr_len)
  if (errstr_len /= 0 .or. iret /= 0 .or. subset /= 'NC001002') stop 6

  ! Test the catching of a bad input unit number to subroutine readns.
  call readns(12, subset, idate, iret)
  call check_for_bort(errstr, errstr_len)
  if ( errstr_len <= 0 .or. index( errstr(1:errstr_len), 'READNS - INPUT BUFR FILE IS CLOSED' ) == 0 ) stop 7
  ! Fix the error and retry so we can continue on.
  call readns(lunit, subset, idate, iret)
  call check_for_bort(errstr, errstr_len)
  if (errstr_len /= 0 .or. iret /= 0 .or. idate /= 23022400) stop 8

  ! Test the catching of a bad (longer than 80 character) input string to subroutine ufbint.
  call ufbint(lunit, r8arr, 18, 2, iret, &
    'YEAR MNTH DAYS HOUR MINU RPID CLAT CLON SELV CORN QMAT TMDB QMDD TMDP REHU QMST SST1')
  call check_for_bort(errstr, errstr_len)
  if ( errstr_len <= 0 .or. index( errstr(1:errstr_len), 'STRING - INPUT STRING (') == 0 .or. &
    index( errstr(1:errstr_len), '> LIMIT OF 80 CHAR.') == 0 ) stop 9

  ! Test the catching of an illegal second input value to subroutine cobfl_c.
  call cobfl_c(filnam, 'j')
  call check_for_bort(errstr, errstr_len)
  if ( errstr_len <= 0 .or. index( errstr(1:errstr_len), 'COBFL - SECOND ARGUMENT WAS (') == 0 .or. &
    index( errstr(1:errstr_len), 'WHICH IS AN ILLEGAL VALUE') == 0 ) stop 10

  ! Test the catching of an erroneous call to subroutine crbmg_c.
  call crbmg_c(bmg, mxmb, nmb, ierr)
  call check_for_bort(errstr, errstr_len)
  if ( errstr_len <= 0 .or. index( errstr(1:errstr_len), 'CRBMG - NO FILE IS OPEN FOR READING') == 0 ) stop 11

  call closbf(lunit)

  print *, 'SUCCESS!'
end program intest14
