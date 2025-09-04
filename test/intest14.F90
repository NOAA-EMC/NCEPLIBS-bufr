! This is a test for NCEPLIBS-bufr.
!
! Test use of CATCH_BORTS for more graceful exits after bort errors
!
! J. Ator, 8/25/2025
program intest14
  implicit none

  integer errstr_len, lunit, idate, iret
  integer*4 isetprm, ireadmg

  character errstr*400, subset*8

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
  call catch_borts(.true.)

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
  if (errstr_len /= 0 .or. iret /= 0 .or. idate /= 2023022400) stop 8

  call closbf(lunit)

  print *, 'SUCCESS!'
end program intest14
