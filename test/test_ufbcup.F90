! This is a test for NCEPLIBS-bufr library.
!
! This tests random subroutines that needed some tests to complete
! test code coverage.
!
! Ed Hartnett 3/17/23
program test_ufbcup
  implicit none
  character*8 subset
  integer jdate, iret
  
  print *, 'Testing ufbcup.'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  ! Open input file.
  open(unit = 11, file = 'testfiles/IN_9', form ='unformatted')
  call openbf(11, 'IN', 11)

  ! Open output file.
  open(unit = 12, file = 'test_ufbcup_out', form ='unformatted')
  call openbf(12, 'OUT', 11)

  ! Read a message.
  call readmg(11, subset, jdate, iret)
  if (iret .ne. 0 .or. subset .ne. ' ADPSFC') stop 10
  print *, subset, jdate, iret

  ! Load a subset of data.
  call readsb(11, iret)
  print *, iret

  ! Call ufbcup.
  call ufbcup(11, 12)

  ! Close files.
  call closbf(11)
  call closbf(12)

  print *, 'SUCCESS'
end program test_ufbcup
