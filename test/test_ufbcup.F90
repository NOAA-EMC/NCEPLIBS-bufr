! This is a test for NCEPLIBS-bufr library.
!
! This tests the ufbcub() subroutine.
!
! Ed Hartnett 4/20/23
program test_ufbcup
  implicit none
  character*8 subset
  integer jdate, iret
  real*8 hdr(1, 1)
  
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
  if (iret .ne. 0 .or. subset .ne. 'ADPSFC' .or. jdate .ne. 23022519) stop 10

  ! Load a subset of data.
  call readsb(11, iret)
  if (iret .ne. 0) stop 20

  ! Create a new message in the output file.
  call openmg(12, subset, jdate)

  ! Call ufbcup.
  call ufbcup(11, 12)

  ! Write the data subset.
  call writsb(12)

  ! Close files.
  call closbf(11)
  call closbf(12)

  ! Now reopen the file as input.
  open(unit = 12, file = 'test_ufbcup_out', form ='unformatted')

  ! Open the file, and read the 1st subset of the 1st message.
  call rdmgsb(12, 1, 1)

  ! Get IREC, the number of BUFR messages, according to ufbint()
  ! documentation. But I get a very large number of messages!
  call ufbint(12, hdr, 1, 1, iret, 'IREC')
  print *, hdr(1,1)
  if (hdr(1,1) .ne. 100000000000.00000_8) stop 30
  
!  call openbf(12, 'IN', 12)

  ! Read a message in the file.
!  call readmg(12, subset, jdate, iret)
!  if (iret .ne. 0 .or. subset .ne. 'ADPSFC' .or. jdate .ne. 23022519) stop 10

  ! Load a subset of data.
!  call readsb(12, iret)
!  if (iret .ne. 0) stop 30

  ! Dump the data subset.
!  open(unit = 13, file = 'test_ufbcup_out.txt')
!  call ufdump(12, 13)
!  close(13)

  ! Close the file again.
  call closbf(12)

  print *, 'SUCCESS'
end program test_ufbcup
