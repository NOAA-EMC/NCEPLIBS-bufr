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
  real*8 EPSILON
  parameter(EPSILON = .01)
  real*8 missing
  character*8 station
  equivalence(station, hdr(1, 1))
  
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
  
  ! Get SID, the station ID.
  call ufbint(12, hdr, 1, 1, iret, 'SID')
  if (station .ne. 'CWGN    ') stop 31

  ! Get the MISSING value.
  call ufbint(12, hdr, 1, 1, iret, 'NUL')
  missing = hdr(1, 1)
  if (abs(hdr(1, 1) - 100000000000.00000_8) > EPSILON) stop 32
  
  ! Get and check more values.
  call ufbint(12, hdr, 1, 1, iret, 'XOB')
  if (abs(hdr(1, 1) - 262.43) > EPSILON) stop 40
  call ufbint(12, hdr, 1, 1, iret, 'YOB')
  if (abs(hdr(1, 1) - 49.03) > EPSILON) stop 41
  call ufbint(12, hdr, 1, 1, iret, 'DHR')
  if (abs(hdr(1, 1) - 0.0) > EPSILON) stop 42
  call ufbint(12, hdr, 1, 1, iret, 'ELV')
  if (abs(hdr(1, 1) - 251.0) > EPSILON) stop 43
  call ufbint(12, hdr, 1, 1, iret, 'TYP')
  if (abs(hdr(1, 1) - 284) > EPSILON) stop 44
  call ufbint(12, hdr, 1, 1, iret, 'T29')
  if (abs(hdr(1, 1) - 512.0) > EPSILON) stop 45
  call ufbint(12, hdr, 1, 1, iret, 'ITP')
  if (abs(hdr(1, 1) - missing) > EPSILON) stop 46

  ! Read a replication.
  call ufbrep(12, hdr, 1, 1, iret, '{PLEVL}')
  if (iret .ne. 1) stop 50
  if (hdr(1, 1) .ne. 0 .or. iret .ne. 1) stop 50

  ! Close the files.
  call closbf(12)

  print *, 'SUCCESS'
end program test_ufbcup
