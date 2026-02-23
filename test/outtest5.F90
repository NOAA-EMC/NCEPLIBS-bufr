! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_5' using DUMPBF, GETABDB, UFDUMP, UFBDMP, and DXDUMP
!
! J. Ator, 2/17/2023
program outtest5
  implicit none

  integer*4 ireadns, catch_borts

  integer jdate(5), jdump(5), ii, jtab, nsub, imgdt, errstr_len

  character cmgtag*8, tabdb(1000)*128, errstr*400

  print *, 'Testing writing OUT_5 using DUMPBF, GETABDB, UFDUMP, UFBDMP, and DXDUMP'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  ! Activate bort catching. No bort errors should occur, but this way we can fully exercise all of
  ! the lines of code in any routines where bort catching is enabled.
  if (catch_borts('Y') /= 0) stop 99

  ! Open the output log (ASCII) file.
  open ( unit = 13, file = 'out5.bufr' )

  ! Make a "FIRST" call to subroutine OPENBF to dynamically allocate internal arrays.  Otherwise, the below call to
  ! subroutine DUMPBF will fail when trying to call subroutine STATUS, because subroutine OPENBF won't yet have been
  ! called.
  call openbf ( 13, 'FIRST', 13 )

  ! Open the input (BUFR) file.  Note that since we're about to call subroutine DUMPBF for this file, then we don't
  ! need to first call subroutine OPENBF for this file, because subroutine DUMPBF will do that internally.
  open ( unit = 11, file = 'testfiles/OUT_5_infile' )

  ! Specify format of Section 1 date/time when reading.
  call datelen ( 10 )

  write ( 13, fmt = '(///,A)' ) '------------ DUMPBF ------------'
  call dumpbf ( 11, jdate, jdump )
  write ( 13, fmt = '(A,5I5)' ) 'jdate =', (jdate(ii), ii=1,5)
  write ( 13, fmt = '(A,5I5)' ) 'jdump =', (jdump(ii), ii=1,5)

  ! Subroutine DUMPBF will have just closed the input (BUFR) file with an internal call to subroutine CLOSBF (which
  ! also does an internal Fortran CLOSE on the logical unit number), so we now need to reopen the file with a new
  ! call to subroutine OPENBF.
  open ( unit = 11, file = 'testfiles/OUT_5_infile' )
  call openbf ( 11, 'IN', 11 )

  write ( 13, fmt = '(///,A)' ) '------------ GETABDB -----------'
  ! First do some quick sanity checks to confirm that getabdb properly handles bad input parameters.
  call getabdb ( 11, tabdb, 0, jtab )
  if (jtab /= 0) stop 1
  call getabdb ( 111, tabdb, 1000, jtab )
  call check_for_bort(errstr, errstr_len)
  if ( errstr_len <= 0 .or. &
    index( errstr(1:errstr_len), 'STATUS - INPUT UNIT NUMBER (111) OUTSIDE LEGAL RANGE OF 1-99') == 0 ) stop 2
  ! Now go ahead and call getabdb to get the expected output.
  call getabdb ( 11, tabdb, 1000, jtab )
  do ii = 1, jtab
    write ( 13, fmt = '(A,I4,2A)' ) 'tabdb entry #', ii, ":", tabdb(ii)
  end do

  ! Write out the internal DX BUFR table.
  write ( 13, fmt = '(///,A,/)' ) '----------- DXDUMP -----------'
  call dxdump ( 11, 13 )

  ! Write out each data subset using both UFDUMP and UFBDMP.
  nsub = 0
  do while ( ireadns ( 11, cmgtag, imgdt ) == 0 )
    nsub = nsub + 1
    write ( 13, fmt = '(///,A,I1,A)' ) '------------------------------ SUBSET #', nsub, '------------------------------'
    write ( 13, fmt = '(//,A)' ) '------------ UFDUMP ------------'
    call ufdump ( 11, 13 )
    write ( 13, fmt = '(//,A)' ) '------------ UFBDMP ------------'
    call ufbdmp ( 11, 13 )
  end do

  ! Close the output log (ASCII) file.
  close ( 13 )

end program outtest5
