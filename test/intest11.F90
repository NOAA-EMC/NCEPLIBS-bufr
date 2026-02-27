! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_11' using STNDRD and RDMEMS
!
! J. Ator, 5/19/2023
program intest11
  use bufr_interface

  implicit none

  integer*4, parameter :: mxbf = 200000
  integer*4 lenmg, ierrb, catch_borts

  integer, parameter :: mxbfd4 = mxbf/4
  integer ibfmg(mxbfd4), ibfmg2(mxbfd4), imesg(50)
  integer ios1, ios2, ncds3, iret, imgdt, errstr_len

  character bfmg(mxbf), cds3(5)*6, cmgtag*8, errstr*400
  character filnam*25 / 'testfiles/IN_11' /

  print *, 'Testing reading IN_11 using STNDRD and RDMEMS'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  ! Activate bort catching. No bort errors should occur, but this way we can fully exercise all of
  ! the lines of code in any routines where bort catching is enabled.
  if (catch_borts('Y') /= 0) stop 99

  ! Use crbmg to read the first message from the file into an array.
  call cobfl_c ( filnam, 'r' )
  call crbmg_c ( bfmg, mxbf, lenmg, ierrb )
  if ( ierrb /= 0 ) stop 1
  ibfmg = transfer ( bfmg(1:lenmg), ibfmg )
  call ccbfl_c ()

  ! Re-open the file for reading via openbf, then pass the array message into stndrd and check some values.
  open ( unit = 11, file = filnam, form = 'unformatted', iostat = ios1 )
  open ( unit = 12, file = 'testfiles/IN_11_bufrtab', iostat = ios2 )
  if ( ios1 /= 0 .or. ios2 /= 0 ) stop 2
  call openbf ( 11, 'IN', 12 )
  call stndrd ( 11, ibfmg, mxbfd4, ibfmg2 )
  call upds3 ( ibfmg2, 5, cds3, ncds3 )
  if ( ncds3 /= 1  .or. cds3(1) /= '310190' ) stop 3
  call closbf ( 11 )

  ! Re-open the file for reading via ufbmex.
  open ( unit = 11, file = filnam, form = 'unformatted', iostat = ios1 )
  if ( ios1 /= 0 ) stop 4
  call ufbmex ( 11, 12, 0, iret, imesg )
  if ( iret /= 2 .or. imesg(1) /= 3 ) stop 5

  ! Read the 8th subset from the 2nd message, which is a standardized copy of the 1st message.
  call rdmemm ( 2, cmgtag, imgdt, iret )
  if ( iret /= 0 .or. cmgtag /= 'NC003010') stop 6
  call rdmems ( 8, iret )
  if ( iret /= 0 ) stop 7

  ! Do a quick sanity check to confirm that upds3 properly handles a bad input parameter.
  call upds3 ( ibfmg2, 0, cds3, ncds3 )
  call check_for_bort(errstr, errstr_len)
  if ( errstr_len /= 0 .or. ncds3 /= 0 ) stop 8

  print *, 'SUCCESS!'
end program intest11
