! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_5', using UFBGET and UFBINX, and checking PREPBUFR code/flag table meaning strings
!
! J. Ator, 2/24/2023
program intest5
  use bufr_interface

  implicit none

  integer, parameter :: mxr8pm = 6
  integer, parameter :: mxr8lv = 10

  integer imgdt, lcmg, ier, nlv

  real*8 r8vals ( mxr8pm, mxr8lv ), r81dvals ( mxr8pm )

  character cmgtag*8, cmeang*40, cmeang_short*5

  print *, 'Testing reading IN_5, using UFBGET and UFBINX, and checking PREPBUFR code/flag table meaning strings.'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  open ( unit = 11, file = 'testfiles/IN_5', form ='unformatted')

  call openbf ( 11, 'IN', 11 )

  ! Set the directory location for the master tables.
  call mtinfo ( '../tables', 90, 91 )

  ! Specify that we want to read in the code and flag tables.
  call codflg ( 'Y' )

  ! Read a data subset.
  call readns ( 11, cmgtag, imgdt, ier )
  if ( ier /= 0 ) stop 1

  ! Read and verify some 1-dimensional values from this data subset.
  call ufbget ( 11, r81dvals, mxr8pm, nlv, 'XOB YOB DHR ELV T29 {PRSLEVEL}' )
  if ( ( nlv /= 0 ) .or. ( nint(r81dvals(1)*100) /= 12223 ) .or. ( nint(r81dvals(2)*100) /= -1795 ) .or. &
      ( nint(r81dvals(3)) /= -1 ) .or. ( nint(r81dvals(4)) /= 9 ) .or. ( nint(r81dvals(5)) /= 11 ) .or. &
      ( nint(r81dvals(6)) /= 44 ) ) stop 2

  ! Retrieve and check some code/flag meaning strings.
  call getcfmng ( 11, 'PRC', 106, ' ', -1, cmeang, lcmg, ier )
  if ( ( ier /= 1 ) .or. ( lcmg /= 8 ) .or. ( cmeang(1:lcmg) /= 'PPC     ' ) ) stop 3
  call getcfmng ( 11, 'PRC', 106, 'PPC', 5, cmeang, lcmg, ier )
  if ( ( ier /= 0 ) .or. ( lcmg /= 34 ) .or. &
              ( cmeang(1:lcmg) /= 'Surface pressure observation error' ) ) stop 4
  call getcfmng ( 11, 'GSES', 10, ' ', -1, cmeang, lcmg, ier )
  if ( ( ier /= 3 ) .or. ( lcmg /= 24 ) .or. ( cmeang(1:lcmg) /= 'GCLONG  OGCE    ORIGC   ' ) ) stop 5
  call getcfmng ( 11, 'GSES', 10, 'GCLONG', 173, cmeang, lcmg, ier )
  if ( ( ier /= 0 ) .or. ( lcmg /= 20 ) .or. ( cmeang(1:lcmg) /= 'Stennis Space Center' ) ) stop 6
  call getcfmng ( 11, 'GCLONG', 10, ' ', -1, cmeang, lcmg, ier )
  if ( ( ier /= 0 ) .or. ( lcmg /= 12 ) .or. (cmeang(1:lcmg) /= 'Cairo (RSMC)') ) stop 7
  call getcfmng ( 11, 'OGCE', 241, ' ', -1, cmeang, lcmg, ier )
  if ( ( ier /= 0 ) .or. ( lcmg /= 6 ) .or. (cmeang(1:lcmg) /= 'Monaco') ) stop 8
  call getcfmng ( 11, 'TABLASS', 0, ' ', -1, cmeang, lcmg, ier )
  if ( ( ier /= 1 ) .or. ( lcmg /= 8 ) .or. (cmeang(1:lcmg) /= 'TABLAT  ') ) stop 9
  ! For these two cases, cmeang_short will not get updated, so no check on the value.
  call getcfmng ( 11, 'TABLASS', 0, ' ', -1, cmeang_short, lcmg, ier )
  if ( ( ier /= -1 ) .or. ( lcmg /= 5 ) ) stop 10
  call getcfmng ( 11, 'GSES', 0, 'DUMMY', -1, cmeang_short, lcmg, ier )
  if ( ( ier /= -1 ) .or. ( lcmg /= 5 ) ) stop 11

  ! Read and verify some values from the 2nd data subset of the 2nd message.
  call ufbinx ( 11, 2, 2, r8vals, mxr8pm, mxr8lv, nlv, 'CLAM CLTP' )
  if ( ( nlv /= 3 ) .or. ( nint(r8vals(1,1)) /= 7 ) .or. ( nint(r8vals(2,1)) /= 38 ) .or. &
      ( nint(r8vals(2,2)) /= 61 ) .or. ( nint(r8vals(2,3)) /= 60 ) ) stop 12

  ! Free the memory that was dynamically allocated when reading the code and flag tables.
  call dlloctbf_c()
  close ( 11 )

  ! Test ufbinx's openbf/closbf calls.
  open ( unit = 12, file = 'testfiles/IN_5', form ='unformatted')
  call ufbinx ( 12, 2, 2, r8vals, mxr8pm, mxr8lv, nlv, 'CLAM CLTP' )
  if ( ( nlv /= 3 ) .or. ( nint(r8vals(1,1)) /= 7 ) .or. ( nint(r8vals(2,1)) /= 38 ) .or. &
      ( nint(r8vals(2,2)) /= 61 ) .or. ( nint(r8vals(2,3)) /= 60 ) ) stop 13

  print *, 'SUCCESS!'
end program intest5
