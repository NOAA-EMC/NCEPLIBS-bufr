! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_5', checking PREPBUFR code/flag table meaning strings
!
! J. Ator, 2/23/23
program intest5
  implicit none

  integer*4 ireadns

  integer imgdt, lcmg1, lcmg2, lcmg3, lcmg4, ier1, ier2, ier3, ier4

  character cmgtag*8, cmeang1*40, cmeang2*40, cmeang3*40, cmeang4*40

  print *, 'Testing reading IN_5, checking PREPBUFR code/flag table meaning strings.'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  open ( unit = 11, file = 'testfiles/IN_5', form ='unformatted')

  call openbf ( 11, 'IN', 11 )

  ! Set the directory location for the master tables.
  call mtinfo ( '../tables', 90, 91 )

  ! Specify that we want to read in the code and flag tables.
  call codflg ( 'Y' )

  ! Read the first data subset from the file.
  if ( ireadns ( 11, cmgtag, imgdt ) .ne. 0 ) stop 1

  ! Retrieve and check some code/flag meaning strings.
  call getcfmng ( 11, 'PRC', 106, ' ', -1, cmeang1, lcmg1, ier1 )
  call getcfmng ( 11, 'PRC', 106, 'PPC', 5, cmeang2, lcmg2, ier2 )
  call getcfmng ( 11, 'GSES', 10, ' ', -1, cmeang3, lcmg3, ier3 )
  call getcfmng ( 11, 'GSES', 10, 'GCLONG', 173, cmeang4, lcmg4, ier4 )

  if ( ( ier1 .ne. 1 ) .or. ( lcmg1 .ne. 8 ) .or. ( cmeang1(1:lcmg1) .ne. 'PPC     ' ) .or. &
       ( ier2 .ne. 0 ) .or. ( lcmg2 .ne. 34 ) .or. &
              ( cmeang2(1:lcmg2) .ne. 'Surface pressure observation error' ) .or. &
       ( ier3 .ne. 3 ) .or. ( lcmg3 .ne. 24 ) .or. ( cmeang3(1:lcmg3) .ne. 'GCLONG  OGCE    ORIGC   ' ) .or. &
       ( ier4 .ne. 0 ) .or. ( lcmg4 .ne. 20 ) .or. ( cmeang4(1:lcmg4) .ne. 'Stennis Space Center' ) ) stop 2

  ! Free the memory that was dynamically allocated when reading the code and flag tables.
  call dlloctbf()

  print *, 'SUCCESS!'
end program intest5
