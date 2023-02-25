! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_5' using OPENBF IO = IN and LUNIN = LUNDX
! using PREPBUFR and code/flag table meaning strings.
!
! Ed Hartnett, J. Ator, 2/25/23
program intest5
  implicit none
  integer*4 ireadns
  integer mxr8pm, mxr8lv
  parameter(mxr8pm = 6)
  parameter(mxr8lv = 50)
  character cmgtag*8, cmeang1*40, cmeang2*40, cmeang3*40, cmeang4*40
  integer ier1, ier2, ier3, ier4, imgdt, lcmg1, lcmg2, lcmg3, lcmg4

  print *, 'Testing reading IN_5 using OPENBF IO = IN, LUNIN = LUNDX, ', &
       'PREPBUFR and code/flag table meaning strings'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  open(unit = 11, file = 'testfiles/IN_5', form ='UNFORMATTED')
  call openbf(11, 'IN', 11)

  call mtinfo('../tables', 90, 91)

  call codflg('Y')

  if (ireadns(11, cmgtag, imgdt) .ne. 0) stop 1

  ! Retrieve and check some code/flag meaning strings.
  call getcfmng(11, 'PRC', 106, ' ', -1, cmeang1, lcmg1, ier1)
  if (ier1 .ne. 1 .or. lcmg1 .ne. 8 .or. cmeang1(1:lcmg1) .ne. 'PPC     ') stop 2

  call getcfmng(11, 'PRC', 106, 'PPC', 5, cmeang2, lcmg2, ier2)
  if (ier2 .ne. 0 .or. lcmg2 .ne. 34 .or. cmeang2(1:lcmg2) .ne. 'Surface pressure observation error') stop 3

  call getcfmng(11, 'GSES', 10, ' ', -1, cmeang3, lcmg3, ier3)
  if (ier3 .ne. 3 .or. lcmg3 .ne. 24 .or. cmeang3(1:lcmg3) .ne. 'GCLONG  OGCE    ORIGC   ') stop 4

  call getcfmng(11, 'GSES', 10, 'GCLONG', 173, cmeang4, lcmg4, ier4)
  if (ier4 .ne. 0 .or. lcmg4 .ne. 20 .or. cmeang4(1:lcmg4) .ne. 'Stennis Space Center') stop 5

  print *, 'SUCCESS!'
end program intest5
