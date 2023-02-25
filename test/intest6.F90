! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_6' using UFBMEM, RDMEMM and UFBMNS.
!
! Ed Hartnett, J. Ator, 2/25/23
program intest6
  implicit none
  integer*4 nmsub
  character cmgtag*8
  integer icnt1, icnt2, iday, ier, ihour, imgdt, iunt1, iunt2, iyr, imon

  print *, 'Testing reading IN_6 using UFBMEM, RDMEMM and UFBMNS'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  open(unit = 21, file = 'testfiles/IN_6_infile1', form = 'UNFORMATTED')
  open(unit = 22, file = 'testfiles/IN_6_infile2', form = 'UNFORMATTED')

  call datebf(22, iyr, imon, iday, ihour, imgdt)
  if (imgdt .ne. 21031900 .or. iyr .ne. 21 .or. iday .ne. 19) stop 1
  rewind(22)

  ! Open the input files.
  call ufbmem(21, 0, icnt1, iunt1)
  call ufbmem(22, 1, icnt2, iunt2)

  if (icnt1 .ne. 926 .or. icnt2 .ne. 344 .or. iunt1 .ne. 21 .or. iunt2 .ne. 21) stop 3

  ! Read message #167 into internal arrays.
  call rdmemm(167, cmgtag, imgdt, ier)

  if (cmgtag .ne. 'NC004002' .or. imgdt .ne. 21031713 .or. NMSUB(iunt2) .ne. 3) stop 4

  ! Read subset #18364 into internal arrays.
  call ufbmns(18364, cmgtag, imgdt)

  if (cmgtag .ne. 'NC002003' .or. imgdt .ne. 21031900 .or. NMSUB(iunt2) .ne. 2) stop 5

  print *, 'SUCCESS!'
end program intest6
