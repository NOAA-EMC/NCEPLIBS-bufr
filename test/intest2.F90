! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_1' using CRBMG with
! OPENBF IO = 'SEC3'. This is based on test test_IN_1.F.
!
! Ed Hartnett, J. Ator, 2/15/23

program intest2
  integer*4 ireadmg, iupvs01, nmsub, ibfms
  parameter (mxr8pm = 10)
  parameter (mxr8lv = 255)
  real*8 r8arr(mxr8pm, mxr8lv), getvalnb
  integer ibit (32)
  character cmgtag*8

  print *, 'Testing reading IN_2, OPENBF IO = IN and LUNIN != LUNDX'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  ! Open the test files.
  open(unit = 11, file = 'testfiles/IN_2', form = 'UNFORMATTED')
  open(unit = 12, file = 'testfiles/IN_2_bufrtab')

  call openbf(11, 'IN', 12)

  ! Read the BUFR message from the BUFR file.
  if (ireadmg(11, cmgtag, imgdt) .ne. 0 ) stop 1

  if (cmgtag .ne. 'NC005064' .or. imgdt .ne. 12101013) stop 2

  if (iupvs01(11,'MSBT') .ne. 64 .or. iupvs01(11,'OGCE') .ne. 7 .or. &
       iupvs01(11,'LENM') .ne. 19926) stop 3

  if (nmsub(11) .ne. 154) stop 4

  ! Read the 5th data subset from the BUFR message.
  do ii = 1, 5
     call readsb(11, ierrsb)
  end do

  IF ( ierrsb .ne. 0 ) THEN
     print *, '        READSB -> FAILED!!'
  ELSE
     print *, '        READSB -> OK'
     CALL UFBINT ( 11, r8arr, MXR8PM, MXR8LV, nr8lv, 'CLAT SAZA PRLC WDIR RPID SIDP' )
     IF (nr8lv .eq. 1 .and. NINT(r8arr(1,1)*100) .eq. 1260 .and. NINT(r8arr(2,1)*100) .eq. 2765 .and. &
          NINT(r8arr(3,1)) .eq. 25540 .and. NINT(r8arr(4,1)) .eq. 218 .and. IBFMS(r8arr(5,1)) .eq. 1) THEN
        print *, '        UFBINT -> OK'
        print *, '         IBFMS -> OK'
     ELSE
        print *, '        UFBINT -> FAILED!!'
        print *, '         IBFMS -> FAILED!!'
     ENDIF

     CALL UPFTBV ( 11, 'SIDP', r8arr(6,1), 32, ibit, nib )
     IF ( ( nib .eq. 1 ) .and. ( ibit(1) .eq. 9 ) ) THEN
        print *, '        UPFTBV -> OK'
     ELSE
        print *, '        UPFTBV -> FAILED!!'
     ENDIF

     CALL UFBREP ( 11, r8arr, MXR8PM, MXR8LV, nr8lv, 'GNAP PCCF MAQC NCTH' )
     IF (nr8lv .eq. 12 .and. NINT(r8arr(1,2)) .eq. 2 .and. NINT(r8arr(2,4)) .eq. 86 .and. &
          NINT(r8arr(2,6)) .eq. 0 .and. IBFMS(r8arr(3,8)) .eq. 1 .and. &
          IBFMS(r8arr(4,9)) .eq. 1 .and. NINT(r8arr(2,11)) .eq. 97 .and. NINT(r8arr(1,12)) .eq. 3) THEN
        print *, '        UFBREP -> OK'
     ELSE
        print *, '        UFBREP -> FAILED!!'
     ENDIF

     IF (NINT(GETVALNB(11,'NCTH',3,'PCCF',-1)) .eq. 0 .and. NINT(GETVALNB(11,'SSNX',1,'SWCM',1)) .eq. 1) THEN
        print *, '      GETVALNB -> OK'
     ELSE
        print *, '      GETVALNB -> FAILED!!'
     ENDIF
  ENDIF
  print *, 'SUCCESS!'
END program

