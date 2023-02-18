! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_3' using nested delayed
! replicationOPENBF IO = IN and LUNIN != LUNDX.
!
! Ed Hartnett, J. Ator, 2/15/23
program intest3
  INTEGER*4 IREADNS, IFBGET, LCMGDF, IBFMS
  PARAMETER (MXR8PM = 6)
  PARAMETER (MXR8LV = 50)
  REAL*8 r8arr(MXR8PM, MXR8LV), r8arf(MXR8PM, MXR8LV), r8arhr(1, MXR8LV), &
       r8arh(MXR8PM, MXR8LV), r8ardr( 1, MXR8LV), r8ard (MXR8PM, MXR8LV)
  CHARACTER cmgtag*8, celem(3)*40, cunit(3)*20
  LOGICAL decodeOK

#ifdef KIND_8
  call setim8b(.true.)
#endif

  OPEN(UNIT = 11, FILE = 'testfiles/IN_3', FORM ='UNFORMATTED')

  ! First, read some values from all of the data subsets.
  CALL UFBTAB(11, r8arr, MXR8PM, MXR8LV, nr8lv, 'CLAT CLON HSMSL {SHRVFFSQ}')
  IF (nr8lv .eq. 10 .and. NINT(r8arr(1,1)*100) .eq. 4025 .and. NINT(r8arr(3,1)) .eq. 88 .and. &
       NINT(r8arr(4,1)) .eq. 12 .and. NINT(r8arr(2,2)*100) .eq. -8852 .and. &
       NINT(r8arr(4,2)) .eq. 20 .and. NINT(r8arr(1,5)*100) .eq. 3352 .and. &
       IBFMS(r8arr(3,5)) .eq. 1 .and. NINT(r8arr(1,8)*100) .eq. 3277 .and. &
       NINT(r8arr(1,9)*100) .eq. 3693 .and. NINT(r8arr(2,9)*100) .eq. -9496 .and. &
       NINT(r8arr(3,9)) .eq. 228 .and. NINT(r8arr(4,9)) .eq. 20) THEN
     print *, '        UFBTAB -> OK'
     print *, '         IBFMS -> OK'
  ELSE
     stop 1
  ENDIF

  ! (Re)open the file for usual reading of each subset one at a time.
  CALL OPENBF ( 11, 'IN', 11 )
  isct = 0
  decodeOK = .true.

  DO WHILE (decodeOK .and. IREADNS( 11, cmgtag, imgdt ) .eq. 0)
     ! Continue checking with the next subset.
     isct = isct + 1

     CALL UFBINT ( 11, r8arr, MXR8PM, MXR8LV, nr8rr, 'YEAR MNTH DAYS HOUR MINU' )
     CALL UFBREP ( 11, r8arf, MXR8PM, MXR8LV, nr8rf, 'TSIG YEAR MNTH DAYS HOUR MINU' )
     CALL UFBINT ( 11, r8arhr, 1, MXR8LV, nr8rhr, '{SHRVHTSQ}' )
     CALL UFBSEQ ( 11, r8arh, MXR8PM, MXR8LV, nr8rh, 'SHRVHTSQ' )
     CALL UFBINT ( 11, r8ardr, 1, MXR8LV, nr8rdr, '{SHRVDCSQ}' )
     CALL UFBSEQ ( 11, r8ard, MXR8PM, MXR8LV, nr8rd, 'SHRVDCSQ' )

     IF (isct .eq. 1) THEN
        CALL RTRCPT ( 11, ityr, itmo, itdy, ithr, itmi, ier )
        IF ( ier .ne. -1 ) decodeOK = .false.

        IF ( ( nr8rr .ne. 1 ) .or. ( NINT(r8arr(2,1)) .ne. 2 ) .or. &
             ( NINT(r8arr(4,1)) .ne. 14 ) .or. ( NINT(r8arr(5,1)) .ne. 3 ) ) decodeOK = .false.

        IF ( ( nr8rf .ne. 12 ) .or. ( NINT(r8arf(1,1)) .ne. 4 ) .or. ( NINT(r8arf(2,1)) .ne. 2015 ) .or. &
             ( NINT(r8arf(3,1)) .ne. 2 ) .or. ( NINT(r8arf(4,1)) .ne. 12 ) .or. ( NINT(r8arf(5,1)) .ne. 18 ) &
             .or. ( NINT(r8arf(4,4)) .ne. 13 ) .or. ( NINT(r8arf(5,4)) .ne. 12 ) .or. &
             ( NINT(r8arf(4,10)) .ne. 15 ) .or. ( NINT(r8arf(5,10)) .ne. 0 ) .or. &
             ( NINT(r8arf(1,11)) .ne. 4 ) .or. ( NINT(r8arf(4,11)) .ne. 15 ) .or. &
             ( NINT(r8arf(5,11)) .ne. 6 ) ) decodeOK = .false.

        IF ( ( nr8rhr .ne. 12 ) .or. ( NINT(r8arhr(1,1)) .ne. 1 ) .or. ( NINT(r8arhr(1,2)) .ne. 1 ) .or. &
             ( NINT(r8arhr(1,3)) .ne. 1 ) .or. ( NINT(r8arhr(1,8)) .ne. 1 ) .or. ( nr8rh .ne. 12 ) .or. &
             ( NINT(r8arh(3,1)*1000) .ne. 1402 ) .or. ( NINT(r8arh(5,1)) .ne. 26 ) .or. ( NINT(r8arh(3,2)*1000) .ne. 1372 ) .or. &
             ( NINT(r8arh(1,8)) .ne. 0 ) .or. ( NINT(r8arh(2,8)) .ne. 0 ) .or. ( nr8rdr .ne. 12 ) .or. &
             ( NINT(r8ardr(1,1)) .ne. 0 ) .or. ( NINT(r8ardr(1,2)) .ne. 1 ) .or. ( NINT(r8ardr(1,4)) .ne. 0 ) .or. &
             ( NINT(r8ardr(1,6)) .ne. 1 ) .or. ( NINT(r8ardr(1,9)) .ne. 0 ) .or. ( NINT(r8ardr(1,10)) .ne. 1 ) .or. &
             ( nr8rd .ne. 3 ) .or. ( NINT(r8ard(3,1)*100) .ne. 33980 ) .or. ( NINT(r8ard(3,2)*100) .ne. 33131 ) .or. &
             ( NINT(r8ard(5,2)) .ne. 26 ) .or. ( NINT(r8ard(1,3)) .ne. 0 ) .or. ( NINT(r8ard(2,3)) .ne. 1 ) .or. &
             ( NINT(r8ard(3,3)*100) .ne. 32564 ) ) decodeOK = .false.

     ELSE IF ( isct .eq. 4 ) THEN
        CALL RTRCPT ( 11, ityr, itmo, itdy, ithr, itmi, ier )
        IF ( ( ier .ne. 0 ) .or. ( ityr .ne. 2014 ) .or. ( itmo .ne. 10 ) .or. ( itdy .ne. 5 ) .or. &
             ( ithr .ne. 12 ) .or. ( itmi .ne. 52 ) ) decodeOK = .false.

        IF ( ( nr8rhr .ne. 20 ) .or. ( NINT(r8arhr(1,1)) .ne. 2 ) .or. ( NINT(r8arhr(1,2)) .ne. 2 ) .or. &
             ( NINT(r8arhr(1,12)) .ne. 2 ) .or. ( NINT(r8arhr(1,19)) .ne. 2 ) .or. ( nr8rh .ne. 40 ) .or. &
             ( NINT(r8arh(1,1)) .ne. 0 ) .or. ( NINT(r8arh(3,1)*1000) .ne. 2286 ) .or. ( NINT(r8arh(1,2)) .ne. 2 ) .or. &
             ( NINT(r8arh(3,2)*1000) .ne. 2286 ) .or. ( NINT(r8arh(1,3)) .ne. 0 ) .or. ( NINT(r8arh(3,3)*1000) .ne. 2256 ) .or. &
             ( NINT(r8arh(1,4)) .ne. 2 ) .or. ( NINT(r8arh(3,4)*1000) .ne. 2256 ) .or. ( NINT(r8arh(1,37)) .ne. 0 ) .or. &
             ( NINT(r8arh(3,37)*1000) .ne. 2225 ) .or. ( NINT(r8arh(1,38)) .ne. 2 ) .or. ( NINT(r8arh(3,38)*1000) .ne. 2225 ) .or. &
             ( nr8rdr .ne. 20 ) .or. ( NINT(r8ardr(1,4)) .ne. 0 ) .or. ( NINT(r8ardr(1,6)) .ne. 0 ) .or. &
             ( NINT(r8ardr(1,9)) .ne. 0 ) .or. &
             ( NINT(r8ardr(1,10)) .ne. 0 ) .or. ( NINT(r8ardr(1,18)) .ne. 0 ) .or. ( nr8rd .ne. 0 ) ) decodeOK = .false.

     ELSE IF ( isct .eq. 6 ) THEN
        IF ( IFBGET ( 11 ) .eq. 0 ) THEN
           print *, '        IFBGET -> OK'
        ELSE
           stop 10
        ENDIF

        IF ( LCMGDF ( 11, cmgtag ) .eq. 0 ) THEN
           print *, '        LCMGDF -> OK'
        ELSE
           stop 11
        ENDIF

        IF ( ( nr8rr .ne. 1 ) .or. ( NINT(r8arr(2,1)) .ne. 10 ) .or. ( NINT(r8arr(4,1)) .ne. 12 ) .or. &
             ( NINT(r8arr(5,1)) .ne. 49 ) ) decodeOK = .false.

        IF ( ( nr8rf .ne. 20 ) .or. ( NINT(r8arf(1,1)) .ne. 4 ) .or. ( NINT(r8arf(2,1)) .ne. 2014 ) .or. &
             ( NINT(r8arf(3,1)) .ne. 10 ) .or. &
             ( NINT(r8arf(4,1)) .ne. 5 ) .or. ( NINT(r8arf(5,1)) .ne. 18 ) .or. &
             ( NINT(r8arf(4,3)) .ne. 6 ) .or. ( NINT(r8arf(5,3)) .ne. 6 ) .or. &
             ( NINT(r8arf(4,9)) .ne. 7 ) .or. ( NINT(r8arf(5,9)) .ne. 18 ) .or. &
             ( NINT(r8arf(1,16)) .ne. 4 ) .or. ( NINT(r8arf(4,16)) .ne. 9 ) .or. &
             ( NINT(r8arf(5,16)) .ne. 12 ) .or. ( NINT(r8arf(4,18)) .ne. 10 ) .or. &
             ( NINT(r8arf(5,18)) .ne. 0 ) ) decodeOK = .false.
     ELSE IF ( isct .eq. 7 ) THEN
        IF ( ( nr8rhr .ne. 20 ) .or. ( NINT(r8arhr(1,1)) .ne. 0 ) .or. &
             ( NINT(r8arhr(1,5)) .ne. 0 ) .or. ( NINT(r8arhr(1,13)) .ne. 0 ) .or. &
             ( NINT(r8arhr(1,18)) .ne. 0 ) .or. ( nr8rh .ne. 0 ) .or. &
             ( nr8rdr .ne. 20 ) .or. ( NINT(r8ardr(1,1)) .ne. 1 ) .or. &
             ( NINT(r8ardr(1,2)) .ne. 1 ) .or. ( NINT(r8ardr(1,11)) .ne. 1 ) .or. &
             ( NINT(r8ardr(1,12)) .ne. 1 ) .or. ( NINT(r8ardr(1,13)) .ne. 1 ) .or. &
             ( NINT(r8ardr(1,15)) .ne. 1 ) .or. ( nr8rd .ne. 20 ) .or. &
             ( NINT(r8ard(3,1)*100) .ne. 10421 ) .or. ( NINT(r8ard(3,4)*100) .ne. 8976 ) .or. &
             ( NINT(r8ard(3,11)*100) .ne. 5069 ) .or. ( NINT(r8ard(3,12)*100) .ne. 4616 ) .or. &
             ( NINT(r8ard(1,13)) .ne. 0 ) .or. ( NINT(r8ard(2,13)) .ne. 0 ) .or. &
             ( NINT(r8ard(3,13)*100) .ne. 4163 ) .or. ( NINT(r8ard(4,13)) .ne. 1 ) .or. &
             ( NINT(r8ard(5,13)) .ne. 26 ) .or. ( NINT(r8ard(3,15)*100) .ne. 3766 ) ) decodeOK = .false.
     ELSE IF ( isct .eq. 10 ) THEN
        IF ( IFBGET ( 11 ) .ne. 0 ) THEN
           print *, '        IFBGET -> OK'
        ELSE
           stop 12
        ENDIF
     ENDIF
  ENDDO

  ! Verify that all available subsets were successfully read.
  IF ( ( decodeOK ) .and. ( isct .eq. 10 ) ) THEN
     print *, '       IREADNS -> OK'
     print *, '        RTRCPT -> OK'
     print *, '        UFBINT -> OK'
     print *, '        UFBREP -> OK'
     print *, '        UFBSEQ -> OK'
  ELSE
     stop 21
  ENDIF

  CALL NEMDEFS ( 11, 'HSMSL', celem(1), cunit(1), ierndh )
  CALL NEMDEFS ( 11, 'SHRV', celem(2), cunit(2), iernds )
  CALL NEMDEFS ( 11, 'DCHG', celem(3), cunit(3), ierndd )
  IF ( ( ierndh .eq. 0 ) .and. ( iernds .eq. 0 ) .and. ( ierndd .eq. 0 ) .and. ( celem(1)(1:36) .eq. &
       'HEIGHT OF STATION GROUND ABOVE MSL  ' ) .and. ( cunit(1)(1:9) .eq. 'METERS   ' ) .and. &
       ( celem(2)(1:24) .eq. 'SHEF DATA REVISION FLAG ' ) .and. ( cunit(2)(1:12) .eq. 'CODE TABLE  ' ) .and. &
       ( celem(3)(1:15) .eq. 'DISCHARGE      ' ) .and. ( cunit(3)(1:20) .eq. 'METERS**3/SECOND    ' ) ) THEN
     print *, '       NEMDEFS -> OK'
  ELSE
     stop 30
  ENDIF

END program intest3
