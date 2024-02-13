C> @file
C> @brief Decode the scale factor, reference value,
C> bit width, and units from a Table B mnemonic definition.
C> @author Woollen @date 1994-01-06

C> Decode the scale factor, reference value,
C> bit width and units (i.e., the "elements") from a Table B mnemonic
C> definition card that was previously read from a user-supplied DX BUFR
C> table file in character format by subroutine rdusdx().
C> These decoded values are then added to the
C> already-existing entry for that mnemonic within the internal BUFR
C> Table B array TABB(*,LUN) in module @ref moda_tababd.
C>
C> @param[in] CARD - character*80: mnemonic definition card that was read
C> from a user-supplied DX BUFR table.
C> @param[in] LUN - integer: file ID
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE ELEMDX(CARD,LUN)

      use moda_tababd

      CHARACTER*128 BORT_STR1,BORT_STR2
      CHARACTER*80  CARD
      CHARACTER*24  UNIT
      CHARACTER*11  REFR,REFR_ORIG
      CHARACTER*8   NEMO
      CHARACTER*4   SCAL,SCAL_ORIG
      CHARACTER*3   BITW,BITW_ORIG
      CHARACTER*1   SIGN,TAB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CAPTURE THE VARIOUS ELEMENTS CHARACTERISTICS
C  --------------------------------------------

      NEMO = CARD( 3:10)
      SCAL = CARD(14:17)
      REFR = CARD(21:31)
      BITW = CARD(35:37)
      UNIT = CARD(41:64)
c  .... Make sure the units are all capitalized
      CALL CAPIT(UNIT)

C  FIND THE ELEMENT TAG IN TABLE B
C  -------------------------------

C     Note that an entry for this mnemonic should already exist within
C     the internal BUFR Table B array TABB(*,LUN).  We now need to
C     retrieve the positional index for that entry within TABB(*,LUN)
C     so that we can access the entry and then add the scale factor,
C     reference value, bit width, and units to it.

      CALL NEMTAB(LUN,NEMO,IDSN,TAB,IELE)
      IF(TAB.NE.'B') GOTO 900

C  LEFT JUSTIFY AND STORE CHARACTERISTICS
C  --------------------------------------

      UNIT = ADJUSTL(UNIT)
      IF(UNIT.EQ.' ') GOTO 904
      TABB(IELE,LUN)(71:94) = UNIT

      SCAL_ORIG=SCAL
      CALL JSTNUM(SCAL,SIGN,IRET)
      IF(IRET.NE.0) GOTO 901
      TABB(IELE,LUN)(95:95) = SIGN
      TABB(IELE,LUN)(96:98) = SCAL(1:3)

      REFR_ORIG=REFR
      CALL JSTNUM(REFR,SIGN,IRET)
      IF(IRET.NE.0) GOTO 902
      TABB(IELE,LUN)( 99: 99) = SIGN
      TABB(IELE,LUN)(100:109) = REFR(1:10)

      BITW_ORIG=BITW
      CALL JSTNUM(BITW,SIGN,IRET)
      IF(IRET.NE.0  ) GOTO 903
      IF(SIGN.EQ.'-') GOTO 903
      TABB(IELE,LUN)(110:112) = BITW

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR1,'("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"MNEMONIC ",A," IS NOT A TABLE B ENTRY '//
     . '(UNDEFINED, TAB=",A,")")') NEMO,TAB
      CALL BORT2(BORT_STR1,BORT_STR2)
901   WRITE(BORT_STR1,'("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"PARSED SCALE VALUE (=",A,") IS NOT '//
     . 'NUMERIC")') SCAL_ORIG
      CALL BORT2(BORT_STR1,BORT_STR2)
902   WRITE(BORT_STR1,'("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"PARSED REFERENCE VALUE (=",A,") IS NOT '//
     . 'NUMERIC")') REFR_ORIG
      CALL BORT2(BORT_STR1,BORT_STR2)
903   WRITE(BORT_STR1,'("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"PARSED BIT WIDTH VALUE (=",A,") IS NOT '//
     . 'NUMERIC")') BITW_ORIG
      CALL BORT2(BORT_STR1,BORT_STR2)
904   WRITE(BORT_STR1,'("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"UNITS FIELD IS EMPTY")')
      CALL BORT2(BORT_STR1,BORT_STR2)
      END
