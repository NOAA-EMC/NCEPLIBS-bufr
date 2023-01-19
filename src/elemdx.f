C> @file
C> @brief Decode the scale factor, reference value,
c> bit width and units (i.e., the "elements") from a table b mnemonic
c> definition card that was previously read from a user-supplied bufr
c> dictionary table file in character format by subroutine rdusdx().
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 1995-06-28 | J. Woollen | Increased the size of internal bufr table arrays in order to handle bigger files.
C> 1998-07-08 | J. Woollen | Replaced call to cray library routine "abort" with call to routine bort(0.
C> 1999-11-18 | J. Woollen | The number of bufr files which can be opened at one time increased from 10 to 32 (necessary for mpi).
C> 2003-11-04 | J. Ator    | Added documentation.
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | Unified/portable for wrf; documentation; outputs more info when routine terminates; changed  bort() to bort2().
C> 2007-01-19 | J. Ator    | Added extra argument for call to jstchr().
C> 2014-12-10 | J. Ator    | Use modules instead of common blocks.
C> 2021-09-30 | J. Ator    | Replace jstchr with Fortran intrinsic adjustl.
C>
C> @author WOOLLEN @date 1994-01-06
      
C> This subroutine decodes the scale factor, reference value,
c> bit width and units (i.e., the "elements") from a table b mnemonic
c> definition card that was previously read from a user-supplied bufr
c> dictionary table file in character format by subroutine rdusdx().
C> These decoded values are then added to the
c> already-existing entry for that mnemonic within the internal bufr
c> table B array TABB(*,LUN) in module tababd.
C>
C> @param[in] CARD - character*80: mnemonic definition card that was read
C> from a user-supplied bufr dictionary table.
C> @param[in] LUN - integer: i/o stream index into internal memory arrays .
C>
C> @author WOOLLEN @date 1994-01-06
      SUBROUTINE ELEMDX(CARD,LUN)

      USE MODA_TABABD

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
      TABB(IELE,LUN)(96:98) = SCAL

      REFR_ORIG=REFR
      CALL JSTNUM(REFR,SIGN,IRET)
      IF(IRET.NE.0) GOTO 902
      TABB(IELE,LUN)( 99: 99) = SIGN
      TABB(IELE,LUN)(100:109) = REFR

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
