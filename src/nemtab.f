C> @file
C> @brief Get information about a descriptor, based on the mnemonic

C> This subroutine returns information about a descriptor from the
C> internal DX BUFR tables, based on the mnemonic associated with
C> that descriptor.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUN -- integer: Internal I/O stream index associated
C>                   with DX BUFR tables
C> @param[in] NEMO -- character*(*): Mnemonic
C> @param[out] IDN -- integer: Bit-wise representation of FXY value
C>                    for descriptor associated with NEMO
C> @param[out] TAB -- character: Type associated with IDN
C>                     - 'B' = Table B descriptor
C>                     - 'D' = Table D descriptor
C>                     - 'C' = Table C operator
C> @param[out] IRET -- integer:
C>                     - Positional index of IDN within internal
C>                       Table B, if TAB = 'B'
C>                     - Positional index of IDN within internal
C>                       Table D, if TAB = 'D'
C>                     - The X portion of the FXY value in IDN, if
C>                       TAB = 'C'
C>                     - 0, otherwise
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1995-06-28 | J. Woollen | Increased the size of internal BUFR table arrays in order to handle bigger files |
C> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
C> | 2000-09-19 | J. Woollen | Added capability to encode and decode data using the operator descriptors (BUFR table C) for changing width and changing scale |
C> | 2003-11-04 | J. Ator    | Added documentation |
C> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
C> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation |
C> | 2005-11-29 | J. Ator    | Added support for 207 and 208 operators |
C> | 2010-03-19 | J. Ator    | Added support for 204 and 205 operators |
C> | 2012-03-02 | J. Ator    | Added support for 203 operator |
C> | 2015-02-25 | J. Ator    | Allow processing of 2-2x, 2-3x and 2-4X non-marker operators in DX tables |
C>
      SUBROUTINE NEMTAB(LUN,NEMO,IDN,TAB,IRET)

      USE MODA_TABABD

      CHARACTER*(*) NEMO
      CHARACTER*8   NEMT
      CHARACTER*1   TAB
      LOGICAL       FOLVAL

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      FOLVAL = NEMO(1:1).EQ.'.'
      IRET = 0
      TAB = ' '

C  LOOK FOR NEMO IN TABLE B
C  ------------------------

      DO 1 I=1,NTBB(LUN)
      NEMT = TABB(I,LUN)(7:14)
      IF(NEMT.EQ.NEMO) THEN
         IDN  = IDNB(I,LUN)
         TAB  = 'B'
         IRET = I
         GOTO 100
      ELSEIF(FOLVAL.AND.NEMT(1:1).EQ.'.') THEN
         DO J=2,LEN(NEMT)
         IF(NEMT(J:J).NE.'.' .AND. NEMT(J:J).NE.NEMO(J:J)) GOTO 1
         ENDDO
         IDN  = IDNB(I,LUN)
         TAB  = 'B'
         IRET = I
         GOTO 100
      ENDIF
1     ENDDO

C  DON'T LOOK IN TABLE D FOR FOLLOWING VALUE-MNEMONICS
C  ---------------------------------------------------

      IF(FOLVAL) GOTO 100

C  LOOK IN TABLE D IF WE GOT THIS FAR
C  ----------------------------------

      DO I=1,NTBD(LUN)
      NEMT = TABD(I,LUN)(7:14)
      IF(NEMT.EQ.NEMO) THEN
         IDN  = IDND(I,LUN)
         TAB  = 'D'
         IRET = I
         GOTO 100
      ENDIF
      ENDDO

C  IF STILL NOTHING, CHECK HERE FOR TABLE C OPERATOR DESCRIPTORS
C  -------------------------------------------------------------

      IF (IOKOPER(NEMO).EQ.1) THEN
         READ(NEMO,'(1X,I2)') IRET
         IDN = IFXY(NEMO)
         TAB = 'C'
         GOTO 100
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
