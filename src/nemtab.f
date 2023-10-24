C> @file
C> @brief Get information about a descriptor, based on the mnemonic.
C>
C> @author J. Woollen @date 1994-01-06

C> Return information about a descriptor from the
C> internal DX BUFR tables, based on the mnemonic associated with
C> that descriptor.
C>
C> @param[in] LUN - integer: File ID.
C> @param[in] NEMO - character*(*): Mnemonic.
C> @param[out] IDN - integer: WMO bit-wise representation of FXY value
C> for descriptor associated with NEMO.
C> @param[out] TAB - character: Type associated with IDN:
C> - 'B' Table B descriptor.
C> - 'D' Table D descriptor.
C> - 'C' Table C operator.
C> @param[out] IRET - integer:
C> - Positional index of IDN within internal Table B, if TAB = 'B'.
C> - Positional index of IDN within internal Table D, if TAB = 'D'.
C> - The X portion of the FXY value in IDN, if TAB = 'C'.
C> - 0, otherwise
C>
C> @author J. Woollen @date 1994-01-06

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
