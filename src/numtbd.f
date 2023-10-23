C> @file
C> @brief Search for a Table B or Table D descriptor within the
C> internal DX BUFR tables.
C>
C> @author J. Woollen @date 2002-05-14

C> Search for a Table B or Table D descriptor within the
C> internal DX BUFR tables.
C>
C> @param[in] LUN - integer: File ID.
C> @param[in] IDN - integer: WMO bit-wise representation of FXY value
C> for Table B or Table D descriptor.
C> @param[out] NEMO - character*(*): Mnemonic associated with IDN.
C> @param[out] TAB - character: Type associated with IDN:
C> - 'B' = Table B descriptor
C> - 'D' = Table D descriptor
C> @param[out] IRET - integer:
C> - Positional index of IDN within internal Table B, if TAB = 'B'
C> - Positional index of IDN within internal Table D, if TAB = 'D'
C> - 0, otherwise
C>
C> @author J. Woollen @date 2002-05-14
      SUBROUTINE NUMTBD(LUN,IDN,NEMO,TAB,IRET)

      USE MODA_TABABD

      CHARACTER*(*) NEMO
      CHARACTER*1   TAB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NEMO = ' '
      IRET = 0
      TAB = ' '

      IF(IDN.GE.IFXY('300000')) THEN

C        LOOK FOR IDN IN TABLE D
C        -----------------------

         DO I=1,NTBD(LUN)
            IF(IDN.EQ.IDND(I,LUN)) THEN
               NEMO = TABD(I,LUN)(7:14)
               TAB  = 'D'
               IRET = I
               GOTO 100
            ENDIF
         ENDDO

      ELSE

C        LOOK FOR IDN IN TABLE B
C        -----------------------

         DO I=1,NTBB(LUN)
            IF(IDN.EQ.IDNB(I,LUN)) THEN
               NEMO = TABB(I,LUN)(7:14)
               TAB  = 'B'
               IRET = I
               GOTO 100
            ENDIF
         ENDDO

      ENDIF

C  EXIT
C  ----

100   RETURN
      END
