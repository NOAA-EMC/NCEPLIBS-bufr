C> @file
C> @brief Search for a descriptor within the internal DX BUFR tables

C> This subroutine searches for a descriptor within Table B and
C> Table D of the internal DX BUFR tables.
C>
C> @author J. Woollen
C> @date 2002-05-14
C>
C> @param[in] LUN - integer: Internal I/O stream index associated
C>                  with DX BUFR tables
C> @param[in] IDN - integer: Bit-wise representation of FXY value
C>                  for descriptor
C> @param[out] NEMO - character*(*): Mnemonic associated with IDN
C> @param[out] TAB - character: Type associated with IDN
C>                     - 'B' = Table B descriptor
C>                     - 'D' = Table D descriptor
C> @param[out] IRET - integer: 
C>                     - Positional index of IDN within internal
C>                       Table B, if TAB = 'B'
C>                     - Positional index of IDN within internal
C>                       Table D, if TAB = 'D'
C>                     - 0, otherwise
C>
C> <b>Program history log:</b>
C> - 2002-05-14  J. Woollen -- Original author
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                           documentation
C> - 2009-04-21  J. Ator    -- Use ifxy() for more efficient searching
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>
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
