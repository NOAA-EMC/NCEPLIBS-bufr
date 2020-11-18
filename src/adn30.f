C> @file
C> @brief Convert an FXY descriptor from its bit-wise (integer)
C> representation to its five or six character ASCII representation.

C> This function converts a descriptor from its bit-wise
C> (integer) representation to its 5 or 6 character ASCII
C> representation.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] IDN - integer: bit-wise representation of descriptor (FXY)
C>                value
C> @param[in] L30 - integer: length of string to be returned; can be
C>                either 5 or 6 characters
C> @returns adn30 - character*(*): ASCII form of descriptor (FXY) value
C>
C> <b>Program History Log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to CRAY library routine
C>                           "ABORT" with call to new internal BUFRLIB
C>                           routine "BORT"
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added
C>                           history documentation
C>
C> <b>This routine calls:</b> bort()
C>
C> <b>This routine is called by:</b> cadn30() dxinit() igetrfel() istdesc()
C> nemtbd() numtab() rdmtbb() rdmtbd() rdmtbf() reads3() seqsdx() sntbde()
C> sntbfe() ufbqcd() upds3() wrdxtb()
C> <br>Normally not called by any application programs.
C>
      
      FUNCTION ADN30(IDN,L30)

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      CHARACTER*(*) ADN30
      CHARACTER*128 BORT_STR

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IF(LEN(ADN30).LT.L30         ) GOTO 900
      IF(IDN.LT.0 .OR. IDN.GT.65535) GOTO 901
      IF(L30.EQ.5) THEN
         WRITE(ADN30,'(I5)') IDN
      ELSEIF(L30.EQ.6) THEN
         IDF = ISHFT(IDN,-14)
         IDX = ISHFT(ISHFT(IDN,NBITW-14),-(NBITW-6))
         IDY = ISHFT(ISHFT(IDN,NBITW- 8),-(NBITW-8))
         WRITE(ADN30,'(I1,I2,I3)') IDF,IDX,IDY
      ELSE
         GOTO 902
      ENDIF

      DO I=1,L30
      IF(ADN30(I:I).EQ.' ') ADN30(I:I) = '0'
      ENDDO

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: ADN30 - FUNCTION RETURN STRING TOO SHORT')
901   CALL BORT('BUFRLIB: ADN30 - INTEGER REPRESENTATION OF '//
     . 'DESCRIPTOR OUT OF 16-BIT RANGE')
902   WRITE(BORT_STR,'("BUFRLIB: ADN30 - CHARACTER LENGTH (",I4,") '//
     . 'MUST BE EITHER 5 OR 6")') L30
      CALL BORT(BORT_STR)
      END
