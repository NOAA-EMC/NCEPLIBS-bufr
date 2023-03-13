C> @file
C> @brief Convert an FXY value from its bit-wise (integer)
C> representation to its five or six character representation.
C>
C> @author J. Woollen @date 1994-01-06

C> This function converts an FXY value from its bit-wise
C> (integer) representation to its 5 or 6 character
C> representation.
C>
C> This function is the logical inverse of function idn30().
C>
C> @param[in] IDN -- integer: Bit-wise representation of FXY value
C> @param[in] L30 -- integer: Length of string to be returned; can be
C> either 5 or 6 characters
C> @returns adn30 -- character*(*): FXY value
C>
C> @author J. Woollen @date 1994-01-06

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
