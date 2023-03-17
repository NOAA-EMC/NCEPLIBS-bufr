C> @file
C> @brief Convert an FXY value from a character string of length 5
C> or 6 to an integer.
C>
C> @author J. Woollen @date 1994-01-06

C> This function converts an FXY value from a character string of length 5
C> or 6 to an integer.
C>
C> This function is the logical inverse of function adn30().
C>
C> @param[in] ADN30 -- character*(*): FXY value. Must be of length 5 or 6.
C> @param[in] L30 -- integer: Length of ADN30; can be either 5 or 6
C>                   characters.
C> @returns idn30 -- integer: FXY integer value.
C>
C> @author J. Woollen @date 1994-01-06
      FUNCTION IDN30(ADN30,L30)

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      CHARACTER*(*) ADN30
      CHARACTER*128 BORT_STR

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IF(LEN(ADN30).LT.L30) GOTO 900
      IF(L30.EQ.5) THEN
         READ(ADN30,'(I5)') IDN30
         IF(IDN30.LT.0 .OR. IDN30.GT.65535) GOTO 901
      ELSEIF(L30.EQ.6) THEN
         IDN30 = IFXY(ADN30)
      ELSE
         GOTO 902
      ENDIF

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: IDN30 - FUNCTION INPUT STRING ",A,'//
     . '" CHARACTER LENGTH (",I4,") IS TOO SHORT (< L30,",I5)')
     . ADN30,LEN(ADN30),L30
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: IDN30 - DESCRIPTOR INTEGER '//
     . 'REPRESENTATION, IDN30 (",I8,"), IS OUTSIDE 16-BIT RANGE '//
     . '(0-65535)")') IDN30
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: IDN30 - FUNCTION INPUT STRING ",A,'//
     . '" CHARACTER LENGTH (",I4,") MUST BE EITHER 5 OR 6")')
     . ADN30,L30
      CALL BORT(BORT_STR)
      END
