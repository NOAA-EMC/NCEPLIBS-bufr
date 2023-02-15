C> @file
C> @brief Decode a real number from a character string
C>
C> @author J. Woollen @date 1994-01-06

C> This function decodes a real number from a character string.
C> The string may contain a leading sign ('+' or '-') character.
C> If the decode fails for any reason, then the current placeholder
C> value for "missing" data is returned.
C>
C> @param[in] STR -- character*(*): String
C> @returns  VALX -- real: Value decoded from STR
C>
C> @author J. Woollen @date 1994-01-06

      FUNCTION VALX(STR)

      USE MODV_BMISS

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR1,BORT_STR2
      CHARACTER*99  BSTR
      CHARACTER*8   FMT

      COMMON /QUIET / IPRT

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      LENS = LEN(STR)
      IF(LENS.GT.99) GOTO 900
      BSTR(1:LENS) = ADJUSTR(STR)
      WRITE(FMT,'(''(F'',I2,''.0)'')') LENS
      VALX = BMISS
      READ(BSTR,FMT,ERR=800) VAL
      VALX = VAL
      GOTO 100
800   IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT('BUFRLIB: VALX - ERROR READING STRING:')
      CALL ERRWRT(BSTR(1:LENS))
      CALL ERRWRT('RETURN WITH VALX = MISSING')
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR1,'("STRING IS: ",A)') STR
      WRITE(BORT_STR2,'("BUFRLIB: VALX - STRING LENGTH EXCEEDS LIMIT '//
     . ' OF 99 CHARACTERS")')
      CALL BORT2(BORT_STR1,BORT_STR2)
      END
