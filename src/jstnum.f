C> @file
C> @brief Left-justify a character string containing an encoded integer
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine left-justifies a character string containing an
C> encoded integer, by removing all leading blanks and any leading
C> sign ('+' or '-') character.  The string is modified in place, and
C> the sign is returned as a separate parameter.  If the input string
C> contains only blank characters, then a call is made to subroutine
C> bort().
C>
C> @param[in,out] STR -- character*(*): String
C> @param[out]   SIGN -- character*1: Sign of encoded integer value
C>                       - '+' = positive value
C>                       - '-' = negative value
C> @param[out]   IRET -- integer: return code
C>                       - 0 = normal return
C>                       - -1 = input string contained non-blank
C>                              characters which were also non-numeric
C>
C> @author J. Woollen @date 1994-01-06
      SUBROUTINE JSTNUM(STR,SIGN,IRET)

      CHARACTER*(*) STR

      CHARACTER*128 ERRSTR
      CHARACTER*1  SIGN

      COMMON /QUIET / IPRT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

      IF(STR.EQ.' ') GOTO 900

      STR = ADJUSTL(STR)
      LSTR = LEN(STR)
      IF(STR(1:1).EQ.'+') THEN
         STR  = STR(2:LSTR)
         SIGN = '+'
      ELSEIF(STR(1:1).EQ.'-') THEN
         STR  = STR(2:LSTR)
         SIGN = '-'
      ELSE
         SIGN = '+'
      ENDIF

      CALL STRNUM(STR,NUM,IER)
      IF(IER.LT.0) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: JSTNUM: ENCODED VALUE WITHIN RESULTANT '//
     .    'CHARACTER STRING (' // STR // ') IS NOT AN INTEGER - '//
     .    'RETURN WITH IRET = -1'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         IRET = -1
      ENDIF

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: JSTNUM - INPUT BLANK CHARACTER STRING NOT '//
     . 'ALLOWED')
      END
