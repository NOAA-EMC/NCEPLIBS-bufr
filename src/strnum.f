C> @file
C> @brief Decode an integer from a character string
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine decodes an integer from a character string.  The
C> string should contain only digits and (optional) trailing blanks.
C> It should not contain any sign ('+' or '-') character nor any
C> leading blanks nor embedded blanks.
C>
C> @param[in]  STR -- character*(*): String
C> @param[out] NUM -- integer: Value decoded from STR
C>                    - -1 = decode was unsuccessful
C>
C> @author J. Woollen @date 1994-01-06
      SUBROUTINE STRNUM(STR,NUM)

      CHARACTER*(*) STR
      CHARACTER*20  STR2

      COMMON /QUIET / IPRT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NUM = 0
      K = 0

C     Note that, in the following call to subroutine STRSUC, the output
C     string STR2 is not used anywhere else in this routine.  In fact,
C     the only reason that subroutine STRSUC is being called here is to
C     determine NUM, which, owing to the fact that the input string STR
C     cannot contain any leading blanks, is equal to the number of
C     digits to be decoded from the beginning of STR.

      CALL STRSUC(STR,STR2,NUM)
      IF(NUM.EQ.-1) GOTO 100

      DO I=1,NUM
      READ(STR(I:I),'(I1)',ERR=99) J
      IF(J.EQ.0 .AND. STR(I:I).NE.'0') GOTO 99
      K = K*10+J
      ENDDO

      NUM = K
      GOTO 100

C     Note that NUM = -1 unambiguously indicates a bad decode since
C     the input string cannot contain sign characters; thus, NUM is
C     always positive if the decode is successful.

99    NUM = -1
      IF(IPRT.GE.0) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT('BUFRLIB: STRNUM - BAD DECODE; RETURN WITH NUM = -1')
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
