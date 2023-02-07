C> @file
C> @brief Check the validity of an FXY value
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 2003-11-04 | J. Ator    | Added documentation.
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | Split non-zero return; unified/ portable for wrf; documentation.
C> 2007-01-19 | J. Ator    | Cleaned up and simplified logic.
C>
C> @author Woollen @date 1994-01-06

C> This function checks the input character string to determine
C> whether it contains a valid FXY (descriptor) value.
C>
C> @param[in] NUMB - character*6: FXY value to be checked.
C>
C> @return indicator as to whether numb is valid:.
C> - 0 yes
C> - -1 no, the first character ("F" value) is not '0', '1', '2', or '3'
C> - -2 no, characters 2-6 ("X" and "Y" values) are not all numeric
C> - -3 no, characters 2-3 ("X" value) are not between '00' and '63'
C> - -4 no, characters 4-6 ("Y" value) are not between '000' and '255'
C>
C> @author Woollen @date 1994-01-06
      FUNCTION NUMBCK(NUMB)

      CHARACTER*6  NUMB
      LOGICAL      DIGIT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK THE FIRST CHARACTER OF NUMB
C  ---------------------------------

      IF( LLT(NUMB(1:1),'0') .OR. LGT(NUMB(1:1),'3') ) THEN
         NUMBCK = -1
         RETURN
      ENDIF

C  CHECK FOR A VALID DESCRIPTOR
C  ----------------------------

      IF(DIGIT(NUMB(2:6))) THEN
         READ(NUMB,'(1X,I2,I3)') IX,IY
      ELSE
         NUMBCK = -2
         RETURN
      ENDIF

      IF(IX.LT.0 .OR. IX.GT. 63) THEN
         NUMBCK = -3
         RETURN
      ELSE IF(IY.LT.0 .OR. IY.GT.255) THEN
         NUMBCK = -4
         RETURN
      ENDIF

      NUMBCK = 0

      RETURN
      END
