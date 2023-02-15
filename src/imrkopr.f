C> @file
C> @brief Check whether a mnemonic is a Table C marker operator.
C>
C> @author J. Ator @date 2016-05-04

C> This function determines whether a specified mnemonic is a
C> Table C marker operator.
C>
C> @param[in] NEMO  -- character*(*): Mnemonic
C> @returns iokoper -- integer: Flag indicating whether NEMO is a
C>                     Table C marker operator
C>                        - 0 = No
C>                        - 1 = Yes
C>
C> @author J. Ator @date 2016-05-04
      INTEGER FUNCTION IMRKOPR(NEMO)

      CHARACTER*(*)  NEMO

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IF (LEN(NEMO).LT.6) THEN
        IMRKOPR = 0
      ELSE IF ( ( NEMO(4:6).EQ.'255' )
     +                  .AND.
     +         ( ( NEMO(1:3).EQ.'223' ) .OR. ( NEMO(1:3).EQ.'224' ) .OR.
     +           ( NEMO(1:3).EQ.'225' ) .OR. ( NEMO(1:3).EQ.'232' ) ) )
     +                  THEN
        IMRKOPR = 1
      ELSE
        IMRKOPR = 0
      ENDIF

      RETURN
      END
