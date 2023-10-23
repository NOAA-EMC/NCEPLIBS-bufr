C> @file
C> @brief Check whether a mnemonic is a Table C operator.
C>
C> @author J. Ator @date 2015-03-06

C> Check whether a specified mnemonic is a
C> Table C operator supported by the NCEPLIBS-bufr software.
C>
C> @param[in] NEMO  -- character*(*): Mnemonic
C> @returns iokoper -- integer: Flag indicating whether NEMO is a
C>                     Table C operator supported by the NCEPLIBS-bufr
C>                     software
C>                        - 0 = No
C>                        - 1 = Yes
C>
C> @author J. Ator @date 2015-03-06
      INTEGER FUNCTION IOKOPER(NEMO)

      CHARACTER*(*)  NEMO

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IF (LEN(NEMO).LT.6) THEN
        IOKOPER = 0
      ELSE IF ( LGE(NEMO(1:3),'201') .AND. LLE(NEMO(1:3),'208') ) THEN
        IOKOPER = 1
      ELSE IF ( NEMO(1:3).EQ.'221') THEN
        IOKOPER = 1
      ELSE IF ( ( ( NEMO(4:6).EQ.'000' ) .OR. ( NEMO(4:6).EQ.'255' ) )
     +                  .AND.
     +         ( ( NEMO(1:3).EQ.'237' ) .OR. ( NEMO(1:3).EQ.'241' ) .OR.
     +           ( NEMO(1:3).EQ.'242' ) .OR. ( NEMO(1:3).EQ.'243' ) ) )
     +                  THEN
        IOKOPER = 1
      ELSE IF ( ( NEMO(4:6).EQ.'000' )
     +                  .AND.
     +         ( ( NEMO(1:3).EQ.'222' ) .OR. ( NEMO(1:3).EQ.'223' ) .OR.
     +           ( NEMO(1:3).EQ.'224' ) .OR. ( NEMO(1:3).EQ.'225' ) .OR.
     +           ( NEMO(1:3).EQ.'232' ) .OR. ( NEMO(1:3).EQ.'235' ) .OR.
     +           ( NEMO(1:3).EQ.'236' ) ) )
     +                  THEN
        IOKOPER = 1
      ELSE
        IOKOPER = IMRKOPR(NEMO)
      ENDIF

      RETURN
      END
