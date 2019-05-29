      INTEGER FUNCTION IOKOPER(NEMO)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    IOKOPER
C   PRGMMR: J. ATOR          ORG: NCEP       DATE: 2015-03-06
C
C ABSTRACT: THIS FUNCTION DETERMINES WHETHER THE GIVEN MNEMONIC
C   CONTAINS A TABLE C OPERATOR KNOWN TO THE BUFR ARCHIVE LIBRARY.
C
C PROGRAM HISTORY LOG:
C 2015-03-06  J. ATOR    -- ORIGINAL AUTHOR
C 2016-05-04  J. ATOR    -- USE IMRKOPR AND ALLOW ADDITIONAL OPERATORS
C
C USAGE:    IOKOPER (NEMO)
C   INPUT ARGUMENT LIST:
C     NEMO     - CHARACTER*(*): MNEMONIC
C
C   OUTPUT ARGUMENT LIST:
C     IOKOPER  - INTEGER: RETURN CODE INDICATING WHETHER NEMO CONTAINS
C                A KNOWN TABLE C OPERATOR
C                  0 - NO
C                  1 - YES
C
C REMARKS:
C    THIS ROUTINE CALLS:        IMRKOPR
C    THIS ROUTINE IS CALLED BY: ISTDESC   NEMTAB   NUMTAB   TABSUB
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

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
     +			.AND.
     +	       ( ( NEMO(1:3).EQ.'237' ) .OR. ( NEMO(1:3).EQ.'241' ) .OR.
     +	         ( NEMO(1:3).EQ.'242' ) .OR. ( NEMO(1:3).EQ.'243' ) ) )
     +			THEN
        IOKOPER = 1
      ELSE IF ( ( NEMO(4:6).EQ.'000' ) 
     +			.AND.
     +	       ( ( NEMO(1:3).EQ.'222' ) .OR. ( NEMO(1:3).EQ.'223' ) .OR.
     +	         ( NEMO(1:3).EQ.'224' ) .OR. ( NEMO(1:3).EQ.'225' ) .OR.
     +           ( NEMO(1:3).EQ.'232' ) .OR. ( NEMO(1:3).EQ.'235' ) .OR.
     +           ( NEMO(1:3).EQ.'236' ) ) )
     +			THEN
        IOKOPER = 1
      ELSE
        IOKOPER = IMRKOPR(NEMO)
      ENDIF

      RETURN
      END
