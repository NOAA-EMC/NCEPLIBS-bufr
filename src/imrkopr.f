      INTEGER FUNCTION IMRKOPR(NEMO)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    IMRKOPR
C   PRGMMR: J. ATOR          ORG: NCEP       DATE: 2016-05-04
C
C ABSTRACT: THIS FUNCTION DETERMINES WHETHER THE GIVEN MNEMONIC
C   CONTAINS A TABLE C MARKER OPERATOR.
C
C PROGRAM HISTORY LOG:
C 2016-05-04  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    IMRKOPR (NEMO)
C   INPUT ARGUMENT LIST:
C     NEMO     - CHARACTER*(*): MNEMONIC
C
C   OUTPUT ARGUMENT LIST:
C     IMRKOPR  - INTEGER: RETURN CODE INDICATING WHETHER NEMO CONTAINS
C                A TABLE C MARKER OPERATOR
C                  0 - NO
C                  1 - YES
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: IGETRFEL IOKOPER  STSEQ
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
        IMRKOPR = 0
      ELSE IF ( ( NEMO(4:6).EQ.'255' )
     +			.AND.
     +	       ( ( NEMO(1:3).EQ.'223' ) .OR. ( NEMO(1:3).EQ.'224' ) .OR.
     +	         ( NEMO(1:3).EQ.'225' ) .OR. ( NEMO(1:3).EQ.'232' ) ) )
     +			THEN
        IMRKOPR = 1
      ELSE
        IMRKOPR = 0
      ENDIF

      RETURN
      END
