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
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: NEMTAB   NUMTAB
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
      ELSE IF ( (NEMO(1:2).EQ.'20') .AND.
     .      LGE(NEMO(3:3),'1') .AND. LLE(NEMO(3:3),'8') )  THEN
        IOKOPER = 1
      ELSE IF ( LGE(NEMO(1:3),'221') .AND. LLE(NEMO(1:3),'243') .AND.
     .     (NEMO(1:6).NE.'223255') .AND. (NEMO(1:6).NE.'224255') .AND.
     .     (NEMO(1:6).NE.'225255') .AND. (NEMO(1:6).NE.'232255') ) THEN
        IOKOPER = 1
      ELSE
        IOKOPER = 0
      ENDIF

      RETURN
      END
