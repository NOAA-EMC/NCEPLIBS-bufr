C> @file
C> @brief Check the validity of a mnemonic.
C>
C> @author J. Woollen @date 1994-01-06

C> Check a mnemonic to verify that it has a
C> length of between 1 and 8 characters and that it only
C> contains characters from the allowable character set.
C>
C> @param[in] NEMO - character*(*): mnemonic to be checked
C> @returns - integer: indicator as to whether NEMO is valid:
C>  - 0 yes
C>  - -1 no, the length is not between 1 and 8 characters
C>  - -2 no, it contains characters from outside of the allowable character set
C>
C> @author J. Woollen @date 1994-01-06
      FUNCTION NEMOCK(NEMO)

      CHARACTER*(*) NEMO
      CHARACTER*38  CHRSET

      DATA CHRSET /'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.'/
      DATA NCHR   /38/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  GET THE LENGTH OF NEMO
C  ----------------------

      LNEMO = 0

      DO I=LEN(NEMO),1,-1
      IF(NEMO(I:I).NE.' ') THEN
         LNEMO = I
         GOTO 1
      ENDIF
      ENDDO

1     IF(LNEMO.LT.1 .OR. LNEMO.GT.8) THEN
         NEMOCK = -1
         GOTO 100
      ENDIF

C  SCAN NEMO FOR ALLOWABLE CHARACTERS
C  ----------------------------------

      DO 10 I=1,LNEMO
      DO J=1,NCHR
      IF(NEMO(I:I).EQ.CHRSET(J:J)) GOTO 10
      ENDDO
      NEMOCK = -2
      GOTO 100
10    ENDDO

      NEMOCK = 0

C  EXIT
C  ----

100   RETURN
      END
