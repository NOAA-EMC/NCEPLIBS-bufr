C> @file
C> @brief Capitalize a character string
C>
C> @author J. Woollen @date 2002-05-14

C> This subroutine capitalizes all of the alphabetic characters in
C> a string.  The string is modified in place.
C>
C> @param[in,out] STR -- character*(*): String
C>
C> @author J. Woollen @date 2002-05-14
      SUBROUTINE CAPIT(STR)

      CHARACTER*(*) STR
      CHARACTER*26 UPCS,LWCS
      DATA UPCS/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LWCS/'abcdefghijklmnopqrstuvwxyz'/

      DO 20 I=1,LEN(STR)
      DO 10 J=1,26
      IF(STR(I:I).EQ.LWCS(J:J)) THEN
         STR(I:I) = UPCS(J:J)
         GOTO 20
      ENDIF
10    CONTINUE
20    CONTINUE

      RETURN
      END
